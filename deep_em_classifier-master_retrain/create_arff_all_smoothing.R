library(readr)
library(dplyr)
library(eyemovements)
library(signal)

# Set project paths
deep_dir <- "/Users/pci/deep_em_classifier-master"

input_file <- "/Users/pci/deep_em_classifier-master/webcam_saccade_detection/data/manual_labels/webdata_manual_labels.csv"

# Read webcam gaze data with ground truth labels
dat <- readr::read_csv(input_file, show_col_types = FALSE)

# Smoothing methods
smooth_methods <- c("raw", "mean", "median", "sg", "sg_p5_n23")

# Smoothing parameters
mean_window <- 3
median_window <- 3
sg_p <- 3
sg_n <- 7
sg_p5 <- 5
sg_n23 <- 23

# Standardize column names to match original CreateArff format
# Force using within-trial relative time (time_start) instead of Unix timestamp
if ("time_start" %in% names(dat)) {
  dat$time <- dat$time_start
} else {
  warning("Column 'time_start' not found, falling back to 'time' column.")
}

if (!"xPos" %in% names(dat) && "x" %in% names(dat)) {
  dat$xPos <- dat$x
}

if (!"yPos" %in% names(dat) && "y" %in% names(dat)) {
  dat$yPos <- dat$y
}

if (!"item" %in% names(dat) && "Trial_Id" %in% names(dat)) {
  dat$item <- dat$Trial_Id
}

if (!"cond" %in% names(dat)) {
  dat$cond <- 0
}

# Replace missing values, but do not delete samples
dat$xPos[is.na(dat$xPos)] <- 0
dat$yPos[is.na(dat$yPos)] <- 0

# Webcam confidence is continuous (algorithm self-rating), not binary like EyeLink.
# Model was trained with binary confidence (0/1), so set all to 1 (normal tracking).
dat$conf <- 1

# Check ground-truth labels
if (!"ground_truth" %in% names(dat)) {
  stop("Column 'ground_truth' is missing. Please provide EyeLink-based labels.")
}

# Convert labels to numeric (matching model's class numbering)
#  0 = UNKNOWN, 1 = FIXATION, 2 = SACCADE, 3 = SP (smooth pursuit), 4 = NOISE
dat$label_num <- dplyr::case_when(
  dat$ground_truth == "fixation" ~ 1L,
  dat$ground_truth == "saccade"  ~ 2L,
  dat$ground_truth == "pso"      ~ 3L,
  dat$ground_truth == "blink"    ~ 4L,
  is.na(dat$ground_truth)        ~ 0L,
  dat$ground_truth == "unclear"  ~ 0L,
  TRUE                           ~ 0L
)

# Smooth one trial
smooth_trial <- function(m, smooth) {
  
  # Do not delete samples; only smooth x/y coordinates
  
  n_orig <- nrow(m)
  
  if (n_orig < 5) {
    return(m)
  }
  
  if (smooth == "raw") {
    
    return(m)
    
  } else if (smooth == "mean") {
    
    d_tmp <- data.frame(
      x      = m$xPos,
      y      = m$yPos,
      time   = m$time,
      GT     = m$ground_truth,
      stringsAsFactors = FALSE
    )
    
    d_tmp <- eyemovements::SmoothSamples(
      d_tmp,
      method      = "Mean",
      window_size = mean_window
    )
    
    # Validate: fall back to raw if row count changed or NA introduced
    if (nrow(d_tmp) == n_orig && !anyNA(d_tmp$x) && !anyNA(d_tmp$y)) {
      m$xPos <- d_tmp$x
      m$yPos  <- d_tmp$y
    } else {
      warning("Mean smoothing produced NA or changed row count (", n_orig, " -> ", nrow(d_tmp),
              "), falling back to raw for this trial.")
    }
    
  } else if (smooth == "median") {
    
    d_tmp <- data.frame(
      x      = m$xPos,
      y      = m$yPos,
      time   = m$time,
      GT     = m$ground_truth,
      stringsAsFactors = FALSE
    )
    
    d_tmp <- eyemovements::SmoothSamples(
      d_tmp,
      method      = "Median",
      window_size = median_window
    )
    
    if (nrow(d_tmp) == n_orig && !anyNA(d_tmp$x) && !anyNA(d_tmp$y)) {
      m$xPos <- d_tmp$x
      m$yPos  <- d_tmp$y
    } else {
      warning("Median smoothing produced NA or changed row count (", n_orig, " -> ", nrow(d_tmp),
              "), falling back to raw for this trial.")
    }
    
  } else if (smooth == "sg") {
    
    if (n_orig >= sg_n) {
      x_smoothed <- signal::sgolayfilt(m$xPos, p = sg_p, n = sg_n)
      y_smoothed <- signal::sgolayfilt(m$yPos, p = sg_p, n = sg_n)
      
      # SG filter may produce NA at edges; fall back to raw for those
      if (length(x_smoothed) == n_orig && !anyNA(x_smoothed)) {
        m$xPos <- x_smoothed
      }
      if (length(y_smoothed) == n_orig && !anyNA(y_smoothed)) {
        m$yPos <- y_smoothed
      }
    }
  } else if (smooth == "sg_p5_n23") {
    
    if (n_orig >= sg_n23) {
      x_smoothed <- signal::sgolayfilt(m$xPos, p = sg_p5, n = sg_n23)
      y_smoothed <- signal::sgolayfilt(m$yPos, p = sg_p5, n = sg_n23)
      
      # SG filter may produce NA at edges; fall back to raw for those
      if (length(x_smoothed) == n_orig && !anyNA(x_smoothed)) {
        m$xPos <- x_smoothed
      }
      if (length(y_smoothed) == n_orig && !anyNA(y_smoothed)) {
        m$yPos <- y_smoothed
      }
    }
  }
  
  return(m)
}

# Create ARFF function
CreateArff <- function(dat = NULL, output_dir = "preproc/output", smooth = "raw") {
  
  # Check input
  if (is.null(dat)) {
    stop("dat is NULL")
  }
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Loop over subjects and trials
  nsubs <- unique(dat$sub)
  
  for (i in seq_along(nsubs)) {
    
    n <- subset(dat, sub == nsubs[i])
    nitems <- unique(n$item)
    
    for (j in seq_along(nitems)) {
      
      m <- subset(n, item == nitems[j])
      
      if (nrow(m) == 0) next
      
      # Arrange samples by time
      m <- m %>%
        dplyr::arrange(time)
      
      # Apply smoothing within trial
      m <- smooth_trial(m, smooth = smooth)
      
      filename <- paste0(
        output_dir,
        "/S", m$sub[1],
        "_E", m$cond[1],
        "I", m$item[1],
        "D0.arff"
      )
      
      fileConn <- file(filename)
      
      # ARFF header
      meta <- c(
        "%@METADATA width_px 1920.0",
        "%@METADATA height_px 1080.0",
        "%@METADATA width_mm 540.0",
        "%@METADATA height_mm 254.0",
        "%@METADATA distance_mm 620.0",
        "@RELATION gaze_labels",
        "",
        "@ATTRIBUTE time INTEGER",
        "@ATTRIBUTE x NUMERIC",
        "@ATTRIBUTE y NUMERIC",
        "@ATTRIBUTE confidence NUMERIC",
        "@ATTRIBUTE handlabeller1 INTEGER",
        "@ATTRIBUTE handlabeller2 INTEGER",
        "@ATTRIBUTE handlabeller_final INTEGER",
        "",
        "@DATA"
      )
      
      write_dat <- character(0)
      
      # Normalize time to start at zero; convert ms -> microseconds
      start <- m$time[1]
      
      for (k in 1:nrow(m)) {
        
        lab <- m$label_num[k]
        # If label is still NA after mapping, treat as UNKNOWN (0)
        if (is.na(lab)) lab <- 0L
        
        # ARFF time in microseconds (integer); round to handle floating time_start
        t_us <- round((m$time[k] - start) * 1000)
        
        string <- paste(
          t_us, ",",
          m$xPos[k], ",",
          m$yPos[k], ",",
          m$conf[k], ",",
          lab, ",", lab, ",", lab,
          sep = ""
        )
        
        write_dat <- c(write_dat, string)
      }
      
      writeLines(c(meta, write_dat), fileConn)
      close(fileConn)
    }
  }
}

# Check original label distribution
table(dat$ground_truth, useNA = "ifany")

# Check how labels will be mapped
dat_check <- dat %>%
  mutate(
    label_num = case_when(
      ground_truth == "fixation" ~ 1L,
      ground_truth == "saccade"  ~ 2L,
      ground_truth == "pso"      ~ 3L,
      ground_truth == "blink"    ~ 4L,
      is.na(ground_truth)        ~ 0L,
      ground_truth == "unclear"  ~ 0L,
      TRUE                       ~ 0L
    )
  )

table(dat_check$ground_truth, dat_check$label_num, useNA = "ifany")

# Generate ARFF files for each smoothing method
for (smooth in smooth_methods) {
  
  if (smooth == "raw") {
    output_dir <- file.path(deep_dir, "preproc", "output_raw")
  } else if (smooth == "mean") {
    output_dir <- file.path(deep_dir, "preproc", "output_mean_w3")
  } else if (smooth == "median") {
    output_dir <- file.path(deep_dir, "preproc", "output_median_w3")
  } else if (smooth == "sg") {
    output_dir <- file.path(deep_dir, "preproc", "output_sg_p3_n7")
  } else if (smooth == "sg_p5_n23") {
    output_dir <- file.path(deep_dir, "preproc", "output_sg_p5_n23")
  }
  
  # Remove old ARFF files before regenerating
  if (dir.exists(output_dir)) {
    unlink(list.files(output_dir, full.names = TRUE, pattern = "\\.arff$"))
  }
  
  # Generate ARFF files
  CreateArff(dat, output_dir = output_dir, smooth = smooth)
  
  # Inspect one generated file
  f <- list.files(output_dir, full.names = TRUE, pattern = "\\.arff$")[1]
  
  cat("\n==============================\n")
  cat("Smoothing method:", smooth, "\n")
  cat("Output folder:", output_dir, "\n")
  cat("Example file:", f, "\n")
  cat("Number of generated ARFF files:", length(list.files(output_dir, pattern = "\\.arff$")), "\n")
  cat("==============================\n")
}