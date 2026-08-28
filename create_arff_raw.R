library(readr)
library(dplyr)

# Set project paths
deep_dir <- "/Users/pci/deep_em_classifier-master"

input_file <- "/Users/pci/webcam_saccade_detection/data/manual_labels/webdata_manual_labels.csv"

output_dir <- file.path(deep_dir, "preproc", "output")

# Read webcam gaze data with ground truth labels
dat <- readr::read_csv(input_file, show_col_types = FALSE)

CreateArff <- function(dat = NULL, output_dir = "preproc/output") {
  
  # Check input
  if (is.null(dat)) {
    stop("dat is NULL")
  }
  
  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Standardize column names to match original CreateArff format
  if (!"item" %in% names(dat) && "Trial_Id" %in% names(dat)) {
    dat$item <- dat$Trial_Id
  }
  
  if (!"xPos" %in% names(dat) && "x" %in% names(dat)) {
    dat$xPos <- dat$x
  }
  
  if (!"yPos" %in% names(dat) && "y" %in% names(dat)) {
    dat$yPos <- dat$y
  }
  
  if (!"time" %in% names(dat) && "time_start" %in% names(dat)) {
    dat$time <- dat$time_start
  }
  
  if (!"cond" %in% names(dat)) {
    dat$cond <- 0
  }
  
  # Replace missing values, but do not delete samples
  dat$xPos[is.na(dat$xPos)] <- 0
  dat$yPos[is.na(dat$yPos)] <- 0
  
  # Ignore confidence; keep ARFF confidence column as 1 for all samples
  dat$conf <- 1
  
  # Check ground-truth labels
  if (!"ground_truth" %in% names(dat)) {
    stop("Column 'ground_truth' is missing. Please provide EyeLink-based labels.")
  }
  
  # Convert labels to numeric
  # fixation = 1
  # saccade = 2
  # everything else = 0
  dat$label_num <- dplyr::case_when(
    dat$ground_truth == "fixation" ~ 1L,
    dat$ground_truth == "saccade"  ~ 2L,
    TRUE                           ~ 0L
  )
  
  # Loop over subjects and trials
  nsubs <- unique(dat$sub)
  
  for (i in seq_along(nsubs)) {
    
    n <- subset(dat, sub == nsubs[i])
    nitems <- unique(n$item)
    
    for (j in seq_along(nitems)) {
      
      m <- subset(n, item == nitems[j])
      
      if (nrow(m) == 0) next
      
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
      
      # Normalize time to start at zero
      start <- m$time[1] - 1
      
      for (k in 1:nrow(m)) {
        
        lab <- m$label_num[k]
        
        string <- paste(
          (m$time[k] - start) * 1000, ",",
          m$xPos[k], ",",
          m$yPos[k], ",",
          m$conf[k], ",",
          lab, ",", lab, ",", lab,
          sep = ""
        )
        
        write_dat <- c(write_dat, string)
      }
      
      writeLines(c(meta, write_dat, "%", "%", "%"), fileConn)
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
      TRUE                       ~ 0L
    )
  )

table(dat_check$ground_truth, dat_check$label_num, useNA = "ifany")

# Remove old ARFF files before regenerating
if (dir.exists(output_dir)) {
  unlink(list.files(output_dir, full.names = TRUE, pattern = "\\.arff$"))
}

# Generate ARFF files
CreateArff(dat, output_dir = output_dir)

# Inspect one generated file
f <- list.files(output_dir, full.names = TRUE, pattern = "\\.arff$")[1]
cat(f, "\n")
readLines(f, n = 50)

# Check number of generated ARFF files
length(list.files(output_dir, pattern = "\\.arff$"))