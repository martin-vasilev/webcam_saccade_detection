library(dplyr)
library(ggplot2)

rm(list= ls())


dat <- readr::read_csv('data/webcam_data_ground_truth.csv')

# Clean data:
dat<- subset(dat,conf>0 & el_pupil>0)# remove blinks:

dat<- dat%>%
  group_by(sub, Trial_Id) %>%
  mutate(n= n()) %>% ungroup()

dat<-   subset(dat, n> 23)

dat<- dat%>% 
  group_by(sub, Trial_Id) %>%
  mutate(x_ma= SMA(x, n=5), 
         y_ma= SMA(y, n=5),
         x_sg= sgolayfilt(x = x, p = 5, n = 23),
         y_sg= sgolayfilt(x = y, p = 5, n = 23), 
         x_median5 = runmed(x, k = 5),
         y_median5 = runmed(y, k = 5))%>% 
  ungroup()


# get ground truth:
#GT <- read_csv("data/webcam_data_ground_truth.csv")


smooth_methods <-  c("el", "raw", "ma", "median", "sg")

vel_thresholds <- seq(20, 70, by = 5)

min_durations <- c(50, 100)

disp_thresholds <- seq(1, 8, by = 0.5)

get_xy <- function(data, smooth){
  
  if(smooth == "raw"){
    x_use <- data$x
    y_use <- data$y
  } else if(smooth == "ma"){
    x_use <- data$x_ma
    y_use <- data$y_ma
  } else if(smooth == "median"){
    x_use <- data$x_median5
    y_use <- data$y_median5
  } else if(smooth == "sg"){
    x_use <- data$x_sg
    y_use <- data$y_sg
  }else if(smooth == "el"){
    x_use <- data$el_x
    y_use <- data$el_y
  }
  
  return(data.frame(time = data$time_start,
                    x = x_use,
                    y = y_use,
                    GT= data$ground_truth))
}

# I-VT
I_VT_results <- data.frame()

for(s in smooth_methods){
  
  for(v in vel_thresholds){
    
    for(d in min_durations){
      
      cat("Running I-VT:", s, v, d, "\n")
      
      temp <- dat %>%
        group_by(sub, Trial_Id) %>%
        group_modify(~ {
          
          df <- get_xy(.x, s)
          
          df2= DetectFixations(
            data = df,
            method = "I-VT",
            vel_threshold = v,
            min_fix_dur = d
          ) 
          
          df2$acc<- NA
          
          for(i in 1:nrow(df2)){
            nrows<- which(df$time>= df2$fix_start[i] & df$time<= df2$fix_end[i])
            df2$acc[i]<- (length(which(df[nrows, 'GT']== "fixation"))/ length(nrows))*100
          }
          
          df2
          
        }) %>%
        ungroup() %>%
        mutate(method = "I-VT",
               smoothing = s,
               vel_threshold = v,
               min_dur = d)
      
      I_VT_results <- bind_rows(I_VT_results, temp)
    }
  }
}

I_VT_results %>%
  group_by(smoothing)%>%
  summarise(M_acc= mean(acc, na.rm=T), 
            SD_acc= sd (acc, na.rm=T)) 

df<-I_VT_results %>%
  group_by(sub, Trial_Id, smoothing)%>%
  summarise(n= n(), M_dur= mean(fix_dur), sd= sd(fix_dur),
            M_acc= mean(acc, na.rm=T))%>% ungroup()
  
df %>%
  group_by(smoothing)%>%
  summarise(n= mean(n), M_dur= mean(M_dur), sd= sd(sd, na.rm=T),
            M_acc= mean(M_acc, na.rm=T))


s<- subset(I_VT_results, fix_dur<=1000)

I_VT_results %>%
#  filter(fix_dur<=3000)%>%
  ggplot(aes(x= log(fix_dur), fill = smoothing))+
  geom_density(alpha= 0.8)+
  facet_wrap(~ smoothing)

I_VT_results %>%
  #  filter(fix_dur<=3000)%>%
  ggplot(aes(x= acc, fill = smoothing))+
  geom_density(alpha= 0.8)+
  facet_wrap(~ smoothing)



####### I-VT summary table and visualization #######

# Aggregate fixation-level results into parameter-level summaries
IVT_table <- I_VT_results %>%
  group_by(smoothing, min_dur, vel_threshold) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    sd_acc   = sd(acc, na.rm = TRUE),
    n        = sum(!is.na(acc)),
    se_acc   = sd_acc / sqrt(n),
    .groups = "drop"
  )
IVT_table$smoothing <- factor(
  IVT_table$smoothing,
  levels = c("el", "raw", "ma", "median", "sg")
)

# Create grouped bar plot
ggplot(IVT_table, aes(x = factor(min_dur), y = mean_acc, fill = factor(vel_threshold))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ smoothing) +
  labs(
    x = "Fixation duration (ms)",
    y = "Accuracy",
    fill = "Velocity threshold",
    title = "I-VT accuracy across fixation durations and velocity thresholds"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  )








# Plot(best parameter)
I_VT_results <- I_VT_results %>%
  mutate(param_combo = paste("vel=", vel_threshold, ", dur=", min_dur))

# Facet by smoothing method, different parameters distinguished by color
ggplot(I_VT_results, aes(x = log(fix_dur), color = factor(vel_threshold), 
                         linetype = factor(min_dur))) +
  geom_density(size = 0.8) +
  facet_wrap(~ smoothing) +
  scale_color_brewer(palette = "Set1", name = "Velocity\nthreshold") +
  scale_linetype_discrete(name = "Min duration\n(ms)") +
  labs(
    title = "IVT: Fixation Duration Distribution by Parameter Combination",
    x = "log(Fixation Duration)",
    y = "Density",
    caption = "More concentrated distribution = more stable fixation detection"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )


# Find the best parameter combination for each smoothing method (based on accuracy)
best_params <- I_VT_results %>%
  group_by(smoothing, vel_threshold, min_dur) %>%
  summarise(M_acc = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  group_by(smoothing) %>%
  slice_max(order_by = M_acc, n = 1)

# Keep only data with the best parameter combinations
best_data <- I_VT_results %>%
  inner_join(best_params, by = c("smoothing", "vel_threshold", "min_dur"))

# Plot comparing different smoothing methods with their best parameters
ggplot(best_data, aes(x = log(fix_dur), fill = smoothing)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ smoothing) + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "IVT: Fixation Duration Distribution - Best Parameters for Each Method",
    x = "log(Fixation Duration)",
    y = "Density",
    fill = "Smoothing\nmethod"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  







# I-DT

I_DT_results <- data.frame()

for(s in smooth_methods){
  for(disp in disp_thresholds){
    for(d in min_durations){
      
      cat("Running I-DT:", s, disp, d, "\n")
      
      temp <- dat %>%
        group_by(sub, Trial_Id) %>%
        group_modify(~ {
          
          df <- get_xy(.x, s)
          
          res <- tryCatch(
            DetectFixations(
              data = df,
              method = "I-DT",
              window_threshold = 100,
              disp_threshold = disp,
              min_fix_dur = d
            ),
            error = function(e) NULL
          )
          
          if (is.null(res) || !is.data.frame(res) || nrow(res) == 0) {
            return(data.frame())
          }
          
          res$acc <- NA
          for(i in 1:nrow(res)){
            nrows <- which(df$time >= res$fix_start[i] &
                             df$time <= res$fix_end[i])
            res$acc[i] <- (length(which(df[nrows,"GT"] == "fixation")) /
                             length(nrows)) * 100
          }
          
          return(res)
          
        }) %>%
        ungroup() %>%
        mutate(method = "I-DT",
               smoothing = s,
               disp_threshold = disp,
               min_dur = d)
      
      I_DT_results <- bind_rows(I_DT_results, temp)
    }
  }
}

# plot
ggplot(I_DT_results, aes(x = log(fix_dur), 
                         color = factor(disp_threshold), 
                         linetype = factor(min_dur))) +
  geom_density(size = 0.8) +
  facet_wrap(~ smoothing) +
  scale_color_brewer(palette = "Set1", name = "Dispersion\nthreshold") +
  scale_linetype_discrete(name = "Min duration\n(ms)") +
  labs(
    title = "IDT: Fixation Duration Distribution by Parameter Combination",
    x = "log(Fixation Duration)",
    y = "Density",
    caption = "More concentrated distribution = more stable fixation detection"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    legend.box = "vertical",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  )

# Find the best parameter combination for each smoothing method (based on accuracy)
best_params_IDT <- I_DT_results %>%
  group_by(smoothing, disp_threshold, min_dur) %>%
  summarise(M_acc = mean(acc, na.rm = TRUE), .groups = "drop") %>%
  group_by(smoothing) %>%
  slice_max(order_by = M_acc, n = 1)

# Keep only data with the best parameter combinations
best_data_IDT <- I_DT_results %>%
  inner_join(best_params_IDT, by = c("smoothing", "disp_threshold", "min_dur"))


# Plot comparing different smoothing methods with their best parameters
ggplot(best_data_IDT, aes(x = log(fix_dur), fill = smoothing)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ smoothing) + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "IDT: Fixation Duration Distribution - Best Parameters for Each Method",
    x = "log(Fixation Duration)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

####### I-DT summary table and visualization #######

# Aggregate fixation-level results into parameter-level summaries
IDT_table <- I_DT_results %>%
  group_by(smoothing, min_dur, disp_threshold) %>%
  summarise(
    mean_acc = mean(acc, na.rm = TRUE),
    sd_acc   = sd(acc, na.rm = TRUE),
    n        = sum(!is.na(acc)),
    se_acc   = sd_acc / sqrt(n),
    .groups = "drop"
  )
IDT_table$smoothing <- factor(
  IDT_table$smoothing,
  levels = c("el", "raw", "ma", "median", "sg")
)

# Create grouped bar plot
ggplot(IDT_table, aes(x = factor(min_dur), y = mean_acc, fill = factor(disp_threshold))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_acc - se_acc, ymax = mean_acc + se_acc),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(~ smoothing) +
  labs(
    x = "Fixation duration (ms)",
    y = "Accuracy",
    fill = "Dispersion threshold",
    title = "I-DT accuracy across fixation durations and dispersion thresholds"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank()
  )

