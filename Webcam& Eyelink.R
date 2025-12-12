library(dplyr)
library(readr)
library(ggplot2)
library(data.table)

fix <- read_csv('data/eyelink_fix_data.csv')
dat<- read_csv('data/webcam_data.zip')


# increment single-item numbers by 100:
which_rows<- which(dat$Task_Name== "Single_line_sentences")
dat$Trial_Id[which_rows]<- dat$Trial_Id[which_rows]+100 

which_rows2<- which(fix$task== "sentence_DC")
fix$item[which_rows2]<- fix$item[which_rows2]+100 


nsubjects<- unique(dat$sub)

dat$ground_truth<- NA
dat$before_1stfix<- NA

dat_new<- NULL

for(i in 1:length(nsubjects)){
  n<- subset(dat, sub== nsubjects[i])
  
  nitems<- unique(n$Trial_Id)
  
  for(j in 1:length(nitems)){
    m<- subset(n, Trial_Id== nitems[j])
    
    ## Eyelink fixation data for that trial:
    el_fix<- subset(fix, sub== nsubjects[i] & item== nitems[j])
    
    if(nrow(el_fix)<1){
      next
    }
    
    for(k in 1:nrow(m)){
      
      if(!is.na(m$el_time[k])){
        # is the sample before the first fixation identified by Eyelink:
        if(m$el_time[k]< el_fix$s_time[1]){
          m$before_1stfix[k]<- 1
        }
      }
      
      row<- which(m$el_time[k]>= el_fix$s_time & m$el_time[k]<= el_fix$e_time)
      
      if(is.na(m$before_1stfix[k])){
        if(length(row)>0){
          m$ground_truth[k]<- 'fixation' 
        }else{

          m$ground_truth[k]<- 'saccade' 
        }
      }
    
    }
    
    dat_new<- rbind(dat_new, m)
   
    
     
  }
  
}





## Webcam
# Take just one trial:
d<- subset(dat, sub==11 & Trial_Id==60)
d<- d %>% filter(el_pupil>0 & conf>0)

### IV-T:

deg_x<- 0.0187
deg_y<- 0.0192

# Get pixel coordinates:
x= d$x 
y= d$y

# get time
t= d$time_start/1000 # turn into seconds

# 1. pixel vectors:
x_px<- diff(x, 1)
y_px<- diff(y, 1)


# 2. Convert vectors to degree per visual angle:
x_deg<- x_px*deg_x
y_deg<- y_px*deg_y

# 3. Time difference between consecutive samples (in seconds):
d_t<- diff(t,1)

# 4. Calculate vector velocities in deg/s
v_x<- x_deg/d_t
v_y<- y_deg/d_t

# 5. calculate magnitude of movement in x, y:
m<- sqrt(v_x^2+ v_y^2)

# 6. Velocity magnitude in deg/s
d$vel_mag <- c(NA, m)   

# 7. Velocity-threshold
v_thr <- 50   

# 8. Classify the point as saccade point
d$is_saccade <- d$vel_mag >= v_thr   # TRUE = saccade, FALSE = non-saccade

# 9. Plot
plot(t, c(NA, m), type = "l",
     xlab = "Time (s)", ylab = "Velocity (deg/s)")
abline(h = v_thr, lty = 2)  

# All trials:
dat_web <- dat %>%
  filter(el_pupil > 0, conf > 0) %>%
  filter(x> 0 & x < 1920 & y>0 & y<1080)%>%
  group_by(sub, Task_Name, Trial_Id) %>%
  arrange(time_start, .by_group = TRUE) %>%
  mutate(
    # Time in seconds
    t_sec = time_start / 1000,
    
    # Pixel vectors
    x_px = c(NA, diff(x)),
    y_px = c(NA, diff(y)),
    
    # Convert vectors to degree per visual angle
    x_deg = x_px * deg_x,
    y_deg = y_px * deg_y,
    
    # Time difference in sec
    dt = c(NA, diff(t_sec)),
    
    # Calculate vector velocities in deg/s
    v_x = x_deg / dt,
    v_y = y_deg / dt,
    
    # Magnitude of movement in x, y
    vel_mag = sqrt(v_x^2 + v_y^2),
    
    # Saccade classification
    is_saccade = vel_mag >= v_thr
  ) %>%
  ungroup()


fix_data<- dat_web %>% 
  filter(!is.na(is_saccade))%>%
  group_by(sub, Task_Name, Trial_Id)%>%
  mutate(fixation_group = cumsum(
    is_saccade != lag(is_saccade, default = TRUE) &
      is_saccade == FALSE
  ))%>%
  filter(is_saccade==F)

fix_data2<- fix_data %>%
  group_by(sub, Task_Name, Trial_Id, fixation_group)%>%
  summarise(start_time= min(time_start), 
            end_time= max(time_start),
            fix_dur= end_time- start_time,
            x= mean(x), 
            y= mean(y),
            avg_vel= mean(vel_mag),
            n_samples= n()) %>%
  filter(n_samples>1)

length(which(fix_data2$fix_dur==0))/nrow(fix_data2)

mean(fix_data2$fix_dur)
sd(fix_data2$fix_dur)

## Eyelink
# Take just one trial:
d_el <- dat %>%
  filter(sub == 11,
         Trial_Id == 60)
d_el <- d_el %>%
  filter(el_pupil > 0 & conf>0)

### IV-T:
deg_x_el <- 0.0187
deg_y_el <- 0.0192

# Eyelink coordinate
x_el <- d_el$el_x
y_el <- d_el$el_y
t_el <- d_el$el_time / 1000

# 1. pixel vectors:
x_px_el <- diff(x_el)
y_px_el <- diff(y_el)

# 3. Time difference between consecutive samples (in seconds):
x_deg_el <- x_px_el * deg_x_el
y_deg_el <- y_px_el * deg_y_el

# Time differences
d_t_el <- diff(t_el)

# Velocity components
v_x_el <- x_deg_el / d_t_el
v_y_el <- y_deg_el / d_t_el

# Velocity magnitude
vel_mag_el <- sqrt(v_x_el^2 + v_y_el^2)

# 6. Velocity magnitude in deg/s
d_el$vel_mag_el <- c(NA, vel_mag_el)

# 7. Velocity-threshold
v_thr_el <- 50  

# 9. Plot
plot(t_el, d_el$vel_mag_el,
     type = "l",
     xlab = "Time (s)",
     ylab = "Velocity (deg/s)",
     main = "Eyelink Velocity Over Time (Fixed Threshold)")

abline(h = v_thr_el, lty = 2)

# All trials:
dat_el <- dat %>%
  filter(el_pupil > 0 & conf > 0) %>%
  group_by(sub, Task_Name, Trial_Id) %>%
  arrange(el_time, .by_group = TRUE) %>%   
  mutate(
    # Eyelink time in seconds
    t_el = el_time / 1000,
    
    # Pixel displacement
    x_px = el_x - lag(el_x),
    y_px = el_y - lag(el_y),
    
    # Convert to degrees
    x_deg = x_px * deg_x_el,
    y_deg = y_px * deg_y_el,
    
    # Time difference
    d_t = t_el - lag(t_el),
    
    # Velocities (deg/s)
    v_x = x_deg / d_t,
    v_y = y_deg / d_t,
    
    # Velocity magnitude
    vel_mag_el = sqrt(v_x^2 + v_y^2),
    
    # Saccade classification
    is_saccade_el = vel_mag_el >= v_thr_el
  ) %>%
  ungroup()

# Eyelink variability
el_var <- dat_el %>%
  group_by(sub) %>% 
  summarise(
    # Standard deviation of horizontal velocity and vertical velocity (deg/s)
    sd_x = sd(v_x, na.rm = TRUE),          
    sd_y = sd(v_y, na.rm = TRUE),            
    sd_total = sqrt(sd_x^2 + sd_y^2)            
  ) %>%
  ungroup()

## Webcam variability 
web_var <- dat_web %>%
  group_by(sub) %>%
  summarise(
    sd_x = sd(v_x, na.rm = TRUE),
    sd_y = sd(v_y, na.rm = TRUE),
    sd_total = sqrt(sd_x^2 + sd_y^2)
  ) %>%
  ungroup()

# Label the source of each dataset
el_var$source  <- "Eyelink"
web_var$source <- "Webcam"

# Combine Eyelink and Webcam variability into one dataframe
var_all <- bind_rows(el_var, web_var)


ggplot(var_all, aes(x = source, y = sd_total, fill = source)) +
  geom_boxplot(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Velocity Variability: Eyelink vs Webcam",
    y = "SD of velocity (deg/s)"
  )





fix <- read_csv("data/eyelink_fix_data.csv") %>%
  rename(Trial_Id = item)   

# Convert both datasets to data.table
dt_samples <- as.data.table(dat_el)
dt_fix     <- as.data.table(fix)

#Treat each sample as a degenerate interval [el_time, el_time]
dt_samples[, `:=`(
  start = el_time,
  end   = el_time
)]
#Set keys for interval matching
setkey(dt_samples, sub, Trial_Id, start, end)

#Treat each fixation as an interval [s_time, e_time]
dt_fix[, `:=`(
  start = s_time,
  end   = e_time
)]
setkey(dt_fix, sub, Trial_Id, start, end)

# Determine whether each sample falls inside a fixation
dt_joined <- foverlaps(
  x = dt_samples,
  y = dt_fix,
  by.x = c("sub", "Trial_Id", "start", "end"),
  by.y = c("sub", "Trial_Id", "start", "end"),
  type   = "within",   
  nomatch = NA        
)

# Assign labels: fixation vs saccade
dt_joined[, ground_truth := ifelse(!is.na(s_time), "fixation", "saccade")]

dat_el_gt <- as_tibble(dt_joined)

