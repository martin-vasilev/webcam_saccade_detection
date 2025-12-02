library(dplyr)
library(readr)

dat<- read_csv('data/webcam_data.zip')

# Take just one trial:
d<- subset(dat, sub==11 & Trial_Id==60)
d<- d %>% filter(el_pupil>0 & conf>0)

### IV-T:

deg_x<- 0.0187
deg_y<- 0.0192

# Get pixel coordinates:
x= d$el_x 
y= d$el_y

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
dat_all <- dat %>%
  filter(el_pupil > 0, conf > 0) %>%
  filter(x> 0 & x < 1920 & y>0 & y<1080)%>%
  group_by(sub, Trial_Id) %>%
  arrange(time_start, .by_group = TRUE) %>%
  mutate(
    # Time in seconds
    t_sec = time_start / 1000,
    
    # Pixel vectors
    x_px = c(NA, diff(el_x)),
    y_px = c(NA, diff(el_y)),
    
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


fix_data<- dat_all %>% 
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
  
  
  



