
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

# 3. calculate magnitude of movement in x, y:
d_t<- diff(t,1)

# 4. Calculate vector velocities in deg/s
v_x<- x_deg/d_t
v_y<- y_deg/d_t

# 5. calculate magnitude of movement in x, y:
m<- sqrt(v_x^2+ v_y^2)


