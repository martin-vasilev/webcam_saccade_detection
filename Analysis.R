
library(readr)

dat<- read_csv('data/webcam_data.zip')

# Take just one trial:
d<- subset(dat, sub==11 & Trial_Id==60)
d<- d %>% filter(el_pupil>0 & conf>0)

d$hz<- 1/(d$time_diff/1000) 
# d<- d %>% mutate(hz2 = hz + dplyr::lag(hz))


### IV-T:

deg_x<- 0.0187
deg_y<- 0.0192

# Eyelink:

# take x position vector
x= d$el_x 
time= d$time_diff[2:length(d$time_diff)]
time= time/1000

# calculate difference to next sample:
diff<- diff(x, 1)

# get absolute value of differences:
diff_a<- abs(diff)

diff_deg<- diff_a*deg_x
diff_deg/time


# vel[2:(length(x)-1)] <- abs(1000/2*(x[3:(length(x))] - x[1:(length(x)-2)])/deg)
# 
# 
# temp$sacc_peak<- max(abs(vel))
# temp$sacc_vel<- mean(abs(vel))
# temp$sacc_ampl<- abs((x[length(x)]- x[1])/deg)



