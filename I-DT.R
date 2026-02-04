

library(dplyr)
library(readr)

dat<- read_csv('data/webcam_data.zip')

d<- subset(dat, sub== 11 & Trial_Id== 14 & Task_Name== 'Single_line_sentences')

deg_x<- 0.0187
deg_y<- 0.0192

### I-DT:

trial_finished= FALSE
window_threshold<- 100 # in ms
disp_threshold<- 3

x_window<- NULL
y_window<- NULL
start<- 1
end<- NULL


fix<- NULL

while(trial_finished== FALSE){
  
  # initiating algorithm, take first window:
  if(length(x_window)== 0 & length(y_window)== 0 ){
    
    if(nrow(d)>=window_threshold){
      
      # find end of the window:
      start_time<- d$time_start[1]
      end<- max(which(d$time_start<= window_threshold+start_time))
      
      x_window<- d$x[start:end]
      y_window<- d$y[start:end]
      
    }else{
      trial_finished<- TRUE
      message("Not enough samples in current trial- proceeding to next one!")
    }
    
  }
  
  # check dispersion within the current window:
  D = (max(x_window) - min(x_window))*deg_x +
      (max(y_window) - min(y_window))*deg_y
  
  # check if window exceeds dispersion threshold:
  if(D>disp_threshold){ 
    t<- data.frame('SFIX'= d$time_start[start], 
                   'EFIX'= d$time_start[end],
                   'x'= mean(x_window, na.rm= T),
                   'y'= mean(y_window, na.rm= T),
                   'FixDur'= d$time_start[end]- d$time_start[start])
    # save fixation in data frame
    fix<- rbind(fix, t)
    
    # Take a new window of samples:
    start<- end+1
    
    start_time<- d$time_start[start]
    end<- max(which(d$time_start<= window_threshold+start_time))
    
    if(start> nrow(d) | end> nrow(d) ){
      trial_finished= TRUE
    }else{
      
      x_window<- d$x[start:end]
      y_window<- d$y[start:end]
    }
    

    
  }else{
    
    # increment window by 1 sample sample to the right:
    end<- end+1
    
    if(end<= nrow(d)){
      x_window<- d$x[start:end]
      y_window<- d$y[start:end]
    }else{
      trial_finished= TRUE
    }
  }
  
  
  
}







