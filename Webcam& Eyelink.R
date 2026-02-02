
rm(list= ls())

library(dplyr)
library(readr)
library(ggplot2)
library(data.table)


if(!file.exists('data/webcam_data_ground_truth.csv')){
  
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
  
  cat(sprintf("Subject: "))
  
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
    
    cat(sprintf("%g ", i))
    
  }
  
  write.csv(dat_new, file = 'data/webcam_data_ground_truth.csv')
  
  
  
}else{
  dat<- read_csv('data/webcam_data_ground_truth.csv')
}





