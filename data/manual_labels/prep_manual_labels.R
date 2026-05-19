
rm(list= ls())

# subject numbers to process:

subs<- c(11,13,14,18,19,20)
dat<- NULL
library(readr)

for(i in 1:length(subs)){
  sub<- read_csv(paste("data/manual_labels/sub",
                       toString(subs[i]), '.csv', sep= ''))
  sub$subject<- subs[i]
  dat<- rbind(dat, sub)
}

write.csv(dat,'data/manual_ground_truth.csv')

## map manual ground truth to sample data:

web <- read_csv('data/webcam_data_ground_truth.csv')

# subset data to subjects we have manual labels for:
web<- subset(web, is.element(sub, subs) & Task_Name== 'Single_line_sentences')
web$ground_truth<- NA
web$before_1stfix<- NULL

# map labels to data:

for(i in 1:nrow(web)){
  # take only events within this trial:
  trial<- dat[which(dat$subject== web$sub[i] & dat$trial== web$Trial_Id[i]-100),]
  
  # find which event belongs to sample:
  evnt<- which(trial$start_time<=web$el_time[i] & trial$end_time>= web$el_time[i])
  if(length(evnt)>0){
    web$ground_truth[i]<- trial$label[evnt]
  }
}

# need to map blinks to labels:
web$ground_truth[which(web$el_pupil==0)]<- "blink"

# save raw sample data with manual labels:
write.csv(web, 'data/manual_labels/webdata_manual_labels.csv')




