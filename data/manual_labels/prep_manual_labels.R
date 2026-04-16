
library(readr)
sub11 <- read_csv("data/manual_labels/sub11.csv")
sub11$subject<-11
sub13 <- read_csv("data/manual_labels/sub13.csv")
sub13$subject<-13

dat<- rbind(sub11, sub13)

# need to map blinks to labels

write.csv(dat,'data/manual_ground_truth.csv')
