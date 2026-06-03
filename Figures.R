
library(readr)
library(tidyverse)
mcc <- read_csv("results/all_mcc_results.csv")


IVT<-  mcc%>%
  filter(method== "IVT")%>%
   ggplot(aes(x= parameter_value, y= MCC,
              colour= smoothing, linetype= smoothing))+
   facet_wrap(~event)+
   geom_line(linewidth= 1.2)+ggtitle("I-VT")+
  theme_bw(18)+
  ylim(0, 0.4)+
  xlab("Velocity threshold (deg/s)")

IDT<-  mcc%>%
  filter(method== "IDT")%>%
  ggplot(aes(x= parameter_value, y= MCC,
             colour= smoothing, linetype= smoothing))+
  facet_wrap(~event)+
  geom_line(linewidth= 1.2)+ggtitle("I-DT")+
  theme_bw(18)+
  ylim(-0.1, 0.4)+
  xlab("Dispersion threshold (deg)")

EK<-  mcc%>%
  filter(method== "EK03")%>%
  ggplot(aes(x= parameter_value, y= MCC,
             colour= smoothing, linetype= smoothing))+
  facet_wrap(~event)+
  geom_line(linewidth= 1.2)+ggtitle("Engbert & Kliegl (2003)")+
  theme_bw(18)+
  ylim(-0.1, 0.4)+
  xlab("Velocity threshold (lambda)")



IHMM<-  mcc%>%
  filter(method== "IHMM")%>%
  ggplot(aes(x= smoothing, y= MCC,
             colour= smoothing, group=1))+
  facet_wrap(~event)+
  geom_point(size= 3)+
  geom_line(linewidth= 1.2)+
  ggtitle("I-HMM")+
  theme_bw(18)+
  ylim(-0.1, 0.4)+
  xlab("Smoothing method")


library(patchwork)

figure<- (IVT+IDT)/(EK+ IHMM)
