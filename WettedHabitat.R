library("xlsx")
library("tidyverse")
library("lme4")
library("MuMIn")
library("rlang")
library("rpart")
library("rpart.plot")
library("randomForest")
library("RColorBrewer")
library("lubridate")
library("corrplot")

wh_data<-read.csv("2017_WH_DOandTemperature.csv")
#Filter data to remove null values


wh_data<-wh_data %>% filter(!is.na(RiffleCrestThalweg)) %>% filter (RiffleCrestThalweg!= -9999) 

wh_data%>%ggplot(aes(x=RiffleCrestThalweg, y = DissolvedOxygen)) + 
  geom_point() +
  geom_hline(yintercept =6, linetype= "dashed", colour ="red") + 
  geom_rect(data=wh_data[1,],aes(xmin=-Inf, xmax = .25, ymin = -Inf, ymax = Inf), fill = "blue", colour = "blue", alpha=0.2)+
  theme_classic() + 
  xlab("Pool Tail Crest")

  