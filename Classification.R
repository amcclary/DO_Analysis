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

ps_data<-read.csv("PoolStudy_Summary.csv")
#Filter data to remove null values
ps_data<-filter(ps_data, !is.na(SiteCode))

#Create categorical variable for DO
ps_data$DOLevelMin[ps_data$Min_DO<6]<-"Bad"
ps_data$DOLevelMin[ps_data$Min_DO>=6]<-"Good"
ps_data$DOCode[ps_data$Min_DO<6]<-0
ps_data$DOCode[ps_data$Min_DO>=6]<-1

ps_data$DOLevelMin<-factor(ps_data$DOLevelMin, levels = c("Good", "Bad"))
ps_data$PoolStudyDate<-as.Date(ps_data$PoolStudyDate)
ps_data$DaysSinceStart<-ps_data$PoolStudyDate-min(ps_data$PoolStudyDate)
ps_data$JulianDate <- yday(ps_data$PoolStudyDate)
#Makes sure that R knows that Sample Number is a category
ps_data$SampleNumber<-factor(ps_data$SampleNumber)
#add ordering to DOLevel
ps_data$DOLevel<-factor(ps_data$DOLevel, levels = c("Critical (<3)","Poor (>3 & <5)", "OK (>5 & <7)", "Good (>7)"))
#Get rid of any nulls
ps_data_CART<-ps_data%>%filter(!is.na(DOLevelMin)& !is.na(RiffleCrestThalweg) & !is.na(Site) &!is.na(ReachType) & !is.na(SampleNumber))
#Sort by DO Level
ps_data_CART$Discharge_cfs<-ds_data_CART$Discharge_cfs
ps_data_CART$Discharge_cfs[is.na(ps_data_CART$Discharge_cfs)]<-0

#Simple linear regression

predictorVarables<-ps_data_CART%>%select(RiffleCrestThalweg, JulianDate, Volume, Discharge_cfs, PoolTailCrest, PoolArea, Average_DO, RiffleArea, MaxDepth, Max_Temperature, Average_Temperature)
predictorVarables$Discharge_cfs[is.na(predictorVarables$Discharge_cfs)]<-0

corrplot(cor(predictorVarables), order="hclust", type = "upper")

lm1<-lm(Min_DO ~ RiffleCrestThalweg + ReachType, data = ps_data_CART )

summary(lm1)
ps_data_CART<-ps_data_CART%>%filter(Site!='PS-DUT-P3')

#Create classification tree model
m1.Binary.DO_RCT_JD_RT_VO<-rpart(DOLevelMin ~ RiffleCrestThalweg  + JulianDate + ReachType + Volume, method = "class", data = ps_data_CART, minbucket = 4)

#Graph
prp(m1.Binary.DO_RCT_JD_RT_VO, 
    #Shows bins on both sides of Split
    type = 2, 
    #Shows number of samples of each category within each grouping
    extra = 2, 
    
    #prevents the categories from extending to the bottom
    fallen.leaves = T, 
    varlen = 0,
    #faclen = 0,
    #Makes text larger 
    cex = 1,  
    #left = FALSE,
    #Set colors for each category
    box.palette = "GnRd",
    #box.palette = list("red", "light green"), 
    #Makes legend bigger
    legend.cex = 1,
    space = 0.5,
    round = 0,
    split.space = .0,
    split.yspace = 0)

m2.Binary.DO_RCT_JD_RT<-rpart(DOLevelMin ~ RiffleCrestThalweg  + JulianDate + ReachType , method = "class", data = ps_data_CART, minbucket = 4)

#Graph
prp(m2.Binary.DO_RCT_JD_RT, 
    #Shows bins on both sides of Split
    type = 2, 
    #Shows number of samples of each category within each grouping
    extra = 2, 
    
    #prevents the categories from extending to the bottom
    fallen.leaves = T, 
    varlen = 0,
    faclen = 0,
    #Makes text larger 
    cex = .8,  
    #left = FALSE,
    #Set colors for each category
    box.palette = "GnRd",
    #box.palette = list("red", "light green"), 
    #Makes legend bigger
    legend.cex = 1,
    space = 0.2,
    round = 0,
    split.space = .1,
    split.yspace = 0)


#Create random forest model

rf_m1<-randomForest(DOLevelMin ~ RiffleCrestThalweg  + JulianDate + ReachType + Volume + Site + Tributary + MaxDepth + WaterTemp + Discharge_cfs, data = ps_data_CART, ntree = 10000, importance=TRUE)
varImpPlot(rf_m1)

#Logistic Regression
m2.LR<-glm(DOCode ~ RiffleCrestThalweg, data = ps_data, family = binomial)
summary(m2.LR)
ggplot(ps_data, aes(x=RiffleCrestThalweg, y = DOCode)) + geom_point() + stat_smooth(method = "glm", method.args=list(family="binomial"), se = F)
