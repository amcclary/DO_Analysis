library ("xlsx")

allData<-read.xlsx("PS_HabSnorkelSum_2017.xlsx", "AllData")
write.csv(allData, "PoolStudy_Summary.csv")
