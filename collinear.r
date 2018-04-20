library(mctest)
library(ppcor)
library(FactoMineR)
library(factoextra)

ps <- read.csv('PoolStudy_Summary_clean.csv')

# convert date from factor to date object
ps$PoolStudyDate <- as.Date(ps$PoolStudyDate, format='%m/%d/%Y')

# add the sine and cosine of day of year as variables
ps$cosdoy <- yday(ps$PoolStudyDate) %>% "*"(2*pi/365) %>% cos %>% "*"(-1)
ps$sindoy <- yday(ps$PoolStudyDate) %>% "*"(2*pi/365) %>% sin

# convert day since start date as variable
ps$StudyDay <- as.numeric(ps$PoolStudyDate) - 
  min(as.numeric(as.Date(ps$PoolStudyDate)))

# remove rows/samples that don't have key data
ps <- filter(ps, !is.na(RiffleCrestThalweg) & !is.na(DissolvedOxygen) &
             !is.na(Min_DO) & !is.na(Discharge_cfs))

# remove variables that have a lot of missing data, creating reduced data set
ps.red <- ps[, is.na(ps) %>% colSums %>% "=="(0)]

# convert sample number and algae cover (?) to categorical variables
ps.red$SampleNumber <- as.factor(ps.red$SampleNumber)
ps.red$AlgaeCover <- as.factor(ps.red$AlgaeCover)

# remove year since only one year of data as well as other identifier variables
# that are unique to sample
var.rm <- c('SiteCode', 'TripID', 'HabitatID', 'Year', 'Crew', 'PoolStudyDate')
ps.red <- ps.red[,!names(ps.red)%in%var.rm]

# add DO threshold variable
ps.red$DOCode[ps.red$Min_DO<6]<-0
ps.red$DOCode[ps.red$Min_DO>=6]<-1
ps.red$DOCode <- as.factor(ps.red$DOCode)

# remove response variable(s) for PCA et al since goal is to reduce number of
# expanatory/predictor variables
var.rm <- c('Average_DO','Max_DO','Min_DO','DOCode', 'DissolvedOxygen')

ps.pred <- ps.red[, !names(ps.red)%in%var.rm]

# split into quantiative and qualitative dataframes
ps.quant <- ps.pred[, sapply(ps.pred, is.numeric)]
ps.quali <- ps.pred[, sapply(ps.pred, is.factor)]

#### Collinearity Analysis ####

# Run factor analysis predictor variables
ps.pca <- PCA(ps.quant, graph=F)
ps.mca <- MCA(ps.quali, graph=F)

# generate plots
if(F){
  fviz_pca_var(ps.pca, col.var="contrib", repel=T, title='PCA Results', 
               xlab='Component 1', ylab='Component 2',
               geom='text')+
    scale_color_gradient2(low="yellow", mid="orange",
                          high="red", name='Contributions') +
    theme_minimal()
  
  fviz_mca_var(ps.mca, repel=T, title='', cex.lab=1.5, cex=3,
               xlab='Component 1',
               ylab='Component 2',
               geom.var="text", choice='mca.cor')+
    scale_color_gradient2(low="yellow", mid="orange",
                          high="red", name='Contributions') +
    theme_minimal()
}

# partial correlation with pearson's r
ps.pcor <- pcor(ps.quant)
write.csv(ps.pcor$estimate, 'pcor_pearson.csv')
write.csv(ps.pcor$p.value, 'pcor_pvalue.csv')


ps.vif <- rep(NA,ncol(ps.pred)) # VIF within group
ps.aic <- rep(NA,ncol(ps.pred)) # AIC of [mixed] logistic regression on single predictor
ps.pval <- rep(NA,ncol(ps.pred)) # pvalue of logistic regression
ps.acc <- rep(NA,ncol(ps.pred)) # accuracy measure using 0.5 as threshold
ps.cor <- matrix(nrow=ncol(ps.pred), ncol=2) # Pearson for linear, Kendall for nonlinear
ps.chi <- rep(NA,ncol(ps.pred)) # chi-square p-value against DOCode for categorical

# perform a logistic regression of quantitative predictor vs DOCode
for(i in names(ps.quant)){
  str <- paste('x <- glm(ps.red$DOCode~ps.quant$',i,',family=binomial)',sep='')
  eval(parse(text=str))
  ind <- match(i,names(ps.pred))
  ps.aic[ind] <- x$aic
  ps.pval[ind] <- summary(x)$coefficients[2,4]
  ps.acc[ind] <- ifelse(predict(x,type='response')>0.5, 1, 0) %>%
    "=="(ps.red$DOCode) %>% mean
}

# perform pearson's r and kendall's tau of quantitative predictor vs. Min_DO
ps.cor[match(names(ps.quant),names(ps.pred)),] <- cbind(apply(ps.quant, 2,
                                                              cor,ps.red$Min_DO,
                                                              method='pearson'),
                                                        apply(ps.quant, 2,
                                                              cor,ps.red$Min_DO,
                                                              method='kendall'))
# TODO: change hard-coded groups when new data come in
# Perform test of collinearity with VIF or variane inflation factor

ps.omc <- omcdiag(x=ps.quant, y=ps.red$Min_DO) #overall
# group 1: Volume, UnitLength, AvgWidth, AvgDepth
# PoolArea, PoolTailCrest, MaxDepth
ps.imc <- as.data.frame(imcdiag(x=ps.quant[,c(2:6,9,12)], y=ps.red$Min_DO)$idiags)
# group 2: RiffleCrestThalweg, RiffleLength, RiffleArea, RiffleAvgWidth
ps.imc <- rbind(ps.imc, as.data.frame(
  imcdiag(x=ps.quant[,c(7,8,10,13)], y=ps.red$Min_DO)$idiags))
# group 3: WaterTemp, Average_Temperature, Max_Temperature,
# StudyDay, sindoy, cosdoy
ps.imc <- rbind(ps.imc,as.data.frame(
  imcdiag(x=ps.quant[,c(1,14:18)], y=ps.red$Min_DO)$idiags))
# collect all VIF values
ps.vif[match(row.names(ps.imc), names(ps.pred))] <- ps.imc[,1]

# perform chi-square test of independence on categorical predictors vs each other
quali.names <- names(ps.quali)
ps.quali.chi <- matrix(nrow=length(quali.names),ncol=length(quali.names))
for(i in 1:length(quali.names)){
  for(j in 1:length(quali.names)){
    if(quali.names[i]==quali.names[j]){
      next
    }else{
      ps.quali.chi[i,j] <- chisq.test(table(
        ps.quali[,c(quali.names[i],quali.names[j])]))$p.value
    }
  }
}

ps.quali.chi <- as.data.frame(ps.quali.chi)
row.names(ps.quali.chi) <- quali.names
names(ps.quali.chi) <- quali.names
write.csv(ps.quali.chi,'category_chisq.csv', na='-')

# perform chi-square test of independence of categorical predictors vs. DOCode
for(i in names(ps.quali)){
  ind <- match(i,names(ps.pred))
  ps.chi[ind] <- chisq.test(table(ps.red[,c('DOCode',i)]))$p.value
}

# peform mixed logistic regression of categorical predictors vs. DOCode
for(i in names(ps.quali)){
  str <- paste('x <- glmer(ps.red$DOCode~(1|ps.quali$',i,'),family=binomial)',sep='')
  eval(parse(text=str))
  ind <- match(i,names(ps.pred))
  ps.aic[ind] <- AIC(x)
  ps.acc[ind] <- ifelse(predict(x,type='response')>0.5,1,0) %>%
    "=="(ps.red$DOCode) %>% mean
}

collinear_out <- data.frame(Predictor=names(ps.pred), VIF=ps.vif, AIC=ps.aic,
                            Accuracy=ps.acc, pval=ps.pval, Pearson=ps.cor[,1],
                            Kendall=ps.cor[,2], ChiSq=ps.chi)
write.csv(collinear_out,'collinear.csv', row.names=F, na='-')

#### Stepwise Regression to investigate in-group collinearities ####

ps.aic.step <- rep(NA,ncol(ps.pred)) # AIC of [mixed] logistic regression on single predictor
ps.pval.step <- rep(NA,ncol(ps.pred)) # pvalue of logistic regression
ps.acc.step <- rep(NA,ncol(ps.pred)) # accuracy measure using 0.5 as threshold
# group 1
for(i in names(ps.pred)[c(3,5,8,9,12,16)]){
  str <- paste('x <- glmer(DOCode~(1|ReachType)+RiffleCrestThalweg+Discharge_cfs+'
               ,i,',data=ps.red,family=binomial)',sep='')
  eval(parse(text=str))
  ind <- match(i,names(ps.pred))
  print(row.names(summary(x)$coefficients)[4])
  ps.pval.step[ind] <- summary(x)$coefficients[4,4]
  ps.aic.step[ind] <- AIC(x)
  ps.acc.step[ind] <- ifelse(predict(x,type='response')>0.5,1,0) %>%
    "=="(ps.red$DOCode) %>% mean
}
# group 2
for(i in names(ps.pred)[c(10,14)]){
  str <- paste('x <- glmer(DOCode~(1|ReachType)+MaxDepth+Discharge_cfs+'
               ,i,',data=ps.red,family=binomial)',sep='')
  eval(parse(text=str))
  ind <- match(i,names(ps.pred))
  print(row.names(summary(x)$coefficients)[4])
  ps.aic.step[ind] <- AIC(x)
  ps.pval.step[ind] <- summary(x)$coefficients[4,4]
  ps.acc.step[ind] <- ifelse(predict(x,type='response')>0.5,1,0) %>%
    "=="(ps.red$DOCode) %>% mean
}
# group 4
for(i in names(ps.pred)[c(1,6,7,20)]){
  str <- paste('x <- glmer(DOCode~RiffleCrestThalweg+MaxDepth+Discharge_cfs+(1|'
               ,i,'),data=ps.red,family=binomial)',sep='')
  eval(parse(text=str))
  ind <- match(i,names(ps.pred))
  print(i)
  print(ind)
  ps.aic.step[ind] <- AIC(x)
  ps.acc.step[ind] <- ifelse(predict(x,type='response')>0.5,1,0) %>%
    "=="(ps.red$DOCode) %>% mean
}

stepwise_out <- data.frame(AIC=ps.aic.step,pval=ps.pval.step,Accuracy=ps.acc.step)
write.csv(stepwise_out, 'stepwise.csv', na='-', row.names=F)


