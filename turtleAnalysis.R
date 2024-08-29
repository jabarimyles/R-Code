library(tidyverse)
library(ggplot2)

turtle<-read.csv("Desktop/ST542/turtleGas.csv")
turtle2<-turtle[which(turtle$survival==1|turtle$survival==0),]
train<-turtle2[1:30,]
test<-turtle2[31:40,]

#Get means by survival status
turtle%>%group_by(survival)%>%summarize(avgWt=mean(weight),avgTemp=mean(Temp))


#Estimate Missing Data
missingData<-lm(WBC~gender+weight+Temp+PCV+TP+Lactate+pH+pCO2+Na+K+iCa+Glu,data=turtle)
predict(missingData,newdata=turtle[12,4:16])
summary(missingData)
predict(missingData,newdata=turtle[27,4:16])
missingDataHet<-lm(Heter~gender+weight+Temp+PCV+TP+Lactate+pH+pCO2+Na+K+iCa+Glu,data=turtle)
predict(missingDataHet, newdata=turtle[27,4:16])
missingDataEosin<-lm(Eosin~gender+weight+Temp+PCV+TP+Lactate+pH+pCO2+Na+K+iCa+Glu,data=turtle)
predict(missingDataEosin,newdata=turtle[27,4:16])
missingDataLymphs<-glm(Lymphs~gender+weight+Temp+PCV+TP+Lactate+pH+pCO2+Na+K+iCa+Glu,data=turtle,
                       family='poisson')
predict(missingDataLymphs,newdata=turtle[12,4:16])
turtle[27,19]<-1
turtle[12,19]<-11
turtle[27,18]<-28.34515
turtle[12,18]<-6.64078
turtle[12,17]<-75.48365
turtle[27,17]<-76.60627
turtle[12,16]<-7150.4
turtle[27,16]<-13779.18

#Plot each variable by survival status
p <- ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  stat_smooth()+
  facet_wrap(~year)

p <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
p + facet_wrap(~cyl)

g<-ggplot(data=turtle,aes())


table(turtle$survival)
length(turtle$survival)

#Classification tree and pruned tree
library(rpart)
fit<-rpart(survival~weight+Temp+PCV+TP+Lactate+pH+pCO2+pO2+Na+K+iCa+Glu+WBC+Heter+Eosin+Lymphs,
      method='class',
      data=turtle2)
printcp(fit)
plotcp(fit)
summary(fit)

pruneFit<-prune(fit,cp=fit$cptable[which.min(fit$cptable[,'xerror']),'CP'])
plot(pruneFit,main="Classification Tree Model")
text(pruneFit,cex=.8)


#Confusion matrix
predPrune = predict(fit, type="class")
table(predPrune)
table(predPrune,turtle2$survival)


cor(turtle[,4:20])
str(turtle)

turtLive<-turtle[which(turtle$survival==1),]
for(i in 6:19){
  plot(turtLive[,i],turtLive[,i+15],
       xlab=colnames(turtLive)[i],
       ylab=colnames(turtLive[i+15]))
  abline(lm(turtLive[,i+15]~turtLive[,i]))
}

for(i in 6:19){
  hist(turtLive[,i],main=paste(colnames(turtLive)[i]))
}

i<-6
plot(turtLive[,i], turtLive[,i+15])
colnames(turtLive)[i]


#Paired wilcoxon rank sum test
wilcox.test(x=turtLive$Lymphs, y=turtLive$Lymphs2, 
            paired = TRUE, 
            alternative = "two.sided")

t.test(x=turtLive$pCO2,y=turtLive$pCO22,
       paired=TRUE,
       alternative='two.sided')

shapiro.test(turt$Lymphs)
hist(turt$TP)

abc<-gather(turtLive)
#Convert data into long format for ggplot
abc[461:782,3]<-"after"
abc[116:437,3]<-"before"
before<-abc[116:437,]
after<-abc[461:782,]
after$key<-substr(after$key,1,nchar(after$key)-1)
fin<-rbind(before,after)
colnames(fin)[3]<-"time"
fin$value<-as.numeric(fin$value)
fin2<-fin
fin2[which(fin2$key=="Lymphs"),2]<-scale(fin2[which(fin2$key=="Lymphs"),2])


bx<-ggplot(data=fin2,aes(x=key,y=value,fill=time))
bx+geom_boxplot()+labs(x="Measurement",y="Z Scores by Measurement",
                       title="Before vs. After Blood Gas Measurements for Surviving Turtles")

#GLM 1
glmFit1<-glm(survival~PCV+pH+WBC+I(pH*WBC),data=turtle2,family='binomial')
newDat1<-as.data.frame(cbind(turtle2$PCV,turtle2$pH,turtle2$WBC))
colnames(newDat1)<-c("PCV","pH","WBC")
summary(glmFit1)#AIC=34.1
predGLM1<-round(predict(glmFit1,newdata=newDat1,type='response'),0)
#Confusion Matrix
table(predGLM1,test$survival)

#GLM 2
glmFit2<-glm(survival~PCV+pH+WBC+I(pH*WBC)+Lactate+I(Lactate*PCV),
              data=turtle2,family='binomial')
summary(glmFit2)#AIC 33.7
newDat2<-as.data.frame(cbind(turtle2$PCV,turtle2$pH,turtle2$WBC,turtle2$Lactate))
colnames(newDat2)<-c("PCV","pH","WBC","Lactate")
predGLM2<-round(predict.glm(glmFit2,newdata=newDat2,type='response'),0)
#Confusion Matrix
table(predGLM2,turtle2$survival)

#ROC
install.packages("ROCR")
library(ROCR)
predPrune #regression tree prediction results



help(prediction)

pred <- prediction(as.numeric(predPrune)-1, labels=turtle2$survival)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
plot(perf,colorize=TRUE,xlab=paste("False Pos Rate   AUC=",round(auc_ROCR,3)),
     main="Regression Tree ROC Curve")

summary(glmFit2)


#Boostrapping stuff
logit.bootstrap <- function(data, indices) {
  d <- data[indices, ]
  fit<-glm(survival~PCV+Lactate+pH+WBC+Eosin,data=d,family='binomial')
  
  return(coef(fit))
}

logit.boot <- boot(data=turtle2, statistic=logit.bootstrap, R=10000) # 10'000 samples

logit.boot
boot.ci(logit.boot, type="bca", index=6)


mean(turtle$Lymphs[which(turtle$survival==0)],na.rm=TRUE)
sd(turtle$Lymphs[which(turtle$survival==0)],na.rm=TRUE)
max(turtle$Lymphs[which(turtle$survival==0)],na.rm=TRUE)
min(turtle$Lymphs[which(turtle$survival==0)],na.rm=TRUE)



for(i in 4:19){
hist(turtle[,i],main=colnames(turtle)[i],xlab="")
}

#Normal
#weight, temp, pcv, pH, pCO2, Na, K, iCA, Heter, Lymphs


#Gamma
#TP, Lactate, PO2, Glu, WBC, Eosin


cor(turtle2$weight,turtle2$pCO2)


hist(rnorm(wbcPred1,wbcPred1SE),main="Distribution of WBC Estimate for Obs 12",xlab="WBC")
hist(rnorm(heterPred1,heterPred1SE),main="Distribution of Heterophil Estimate for Obs 12",xlab="Heterophil")
hist(rnorm(eosinPred1,eosinPred1SE,n=100),main="Distribution of Eosinophil Estimate for Obs 12",xlab="Eosinophil")
histL<-hist(rnorm(lymphPred1,lymphPred1SE,n=100),main="Distribution of Lymphocytes Estimate for Obs 12",xlab="Lymphocytes")
plot(histL,xlim=c(0,50))

zzz<-turtle[which(turtle$Lactate!=0),8]
lacFitL<-fitdist(zzz,'lnorm')
lacFitG$estimate[1]
xyz<-rbinom(n=40,p=.9,size=1)*rgamma(n=40,shape=lacFitG$estimate[1],rate=lacFitG$estimate[2])


hist(xyz)
hist(turtle2$Lactate)

zzz<-turtle[which(turtle$Lactate!=0),8]
lacFitW<-fitdist(zzz,'weibull')
lacFitG<-fitdist(zzz,'gamma')
lacFitL<-fitdist(zzz,'lnorm')
denscomp(list(lacFitG,lacFitW,lacFitL))




#which distributions take longest time to generate...turns out it's really none
benchmark(W={rweibull(n=100,shape=.87,scale=4.9)},
          G={rgamma(n=100,shape=.85,rate=.16)},
          L={rlnorm(n=100,meanlog=.975,sdlog=1.23)},
          replications=1000
          )

chisq.test(turtle2$Lactate,xyz)





# load the package
library(randomForest)
# load data

# fit model
fitRF <- randomForest(survival~weight+Temp+PCV+TP+Lactate+pH+pCO2+pO2+Na+K+iCa+Glu+WBC+Heter+Eosin+Lymphs, 
                    class=TRUE,
                    data=turtle2)
# load the package
library(ipred)
# fit model
fit2 <- bagging(as.factor(survival)~weight+Temp+PCV+TP+Lactate+pH+pCO2+pO2+Na+K+iCa+Glu+WBC+Heter+Eosin+Lymphs,
               coob=TRUE,
               data=turtle2)
# summarize the fit
summary(fit2)
print(fit2)
# make predictions
predictions <- predict(fit2, turtle2[,4:19], type="class")
# summarize accuracy
table(predictions, turtle2$survival)



fit3 <- randomForest(as.factor(survival)~weight+Temp+PCV+TP+Lactate+pH+pCO2+pO2+Na+K+iCa+Glu+WBC+Heter+Eosin+Lymphs,
                ntrees=500,
                mtry=5,
                data=turtle2)
round(importance(fit3),digits=2)
plot(importance(fit3))

findMyrtle<-as.data.frame(cbind(X,turt$survival))
findMyrtle<-findMyrtle[which(findMyrtle[17]==1),]
myrtle<-summarize_all(as.data.frame(findMyrtle),.funs=mean,na.rm=TRUE)
quantile(X[,3],probs=.15)
myrtle[3]=-1.153

abc<-cbind(xxx,turtle2$survival)
myrtleReal<-summarize_all(abc[which(abc$survival==1),],.funs=mean(.,na.rm=TRUE))

myrtleReal<-abc[which(abc$survival==1),] %>% 
  summarize_all(funs(mean), na.rm = TRUE)

myrtleProb<-exp(beta[1]*myrtle[1]+
                  beta[2]*myrtle[2]+
                  beta[3]*myrtle[3]+
                  beta[4]*myrtle[4]+
                  beta[5]*myrtle[5]+
                  beta[6]*myrtle[6]+
                  beta[7]*myrtle[7]+
                  beta[8]*myrtle[8]+
                  beta[9]*myrtle[9]+
                  beta[10]*myrtle[10]+
                  beta[11]*myrtle[11]+
                  beta[12]*myrtle[12]+
                  beta[13]*myrtle[13]+
                  beta[14]*myrtle[14]+
                  beta[15]*myrtle[15]+
                  beta[16]*myrtle[16])
myrtlePred<-myrtleProb
myrtlePred[which(round(myrtleProb,0)>0)]=1
myrtlePred


zWeight<-(10-mean(turtle2$weight))/sd(turtle2$weight)
zTemp<-(10-mean(turtle2$Temp))/sd(turtle2$Temp)
zPCV<-(10-mean(turtle2$PCV))/sd(turtle2$PCV)
zTP<-(10-mean(turtle2$TP))/sd(turtle2$TP)
zLactate<-(10-mean(turtle2$Lactate))/sd(turtle2$Lactate)
zPH<-(10-mean(turtle2$pH))/sd(turtle2$pH)
zpCO2<-(10-mean(turtle2$pCO2))/sd(turtle2$pCO2)
zpO2<-(10-mean(turtle2$pO2))/sd(turtle2$pO2)
zNa<-(10-mean(turtle2$Na))/sd(turtle2$Na)
zK<-(10-mean(turtle2$K))/sd(turtle2$K)
ziCa<-(10-mean(turtle2$iCa))/sd(turtle2$iCa)
zGlu<-(10-mean(turtle2$Glu))/sd(turtle2$Glu)
zWBC<-(10-mean(turtle2$WBC,na.rm=TRUE))/sd(turtle2$WBC,na.rm=TRUE)
zHeter<-(10-mean(turtle2$Heter,na.rm=TRUE))/sd(turtle2$Heter,na.rm=TRUE)
zEosin<-(10-mean(turtle2$Eosin,na.rm=TRUE))/sd(turtle2$Eosin,na.rm=TRUE)
zLymphs<-(10-mean(turtle2$weight,na.rm=TRUE))/sd(turtle2$weight,na.rm=TRUE)
zScores<-c(zWeight,zTemp,zPCV,zTP,zLactate,zPH,zpCO2,zpO2,zNa,zK,ziCa,zGlu,
           zWBC,zHeter,zEosin,zLymphs)
zScores
#Prediction
logOdds<-exp(beta[1]+
               beta[2]*zScores[1]+
               beta[3]*zScores[2]+
               beta[4]*zScores[3]+
               beta[5]*zScores[4]+
               beta[6]*zScores[5]+
               beta[7]*zScores[6]+
               beta[8]*zScores[7]+
               beta[9]*zScores[8]+
               beta[10]*zScores[9]+
               beta[11]*zScores[10]+
               beta[12]*zScores[11]+
               beta[13]*zScores[12]+
               beta[14]*zScores[13]+
               beta[15]*zScores[14]+
               beta[16]*zScores[15]+
               beta[17]*zScores[16])
prob<-logOdds/(1+logOdds)
survivalProb<-round(prob,3)
survivalProb<-survivalProb