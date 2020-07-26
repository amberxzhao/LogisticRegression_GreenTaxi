taxi=read.csv("/Users/AmberZhao/Desktop/taxi1.csv")
head(taxi)
Trip=ifelse(taxi$Trip_type==1,"StreetHail","Dispatch")
Taxi=data.frame(taxi,Trip)
Taxi=Taxi[,-12]
dim(Taxi)
head(Taxi)
set.seed(123)
smp_size=floor(0.75*nrow(Taxi))
train=sample(seq_len(nrow(Taxi)),size=smp_size)
trainset=Taxi[train,]
testset=Taxi[-train,]

glm.fit=glm(Trip~VendorID+RateCodeID+Passenger_count+Trip_distance+Fare_amount
            +MTA_tax+Tip_amount+Tolls_amount+improvement_surcharge
            +Total_amount+Payment_type, data=trainset,family=binomial)
summary(glm.fit)
library(MASS)
library(tidyverse)
library(caret)
install.packages("lattice")
library(lattice)
library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(magrittr)
library(leaps)
#best selection 
library(bestglm)
model=bestglm(trainset,IC="BICq",family=binomial)
model
model1=bestglm(trainset,IC="AIC",family=binomial)
model1
#stepwise 
full=glm(Trip~.,family=binomial,data=trainset)
summary(full)
step=stepAIC(full,trace=FALSE)
step$anova
forward=stepAIC(full,direction="forward",trace=FALSE)
forward$anova
backward=stepAIC(full,direction="backward",trace=FALSE)
backward$anova

#model fit 
#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#logistic-regression-diagnostics
head(trainset)
library(ggplot2)
plot.lm(model1)
new=cbind(trainset$Fare_amount,trainset$MTA_tax,trainset$Tip_amount,
              trainset$Tolls_amount,trainset$improvement_surcharge,
              trainset$Total_amount)
new=trainset[,5:10]
head(new)
bestmodel=glm(Trip~RateCodeID++Fare_amount+MTA_tax+Tip_amount+Tolls_amount
              +improvement_surcharge+Total_amount+Payment_type, 
              data=trainset,family=binomial)
summary(bestmodel)
probabilities=predict(bestmodel,type="response")
predicted.classes=ifelse(probabilities>0.5,"Dispatch","StreetHail")
head(predicted.classes)
library(broom)
library(tidyverse)
new=new %>% 
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key="predictors",value="predictor.value",-logit)
ggplot(new,aes(logit,predictor.value))+
  geom_point(size=0.5,alpha=0.5)+
  geom_smooth(method="loess")+
  theme_bw()+
  facet_wrap(~predictors,scales="free_y")

plot(bestmodel,which=4,id.n=3)
model.data=augment(bestmodel) %>%
  mutate(index=1:n())
model.data %>% top_n(3,.cooksd)
ggplot(model.data,aes(index,.std.resid))+
  geom_point(aes(color=Trip),alpha=.5)+
  theme_bw()
model.data %>% 
  filter(abs(.std.resid)>3)

car::vif(bestmodel)

#importance of variable
install.packages("dominanceanalysis")
library(dominanceanalysis)
str(trainset)
anova(bestmodel,test="Chisq")
install.packages("pscl")
library(pscl)
pR2(bestmodel)
dapres=dominanceAnalysis(bestmodel)
getFits(dapres,"r2.m")
dominanceMatrix(dapres,type="complete",fit.function="r2.m")
contributionByLevel(dapres,fit.functions="r2.m")
dominanceMatrix(dapres,type="conditional",fit.function="r2.m")
averageContribution(dapres,fit.functions = "r2.m")
dominanceMatrix(dapres,type="general",fit.function="r2.m")

boxplot(Trip~RateCodeID++Fare_amount+MTA_tax+Tip_amount+Tolls_amount
        +improvement_surcharge+Total_amount+Payment_type, 
        data=trainset)

