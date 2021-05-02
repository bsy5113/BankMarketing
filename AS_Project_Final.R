
library(tidyverse)
library(dplyr)
library(MASS)
library(reshape)
library(reshape2)
library(directlabels)
library(ggplot2)
library(scales)
library(gridExtra)
library(cowplot)
library(glmnet)
library(GGally)
library(heplots)
library(car)
library(ROCR)
library(class)
library(caret)
library(e1071)
library(qpcR)
library(gridExtra)
library(car)
library(corrplot)
library(randomForest)
library(ROCR)
library(scales)
library(cowplot)
library(effects)
library(bestglm)


# ---DATA --- ----------------------------------#######
Bankfull = read.csv("Rfolder/bank-additional-full.csv", header = TRUE, sep = ';')
Bankfull = as_tibble(Bankfull)
head(Bankfull)
str(Bankfull)
summary(Bankfull)
Bankfull$y = as.factor(Bankfull$y)

###----------------------EDA for continueos data set ----------------------####
###-----------------------------------------------------------------------#####
Bankfull.Contineous = Bankfull %>%  dplyr::select(c(age,duration, campaign,pdays,previous,
                                             emp.var.rate, cons.price.idx, cons.conf.idx, 
                                             euribor3m, nr.employed, y))
Bankfull.Contineous.melt = melt(Bankfull.Contineous, id = c('y'))
str(Bankfull.Contineous.melt)
## Tring to draw boxplot for all data, but the scales are different
ggplot(Bankfull.Contineous.melt, aes(x= variable, y = value, fill=y)) +
  geom_boxplot()

## Checking continueous dataset one by one 
ggplot(Bankfull, aes(x= y, y = age, fill = y)) +
    geom_boxplot()
  
ggplot(Bankfull, aes(x= y, y = duration, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = campaign, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = pdays, fill = y)) +
  geom_boxplot() ### need to categorize the data based on range or distribution

ggplot(Bankfull, aes(x= y, y = previous, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = emp.var.rate, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = cons.price.idx, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = cons.price.idx, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = cons.conf.idx, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = euribor3m, fill = y)) +
  geom_boxplot()

ggplot(Bankfull, aes(x= y, y = nr.employed, fill = y)) +
  geom_boxplot()

###----------------------EDA for categorical data set ----------------------####
###-------------------------------------------------------------------------####
Bankfull.Catg = Bankfull %>%  dplyr::select(c(job,marital,education,default,housing,
                                              loan, contact, month, day_of_week, y))
names = c(1:9)
Bankfull.Catg[,names] = lapply(Bankfull.Catg[,names] , factor)
str(Bankfull.Catg)

ggplot(Bankfull.Catg, aes(fill=y, x=job, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=marital, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=education, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=default, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=housing, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=loan, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=contact, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=month, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

ggplot(Bankfull.Catg, aes(fill=y, x=day_of_week, y=y)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5, angle = 90, size = 8, face = 'bold'))

####==================================== Heat Map and Correlation plot using Down Sampling ======###
####============================================================================================###

# Unbalance of no and yes in the y variable -> Need to downsample
levels(Bankfull$y)<-c("no","yes")
yes<-which(Bankfull$y=="yes")
no<-which(Bankfull$y=="no")
length(yes)
length(no)

# Display of unbalance of No and Yes data
prop.table(table(Bankfull$y))

# Sampling randomly from no with the amount of yes
no.downsample<- sample(no,length(yes))

bank.down<-Bankfull[c(no.downsample,yes),]
str(bank.down)
length(which(bank.down$y=="yes"))
length(which(bank.down$y=="no"))
bank.down$pdays

summary(bank.down)  

# Create reduced model with just the continuous variables
bank.down%>%ggpairs(columns=c("y","job","age","duration", "campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"), aes(color=y))


# Boxplots with categorical variables 
bank.down%>%ggpairs(columns=c("y","job","marital", "education", "default", "housing", "loan", "contact", "poutcome"), aes(color=y))


# HEATMAP --------------------------------------------------------------
#Examine the correlation between the continous predictors
continuous.bank <- Bankfull[,-c(2,3,4,5,6,7,8,9,10,13,14,15,21)]
pairs(continuous.bank)
my.cor<-cor(continuous.bank)
my.cor
pairs(continuous.bank,col=y)

#If you have a lot of predictors, heatmap with correlations could
#be helpful to examine redundancy.    # emp.var.rate and euribor3m are collinear
heatmap.2(my.cor,col=redgreen(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")


#####================  PCA   ===============================================================######
#####---------------------------------------------------------------------------------------######
Bankfull.PCA = Bankfull.Contineous %>%  dplyr::select(-c(pdays,y,nr.employed))
str(Bankfull.PCA)
pca.result = prcomp(Bankfull.PCA,scale.=TRUE)
pca.result
pca.scores<-pca.result$x
pca.scores
#pairs(pca.scores)
#cor(pca.scores)
var.PCA<-apply(pca.scores,2,var)
var.PCA
#Total Variance of PC's
sum(var.PCA)
#List of eigenvectors
pca.result$rotation
## Scree plot and cummulative proportion
par(mfrow=c(1,2))
eigenvals<-(pca.result$sdev)^2
plot(1:8,eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained")
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
plot(1:8,cumulative.prop,type="l",main="Cumulative proportion",ylim=c(0,1))
par(mfrow=c(1,1))

## Only take 6 pcs for analysis
pca.result$rotation[,1:6]

eigenvals<-(pca.result$sdev)^2
eigenvals
variance = eigenvals*100/sum(eigenvals)
variance

## Extract PC1 and PC2 values
str(pca.result)
pca.result$x
Bankfull.PCA = cbind(Bankfull.Contineous,pca.result$x[,1:2])

ggplot(Bankfull.PCA, aes(PC1, PC2, col = y, fill = y)) +
  stat_ellipse(geom = "polygon",col = "black", alpha = 0.5) +
  geom_point(aes(col = y),shape = 19) +
  scale_color_manual(values=c("aquamarine3", 'bisque4')) +
  theme_classic()


#####--- ------------------- Lasso for continueous variables -------------------------#####
#####---------------------------------------------------------------------------------#####

Bankfull.Contineous.Final = Bankfull %>%  dplyr::select(c(age,duration, campaign,
                                                          emp.var.rate, cons.price.idx, cons.conf.idx, 
                                                          euribor3m, y))
varmtx <- model.matrix(y~.-1, data=Bankfull.Contineous.Final)
varmtx
str(varmtx)
response <- ifelse(Bankfull.Contineous.Final$y =="yes",1,0)
response
lasso <- glmnet(scale(varmtx), response, family='binomial',alpha=1, type.measure = 'mse')
cv.lasso = cv.glmnet(varmtx,response,alpha=1,family='binomial',type.measure = 'mse' )
plot(cv.lasso)

plot(lasso, xvar = "lambda", label=T)

### --------------------------------- Downsampling ---------------------------###
###--EDA for downsampled continuous data set(To balance Yes and No response)--####
levels(Bankfull$y)<-c("no","yes")
yes<-which(Bankfull$y=="yes")
no<-which(Bankfull$y=="no")
length(yes)
length(no)

# Display of unbalance of No and Yes data
prop.table(table(Bankfull$y))

# Sampling randomly from no with the amount of yes
no.downsample<- sample(no,length(yes))
bank.balanced<-Bankfull[c(no.downsample,yes),]
str(bank.balanced)
as_tibble(bank.balanced)

length(which(bank.balanced$y=="yes"))
length(which(bank.balanced$y=="no"))
bank.balanced$pdays

# Scatterplot of the reduced continuous set
bank.reduced%>%ggpairs(columns=c("y","job","age","duration", "campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed"), aes(color=y))

###--Running logistic regression for Categorical variables to verify the significance-----#####

str(Bankfull.Catg)

EDA.model = glm(y~., Bankfull.Catg, family = 'binomial')
summary(EDA.model)
### Null deviance: 28999  on 41187  degrees of freedom
### Residual deviance: 25557  on 41147  degrees of freedom
### AIC: 25639

EDA.model1 = glm(y~.-job, Bankfull.Catg, family = 'binomial')
summary(EDA.model1)
### Residual deviance: 25813  on 41158  degrees of freedom
### AIC: 25873
### AIC and Deviance have increased ...so keep job

EDA.model2 = glm(y~.-education, Bankfull.Catg, family = 'binomial')
summary(EDA.model2)
### Residual deviance: 25580  on 41154  degrees of freedom
### AIC: 25648
### AIC and Deviance have increased ...so keep education

EDA.model4 = glm(y~.-default, Bankfull.Catg, family = 'binomial')
summary(EDA.model4)
### Residual deviance: 25696  on 41149  degrees of freedom
### AIC: 25774
### AIC and Deviance have increased ...so keep default

EDA.model5 = glm(y~.-housing, Bankfull.Catg, family = 'binomial')
summary(EDA.model5)
### Residual deviance: 25557  on 41148  degrees of freedom
### AIC: 25637
### AIC is 2 point low and Deviance is same ...so keep housing

EDA.model6 = glm(y~.-loan, Bankfull.Catg, family = 'binomial')
summary(EDA.model6)
### Residual deviance: 25558  on 41148  degrees of freedom
### AIC: 25638
### AIC is 1 point low and Deviance is 1 point up ...so keep loan

EDA.model7 = glm(y~.-contact, Bankfull.Catg, family = 'binomial')
summary(EDA.model7)
### Residual deviance: 26154  on 41148  degrees of freedom
### AIC: 26234
### AIC is 595 point up and Deviance is 597 point up ...so remove contact

EDA.model8 = glm(y~.-month, Bankfull.Catg, family = 'binomial')
summary(EDA.model8)
### Residual deviance: 27006  on 41156  degrees of freedom
### AIC: 27070
### AIC is 1431 point up and Deviance is 1449 point up ...so remove month

EDA.model9 = glm(y~.-day_of_week, Bankfull.Catg, family = 'binomial')
summary(EDA.model9)
### Residual deviance: 25596  on 41151  degrees of freedom
### AIC: 25670
### AIC is 74 point up and Deviance is 39 point up ...so remove day_of_week

EDA.model9 = glm(y~.-marital, Bankfull.Catg, family = 'binomial')
summary(EDA.model9)
### Residual deviance: 25570  on 41150  degrees of freedom
### AIC: 25646
### AIC is 7 point up and Deviance is 7 point up ...so not sure

####======================================================================================####
###--------Feature selection on balanced dataset --------------------- -------------------####
###--------run the feature selection and caculate the AIC --------------------------------####

Bankfull.features = bank.balanced %>%  dplyr::select(c(age,duration, campaign,previous,
                                                  emp.var.rate, cons.price.idx, 
                                                  cons.conf.idx, euribor3m,job,marital,
                                                  education,default,housing, loan, 
                                                  contact, month, day_of_week, y))
names.feature = c(9:18)
Bankfull.features[,names.feature] = lapply(Bankfull.features[,names.feature] , factor)
str(Bankfull.features)


## Stepwise method
FE.model = glm(y~., Bankfull.features, family = 'binomial') %>% stepAIC(direction="both", trace = TRUE)
summary(FE.model)

## Backward selection
backward.selection = glm(y~., Bankfull.features, family = 'binomial') %>% stepAIC(direction="backward", trace = 1)
summary(backward.selection)

## Forward selection
forward.selection = glm(y~., Bankfull.features, family = 'binomial') %>% stepAIC(direction="forward", trace = TRUE)
summary(forward.selection)


############################# Feature Selection Continued with Lasso -------###
## ------------------- Ridge, Lasso and elastic net -------------------------##
str(Bankfull.features)
FE.matrix = model.matrix(y~.-1, data=Bankfull.features)
FE.matrix
str(FE.matrix)
FE.res <- ifelse(Bankfull.features$y =="yes",1,0)
FE.res
grid = 10^seq(2,-10,length=50)
grid
FE.lasso = glmnet(scale(FE.matrix), FE.res, family='binomial', alpha = 1)
summary(FE.lasso)
cv.lasso = cv.glmnet(FE.matrix,FE.res,alpha=1,family='binomial')
plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se
plot(FE.lasso, xvar = "lambda", label=T)
cv.lasso$glmnet.fit$call
coef(FE.lasso)
round(coef(cv.lasso$glmnet.fit),3)

coeffs <- coef(cv.lasso, cv.lasso$lambda.1se) 
coeffs.dt <- data.frame(name = coeffs@Dimnames[[1]][coeffs@i + 1], coefficient = coeffs@x) 

# reorder the variables in term of coefficients
coeffs.dt[order(coeffs.dt$coefficient, decreasing = T),]


########################################################################################
###########    Runing logistic regression to finalize the features             #########
###########====================================================================#########

str(Bankfull.features)
## taking marital, loan, day_of_week, age out
Bankfull.Final.age = Bankfull.features %>%  dplyr::select(-c(marital,loan,day_of_week,age))
str(Bankfull.Final.age)

## without age to check AIC and deviance
Bankfull.Final.age.model = glm(y~., Bankfull.Final.age, family = 'binomial')
summary(Bankfull.Final.age.model)

## taking marital, loan, day_of_week out
Bankfull.Final = Bankfull.features %>%  dplyr::select(-c(marital,loan,day_of_week))
str(Bankfull.Final)

## with age to check AIC and deviance
Bankfull.Final.model = glm(y~., Bankfull.Final, family = 'binomial')
summary(Bankfull.Final.model)

### set of variables
str(Bankfull.Final)

####===================================== Interpertation Model ==================================####
####=============================================================================================####
# Building Logistic regression model 
bank.down$y <- ifelse(bank.down$y == "yes", 1, 0)
bank.down$y <- factor(bank.down$y, levels = c(0, 1))

# Combining selected variables into a model
model.main<-glm(y ~ campaign+euribor3m+cons.price.idx+emp.var.rate+previous+duration+cons.conf.idx+education+age+default+job+month+housing+contact, data=bank.down,family = binomial(link="logit"))

# Checking residuals
par(mfrow=c(2,2))
plot(model.main)

# Cooks D plot
plot(model.main, pch=18, col="red", which=c(4))  # Seems fine, no outliers or heavy leverage points. 

summary(model.main)

# Odd ratio and 95% confidence level table 
exp(cbind("Odds ratio" = coef(model.main), confint.default(model.main, level = 0.95)))

# Lack of fit test - Deviance test is used 
1-pchisq(6076.3,9240)
plot(allEffects(model.main))

# Effects plot for the interpretation model 
effplot <- allEffects(model.main)

####-------------------------- Building Prediction Model for logistic Regression -----------#######
####------------------------------------------------------------------------------------------#####

library(glmnet)
library(bestglm)
dat.train.x <- model.matrix(y~campaign+cons.price.idx+emp.var.rate+previous+duration+cons.conf.idx+education+default+job+month+housing+contact,bank.down)
dat.train.y<-bank.down[,21]
library(glmnet)
cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")
#CV misclassification error rate is little below .1
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

#Optimal penalty
cvfit$lambda.min

#For final model predictions go ahead and refit lasso using entire
#data set
finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(finalmodel, newx = dat.train.x, type = "response")

library(ROCR)
pred <- prediction(fit.pred[,1], dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="LASSO")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


# Going back to the coefficients, we can remove insignfiicant variables. 
coef(finalmodel)

olog<-glm(y~campaign+euribor3m+cons.price.idx+emp.var.rate+previous+duration+cons.conf.idx+education+default+job+month+housing+contact,data=bank.down,family=binomial)

fit.pred <- predict(olog, newx = dat.train.x, type = "response")

pred <- prediction(fit.pred, dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="Ordingary Logistic")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

## ----------------------------------------------------------------------------------------------

## Model 2 - Model 1 + interaction terms (age:campaign, duration:campaign) added  ---------------------------------
library(glmnet)
library(bestglm)
dat.train.x <- model.matrix(y~cons.price.idx+emp.var.rate+previous+duration+cons.conf.idx+default+job+month+contact+duration:campaign+campaign:cons.conf.idx,bank.down)
dat.train.y<-bank.down[,21]
library(glmnet)
cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")
#CV misclassification error rate is little below .1
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

#Optimal penalty
cvfit$lambda.min

#For final model predictions go ahead and refit lasso using entire
#data set
finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(finalmodel, newx = dat.train.x, type = "response")

library(ROCR)
pred <- prediction(fit.pred[,1], dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="LASSO")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


######==============================================================================####
#####                    Discriminate Analysis                                      ####
#####===============================================================================####

## Final datasets of contineous variables for LDA
Bankfull.Contineous.Final = Bankfull %>%  dplyr::select(c(age,duration, campaign,previous,
                                                    emp.var.rate, cons.price.idx, 
                                                    cons.conf.idx, euribor3m, y))
str(Bankfull.Contineous.Final)
#library(psych)
##pairs.panels(Bankfull.Contineous.Final[1:8],
 #             gap = 0,
 #             bg = c("aquamarine3", 'bisque4', "blue2")[Bankfull.Contineous.Final$y],
 #              pch = 19)
## Data partition
set.seed(777)
ind = sample(2, nrow(Bankfull.Contineous.Final),
             replace = TRUE, 
             prob = c(0.7,0.3))

training.LDA = Bankfull.Contineous.Final[ind ==1,]
dim(training.LDA)
test.LDA = Bankfull.Contineous.Final[ind ==2,]
dim(test.LDA)

LDA.model = lda(y~., training.LDA)
LDA.model              

LDA.model$counts
LDA.model$scaling

##Histogram
lda.pred = predict(LDA.model, training.LDA)
lda.pred$class
lda.pred$posterior
lda.pred$x
## for LD1
ldahist(data =lda.pred$x[,1], g = training.LDA$y)

## confusion matrix 
pred<-predict(LDA.model,newdata=test.LDA)$class 
Truth<-test.LDA$y
x<-table(pred,Truth) 
x
#Missclassification Error
ME<-(x[2,1]+x[1,2])/nrow(Bankfull.Contineous.Final)
ME
#Calculating overall accuracy
1-ME

### checking assumptions
leveneTest(y ~ .,Bankfull.Contineous.Final)  

###-------------------------------------------------------------------#####
###                        QDA                                        #####
###===================================================================#####

QDA.model = qda(y~., training.LDA)
QDA.model              

QDA.model$counts
QDA.model$scaling

##Histogram
qda.pred = predict(QDA.model, training.LDA)
qda.pred$class
qda.pred$posterior
qda.pred$x

## confusion matrix 
pred<-predict(QDA.model,newdata=test.LDA)$class 
Truth<-test.LDA$y
x<-table(pred,Truth) 
x
#Missclassification Error
ME<-(x[2,1]+x[1,2])/nrow(Bankfull.Contineous.Final)
ME
#Calculating overall accuracy
1-ME

### checking assumptions
leveneTest(y ~ .,Bankfull.Contineous.Final)  



####===============================================================================#####
####                    Running the model with logistic regression                 #####
####===============================================================================#####

## Final model with balanced data
Bankfull.Final.model = glm(y~., Bankfull.Final, family = 'binomial')
summary(Bankfull.Final.model)

## predict the response on training and testing
Pred.logis.training = predict(Bankfull.Final.model,Bankfull.Final,type = 'response')
Pred.logis.training
t = table(Actualvalue = Bankfull.Final$y, Predictedvalue = Pred.logis.training>0.5)
t
#Missclassification Error
ME<-(t[2,1]+t[1,2])/nrow(Bankfull.Final)
ME
#Calculating overall accuracy
1-ME
## ROC for training data
ROC.Pred<-prediction(Pred.logis.training, Bankfull.Final$y,label.ordering=c("no","yes"))
ROC.Pred
ROC.Perf = performance(ROC.Pred, measure = "tpr", x.measure = "fpr")
ROC.Perf
plot(ROC.Perf,colorize = TRUE, print.cutoff.at = seq(0.1,by=0.1))
abline(a=0, b= 1)
## new cutoff and misclassification error
cutoff = 0.55
t1 = table(Actualvalue = Bankfull.Final$y, Predictedvalue = Pred.logis.training>cutoff)
t1
#Missclassification Error
ME<-(t1[2,1]+t1[1,2])/nrow(Bankfull.Final)
ME
#Calculating overall accuracy
1-ME


### Apply the model to test data (entire dataset)

Final.fulldata = Bankfull %>%  dplyr::select(c(age,duration, campaign,previous,
                                               emp.var.rate, cons.price.idx, 
                                               cons.conf.idx, euribor3m,job,education,
                                               default,housing, contact,month, y))
names = c(9:14)
Final.fulldata[,names] = lapply(Final.fulldata[,names] , factor)
str(Final.fulldata)

Pred.logis.testing = predict(Bankfull.Final.model,Final.fulldata,type = 'response')
Pred.logis.testing
tt = table(Actualvalue = Final.fulldata$y, Predictedvalue = Pred.logis.testing>0.5)
tt
#Missclassification Error
ME<-(tt[2,1]+tt[1,2])/nrow(Final.fulldata)
ME
#Calculating overall accuracy
1-ME

####--------- Using logistic regression using traditional 70-30 split -----------#####
## Data partition
set.seed(777)
ind1 = sample(2, nrow(Final.fulldata),
             replace = TRUE, 
             prob = c(0.7,0.3))

training.logis = Final.fulldata[ind ==1,]
dim(training.logis)
test.logis = Final.fulldata[ind ==2,]
dim(test.logis)
## Final model with training data
Bankfull.Final.model.2 = glm(y~., training.logis, family = 'binomial')
summary(Bankfull.Final.model.2)

## predict the response on training and testing
Pred.logis.training.2 = predict(Bankfull.Final.model.2,training.logis,type = 'response')
Pred.logis.training.2
t.tr = table(Actualvalue = training.logis$y, Predictedvalue = Pred.logis.training.2>0.5)
t.tr
#Missclassification Error
ME<-(t.tr[2,1]+t.tr[1,2])/nrow(training.logis)
ME
#Calculating overall accuracy
1-ME

## ROC for training data (tranditional 70-30 split)
ROC.Pred.2<-prediction(Pred.logis.training.2, training.logis$y,label.ordering=c("no","yes"))
ROC.Pred.2
ROC.Perf.2 = performance(ROC.Pred.2, measure = "tpr", x.measure = "fpr")
ROC.Perf.2
plot(ROC.Perf.2,colorize = TRUE, print.cutoff.at = seq(0.1,by=0.1))
abline(a=0, b= 1)
## new cutoff and misclassification error
cutoff = 0.40
t.tr1 = table(Actualvalue = training.logis$y, Predictedvalue = Pred.logis.training.2>cutoff)
t.tr1
#Missclassification Error
ME<-(t.tr1[2,1]+t.tr1[1,2])/nrow(training.logis)
ME
#Calculating overall accuracy
1-ME

### Apply the model to test data --------------
cutoff = 0.4
Pred.logis.testing.2 = predict(Bankfull.Final.model.2,test.logis,type = 'response')
Pred.logis.testing.2
tt.tr = table(Actualvalue = test.logis$y, Predictedvalue = Pred.logis.testing.2>cutoff)
tt.tr
#Missclassification Error
ME<-(tt.tr[2,1]+tt.tr[1,2])/nrow(test.logis)
ME
#Calculating overall accuracy
1-ME

###---------------------------------------------------------------------------------------------####
###===============      Non-Parametric Model - Random Forest                ====================####
###---------------------------------------------------------------------------------------------####

###--------------------------------------------Model 1 ----------------------------------------#####
#Random forest is able to more successfully handle non-numeric data so the full dataset is made available to it.
subset.rfTraining = Bankfull
#70-30 set split for training and testing sets
splitPercentage = 0.7
set.seed(6)
#Create training and testing sets
trainingIndicies = sample(1:nrow(subset.rfTraining)[1],round(splitPercentage * nrow(subset.rfTraining)[1]))
train = subset.rfTraining[trainingIndicies,]
test=subset.rfTraining[-trainingIndicies,]
#Create the Random Forest model
RFmodel1 <- randomForest(y ~ ., 
                         data = train, method = 'rf',mtry = 8, trControl = trainControl(method = 'cv',savePredictions = "final"))
#Tune model using mtry value with minimum out of bag error
mtry <- tuneRF(train[-21], train$y, ntreeTry = 500)
best.m <-mtry[mtry[,2] == min(mtry[,2]),1]
print( mtry)
print(best.m)

#####---------------------------------------Model 2 ------------------------------------------######
###Random Forest with backward feature selection variables (Model 2)
#Random forest is able to more successfully handle non-numeric data so the full dataset is made available to it.
subset.rfTraining = Bankfull
#70-30 set split for training and testing sets
splitPercentage = 0.7
set.seed(6)
#Create training and testing sets
trainingIndicies = sample(1:nrow(subset.rfTraining)[1],round(splitPercentage * nrow(subset.rfTraining)[1]))
train = subset.rfTraining[trainingIndicies,]
test=subset.rfTraining[-trainingIndicies,]
#Create the Random Forest model
RFmodel2 <- randomForest(y ~ campaign + education + default + contact + previous + job + euribor3m + cons.price.idx + emp.var.rate + month + duration, data = train, method = 'rf', mtry = 4, trControl = trainControl(method = 'cv',savePredictions = "final"))
#Tune model using mtry value with minimum out of bag error
mtry <- tuneRF(train[-21], train$y, ntreeTry = 500)
best.m <-mtry[mtry[,2] == min(mtry[,2]),1]
print( mtry)
print(best.m)

#####------------------------------------- Model 3 ---------------------------------------------#####
###Random Forest with downsampled data and reduced data selection (Model 3)
#using the reduced/downsampled data
subset.rfTraining = bank.reduced
#70-30 set split for training and testing sets
splitPercentage = 0.7
set.seed(6)

#Create training and testing sets
trainingIndicies = sample(1:nrow(subset.rfTraining)[1],round(splitPercentage * nrow(subset.rfTraining)[1]))
train = subset.rfTraining[trainingIndicies,]
test=subset.rfTraining[-trainingIndicies,]

#Create the Random Forest model
RFmodel3 <- randomForest(y ~ education + default + job + month + housing + contact + campaign + euribor3m + cons.price.idx + emp.var.rate + previous + duration + cons.conf.idx + age, data = train, method = 'rf', mtry = 4, trControl = trainControl(method = 'cv',savePredictions = "final"))
predictRF = predict(RFmodel3, newdata=test)
table(test$y, predictRF)

#Tune model using mtry value with minimum out of bag error
mtry <- tuneRF(train[-21], train$y, ntreeTry = 500)
best.m <-mtry[mtry[,2] == min(mtry[,2]),1]
print( mtry)
print(best.m)

#Evaluate variable importance
importance(RFmodel3)
varImpPlot(RFmodel3)

########--------------------------------------Model 4 ----------------------------------------####
###Random Forest with downsampled data and full data selection (Model 4)
#using the reduced/downsampled data
subset.rfTraining = bank.reduced
#70-30 set split for training and testing sets
splitPercentage = 0.7
set.seed(6)
#Create training and testing sets
trainingIndicies = sample(1:nrow(subset.rfTraining)[1],round(splitPercentage * nrow(subset.rfTraining)[1]))
train = subset.rfTraining[trainingIndicies,]
test=subset.rfTraining[-trainingIndicies,]
#Create the Random Forest model
RFmodel4 <- randomForest(y ~ ., data = train, method = 'rf',mtry = 8, trControl = trainControl(method = 'cv',savePredictions = "final"))
predictRF = predict(RFmodel4, newdata=test)
table(test$y, predictRF)
#Tune model using mtry value with minimum out of bag error
mtry <- tuneRF(train[-21], train$y, ntreeTry = 500)
best.m <-mtry[mtry[,2] == min(mtry[,2]),1]
print( mtry)
print(best.m)
#Evaluate variable importance
importance(RFmodel4)
varImpPlot(RFmodel4)

#######-------          Comparing different Models ----------------------------------------###########

#Comparing the different models!
#Model 1
predictRF1 = predict(RFmodel1, newdata=Bankfull)
table(Bankfull$y, predictRF1)
pred1 = predict(RFmodel1, type = "prob", newdata = Bankfull)
perf1 = prediction(pred1[,2], Bankfull$y)
#Area under the curve 1:
auc1 = performance(perf1, measure = "auc")
auc1@y.values[[1]]
#True Positive and Negative Rate 1
tpnr1 = performance(perf1, "tpr", "fpr")
#ROC Curve Plot 1
plot(tpnr1, main="ROC Curve 1", col = 2, lwd = 2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
#Model 2
predictRF2 = predict(RFmodel2, newdata=Bankfull)
table(Bankfull$y, predictRF2)
pred2 = predict(RFmodel2, type = "prob", newdata = Bankfull)
perf2 = prediction(pred2[,2], Bankfull$y)
#Area under the curve 2:
auc2 = performance(perf2, measure = "auc")
auc2@y.values[[1]]
#True Positive and Negative Rate 2
tpnr2 = performance(perf2, "tpr", "fpr")
#ROC Curve Plot 2
plot(tpnr2, main="ROC Curve 2", col = 2, lwd = 2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
#Model 3
predictRF3 = predict(RFmodel3, newdata=Bankfull)
table(Bankfull$y, predictRF3)
pred3 = predict(RFmodel3, type = "prob", newdata = Bankfull)
perf3 = prediction(pred3[,2], Bankfull$y)
#Area under the curve 3:
auc3 = performance(perf3, measure = "auc")
auc3@y.values[[1]]
#True Positive and Negative Rate 3
tpnr3 = performance(perf3, "tpr", "fpr")
#ROC Curve Plot 3
plot(tpnr3, main="ROC Curve 3", col = 2, lwd = 2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
#Model 4
predictRF4 = predict(RFmodel4, newdata=Bankfull)
table(Bankfull$y, predictRF4)
pred4 = predict(RFmodel4, type = "prob", newdata = Bankfull)
perf4 = prediction(pred4[,2], Bankfull$y)
#Area under the curve 4:
auc4 = performance(perf4, measure = "auc")
auc4@y.values[[1]]
#True Positive and Negative Rate 4
tpnr4 = performance(perf4, "tpr", "fpr")
#ROC Curve Plot 4
plot(tpnr4, main="ROC Curve 4", col = 2, lwd = 2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")












