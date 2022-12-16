#PROJECT
library(pROC)
library(car)
library(MASS)
library(plyr)
library(epitools)
library(rpart)
library(regclass)
library(e1071) 
library(psych)
library(dplyr)


#Data Process
mydata <- cbind(ProjectData[3:18])
View(mydata)
dead<-mydata$APP_DEAD
table(dead)

#Descriptives
describe(mydata)
cor(mydata)
table(mydata$APP_ISPAID)
table(mydata$INAPP_PURCHASE)
table(mydata$CONTENT_TYPE)
table(mydata$APP_ENGLISH)
table(mydata$TOP_DEVELOPER)

#Model-free analysis
#Chi-square tests
table(mydata$APP_ISPAID,mydata$APP_DEAD)
prop.table(table(mydata$APP_ISPAID,mydata$APP_DEAD))
prop.table(table(mydata$APP_ISPAID,mydata$APP_DEAD),1)
chisq.test(mydata$APP_ISPAID,mydata$APP_DEAD)

table(mydata$INAPP_PURCHASE,mydata$APP_DEAD)
prop.table(table(mydata$INAPP_PURCHASE,mydata$APP_DEAD))
prop.table(table(mydata$INAPP_PURCHASE,mydata$APP_DEAD),1)
chisq.test(mydata$INAPP_PURCHASE,mydata$APP_DEAD)

table(mydata$TOP_DEVELOPER,mydata$APP_DEAD)
prop.table(table(mydata$TOP_DEVELOPER,mydata$APP_DEAD))
prop.table(table(mydata$TOP_DEVELOPER,mydata$APP_DEAD),1)
chisq.test(mydata$TOP_DEVELOPER,mydata$APP_DEAD)

table(mydata$APP_ENGLISH,mydata$APP_DEAD)
prop.table(table(mydata$APP_ENGLISH,mydata$APP_DEAD))
prop.table(table(mydata$APP_ENGLISH,mydata$APP_DEAD),1)
chisq.test(mydata$APP_ENGLISH,mydata$APP_DEAD)

table(mydata$CONTENT_TYPE,mydata$APP_DEAD)
prop.table(table(mydata$CONTENT_TYPE,mydata$APP_DEAD))
prop.table(table(mydata$CONTENT_TYPE,mydata$APP_DEAD),1)
chisq.test(mydata$CONTENT_TYPE,mydata$APP_DEAD)

#Logistic Regression
logistic <- glm(APP_DEAD ~. , family=binomial(link="logit"), na.action=na.pass, data=mydata, )
summary(logistic) 
#VIF
vif(logistic)

mydata2 <- mydata[,-13]
View(mydata2)
mydata3 <- mydata2[,-13]
View(mydata3)
mydata4 <- mydata3[,-13]
View(mydata4)
logistic2 <- glm(APP_DEAD ~. , family=binomial(link="logit"), na.action=na.pass, data=mydata4, )
summary(logistic2)
vif(logistic2)

#Step-wise Model Selection
step.model <- stepAIC(logistic)
summary(step.model)

#Final Model
finallog<- glm(APP_DEAD ~. , family=binomial(link="logit"), na.action=na.pass, data=mydata4, )
summary(finallog)
#Odds Ratio
exp(-0.8896688)
exp(-0.8925651)
exp(-1.0656039)
exp(-0.0214123)
exp(-0.0736729)
exp(-0.0067199)
exp(-0.4691008)
exp(0.8347211)
exp(-0.5032507)

#Confusion matrix
confusion_matrix(finallog)
confusion.log <- regclass::confusion_matrix(finallog)
confusion.log <- confusion.log[1:2,1:2]
prop.table(confusion.log)
prop.table(confusion.log,1)


plot(finallog$fitted.values)
hist(finallog$fitted.values, col="blue")

roc(mydata4$APP_DEAD, as.vector(fitted.values(finallog)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, 
    plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, 
    main = paste("ROC curve using","(N = ",nrow(mydata4),")") )

##Decision Tree
#Tree with all variables
tree <- rpart(APP_DEAD ~. , data=mydata4, cp=.001, method="class",  parms=list(split="gini"))
printcp(tree)
prune.t <- prune(tree, cp=0.0089366)
predictions <- predict(prune.t, mydata4[,-13], type="class")
confusion.dt <- table(predictions, mydata4$APP_DEAD)
confusion.dt 
prop.table(confusion.dt)

#Tree with significant variables
tree2 <- rpart(APP_DEAD~APP_ISPAID+INAPP_PURCHASE+APP_ENGLISH+APP_DESCRIPTION+APP_IMAGE+APP_DEVNUM+RATING_AVG+
                    RATING_DEV+CUM_TOP, data=mydata4, cp=.001, method="class",  parms=list(split="gini"))
printcp(tree2) 
prune.t2 <- prune(tree2, cp=0.0071492 )
predictions2 <- predict(prune.t2, mydata4[,-13], type="class")
confusion.dt2 <- table(predictions2, mydata4$APP_DEAD)
confusion.dt2
prop.table(confusion.dt2)

#Visualized Decision Tree
library(rpart.plot)
rpart.plot(prune.t)
prp(prune.t,  faclen  = 0, cex = 0.75, extra = 1)
rpart.plot(prune.t2)
prp(prune.t2,  faclen  = 0, cex = 0.75, extra = 1)

##Discriminant Analysis
da<-lda(mydata4[,-13],mydata$APP_DEAD,prior = c(1,1)/2,CV=TRUE)
da
da$class
confusion.t.da <- table(mydata4$APP_DEAD,da$class)
confusion.t.da
prop.table(confusion.t.da)

#Support Vector Machine
mydata4$APP_DEAD = factor(mydata$APP_DEAD, level = c(0,1), label = c("Survival","Death"))
model <- svm(APP_DEAD ~ ., data = mydata4)     
summary(model)
pred = fitted(model)
obs = ProjectData$APP_DEAD_CODE
confusion.svm <- table(pred, obs)		
confusion.svm
prop.table(confusion.svm)
#goodness of fit
confusion.svm/sum(confusion.svm)

