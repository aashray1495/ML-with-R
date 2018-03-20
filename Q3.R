install.packages(c("caTools", "ROCR", "dplyr", "ggplot2", "GGally"))

#Load packages
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)

#Read Data
heart <- read.csv("C:/Users/Aashr/Desktop/IEOR 242/framingham.csv")

set.seed(144)

#Split data into train and test set
split = sample.split(heart$TenYearCHD, SplitRatio = 0.7)
heart.train <- filter(heart, split == TRUE)
heart.test <- filter(heart, split == FALSE)

#Train dataset statistics
table(heart.train$TenYearCHD)

ggscatmat(heart.train$male, alpha = 0.8)

#Logistic regression model

mod <- glm(TenYearCHD ~., data=heart.train$male, family="binomial")
summary(mod)
oddsratio(mod)

#Applying the model to test set
#Break-even p=0.15, i.e. Only reasonable to prescribe medication if p>0.15

predTest = predict(mod$male, newdata=heart.train, type="response")
summary(predTest)

table(heart.test$TenYearCHD, predTest > 0.15)
#Accuracy of model
accu1 <- (604+105)/nrow(heart.test)
#TPR for test set for model 1
TPR1 <- 105/(105+62)
#FPR for test set for model 1
FPR1 <- 326/(326+604)
TPR1
print(paste("Accuracy: ", accu1))
print(paste("TPR: ", TPR1))
print(paste("FPR: ", FPR1))

#Logistic regression model 2: Dropping heartRate

mod2 <- glm(TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + 
            prevalentHyp + totChol + sysBP + diaBP + BMI + diabetes + glucose, data=heart.train, family="binomial")
summary(mod2)

#Logistic regression model 3: Dropping diabetes

mod3 <- glm(TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + 
              prevalentHyp + totChol + sysBP + diaBP + BMI + glucose, data=heart.train, family="binomial")
summary(mod3)

#Logistic regression model 4: Dropping BMI

mod4 <- glm(TenYearCHD ~ male + age + education + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + 
              prevalentHyp + totChol + sysBP + diaBP + glucose, data=heart.train, family="binomial")
summary(mod4)

#Logistic regression model 5: Dropping education

mod5 <- glm(TenYearCHD ~ male + age + currentSmoker + cigsPerDay + BPMeds + prevalentStroke + 
              prevalentHyp + totChol + sysBP + diaBP + glucose, data=heart.train, family="binomial")
summary(mod5)

#Logistic regression model 6: Dropping BPMeds

mod6 <- glm(TenYearCHD ~ male + age + currentSmoker + cigsPerDay + prevalentStroke + 
              prevalentHyp + totChol + sysBP + diaBP + glucose, data=heart.train, family="binomial")
summary(mod6)

#Logistic regression model 7: Dropping currentSmoker

mod7 <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
              prevalentHyp + totChol + sysBP + diaBP + glucose, data=heart.train, family="binomial")
summary(mod7)

#Logistic regression model 8: Dropping diaBP

mod8 <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
              prevalentHyp + totChol + sysBP + glucose, data=heart.train, family="binomial")
summary(mod8)

#Logistic regression model 9: Dropping prevalentHyp

mod9 <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
              totChol + sysBP + glucose, data=heart.train, family="binomial")
summary(mod9)

#Logistic regression model 10: Dropping totChol

mod10 <- glm(TenYearCHD ~ male + age + cigsPerDay + prevalentStroke + 
              sysBP + glucose, data=heart.train, family="binomial")
summary(mod10)

#Logistic regression model 11: Dropping prevalentStroke

mod11 <- glm(TenYearCHD ~ male + age + cigsPerDay + sysBP + glucose, data=heart.train, family="binomial")
summary(mod11)

#Applying the model to test set
#Break-even p=0.15, i.e. Only reasonable to prescribe medication if p>0.15

predTest = predict(mod, newdata=heart.test, type="response")
summary(predTest)

table(heart.test$TenYearCHD, predTest > 0.15)
#Accuracy of model
accu1 <- (612+114)/nrow(heart.test)
#TPR for test set for model 1
TPR1 <- 114/(114+53)
#FPR for test set for model 1
FPR1 <- 318/(318+612)
print(paste("Accuracy: ", accu1))
print(paste("TPR: ", TPR1))
print(paste("FPR: ", FPR1))

predTest2 = predict(mod11, newdata=heart.test, type="response")
summary(predTest2)

table(heart.test$TenYearCHD, predTest2 > 0.15)
#Accuracy of model
accu2 <- (607+111)/nrow(heart.test)
#TPR for test set for model 1
TPR2 <- 111/(111+56)
#FPR for test set for model 1
FPR2 <- 323/(323+607)
print(paste("Accuracy: ", accu2))
print(paste("TPR: ", TPR2))
print(paste("FPR: ", FPR2))

#a.v
#Assuming treatment to be provided if p>0.15
#We add a column to dataset if treatment should be provided or not.
heart.test$Treatment <- 0
heart.test[predTest > 0.15,]$Treatment <- 1

#Add column to estimate expected cost for patients if treatment doesn't affect CHR risk
heart.test$CHR.Estimate <- predTest
heart.test$cost1 <- 0
heart.test$cost1 <- (heart.test$Treatment*heart.test$CHR.Estimate*442000) + 
             (heart.test$Treatment*(1-heart.test$CHR.Estimate)*42000) +
            ((1-heart.test$Treatment)*heart.test$CHR.Estimate*400000)
heart.test$cost2 <- 0
heart.test$cost2 <- ((heart.test$Treatment)*(heart.test$CHR.Estimate/3)*442000) + 
  (heart.test$Treatment*(1-(heart.test$CHR.Estimate/3))*42000) +
  ((1-heart.test$Treatment)*heart.test$CHR.Estimate*400000)
#write.csv(heart.test, "hearttestcosts.csv")

#Performance of baseline model
table(heart.test$TenYearCHD, predTest > 1)

#a.vii
#Predict if woman will get CHD in next 10 years and if treatment should be given

patient.chd <- data.frame(male=0, age=51, education = "College", currentSmoker = 1, cigsPerDay = 20,
                          BPMeds = 0, prevalentStroke = 0, prevalentHyp = 1, diabetes = 0, totChol = 220, 
                          sysBP = 140, diaBP = 100, BMI = 31, heartRate = 59, glucose = 78)
predict(mod, newdata=patient.chd, type="response")

# Question 3: Part b
rocr.log.pred <- prediction(predTest, heart.test$TenYearCHD)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

#Question 3: Part c
library(MASS)
ldamod <- lda(TenYearCHD ~., data=heart.train)

predTestLDA <- predict(ldamod, newdata=heart.test)
predTestLDA_probs <- predTestLDA$posterior[,2]

#LDA Performance
table(heart.test$TenYearCHD, predTestLDA_probs > 0.15)

#LDA ROCR
rocr.lda.pred <- prediction(predTestLDA_probs, heart.test$TenYearCHD)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)

#Combined Logistic-LDA ROCR plots
plot(logPerformance, col="blue")
plot(ldaPerformance, col="red", add=TRUE)
abline(0,1)
