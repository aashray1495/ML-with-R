df=read.csv('C:/Users/Aashr/Desktop/IEOR 242/Letters242.csv')
df
split <- sample.split(df$TenYearCHD, 0.65)
train <- filter(df, split == TRUE)
test <- filter(df, split == FALSE)
model <- glm(Letter ~.,family='binomial',data=train)
summary(model)


predTest = predict(model, newdata=test, type="response")
summary(predTest)

table(test$isB, predTest > 0.5)

accu1 <- (635+115)/nrow(test)
#TPR for test set for model 1
TPR1 <- 115/(115+52)
#FPR for test set for model 1
FPR1 <- 295/(295+635)
TPR1
print(paste("Accuracy: ", accu1))
print(paste("TPR: ", TPR1))
print(paste("FPR: ", FPR1))

#a.v
#Assuming treatment to be provided if p>0.15
#We add a column to dataset if treatment should be provided or not.
test$Treatment <- 0
test[predTest > 0.15,]$Treatment <- 1

#Add column to estimate expected cost for patients if treatment doesn't affect CHR risk
test$CHR.Estimate <- predTest
test$cost1 <- 0
test$cost1 <- (test$Treatment*test$CHR.Estimate*442000) + 
  (test$Treatment*(1-test$CHR.Estimate)*42000) +
  ((1-test$Treatment)*test$CHR.Estimate*400000)
test$cost2 <- 0
test$cost2 <- ((test$Treatment)*(test$CHR.Estimate/3)*442000) + 
  (test$Treatment*(1-(test$CHR.Estimate/3))*42000) +
  ((1-test$Treatment)*test$CHR.Estimate*400000)
write.csv(test, "costs1.csv")

#Performance of baseline model
table(test$TenYearCHD, predTest > 1)
test$Treatmentb <- 0
test[predTest > 1,]$Treatmentb <- 1

#Add column to estimate expected cost for patients if treatment doesn't affect CHR risk for baseline model
test$CHR.Estimater <- predTest
test$cost3 <- 0
test$cost3 <- (test$Treatmentb*test$CHR.Estimater*442000) + 
  (test$Treatmentb*(1-test$CHR.Estimater)*42000) +
  ((1-test$Treatmentb)*test$CHR.Estimater*400000)
test$cost4 <- 0
test$cost4 <- ((test$Treatmentb)*(test$CHR.Estimater/3)*442000) + 
  (test$Treatmentb*(1-(test$CHR.Estimater/3))*42000) +
  ((1-test$Treatmentb)*test$CHR.Estimater*400000)
write.csv(test, "costs2.csv")

#a.vii
#Predict if woman will get CHD in next 10 years and if treatment should be given

patient.chd <- data.frame(male=0, age=51, education = "College", currentSmoker = 1, cigsPerDay = 20,
                          BPMeds = 0, prevalentStroke = 0, prevalentHyp = 1, diabetes = 0, totChol = 220, 
                          sysBP = 140, diaBP = 100, BMI = 31, heartRate = 59, glucose = 78)
predict(model, newdata=patient.chd, type="response")


# Question 3: Part b
rocr.log.pred <- prediction(predTest, test$TenYearCHD)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

#Question 3: Part c
library(MASS)
ldamod <- lda(TenYearCHD ~., data=train)

predTestLDA <- predict(ldamod, newdata=test)
predTestLDA_probs <- predTestLDA$posterior[,2]

#LDA Performance
table(test$TenYearCHD, predTestLDA_probs > 0.15)

#LDA ROCR
rocr.lda.pred <- prediction(predTestLDA_probs, test$TenYearCHD)
ldaPerformance <- performance(rocr.lda.pred, "tpr", "fpr")
plot(ldaPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.lda.pred, "auc")@y.values)

#Combined Logistic-LDA ROCR plots
plot(logPerformance, col="blue")
plot(ldaPerformance, col="red", add=TRUE)
abline(0,1)