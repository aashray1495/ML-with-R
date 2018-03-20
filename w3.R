library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(dplyr)
library(ggplot2)
library(randomForest)

df=read.csv('C:/Users/Aashr/Desktop/IEOR 242/W3/Letters242.csv')
head(df)
table(df$letter)
richard@twindom.comboxplot(split(df$ybox,df$letter),main='ybox by Letter')
boxplot(split(df$width,df$letter),main='width by Letter')
boxplot(split(df$height,df$letter),main='height by Letter')
boxplot(split(df$onpix,df$letter),main='On Pixels by Letter')



plot(df[,-c(1)])

dfs <- stack(df$xbox +df$ybox)
ggplot(dfs, aes(x=values)) + geom_density(aes(group=ind))

histogram(~ letter, data = df)
histogram(df$letter)

plot(density(df$xbox))
densityplot(~ xbox, group = letter, data = df, auto.key = TRUE)
densityplot(~ ybox, group = letter, data = df, auto.key = TRUE)
densityplot(~ width, group = letter, data = df, auto.key = TRUE)
densityplot(~ height, group = letter, data = df, auto.key = TRUE)

ggplot(df) + geom_density(aes(x = xbox, fill = letter), alpha = 0.2)
densityplot(~ width | letter, data = df)
densityplot(~ height | letter, data = df)
densityplot(~ xbox | letter, data = df)
densityplot(~ ybox | letter, data = df)
df$isB = as.factor(df$letter == "B")
set.seed(623)
train.ids = sample(nrow(df), 0.65*nrow(df))
df.train = df[train.ids,]
df.test = df[-train.ids,]

table(df.test$isB)

accu1 <- 827/(827+264)
print(accul)

model <- glm(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor,family='binomial',data=df.train)
summary(model)

predTest = predict(model, newdata=df.test, type="response")
summary(predTest)
table(df.test$isB, predTest > 0.5)
(798+29)/nrow(df.test)

rocr.log.pred <- prediction(predTest, df.test$isB)
logPerformance <- performance(rocr.log.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.log.pred, "auc")@y.values)

mod <- rpart(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor,
             data = df.train, method="class", 
             minbucket=5, cp = 0.001)


prp(mod)
#CART with CP
cpVals = data.frame(cp = seq(0, .04, by=.002))


set.seed(123)
train.cart <- train(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor,
                    data = df.train,
                    method = "rpart",
                    tuneGrid = cpVals,
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

# look at the cross validation results, stored as a data-frame
# https://machinelearningmastery.com/machine-learning-evaluation-metrics-in-r/
train.cart$results # please ignore kappa
train.cart

# plot the results
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point()
# We can increase the size of the points:
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3)
# We can change the default axis labels
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()


# Extract the best model and make predictions
train.cart$bestTune
mod123 = train.cart$finalModel
prp(mod123, digits=3)

# We need to extract the "model matrix" for parole.test before we can make predictions
# This is because caret does not work with factors, instead it creates dummy variables 
df.test.mm = as.data.frame(model.matrix(isB~.+0, data=df.test))
pred = predict(mod123, newdata=df.test.mm, type="class")
table(df.test$isB, pred)



# Now let's try random forests
# First, basic training of a RF, can take a minute
mod.rf <- randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor, data = df.train)

pred.rf <- predict(mod.rf, newdata = df.test) # just to illustrate

table(df.test$isB, pred.rf)
(816+11)/(827+237+27+11)

table(df.test$letter)
298/(298+264+258+271)

library(MASS)

lda_mod <- lda(letter~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor, data = df.train)

predTest_LDA <- predict(lda_mod, newdata=df.test)

predTest_LDA_probs <- predTest_LDA$posterior[,2]

table(df.test$letter, predTest_LDA_probs > 0.5)

cpVals = data.frame(cp = seq(0, .04, by=.002))


set.seed(123)
train.cart <- train(letter ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + xy2bar + xedge + yedge + xedgeycor + yedgexcor,
                    data = df.train,
                    method = "rpart",
                    tuneGrid = cpVals,
                    trControl = trainControl(method = "cv", number=10),
                    metric = "Accuracy")

train.cart$results # please ignore kappa
train.cart


# plot the results
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point()
# We can increase the size of the points:
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3)
# We can change the default axis labels
ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
  xlab("Complexity Parameter (cp)") + geom_line()


# Extract the best model and make predictions
train.cart$bestTune
mod123 = train.cart$finalModel
prp(mod123, digits=3)

# We need to extract the "model matrix" for parole.test before we can make predictions
# This is because caret does not work with factors, instead it creates dummy variables 
df.test.mm = as.data.frame(model.matrix(letter~.+0, data=df.test))
pred = predict(mod123, newdata=df.test.mm, type="class")
table(df.test$letter, pred)
