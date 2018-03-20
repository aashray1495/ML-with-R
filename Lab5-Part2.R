# OK, we are now, finally, ready to build models!
# Up to here, we have just pre-processed and prepared our data.

# Split data into training and testing sets
set.seed(123)  # So we get the same results
spl = sample.split(TweetsTM$Negative, SplitRatio = 0.7)

TweetsTrain = TweetsTM %>% filter(spl == TRUE)
TweetsTest = TweetsTM %>% filter(spl == FALSE)

# Side note: explain the pipe %>% 
# Many functions in dplyr, ggplot, and R in general take some type of data as
# the first argument, then they apply some type of transformation which is
# specified by the second, third, fourth, etc. arguments
# For example, filter(TweetsTM, spl == TRUE) says to filter the rows of TweetsTM
# to those with spl == TRUE
# The pipe operator %>% feeds the left side into the first argument of the right
# side
# Here we apply it once, but the real power comes in applying this
# recursively...


# Now let's build some models:

# Baseline accuracy
table(TweetsTrain$Negative)
table(TweetsTest$Negative)

300/(300 + 55)


# Basic Random Forests:
TweetRF = randomForest(Negative ~ ., data=TweetsTrain)

PredictRF = predict(TweetRF, newdata = TweetsTest)
table(TweetsTest$Negative, PredictRF)
tableAccuracy(TweetsTest$Negative, PredictRF)


# Cross-validated CART model
set.seed(3421)
train.cart = train(Negative ~ .,
                   data = TweetsTrain,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.4, 0.002)),
                   trControl = trainControl(method="cv", number=10))
train.cart
train.cart$results

ggplot(train.cart$results, aes(x = cp, y = Accuracy)) + geom_point(size = 2) + geom_line() + 
  ylab("CV Accuracy") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

mod.cart = train.cart$finalModel
prp(mod.cart)

predict.cart = predict(mod.cart, newdata = TweetsTest, type = "class") # why no model.matrix? 
table(TweetsTest$Negative, predict.cart)
tableAccuracy(TweetsTest$Negative, predict.cart)


# Cross validated RF
# WARNING: this took me approx. 1 hour to run
set.seed(311)
train.rf = train(Negative ~ .,
                 data = TweetsTrain,
                 method = "rf",
                 tuneGrid = data.frame(mtry = 1:120),
                 trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
train.rf
train.rf$results

ggplot(train.rf$results, aes(x = mtry, y = Accuracy)) + geom_point(size = 2) + geom_line() + 
  ylab("CV Accuracy") + theme_bw() + 
  theme(axis.title=element_text(size=18), axis.text=element_text(size=18))

mod.rf = train.rf$finalModel
predict.rf = predict(mod.rf, newdata = TweetsTest)
table(TweetsTest$Negative, predict.rf)
tableAccuracy(TweetsTest$Negative, predict.rf)

# Variable importance using dplyr and the pipe
# Step 1: turn mod.rf$importance into a data frame
# Step 2: create a new variable (column) called Words equal to the rownames of mod.rf$importance
# Step 3: arrange in descendending order according to variable importance measure
as.data.frame(mod.rf$importance) %>%
  mutate(Words = rownames(mod.rf$importance)) %>%
  arrange(desc(MeanDecreaseGini))


# Logistic Regression

TweetLog = glm(Negative ~ ., data = TweetsTrain, family = "binomial")
# You may see a warning message - suspicious, but we will just ignore this
summary(TweetLog)

# Predictions on test set
PredictLog = predict(TweetLog, newdata = TweetsTest, type = "response")
table(TweetsTest$Negative, PredictLog > 0.5)
tableAccuracy(TweetsTest$Negative, PredictLog > 0.5)
# Not as good as CART or RF

# But what about training set?
PredictLogTrain = predict(TweetLog, type = "response")
table(TweetsTrain$Negative, PredictLogTrain > 0.5)
tableAccuracy(TweetsTrain$Negative, PredictLogTrain > 0.5)

# What about CART on training set?
PredictCARTTrain = predict(mod.cart, type = "class")
table(TweetsTrain$Negative, PredictCARTTrain)
tableAccuracy(TweetsTrain$Negative, PredictCARTTrain)
# Quite similar to test set performance

# Let's do stepwise regression
# WARNING: this took ~20 mins
TweetStepLog = step(TweetLog, direction = "backward")
summary(TweetStepLog)
length(TweetStepLog$coefficients)
# may want to manually prune beyond this point...

PredictStepLog = predict(TweetStepLog, newdata = TweetsTest, type = "response")
table(TweetsTest$Negative, PredictStepLog > 0.5)
tableAccuracy(TweetsTest$Negative, PredictStepLog > 0.5)


# Boosting
tGrid = expand.grid(n.trees = (1:100)*50, interaction.depth = c(1,2,4,6,8,10,12,14,16),
                    shrinkage = 0.01, n.minobsinnode = 10)

set.seed(232)
# WARNING: this took me ~1 hour
train.boost <- train(Negative ~ .,
                     data = TweetsTrain,
                     method = "gbm",
                     tuneGrid = tGrid,
                     trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                     metric = "Accuracy",
                     distribution = "bernoulli")
train.boost
train.boost$results

ggplot(train.boost$results, aes(x = n.trees, y = Accuracy, colour = as.factor(interaction.depth))) + geom_line() + 
  ylab("CV Accuracy") + theme_bw() + theme(axis.title=element_text(size=18), axis.text=element_text(size=18)) + 
  scale_color_discrete(name = "interaction.depth")

mod.boost = train.boost$finalModel

TweetsTest.mm = as.data.frame(model.matrix(Negative ~ . +0, data = TweetsTest))
predict.boost = predict(mod.boost, newdata = TweetsTest.mm, n.trees = 4100, type = "response")
table(TweetsTest$Negative, predict.boost < 0.5) # for some reason the probabilities are flipped in gbm
tableAccuracy(TweetsTest$Negative, predict.boost < 0.5)


# Linear Discriminant Analysis
# Normally distributed features?
library(MASS)
lda.mod = lda(Negative ~ ., data = TweetsTrain)

predict.lda = predict(lda.mod, newdata = TweetsTest)$class
table(TweetsTest$Negative, predict.lda)
tableAccuracy(TweetsTest$Negative, predict.lda)

