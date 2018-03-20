library(dplyr)
library(ggplot2)
library(lubridate)

# Out of sample R^2
OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# R^2 with a particular baseline
BaselineR2 <- function(predictions, truth, baseline) {
  SSE <- sum((truth - predictions)^2)
  SST <- sum((truth - baseline)^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# Load data and check it out
sales <- read.csv("sales.csv")
str(sales)

# Convert date to a better format using lubridate package This allows us to
# conveniently extract info from each date For more info, read R4DS (R for Data
# Science) book ymd() tells R that the date is currently in "year-month-day"
# format
sales <- sales %>% mutate(Date = ymd(Date))

# for example now we can just call year() function to get the year associated
# with each date
table(year(sales$Date))

# Use 2015 as testing data
salesTrain <- sales %>% filter(year(Date) < 2015)
salesTest <- sales %>% filter(year(Date) == 2015)

# Plot initial data -- ggplot knows how to plot dates
ggplot(sales, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point()

# Linear trend model training data -- Make a new column for the time period
# number (1, 2, ...). The dplyr syntax is a little tricky here -- n() is the
# number of rows in salesTrain, and seq_len(n()) returns the vector 1, 2, ...,
# n(). The end result is that we added a new variable called TimePeriod that
# takes values 1, 2, ..., n().
# https://stackoverflow.com/questions/25687727/shifting-a-column-down-by-one
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
salesTrainLM <- salesTrain %>% mutate(TimePeriod = seq_len(n()))
head(salesTrainLM)

# Build and plot linear trend model
modLM <- lm(Sales~TimePeriod, data=salesTrainLM)
summary(modLM)
# In the second geom_line statement, we are passing a new y variable containing the
# predictions. This results in an additional line with the original x (Date)
# being plotted against the new y (predictions).
ggplot(salesTrainLM, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(modLM)), col="red", lwd=1.5)



###### Random Walk model training data -- create a variable SalesYesterday
# that contains the sales yesterday. This new variable will start with
# NA (a missing value), because we don't have the data for the day before
# the first observation. After that we include all the Sales data except
# for the very last observation, which we can obtain with head(Sales, -1) --
# here "-1" means "all but the last 1 observation".
salesTrainRW <- salesTrain %>% mutate(SalesYesterday = c(NA, head(Sales, -1)))
head(salesTrainRW)

# Plot with an additional red line for our predictions as before
ggplot(salesTrainRW, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=SalesYesterday), col="red")
# What's the warning message about? 

# Zoom in to the last 50 observations by using tail
ggplot(tail(salesTrainRW, 50), aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=SalesYesterday), col="red")

# Proportion of points for which difference is more than 1000 sales units.
table(abs(salesTrainRW$Sales-salesTrainRW$SalesYesterday) >= 1000)

# Compute training set R^2
# Note that we need to remove the first observation since there is no
# prediction. This is achieved using tail(.., -1) which says to take all but
# the first observation.
# Why do we need a "special baseline"?
BaselineR2(tail(salesTrainRW$SalesYesterday, -1), 
           tail(salesTrainRW$Sales, -1),
           mean(salesTrainRW$Sales))


###### AR model

# We need to add sales yesterday and sales two days ago for the two term AR model
# head(.., -2) says take all but the last two 
salesTrainAR <- salesTrain %>%
  mutate(SalesYesterday=c(NA, head(Sales, -1))) %>%
  mutate(SalesTwoDaysAgo = c(NA, NA, head(Sales, -2)))
head(salesTrainAR)

# Do the regression with one lag term, and compute the number of days
# in the training set where our error is 1000 or greater.
mod2a <- lm(Sales~SalesYesterday, data=salesTrainAR)
summary(mod2a)
# Note the message "(1 observation deleted due to missingness)" in the
# regression output. This is because one of our observations in the training
# set (the first one) is missing the SalesYesterday value. Linear regression
# throws away observations that are missing any of the independent
# variables, while CART and Random Forest handle missing data much more
# gracefully.
table(abs(salesTrainAR$Sales-predict(mod2a, newdata=salesTrainAR)) >= 1000)

# Make the plot
ggplot(salesTrainAR, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod2a, newdata=salesTrainAR)), col="red")

# Zoom in
ggplot(tail(salesTrainAR, 50), aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=tail(predict(mod2a, newdata=salesTrainAR), 50)), col="red")

# 2-term autoregressive model -- all we do differently is add the
# SalesTwoDaysAgo variable to our linear regression model.
mod2b <- lm(Sales~SalesYesterday+SalesTwoDaysAgo, data=salesTrainAR)
summary(mod2b)


### Seasonality -- need to add variables for Month and DayOfWeek. This can easily
# be done with lubridate.
salesTrainLast <- salesTrainAR %>% 
  mutate(Month = factor(month(Date, label = TRUE, abbr = FALSE), ordered = FALSE)) %>%
  mutate(DayOfWeek = factor(wday(Date, label = TRUE), ordered = FALSE))
head(salesTrainLast)
str(salesTrainLast)

# Look at mean sales by month with a barplot, which we can get with
# geom_bar(stat="identity")
salesTrainLast %>%
  group_by(Month) %>%
  summarize(Sales=mean(Sales)) %>%
  ggplot(aes(x=Month, y=Sales)) + geom_bar(stat="identity")

# Look at mean sales by day of the week with a barplot.
salesTrainLast %>%
  group_by(DayOfWeek) %>%
  summarize(Sales=mean(Sales)) %>%
  ggplot(aes(x=DayOfWeek, y=Sales)) + geom_bar(stat="identity")

# Add DayOfWeek and Month to the auto-regressive model trained using
# the SalesYesterday variable.
mod2a.season1 <- lm(Sales~SalesYesterday+DayOfWeek+Month, data=salesTrainLast)
summary(mod2a.season1)

# Create separate variable for December, since this is the only month
# that seems significantly different from January.
salesTrainLast <- salesTrainLast %>%
  mutate(MonthDecember = ifelse(Month == "December", 1, 0))
mod2a.season <- lm(Sales~SalesYesterday+DayOfWeek+MonthDecember, data=salesTrainLast)
summary(mod2a.season)

# Add Promo and SchoolHoliday as well.
mod2a.fulla <- lm(Sales~SalesYesterday+DayOfWeek+MonthDecember+Promo+SchoolHoliday, data=salesTrainLast)
summary(mod2a.fulla)

# Remove SchoolHoliday.
mod2a.full <- lm(Sales~SalesYesterday+DayOfWeek+MonthDecember+Promo, data=salesTrainLast)
summary(mod2a.full)

# Final Model Fit:
ggplot(salesTrainLast, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod2a.full, newdata=salesTrainLast)), col="red")

# Zoom in
ggplot(tail(salesTrainLast, 50), aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=tail(predict(mod2a.full, newdata=salesTrainLast), 50)), col="red")


### Let's try basic Random Forests (no CV)
library(randomForest)
set.seed(349)
mod.rf <- randomForest(Sales ~ SalesYesterday + SalesTwoDaysAgo + Month + DayOfWeek + Promo + SchoolHoliday, data = tail(salesTrainLast, -2))

ggplot(salesTrainLast, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod.rf, newdata=salesTrainLast)), col="green")

# Zoom in
ggplot(tail(salesTrainLast, 50), aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=tail(predict(mod.rf, newdata=salesTrainLast), 50)), col="green")


# Both on the same plot:
ggplot(salesTrainLast, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod2a.full, newdata=salesTrainLast)), col="red") +
  geom_line(aes(y=predict(mod.rf, newdata=salesTrainLast)), col="green")

# Zoom in
ggplot(tail(salesTrainLast, 50), aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=tail(predict(mod2a.full, newdata=salesTrainLast), 50)), col="red") +
  geom_line(aes(y=tail(predict(mod.rf, newdata=salesTrainLast), 50)), col="green")


# Create the test set with the extra columns. Note that we could have
# avoided the duplication of effort in building salesTrainLast and
# salesFinalTest by first adding all our variables to "sales" (our original
# data frame) and then splitting it.
salesFinalTest <- sales %>%
  mutate(SalesYesterday=c(NA, head(Sales, -1))) %>%
  mutate(SalesTwoDaysAgo = c(NA, NA, head(Sales, -2))) %>%
  mutate(Month = factor(month(Date, label = TRUE, abbr = FALSE), ordered = FALSE)) %>%
  mutate(DayOfWeek = factor(wday(Date, label = TRUE), ordered = FALSE)) %>%
  mutate(MonthDecember=ifelse(Month == "December", 1, 0)) %>%
  filter(year(Date) == 2015)

# Test set prediction and OSR^2
# Test-set prediction
pred.test <- predict(mod2a.full, newdata = salesFinalTest)
OSR2(pred.test, salesTrainLast$Sales, salesFinalTest$Sales)

pred.test.rf <- predict(mod.rf, newdata = salesFinalTest)
OSR2(pred.test.rf, salesTrainLast$Sales, salesFinalTest$Sales)

# Test set plots
ggplot(salesFinalTest, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=pred.test), col="red")

ggplot(salesFinalTest, aes(x=Date, y=Sales)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=pred.test), col="red") +
  geom_line(aes(y=pred.test.rf), col="green")

