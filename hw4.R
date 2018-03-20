# First, install the required packages
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
Z
# Load packages
library(tm)
library(SnowballC)
library(wordcloud)
library(MASS)
library(caTools)
library(dplyr)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)


df=read.csv('C:/Users/Aashr/Desktop/IEOR 242/ggplot2questions2018.csv', stringsAsFactors=FALSE)
#removing tags
library(tm)
library(tm.plugin.webmining)
for (i in 0:length(df)-1) {
  df$Body[i]=extractHTMLStrip(df$Body[i])
}
print("HTML data successfully parsed.")
head(df$body)


# Lets create a new variable called "Useful" that converts the 
# "sentiment" number to positive (or not negative)
# anything to useful >=1
df$Useful = as.factor(as.numeric(df$Score >= 1))


# Before going any further, lets understand the rough distribution of
# useful questions in our data set
table(df$Useful)

acc=3604/(3604+3014) *100
# baseline accuracy is 54.457%. We will classify every question as useful

# Step 1: Convert questions to a "corpus"
# A vector source interprets each element of the vector as a document.
# Corpus creates a collection of documents 
corpus = Corpus(VectorSource(df$Body))
# The tweets are now "documents"
corpus[[1]]
strwrap(corpus[[1]])

# Step 2: Change all the text to lower case.
# tm_map applies an operation to every document in our corpus
# Here, that operation is 'tolower', i.e., 'to lowercase'
corpus = tm_map(corpus, tolower)
# Lets check:
strwrap(corpus[[1]])


# Step 3: Remove all punctuation
corpus = tm_map(corpus, removePunctuation)
# Take a look:
strwrap(corpus[[1]])

# Step 4: Remove stop words
# First, take a look at tm's stopwords:
stopwords("english")[1:10]
length(stopwords("english"))
# Just remove stopwords:
# corpus = tm_map(corpus, removeWords, stopwords("english"))
# Remove stopwords and "apple" - this is a word common to all of our tweets
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# Take a look:
strwrap(corpus[[1]])

# Step 5: Stem our document
# Recall, this means chopping off the ends of words that aren't maybe
# as necessary as the rest, like 'ing' and 'ed'
corpus = tm_map(corpus, stemDocument)
# Take a look:
strwrap(corpus[[1]])


# Step 6: Create a word count matrix (rows are tweets, columns are words)
# We've finished our basic cleaning, so now we want to calculate frequencies
# of words across the tweets
frequencies = DocumentTermMatrix(corpus)
# We can get some summary information by looking at this structure
frequencies


# Step 7: Account for sparsity
# We currently have way too many words, which will make it hard to train
# our models and may even lead to overfitting.
# Use findFreqTerms to get a feeling for which words appear the most

# Words that appear at least 50 times:
findFreqTerms(frequencies, lowfreq=50)
# Words that appear at least 20 times:
findFreqTerms(frequencies, lowfreq=20)

# Our solution to the possibility of overfitting is to only keep terms
# that appear in x% or more of the tweets. For example:
# 1% of the tweets or more (= 12 or more)
sparse = removeSparseTerms(frequencies, 0.99)

# 0.5% of the tweets or more (= 6 or more)
sparse = removeSparseTerms(frequencies, 0.995)
# How many did we keep?
sparse

# Let's keep it at the 1%
sparse = removeSparseTerms(frequencies, 0.99)


# Step 8: Create data frame from the document-term matrix
QTM = as.data.frame(as.matrix(sparse))
# We have some variable names that start with a number, 
# which can cause R some problems. Let's fix this before going
# any further
colnames(QTM) = make.names(colnames(QTM))
# This isn't our original dataframe, so we need to bring that column
# with the dependent variable into this new one
QTM$Useful = df$Useful

# Bonus: make a cool word cloud!
wordcloud(corpus, max.words = 200, random.order = FALSE, rot.per = .1, 
          colors = brewer.pal(8, "Dark2"))

# Split data into training and testing sets
set.seed(123)  # So we get the same results
spl = sample.split(QTM$Useful, SplitRatio = 0.7)

Train = QTM %>% filter(spl == TRUE)
Test = QTM %>% filter(spl == FALSE)

# Baseline accuracy
table(Train$Useful)
table(Test$Useful)

2523/(2523+2110)

# Basic Random Forests:
QRF = randomForest(Useful ~ ., data=Train)

PredictRF = predict(QRF, newdata = Test)
table(Test$Useful, PredictRF)
tableAccuracy(Test$Useful, PredictRF)


# Cross-validated CART model
set.seed(3421)
train.cart = train(Useful ~ .,
                   data = Train,
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

predict.cart = predict(mod.cart, newdata = Test, type = "class") # why no model.matrix? 
table(Test$Useful, predict.cart)
tableAccuracy(Test$Useful, predict.cart)
