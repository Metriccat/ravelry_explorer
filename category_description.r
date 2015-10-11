# Build a dataset of pattern categories and text description from Ravelry pages
# Try some classification algorithms on it

library(XML)
library(RCurl)
library(tm)
library(e1071)
library(nnet)
library(glmnet)
library(plyr)

## Build dataset from Ravelry API: pattern permalink, pattern category, pattern text description
# Get url to patterns of interest from API search
pat0 <- GET("https://api.ravelry.com/patterns/search.json?page_size=2000&craft=knitting", config=config("token"=ravelry.token))
pat <- content(pat0)

permalinks <- sapply(pat$patterns, function(x) x$permalink)
permalinks_full <- sapply(permalinks, function(name) paste("http://www.ravelry.com/patterns/library/",name,sep="",collapse=""))
names(permalinks_full) <- permalinks

# Get top level pattern category and description text using web scraping 
pattern_info <- lapply(permalinks_full, htmlTreeParse, useInternalNodes = TRUE)

pattern_description_par <- lapply(pattern_info, getNodeSet, path="//p", fun=xmlValue)
pattern_description <- sapply(pattern_description_par, paste, collapse=" ")

pattern_cat <- lapply(pattern_info, getNodeSet, path="//div[@class='category']/a/span/text()", fun=xmlValue)
pattern_topcat <- simplify2array(sapply(pattern_cat, head, 1))

## Data: 3 columns with pattern permalink, text description, and toplevel category
data <- as.data.frame(cbind(permalinks, pattern_topcat, pattern_description),stringsAsFactors=F,row.names=F)
names(data) <- c("permalink", "category", "description")
data$category <- as.factor(data$category)

cat_freq <- table(data$category)
nbr_examples <- dim(data)[1]

# Remove from data the categories with too few examples
data <- subset(data, subset=(cat_freq[category] > nbr_examples/5))
data$category <- factor(data$category)
str(data)

# Keep only some categories (manually)
#data <- subset(data, subset = data$category %in% c("Sweater","Neck / Torso","Feet / Legs","Hat","Hands"))
#data$category <- factor(data$category)

## Text mining to predict category

cleanCorpus = function(corpus){
  # To lowercase
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove stopwords first, else for ex. l'or becomes lor and l' is not removed
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # Remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(function(str) gsub("[^[:alnum:] ]", " ",str)))
  # Remove  html tags with regexp
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("<[a-z]*>", " ", x)))
  # Remove numbers - but they may be useful ... TODO ?
  corpus <- tm_map(corpus, removeNumbers)
  # Simplify whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Stem words (tm_map stem has type error), use option lazy=T on mac os
  corpus <- tm_map(corpus, stemDocument, "english", lazy=T)
}

buildData = function(corpus, sparsity=0.999){
  # Arg: corpus where one document is one pattern description
  #      optionnal float word sparsity threshold 
  #      default: remove (almost) nothing
  # Returns Document Term Matrix
  dtm <- DocumentTermMatrix(corpus)
  # remove words that don't appear often enough for every category, else weird words and very large matrix
  dtm <- removeSparseTerms(dtm, sparsity)
}

## Build train and test corpus and associated dtm 
test_ind <- sample(dim(data)[1], dim(data)[1]/3)
test_data <- data[test_ind,]
train_data <- data[-test_ind,]

corpus_train <- Corpus(VectorSource(train_data$description))
names(corpus_train) <- train_data$category
y_train <- train_data$category

# Check the category frequencies
sort(table(data$category))
# Check text example
corpus_train[[1]]$content

corpus_test <- Corpus(VectorSource(test_data$description))
names(corpus_test) <- test_data$category
y_test <- test_data$category

clean_train <- cleanCorpus(corpus_train)
clean_test <- cleanCorpus(corpus_test)

clean_train[[1]]

dtm_train = buildData(clean_train,0.9)
train <- as.data.frame(as.matrix(dtm_train))
names(train) <- dtm_train$dimnames$Terms
train[1:3,1:3]

dtm_test = buildData(clean_test,0.9)
test <- as.data.frame(as.matrix(dtm_test))
names(test) <- dtm_test$dimnames$Terms

## Naive bayes 
# Tests with various categories show that perf goes down when adding
# categories with not a lot of cases, perf good with 2 top frequent factors
model <- naiveBayes(train, y_train)
# Can't predict categories never seen in train set => remove them
filter <- names(test) %in% names(train) 
test2 <- test[,filter]
pred <- predict(model, test)
(cbind(y_test,pred))
# Inspect model: probabilities of good predictors ("sweater", "warm") are high for the relevant categories
mean(pred==y_test)
table(pred, y_test)
# Interpretation: mean number of word occurences in each target class
count <- model$tables
count2 <- as.data.frame(do.call(rbind,count), row.names=seq_along(count))
names(count2) <- c("meanOcc","stdOcc")
count2$word = rep(names(count), each=2)
count2$word = rep(names(count), each=2)
count2$categ = rep(c("Neck / Torso","Sweater"), length(count2)/2)
head(arrange(count2,desc(meanOcc)))

## Regularized logistic regression 
regLinCV <- cv.glmnet(as.matrix(train), y_train, family="binomial", alpha=1)
plot(regLinCV)
# Get consistent set of words between test and train
# Keep from test only the words that also are in train data
test2 <- test[,intersect(colnames(train),colnames(test))]
# Add to test the words that were in train data but not test data, marks as 0 occurences
# TODO: clean this ...
trainWordsNotInTest <- setdiff(names(train), names(test2))
yy <- data.frame(matrix(0, ncol = length(trainWordsNotInTest),
                        nrow = dim(test2)[1]))
names(yy) <- trainWordsNotInTest
names(yy) <- names(train)
# Final processed test set
test3 <- cbind(test2, yy)
# Prediction: NB returns probabilities for each category, take max as prediction
newCV <- predict(regLinCV, as.matrix(test3), s="lambda.min")
# For multinomial (> 2 categories)
newCVmax = apply(newCV[,,1], MARGIN=1, FUN=function(row) names(row)[which.is.max(row)])
# For binomial (use only 2 categories)
newCVmax = as.factor(ifelse(newCV < 0.5,"Sweater","Neck / Torso"))
# Percentage of correct answers on our test set
mean(newCVmax == as.character(y_test))
table(newCVmax, as.character(y_test))

# svm notes - TODO
mysvm <- svm(dtm_train, y_train)
dtm_test_filtered <- only terms appearing in test set
pred <- predict(mysvm, dtm_test_filtered)
