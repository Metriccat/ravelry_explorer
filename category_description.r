# Build a dataset of pattern categories and text description from Ravelry pages
# Try some classification algorithms on it 

library(XML)
library(httr)
library(tm)
library(randomForest)
library(caret)

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
data <- subset(data, subset=(cat_freq[category] > 50))
data$category <- factor(data$category)
# str(data)

# Keep only some categories (manually)
#data <- subset(data, subset = data$category %in% c("Sweater","Feet / Legs","Hat", "Neck / Torso"))
#data$category <- factor(data$category)

cleanCorpus = function(corpus){
  # Clean the text data to remove punctuation, suffixes, numbers etc
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
  dtm <- DocumentTermMatrix(corpus, 
                            control = list(weighting = weightTfIdf))
  # remove words that don't appear often enough for every category, else weird words and very large matrix
  dtm <- removeSparseTerms(dtm, sparsity)
}

prepareData <- function(df){
  # make clean cases and outcome based on text/category data frame
  corpus <- Corpus(VectorSource(df$description))
  names(corpus) <- df$category
  y <- df$category
  clean <- cleanCorpus(corpus)
  dtm = buildData(clean, 0.9)
  data <- as.data.frame(as.matrix(dtm))
  names(data) <- dtm$dimnames$Terms
  return (list("category" = y, "data" = data))
}

## Build train, cross-validation, and test sets, 50% of cases go to train set
sampler <- createDataPartition(data$category, times=3)
trainData <- data[sampler[[1]],]
crossValData <- data[sampler[[2]],]
testData <- data[sampler[[3]],]

# build train, cross-validation, and test sets
trainFull <- prepareData(trainData)
y_train <- trainFull$category
train <- trainFull$data

cvFull <- prepareData(crossValData)
y_cv <- cvFull$category
cv <- cvFull$data

testFull <- prepareData(testData)
y_test <- testFull$category
test <- testFull$data

matchWords <- function(testDtm, referenceDtm){
  # Can't predict categories never seen in reference set 
  # => remove them from the set used for prediction
  # and add to predicting set the words that were in train set but not in predicting set
  # Args: document term matrix to modify and reference document term matrix
  # Returns the modified dtm with columns matching the reference dtm
  test2 <- testDtm[, intersect(colnames(referenceDtm), colnames(testDtm))]
  trainWordsNotInTest <- setdiff(names(referenceDtm), names(test2))
  yy <- data.frame(matrix(0, ncol = length(trainWordsNotInTest),
                          nrow = dim(test2)[1]))
  names(yy) <- trainWordsNotInTest
  # Final processed test set
  return(cbind(test2, yy))
}

cvMatched <- matchWords(cv, train)
testMatched <- matchWords(test, train)

## Random Forest
rf <- randomForest(train, y_train)
pred <- predict(rf, cvMatched)
table(pred, y_cv)
# interpretation: graph showing which words make the most interesting splits in the trees
varImpPlot(rf, type=2) 
# on test set:
predTest <- predict(rf, testMatched)
table(predTest, y_test)

# (predTest)      Feet / Legs Hands Hat Neck / Torso Softies Sweater (y_test)
# Feet / Legs          102     0   1           12       3       8
# Hands                  0    21   0            0       0       0
# Hat                    2     2 132            5       2       1
# Neck / Torso          12    12  10          363      11      23
# Softies                0     0   0            0      13       0
# Sweater                1     2   1            8       0     145

# (pred)          Feet / Legs Hands Hat Neck / Torso Softies Sweater     (y_cv)
# Feet / Legs           97     3   3           12       2       8
# Hands                  0    20   0            0       0       0
# Hat                    2     3 130            6       1       1
# Neck / Torso          17    10   8          362       9      28
# Softies                0     0   0            1      17       0
# Sweater                1     1   3            7       0     140

# benchmark test: predict category whose keywords appearing the most in the text
# if no keywords in text, predict most frequent category
predEasy <- function(text, sortedCategories){
  # assumes categories sorted by most frequent in trainning set
  # categoryInText holds for each category the indexes where the category keywords appears in the text
  categoryInText <- sapply(sortedCategories, function(pattern) gregexpr(pattern, text)[[1]])
  # catNbrOccurences holds the number of times a category keyword appears in the text 
  catNbrOccurences <- sapply(categoryInText, function(l) sum(l > 0))
  # return category with most keywords occurences
  cat <- sortedCategories[which.max(catNbrOccurences)]
}

sort(table(y_train), decreasing = T)
sortedCategories <- c("[sS]hawl|[sS]carf", "[Ss]weater|[sS]leeve", "\\b[Hh]at\\b", 
                      "[Ff]eet|[Ff]oot|[sS]ock", "\\b[Hh]and\\b|[gG]love|[mMitt]]", "[Ss]ofties|[tT]oy")
y_easy <- sapply(crossValData$description, predEasy, sortedCategories)
# reorder y_cv names to have true positives in the diagonal (regexp letters mess up ordering)
table(y_easy, y_cv)[, c(1,5,6,4,2,3)]


# y_easy                     y_cv     Feet / Legs Softies Sweater Neck / Torso Hands Hat
# [Ff]eet|[Ff]oot|[sS]ock                  79       6       5           11     4   3
# [Ss]ofties|[tT]oy                         0       6       0            0     0   0
# [Ss]weater|[sS]leeve                      0       1     123            2     1   1
# [sS]hawl|[sS]carf                        38      13      45          361    10  36
# \\b[Hh]and\\b|[gG]love|[mMitt]]           0       2       4            6    16   3
# \\b[Hh]at\\b                              0       1       0            8     6 101

##### other things
## Naive bayes 
# # Tests with various categories show that perf goes down when adding
# # categories with not a lot of cases, perf good with 2 top frequent factors
# nb <- naiveBayes(train, y_train)
# pred <- predict(nb, cv)
# # Inspect model: probabilities of good predictors ("sweater", "warm") are high for the relevant categories
# table(pred, y_cv)
# # Interpretation: mean number of word occurences in each target class
# count <- nb$tables
# count2 <- as.data.frame(do.call(rbind, count), row.names = seq_along(count))
# names(count2) <- c("meanOcc","stdOcc")
# # match with the coresponding word and category
# count2$word = rep(names(count), each = length(row.names(count[[1]])))
# count2$category = rep(row.names(count[[1]]), dim(count2)[1]/length(row.names(count[[1]])) )
# head(arrange(count2, desc(meanOcc)))
# # todo bad perf because bad normalizization ? high occ words are generic

# ## Regularized logistic regression 
# regLinCV <- cv.glmnet(as.matrix(train), y_train, family="binomial", alpha=1)
# plot(regLinCV)
# # Get consistent set of words between test and train
# # Keep from test only the words that also are in train data
# test2 <- test[,intersect(colnames(train),colnames(test))]
# # Prediction: NB returns probabilities for each category, take max as prediction
# newCV <- predict(regLinCV, as.matrix(testMatched), s="lambda.min")
# # For multinomial (> 2 categories)
# newCVmax = apply(newCV[,,1], MARGIN=1, FUN=function(row) names(row)[which.is.max(row)])
# # For binomial (use only 2 categories)
# newCVmax = as.factor(ifelse(newCV < 0.5,"Sweater","Neck / Torso"))
# # Percentage of correct answers on our test set
# mean(newCVmax == as.character(y_test))
# table(newCVmax, as.character(y_test))

