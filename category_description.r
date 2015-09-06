# pattern category from rav page (not API)

library(XML)
library(RCurl)
library(tm)
library(e1071)
library(nnet)

pat0 <- GET("https://api.ravelry.com/patterns/search.json?page_size=2000&craft=knitting", config=config("token"=ravelry.token))
pat <- content(pat0)

permalinks <- sapply(pat$patterns, function(x) x$permalink)
permalinks_full <- sapply(permalinks, function(name) paste("http://www.ravelry.com/patterns/library/",name,sep="",collapse=""))
names(permalinks_full) <- permalinks

# get toplevel category and description text from web (not API)
pattern_info <- lapply(permalinks_full, htmlTreeParse, useInternalNodes = TRUE)

pattern_description_par <- lapply(pattern_info, getNodeSet, path="//p", fun=xmlValue)
pattern_description <- sapply(pattern_description_par, paste, collapse=" ")

pattern_cat <- lapply(pattern_info, getNodeSet, path="//div[@class='category']/a/span/text()", fun=xmlValue)
pattern_topcat <- simplify2array(sapply(pattern_cat, head, 1))

data <- as.data.frame(cbind(permalinks, pattern_topcat, pattern_description),stringsAsFactors=F,row.names=F)
names(data) <- c("permalink", "category", "description")
data$category <- as.factor(data$category)

cat_freq <- table(data$category)

# remove from data the categories with too few examples
data <- subset(data, subset=(cat_freq[category] > 5))
data$category <- factor(data$category)

# text mining to find category

cleanCorpus = function(corpus){
  # lowercase
  corpus <- tm_map(corpus, tolower)
  # Remove stopwords first, else for ex. l'or becomes lor and l' is not removed
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  # remove punctuation
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, function(str) gsub("[^[:alnum:] ]", " ",str))
  # remove  html tags with regexp
  corpus <- tm_map(corpus, function(x) gsub("<[a-z]*>", " ", x))
  # remove numbers - but they may be useful as dimensions or phone hours ...
  corpus <- tm_map(corpus, removeNumbers)
  # simplify whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # stem words (tm_map stem has type error)
  corpus <- tm_map(corpus, stemDocument, "english")
}

buildData = function(corpus, sparsity=0.999){
  dtm <- DocumentTermMatrix(corpus)
  # remove words that don't appear often enough for every category, else weird words and very large matrix
  # default: remove (almost) nothing
  dtm <- removeSparseTerms(dtm, sparsity)
}

test_ind <- sample(dim(data)[1], 200)
test_data <- data[test_ind,]
train_data <- data[-test_ind,]

corpus_train <- Corpus(VectorSource(train_data$description))
names(corpus_train) <- train_data$category
y_train <- train_data$category

corpus_test <- Corpus(VectorSource(test_data$description))
names(corpus_test) <- test_data$category
y_test <- test_data$category

clean_train <- cleanCorpus(corpus_train)
clean_test <- cleanCorpus(corpus_test)

dtm_train = buildData(clean_train,0.8)
train <- as.data.frame(as.matrix(dtm_train))
names(train) <- dtm_train$dimnames$Terms
  
dtm_test = buildData(clean_test,0.8)
test <- as.data.frame(as.matrix(dtm_test))
names(test) <- dtm_test$dimnames$Terms

# naive bayes 
model <- naiveBayes(train, y_train)
filter <- names(test) %in% names(train)
test2 <- test[,filter]
pred <- predict(model, test)
(cbind(y_test,pred))
# inspect model: probabilities of good predictors ("sweater", "warm") are high for the relevant categories
# random is 1/16=6% success on y_test; most frequent value cte is 37% success with 100 in test set, 44% with 200
#sparsity=0.95: 0% success, 0.8 19%, 0.7 23%, 0.6 37%, 0.5 35%,  0.4 34%, 0.2 37% 
sum(pred==y_test)/length(y_test) 

# multinomial logistic regression neural network: error msg
train_tot <- cbind(as.data.frame(train_matrix),"category"=y_train)
mylogit <- glm(category~., data=train_tot)

# svm
mysvm <- svm(dtm_train, y_train)
dtm_test_filtered <- only terms appearing in test set
pred <- predict(mysvm, dtm_test_filtered)
