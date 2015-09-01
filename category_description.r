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
test <- sample(dim(data)[1], 100)
test_data <- data[test,]
train_data <- data[-test,]

corpus_train <- Corpus(VectorSource(train_data$description))
names(corpus_train) <- train_data$category
y_train <- train_data$category

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

clean_train <- cleanCorpus(corpus_train)

buildData = function(corpus, sparsity=0.999){
  dtm <- DocumentTermMatrix(corpus)
  # remove words that don't appear often enough for every category, else weird words and very large matrix
  # default: remove (almost) nothing
  dtm <- removeSparseTerms(dtm, sparsity)
}

dtm_train = buildData(clean_train,0.95)
train_matrix <- as.matrix(dtm_train)

# naive bayes bad because categories dominated by Neck/Torso => predicts Neck/torso always
model <- naiveBayes(train_matrix, y_train)
pred <- predict(model, test_data$description)

# multinomial logistic regression neural network: error msg
train_tot <- cbind(as.data.frame(train_matrix),"category"=y_train)
mylogit <- glm(category~., data=train_tot)
