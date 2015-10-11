# Explore tags on Ravelry knitting projects

library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)

GetTags <- function(projects, fav_threshold){
  # Args: list of project objects returned by Ravelry API
  #       threshold for the number of favorites (numeric or vector)
  # Returns document corpus
  # if threshold is numeric, 1 document in corpus with all tags in project list having
  # nbr of favorites >= fav_threshold
  # if threshold is a vector, each document in corpus coresponds to tags in projects having
  # nbr of favorites >= threshold
  
  tags <- 1:length(fav_threshold)
  for (i in seq_along(fav_threshold)) {
    tags_threshold <- lapply(projects, function(x) ifelse(x$favorites_count >= fav_threshold[i],
                                                          x$tag_names,
                                                          ""))
    tags[i] <- paste(unlist(tags_threshold), collapse=" ")
  }
  corpus_tags <- Corpus(VectorSource(tags))
  names(corpus_tags) = sapply(fav_threshold, function(x) paste("fav",as.character(x),sep=""))
  # all tags are lowercase
  corpus_tags <- tm_map(corpus_tags, removeNumbers) # ignore dates
  corpus_tags <- tm_map(corpus_tags, removePunctuation) 
  corpus_tags <- tm_map(corpus_tags, stemDocument) 
  return(corpus_tags)
}

TagsFrequencies <- function(corpus_tags) {
  # Arg: document corpus built using the get_tags function
  # Returns data frame with tags and their frequencies in each document of corpus
  tdm = TermDocumentMatrix(corpus_tags) 
  tdmframe = as.data.frame(as.matrix(tdm) )
  names(tdmframe) = names(corpus_tags)
  tdmframe$tags = tdm$dimnames$Terms
  return(tdmframe)
}

PlotTagFreq <- function(allFreq){
  # Plot tags frequency with labels
  ggplot() +
    geom_bar(aes(x=seq_along(allFreq$tags), y = allFreq$fav0), stat="identity", fill='Gold') +
    #geom_bar(aes(x=seq_along(allFreq$tags), y = allFreq$fav500), stat="identity", fill='#11AAAA') +
    geom_text(aes(x=seq_along(allFreq$tags), 
                  y=allFreq$fav0, 
                  label=allFreq$tags),
              hjust=1.01, 
              position=position_dodge(width=0.9) ) +
    scale_x_reverse() +
    ylab("tag frequency") +
    xlab("tag") +
    ggtitle("Frequency of most used tags on popular projects.") +
    theme(axis.text.y=element_blank()) +
    theme(axis.ticks.y=element_blank()) +
    coord_flip()
}


# Get from API a list of knitting projects sorted by most favorites
knitAll <- GET("https://api.ravelry.com/projects/search.json?page_size=5000&craft=knitting&sort=favorites", config=config("token"=ravelry.token))
knit <- content(knitAll)

# use the above functions to explore the tags
alltags = GetTags(knit$projects, 0)
#wordcloud(alltags, max.words=100,colors=brewer.pal(8, "Dark2"),rot.per=0)
allFreq = TagsFrequencies(alltags)
# sort by most frequent tags on all dataset
allFreq = arrange(allFreq, desc(fav0)) 
# keep only the most frequent
allFreq=allFreq[1:30,]
# corpus stems words to calculate frequencies;
# get full word for more attractive labelling
correction <- c(babi="baby", cabl="cable",pullov="pullover",
                bulki="bulky", contigu="contiguous", finger="fingering")
RealWord <- function(x) ifelse(x %in% names(correction), correction[x], x)
allFreq$tags <- sapply(allFreq$tags, RealWord)
wordcloud(words=allFreq$tags, freq=allFreq$fav0, colors=brewer.pal(8, "Dark2"), rot.per=0)

# get real tag names
#library(wordnet)
#setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict") # set Dict variable for wordNet
#filter <- sapply(ta, function(x) getTermFilter("StartsWithFilter", x, TRUE))
#terms <- lapply(filter, function(x) getIndexTerms("NOUN", 1, x))
#realtags <- sapply(terms, function(x) sapply(x, getLemma))
# TODO quite complex, use fast hack above in the meantime

PlotTagFreq(allFreq)
