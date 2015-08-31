library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)

#### tag mining on projects

get_tags <- function(projects, fav_threshold){
  # make corpus from project tags, use only projects where nbr of favorites >= fav_threshold
  # if fav_threshold is a vector, each document in corpus coresponds nbr of favorites >= threshold
  tags <- 1:length(fav_threshold)
  for (i in seq_along(fav_threshold)){
    tags_threshold <- lapply(projects, function(x) ifelse(x$favorites_count>=fav_threshold[i],x$tag_names,""))
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

tags_frequencies <- function(corpus_tags){
  # return tags and their frequencies in each document of corpus
  tdm = TermDocumentMatrix(corpus_tags) 
  tdmframe = as.data.frame(as.matrix(tdm) )
  names(tdmframe) = names(corpus_tags)
  tdmframe$tags = tdm$dimnames$Terms
  return(tdmframe)
}

plot_tag_freq <- function(all_freq){
  # plot tags frequency with labels
  ggplot()+
    geom_bar(aes(x=seq_along(all_freq$tags), y = all_freq$fav0), stat="identity", fill='Gold')+
    #geom_bar(aes(x=seq_along(all_freq$tags), y = all_freq$fav500), stat="identity", fill='#11AAAA')+
    geom_text(aes(x=seq_along(all_freq$tags), 
                  y=all_freq$fav0, 
                  label=all_freq$tags),
              hjust=1.01, 
              position=position_dodge(width=0.9) ) +
    scale_x_reverse()+
    ylab("tag frequency")+
    xlab("tag")+
    ggtitle("Frequency of most used tags on popular projects.")+
    theme(axis.text.y=element_blank())+
    theme(axis.ticks.y=element_blank())+
    coord_flip()
}


# all knitting projects sorted by most favorites
knit_all <- GET("https://api.ravelry.com/projects/search.json?page_size=5000&craft=knitting&sort=favorites", config=config("token"=ravelry.token))
knit <- content(knit_all)

alltags = get_tags(knit$projects, 0)
#wordcloud(alltags, max.words=100,colors=brewer.pal(8, "Dark2"),rot.per=0)
all_freq = tags_frequencies(alltags)
all_freq = arrange(all_freq, desc(fav0)) # sort by most frequent tags on all dataset
all_freq=all_freq[1:30,]
correction <- c(babi="baby", cabl="cable",pullov="pullover",bulki="bulky",contigu="contiguous",finger="fingering")
realword <- function(x) ifelse(x %in% names(correction), correction[x],x)
all_freq$tags <- sapply(all_freq$tags, realword)
wordcloud(words=all_freq$tags, freq=all_freq$fav0,colors=brewer.pal(8, "Dark2"),rot.per=0)

# get real tag names
#library(wordnet)
#setDict("C:\\Program Files (x86)\\WordNet\\2.1\\dict") # set Dict variable for wordNet
#filter <- sapply(ta, function(x) getTermFilter("StartsWithFilter", x, TRUE))
#terms <- lapply(filter, function(x) getIndexTerms("NOUN", 1, x))
#realtags <- sapply(terms, function(x) sapply(x, getLemma))
# TODO quite complex, fast hack in the meantime
plot_tag_freq(all_freq)