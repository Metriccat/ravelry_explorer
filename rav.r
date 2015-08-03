# ravelry API poking, analyse knitting data

library(httr)
library(jsonlite)
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)

credentials <- readLines("user_rav.txt")
names(credentials) <- c("user","access_key","secret_key")

open_connection <- function(credentials){
  # open connection to Ravelry API and return token
  reqURL <- "https://www.ravelry.com/oauth/request_token"
  accessURL <- "https://www.ravelry.com/oauth/access_token"
  authURL <- "https://www.ravelry.com/oauth/authorize"

  ravelry.app <- oauth_app("ravelry", credentials["access_key"], secret=credentials["secret_key"])
  ravelry.urls <- oauth_endpoint(reqURL,authURL,accessURL)
  return(oauth1.0_token(ravelry.urls, ravelry.app))
}

test_connection <- function(ravelry.token,credentials){
  #quick test of API connection by getting connected user info
  test <- GET("https://api.ravelry.com/current_user.json", config=config("token"=ravelry.token)) 
  print(content(test)$user$username)
}

ravelry.token <- open_connection(credentials)
test_connection(ravelry.token,credentials)

########### patterns
# patterns using thick yarns (not necessarily all linked to thick yarn projects)
coldpatterns  <- GET("https://api.ravelry.com/patterns/search.json?page_size=5000&craft=knitting&pc=clothing&weight=worsted%7Caran%7Cbulky%7Csuper-bulky", config=config("token"=ravelry.token))
coldp <- content(coldpatterns)
#pattern data frame
# pattern_source format changes; first list item should have author and popularity info 
# popularity rank can be shared among patterns (?)
sapply(coldp$patterns, function(x) length(x$pattern_source))
# pat_author_pop <- function(x){
#   l <- length(x$pattern_source)
#   if (l==1) {return(c(x$pattern_sources[[1]]$author,x$pattern_sources[[1]]$popularity_rank))}
#   else if (l==2) {return(c(x$pattern_sources[[1]]$author,x$pattern_sources[[1]]$popularity_rank))}
#   else {return c(NA,NA)}
# }

patterns <- lapply(coldp$patterns, function(x) c(x$id,
                                                 x$permalink,
                                                 x$free,
                                                 ifelse(is.null(x$pattern_sources[[1]]$author),NA,x$pattern_sources[[1]]$author),
                                                 x$pattern_sources[[1]]$favorites_count))
nrows <- length(patterns)
patterns <- data.frame(matrix(unlist(patterns), nrow=nrows, byrow=T), stringsAsFactors=FALSE)
names(patterns) <-c("id","name","is.free","author","favorites_count")
head(patterns)
patterns$favorites_count = as.numeric(patterns$favorites_count)
# favorites_count limit at 99 when as.factors (?)
patterns <- patterns[order(patterns$favorites_count,decreasing = T),]

################ projects

# projects using thick yarns (cold weather) sorted by best match
colddata  <- GET("https://api.ravelry.com/projects/search.json?page_size=5000&craft=knitting&pc=clothing&weight=worsted%7Caran%7Cbulky%7Csuper-bulky", config=config("token"=ravelry.token))
cold <- content(colddata)

# all knitting projects sorted by most favorites
knit_all <- GET("https://api.ravelry.com/projects/search.json?page_size=5000&craft=knitting&sort=favorites", config=config("token"=ravelry.token))
knit <- content(knit_all)

#### start/end dates mining

# get start dates for projects
# TODO split with people in northern/southern hemisphere (list countries, merge with people data)
# around 10%-20% of users are in SOuth hemisphere
# TODO compare with total start dates
# and start dates for non winter (toys, tanks, not weight (thread held double))
# TODO end dates
coldstart <- as.Date(unlist(lapply(cold$projects, function(x) x$started))) #list projects start date
summary(coldstart)
head(coldstart)
coldstart <- as.POSIXct(coldstart)
coldmd <- data.frame("start_month"=strftime(coldstart, "%m"), "start_day"=strftime(coldstart, "%d"))
plot(table(coldmd$start_month))


#### tag mining

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
  tdm0 = TermDocumentMatrix(corpus_tags) 
  tdm = as.data.frame(as.matrix(tdm0) )
  names(tdm) = names(corpus_tags)
  tdm$tags = tdm0$dimnames$Terms
  return(tdm)
}

plot_tag_freq <- function(all_freq){
  # plot tags frequency with labels
  ggplot()+
    geom_bar(aes(x=seq_along(all_freq$tags), y = all_freq$fav0), stat="identity", fill='#99FFFF')+
    geom_bar(aes(x=seq_along(all_freq$tags), y = all_freq$fav500), stat="identity", fill='#11AAAA')+
    geom_text(aes(x=seq_along(all_freq$tags), 
                  y=all_freq$fav0, 
                  label=all_freq$tags),
                  hjust= 1, 
                  position=position_dodge(width=0.9) ) +
    scale_x_reverse() +
    ylab("tag frequency")+
    xlab("")+
    theme(axis.text.y=element_blank())+
    coord_flip()
}

alltags = get_tags(knit$projects, c(0,5,500))
#wordcloud(alltags, max.words=100,colors=brewer.pal(8, "Dark2"))
all_freq = tags_frequencies(alltags)
all_freq = arrange(all_freq, desc(fav0)) # sort by most frequent tags on all dataset
all_freq=all_freq[1:50,]
plot_tag_freq(all_freq)




# made for who ? (numbers)
# TODO compare with yardage, need to merge with stash info ... baby/not baby
# TODO made for raveler proportion
me <- unlist(lapply(cold$projects, function(x) grepl("[mM]e|[mM]yself",x$made_for)))
sum(me) #2725 on colddata
unknown <- unlist(lapply(cold$projects, function(x) x$made_for=="")) # 866
raveler <- unlist(lapply(cold$projects, function(x) !is.null(x$made_for_user_id))) #130



# what corelates with number of project favorites ?
# a priori: featured projects pics, shared in active forums, KAL, "famous" knitters, nbr pics
# prb not all of those are linked in the project ! merge on pattern id with pattern database ? works if projects from popular patterns
admirers <- lapply(cold$projects, function(x) c(x$favorites_count,x$comments_count, x$photos_count, length(x$tag_names)))
nrows <- length(admirers)
admirers <- data.frame(matrix(unlist(admirers), nrow=nrows, byrow=T))
names(admirers) <- c("favorites", "comments", "photos_count","tag_count")
hist(admirers$comments, breaks=70)
pairs(admirers)
corel <- cor(admirers) # corel  between fav and comments, between coments and nbr photos
ggplot()+  geom_point(aes(x=admirers$photos_count,y=log(admirers$favorites)), size=3, color="red", alpha=0.1)
low_project_count_photos <- names(which(table(admirers$photos_count)<30))
ggplot(aes(y = log(favorites), x = as.factor(photos_count)), data = admirers[!admirers$photos_count %in% low_project_count_photos,]) + geom_boxplot()
ggplot(aes(y = log(favorites), x = as.factor(photos_count)), data = admirers) + geom_boxplot()

# distribution of favorites
hist(admirers$favorites, breaks=50)
hist(admirers$favorites[admirers$favorites>500], breaks=50)


### colors in prjects



################ people



################ stashed yarns 

# split on handspun ? could be factor column inn data frame, but handspun is a special thing :)
yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=no&sort=favorited", config=config("token"=ravelry.token))
nothandspun <- content(yarns)
yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=yes&sort=favorited", config=config("token"=ravelry.token))
handspun <- content(yarns)
yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&sort=best&handspun=no&photo=yes", config=config("token"=ravelry.token))
allyarns <- content(yarns)

null2NA <- function(value){ifelse(is.null(value),NA,value)}

yarndata <- lapply(allyarns$stashes, function(x) sapply(list(x$color_family_name,
                                                            x$yarn$yarn_company$permalink,
                                                            x$yarn$rating_average,
                                                            x$yarn$yarn_weight$name,
                                                            x$favorites_count,
                                                            x$name),null2NA) )
summary(yarndata)
#wordcloud(yarndata$company,max.words=100,colors=brewer.pal(8,"Dark2"),rot.per=0)

nrows <- length(yarndata)
yarndata <- data.frame(matrix(unlist(yarndata), nrow=nrows, byrow=T))
names(yarndata) <- c("color", "company", "rating","weight","favorites","name")

# color selected among the 140 most supported color names
rav_colors = c("Black"="#000000","Blue"="#0000ff","Blue-green"="#008080","Blue-purple"="#6A5ACD","Brown"="#A52A2A",
                "Gray"="#808080","Green"="#008000","Natural/Undyed"="#e0d8c8","Orange"="#FFA500","Pink"="#FFC0CB",
                "Purple"="#800080","Red"="#ff0000","Red-orange"="#FF4500","Red-purple"="#C71585","White"="#ffffff",
                "Yellow"="#ffff00","Yellow-green"="#ADFF2F","Yellow-orange"="#FFD700")

ggplot(data=na.omit(yarndata), aes(x=reorder(color,color,length), fill = factor(reorder(color,color,length) ) )) +
  geom_bar(stat="bin")+
  guides(fill=guide_legend(title="Ravelry color category"))+
  theme(axis.text.x=element_blank())+
  theme(axis.ticks.x=element_blank())+
  scale_fill_manual(values=rav_colors)+
  xlab("")+
  ylab("Number of stash items")+
  ggtitle("Color distribution of stashed yarns")

ggplot(data=na.omit(yarndata), aes(x=reorder(weight,weight,length) )) +
  geom_bar(stat="bin",fill="MediumSlateBlue ")+
  guides(fill=guide_legend(title="Ravelry weight category"))+
  xlab("")+
  ylab("Number of stash items")+
  ggtitle("Weight distribution of stashed yarns")


