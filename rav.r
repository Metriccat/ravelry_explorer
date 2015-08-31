# ravelry API poking, analyse knitting data

library(httr)
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(treemap)
library(reshape2)

null2NA <- function(value) ifelse(is.null(value), NA, value)

# todo: get raw html from pattern link and extract category from these (is public, use html tags)

########### patterns

# patterns using thick yarns (not necessarily all linked to thick yarn projects)
coldpatterns  <- GET("https://api.ravelry.com/patterns/search.json?page_size=5000&craft=knitting&pc=clothing&weight=worsted%7Caran%7Cbulky%7Csuper-bulky", config=config("token"=ravelry.token))
coldp <- content(coldpatterns)
# all clothing patterns
pclothing  <- GET("https://api.ravelry.com/patterns/search.json?page_size=5000&craft=knitting&pc=clothing", config=config("token"=ravelry.token))
pclothing <- content(pclothing)
#pattern data frame
# pattern_source format changes; first list item should have author and popularity info 
# popularity rank can be shared among patterns (?)
sapply(pclothing$patterns, function(x) length(x$pattern_source))
# pat_author_pop <- function(x){
#   l <- length(x$pattern_source)
#   if (l==1) {return(c(x$pattern_sources[[1]]$author,x$pattern_sources[[1]]$popularity_rank))}
#   else if (l==2) {return(c(x$pattern_sources[[1]]$author,x$pattern_sources[[1]]$popularity_rank))}
#   else {return c(NA,NA)}
# }

patterns <- lapply(pclothing$patterns, function(x) c(x$id,
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
admirers <- lapply(knit$projects, function(x) c(x$favorites_count,
                                                x$comments_count, 
                                                x$photos_count, 
                                                ifelse(length(grepl("[mM]e|[mM]yself",x$made_for))==0,
                                                       0,
                                                       grepl("[mM]e|[mM]yself",x$made_for)),
                                                length(x$tag_names)) )
nrows <- length(admirers)
admirers <- data.frame(matrix(unlist(admirers), nrow=nrows,byrow=T))
names(admirers) <- c("favorites", "comments", "photos_count","for_me","tag_count")
hist(admirers$comments, breaks=70)
pairs(admirers)
corel <- cor(admirers) # corel  between fav and comments, between coments and nbr photos
ggplot()+  geom_point(aes(x=admirers$photos_count,y=log(admirers$favorites)), size=3, color="red", alpha=0.1)
low_project_count_photos <- names(which(table(admirers$photos_count)<30))
ggplot(aes(y = log(favorites), x = as.factor(photos_count)), data = admirers[!admirers$photos_count %in% low_project_count_photos,]) + geom_violin()
ggplot(aes(y = log(favorites), x = as.factor(photos_count)), data = admirers) + geom_violin()

# distribution of favorites
hist(admirers$favorites, breaks=50)
hist(admirers$favorites[admirers$favorites>500], breaks=50)


