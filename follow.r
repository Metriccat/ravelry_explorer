# follow number of favorites/projects as a function of time for patterns in list
# scheduled with cron job to get data every day at 9h, Paris time (workday ended in all the USA: 0h in LA, 3h NY)

library(httr)

#get token from saved environement
load("/home/saskia/R/ravelry_explorer/token_ID_to_follow.RData")

# launch then put in a R environment: gets pattern IDs from names
# get IDs from permalinks; one search result per permalink search (unique identification)
# permalinks_to_follow <- c("barley-2","hitofude-cardigan","honey-cowl")
# queries <- lapply(permalinks_to_follow, 
#                   function(x) GET(paste("https://api.ravelry.com/patterns/search.json?page_size=10&query=",x,sep=""),
#                                   config=config("token"=ravelry.token)))
# content_to_follow <- lapply(queries, content)
# ID_to_follow <- sapply(content_to_follow, function(x) x$patterns[[1]]$id)

# get patterns info through ID
links_to_follow <- sapply(ID_to_follow, function(name) paste("https://api.ravelry.com/patterns/",name,".json",sep="",collapse=""))
pat0 <- lapply(links_to_follow, GET, config=config("token"=ravelry.token))
pats <- lapply(pat0, content)

pattern_data <- sapply(pats, function(x) x$pattern[c("permalink",
                                                     "queued_projects_count",
                                                     "projects_count",
                                                     "favorites_count",
                                                     "comments_count")])

pattern_df <- data.frame(matrix(unlist(pattern_data), nrow=length(ID_to_follow), byrow=T))
pattern_df$time <- Sys.Date()
#names(pattern_df) <- c("permalink","queued_projects_count","projects_count","favorites_count","comments_count","time")

write.table(pattern_df, file = "/home/saskia/R/ravelry_explorer/pattern_follow.txt",append=T,row.names=F,col.names=F,quote=F)
