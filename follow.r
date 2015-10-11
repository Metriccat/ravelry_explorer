# Follow the number of favorites/projects/... as a function of time for patterns in input list
# Scheduled with cron job to get data every day 

library(httr)

# Get token from saved environement
load("/Users/saskia/R/ravelry_explorer/ID_3popular.RData")

# To get the patterns IDs into a saved environement (comment after saving the environement):
# Get IDs from permalinks; one search result per permalink search (unique identification)
# It is actually possible to get the patterns directly from permalinks without going through IDs
#permalinks_to_follow <- c("barley-2","hitofude-cardigan","honey-cowl")
#queries <- lapply(permalinks_to_follow, 
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

# Reshape to data frame and add date/time
pattern_df <- data.frame(matrix(unlist(pattern_data), nrow=length(ID_to_follow), byrow=T))
pattern_df$time <- Sys.Date()
# FYI because names won't be in saved text file to avoid repeated header
#names(pattern_df) <- c("permalink","queued_projects_count","projects_count","favorites_count","comments_count","time")

write.table(pattern_df, file = "/Users/saskia/R/ravelry_explorer/pattern_follow2.txt", 
            append=T, row.names=F, col.names=F, quote=F)
