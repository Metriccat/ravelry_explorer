# follow brooklyn tweed patterns by following the top 30 search results in
# most recently published for keywords "brooklyn tweed"


library(httr)

#get token from saved environement
load("/home/saskia/R/ravelry_explorer/token_ID_to_follow.RData")

# seacrh for recent BT patterns (will also have others made with BT yarns ...)
BT_query <- GET("https://api.ravelry.com/patterns/search.json?page_size=30&query=Brooklyn%20tweed&sort=date",
             config=config("token"=ravelry.token))
content_to_follow <- content(BT_query)

permalinks_to_follow <- sapply(content_to_follow[[1]], function(x) x$permalink)

# get patterns info through ID
links_to_follow <- sapply(permalinks_to_follow, function(name) paste("https://api.ravelry.com/patterns/",name,".json",sep="",collapse=""))
pat0 <- lapply(links_to_follow, GET, config=config("token"=ravelry.token))
pats <- lapply(pat0, content)

pattern_data <- sapply(pats, function(x) x$pattern[c("permalink",
                                                     "queued_projects_count",
                                                     "projects_count",
                                                     "favorites_count",
                                                     "comments_count")])

pattern_df <- data.frame(matrix(unlist(pattern_data), nrow=length(links_to_follow), byrow=T))
pattern_df$time <- Sys.Date()

write.table(pattern_df, file = "/home/saskia/R/ravelry_explorer/BT_follow.txt",append=T,row.names=F,col.names=F,quote=F)
