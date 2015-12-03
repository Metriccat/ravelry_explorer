# Follow Brooklyn Tweed (BT) Fall 2015 collection patterns by following the top 30 search results in
# the most recently published for keywords "brooklyn tweed"
# (Scheduled with Cron)

library(httr)

# Get token from saved environement
load("/Users/saskia/R/ravelry_explorer/token_rav.RData")

# Search for recent BT patterns (will also have others made with BT yarns)
BT_query <- GET("https://api.ravelry.com/patterns/search.json?page_size=30&query=Olga%20Buraya%20Kefelian&sort=date",
             config=config("token"=ravelry.token))
content_to_follow <- content(BT_query)
permalinksOlga <- sapply(content_to_follow[[1]], function(x) x$permalink)

# Once the collection is out, pattern names are known, no need to use search (code above)
permalinksFall2015 <- c("ashland-2","bannock","birch-bay-2","cascades","deschutes-2","copse-2",
              "fletching-3","lander","lolo","mcloughlin","nehalem-2","riverbend-2",
              "sauvie","trailhead-2","willamette-7")

# Get patterns info 
patternInfoToDF <- function(links, filename) {
  
  links_to_follow <- sapply(links, function(name) paste("https://api.ravelry.com/patterns/",
                                                        name,".json",sep="",collapse=""))
  pat0 <- lapply(links_to_follow, GET, config=config("token"=ravelry.token))
  pats <- lapply(pat0, content)
  
  pattern_data <- sapply(pats, function(x) x$pattern[c("permalink",
                                                     "queued_projects_count",
                                                     "projects_count",
                                                     "favorites_count",
                                                     "comments_count")])

  pattern_df <- data.frame(matrix(unlist(pattern_data), nrow=length(links_to_follow), byrow=T))
  pattern_df$time <- Sys.time()
  write.table(pattern_df, file = filename,
            append=T, row.names=F, col.names=F, quote=F)
}

patternInfoToDF(permalinksFall2015, "/Users/saskia/R/ravelry_explorer/BT_follow2.txt")
patternInfoToDF(permalinksOlga, "/Users/saskia/R/ravelry_explorer/olga_follow.txt")


