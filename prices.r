# API + web scraping for most made pattern prices by category

library(httr)
library(XML)
library(reshape2)

categories = c("hat","sweater","neck-torso","feet-legs","hands","home","toysandhobbies","pattern-component","pet")

search_url = "https://api.ravelry.com/patterns/search.json?page_size=5000&sort=projects&craft=knitting&availability=ravelry%2B-free&pc="

cat_search <- sapply(categories, function(name) paste(search_url,name,sep="",collapse=""))

# get lists of search results; price attribute is NULL ! 
pat0 <- lapply(cat_search, GET, config=config("token"=ravelry.token))
pat <- lapply(pat0, content)

# extract patterns permalinks in each category
permalinks <- sapply(pat, function(x) sapply(x$patterns, function(y) y$permalink))
names(permalinks) <- categories
permalinks <- melt(permalinks)
names(permalinks) <- c("link","category")

permalinks_full <- sapply(permalinks$link, function(name) paste("http://www.ravelry.com/patterns/library/",name,sep="",collapse=""))
               
# web scraping to get the price from the pattern page
# about 1 min for 50 links
pattern_info <- lapply(permalinks_full, htmlTreeParse, useInternalNodes = TRUE)
names(pattern_info) <- permalinks$link
# euros conversion only for logged users; price available only for rav downloads
pattern_prices <- lapply(pattern_info, function(html) getNodeSet(html, 
                                                                 path="//strong[@class='price']/a/text()", 
                                                                 fun=xmlValue)[[1]] )



