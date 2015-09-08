# API + web scraping for most made pattern prices by category

library(httr)
library(XML)
library(reshape2)
library(quantmod)

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

num_prices <- lapply(pattern_prices, function(str) c("price"=regmatches(str,
                                                                regexpr("[[:digit:]]+\\.*[[:digit:]]*",str)),
                                                     "currency"=substr(str,nchar(str)-2,nchar(str)) 
                                                     )
                     )

price_data  <- data.frame(matrix(unlist(num_prices), nrow=length(num_prices), byrow=T), stringsAsFactors=F)
price_data <- cbind(permalinks[1:45,], price_data )
names(price_data) <- c("link", "category", "price", "currency")
price_data$price <- as.numeric(price_data$price)

currencies_codes = sapply(price_data$currency, paste,"EUR",sep="")
# puts exchange rate in the environment, sapply does not change env variables
for (curr in unique(price_data$currency)) getFX(paste(curr,"/EUR",sep=""),from = Sys.Date())
exchange_rates = sapply(currencies_codes, get)
price_data$price_eur = price_data$price * exchange_rates


