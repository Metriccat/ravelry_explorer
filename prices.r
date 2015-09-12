# API + web scraping for most made pattern prices by category

library(httr)
library(XML)
library(reshape2)
library(quantmod)
library(ggplot2)

categories <- c("hat","sweater","neck-torso","feet-legs","hands","home","toysandhobbies","pattern-component","pet")

# full treatment 1h30 for 500/category
search_url <- "https://api.ravelry.com/patterns/search.json?page_size=500&sort=projects&craft=knitting&availability=ravelry%2B-free&pc="

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

samp = sample(1:length(permalinks$link),length(permalinks$link))
permalinks_full <- permalinks_full[samp]    #random sampling    
permalinks <- permalinks[samp,]       

# web scraping to get the price from the pattern page
# about 1 min for 50 links
n=dim(permalinks)[1] # 1000 ok
pattern_info <- lapply(permalinks_full[1:n], htmlTreeParse, useInternalNodes = TRUE)
names(pattern_info) <- permalinks$link[1:n]
# euros conversion only for logged users; price available only for rav downloads
pattern_prices <- lapply(pattern_info, function(html) getNodeSet(html, 
                                                                 path="//strong[@class='price']/a/text()", 
                                                                 fun=xmlValue)[[1]] )

num_prices <- lapply(pattern_prices, function(str) c("price"=regmatches(str,
                                                                regexpr("[[:digit:]]+\\.*[[:digit:]]*",str)),
                                                     "currency"=substr(str,nchar(str)-2,nchar(str)) 
                                                     )
                     )


pattern_nbr_projects <- melt(sapply(pattern_info, nbr_projects))

price_data  <- data.frame(matrix(unlist(num_prices), nrow=length(num_prices), byrow=T), stringsAsFactors=F)
price_data <- cbind(pattern_nbr_projects, permalinks[1:n,], price_data)
names(price_data) <- c("nbr_projects", "link","category", "price", "currency")
price_data$price <- as.numeric(price_data$price)

currencies_codes = sapply(price_data$currency, paste,"USD",sep="")
# puts exchange rate in the environment, sapply does not change env variables
for (curr in unique(price_data$currency)) getFX(paste(curr,"/USD",sep=""),from = Sys.Date())
exchange_rates = sapply(currencies_codes, get)
price_data$price_usd = price_data$price * exchange_rates

ggplot(price_data,aes(x = category, y=price_usd,fill=category)) + 
  geom_boxplot(alpha=0.5) +
  xlab("Category") +
  ylab("Price in USD") +
  ggtitle("Pattern prices distributions in each category")


ggplot(price_data,aes(x = category, y=price_usd,fill=category)) + 
  geom_violin(alpha=0.5) +
  xlab("Category") +
  ylab("Price in USD") +
  ggtitle("Pattern prices distributions in each category")

# histogram, outliers cut away
ggplot(price_data) + 
  geom_histogram(aes(x = price_usd), fill='Blue', alpha=0.5, binwidth=0.5) +
  scale_x_continuous(limits = c(0, 20), breaks = round(seq(0, 20, by = 1),1)) +
  xlab("Pattern price in USD") +
  ylab("Number of patterns") +
  ggtitle("Distribution of knitting patterns prices")


plot(sort(price_data$nbr_projects))

# not sure looking at correlation between price and nbr projects is useful
# nbr projects should only weakly depend on price point, many outliers will skew distribution
# => exclude outliers ? 
# or make other post with decreasing nbr of projects plot to check what the median nbr of projects is
