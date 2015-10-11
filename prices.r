# Explore Ravelry pattern prices by category

library(httr)
library(XML)
library(reshape2)
library(quantmod)
library(ggplot2)

categories <- c("hat","sweater","neck-torso","feet-legs","hands","home","toysandhobbies","pattern-component","pet")

# Get dataset of non-free patterns with a high number of projects, available as pdf downloads
# (Full treatment 1h30 for 500/category)
search_url <- "https://api.ravelry.com/patterns/search.json?page_size=500&sort=projects&craft=knitting&availability=ravelry%2B-free&pc="
cat_search <- sapply(categories, function(name) paste(search_url, name,sep="", collapse=""))

# Get lists of search results; price attribute is NULL => use web scraping to get it
pat0 <- lapply(cat_search, GET, config=config("token"=ravelry.token))
pat <- lapply(pat0, content)

# Extract patterns permalinks in each category
permalinks <- sapply(pat, function(x) sapply(x$patterns, function(y) y$permalink))
names(permalinks) <- categories
permalinks <- melt(permalinks)
names(permalinks) <- c("link","category")

permalinks_full <- sapply(permalinks$link, function(name) paste("http://www.ravelry.com/patterns/library/",name,sep="",collapse=""))

# Random sampling for testing
samp = sample(1:length(permalinks$link),length(permalinks$link))
permalinks_full <- permalinks_full[samp]       
permalinks <- permalinks[samp,]       

# Web scraping to get the price from the pattern page
# Takes about 1 min for 50 links
n=dim(permalinks)[1] # 1000 ok
pattern_info <- lapply(permalinks_full[1:n], htmlTreeParse, useInternalNodes = TRUE)
names(pattern_info) <- permalinks$link[1:n]

pattern_prices <- lapply(pattern_info, function(html) getNodeSet(html, 
                                                                 path="//strong[@class='price']/a/text()", 
                                                                 fun=xmlValue)[[1]] )

num_prices <- lapply(pattern_prices, function(str) c("price"=regmatches(str,
                                                                regexpr("[[:digit:]]+\\.*[[:digit:]]*",str)),
                                                     "currency"=substr(str, nchar(str)-2, nchar(str)) 
                                                     )
                     )


pattern_nbr_projects <- melt(sapply(pattern_info, nbr_projects))
price_data  <- data.frame(matrix(unlist(num_prices), nrow=length(num_prices), byrow=T), stringsAsFactors=F)
price_data <- cbind(pattern_nbr_projects, permalinks[1:n,], price_data)
names(price_data) <- c("nbr_projects", "link","category", "price", "currency")
price_data$price <- as.numeric(price_data$price)

# Local currency conversion is proposed by Ravelry only for logged in users
# => do normalizeing of prices here
currencies_codes = sapply(price_data$currency, paste,"USD",sep="")
# getFX puts exchange rate in the environment, but sapply does not change env. variables
for (curr in unique(price_data$currency)) getFX(paste(curr, "/USD", sep=""), from = Sys.Date())
exchange_rates = sapply(currencies_codes, get)
price_data$price_usd = price_data$price * exchange_rates

ggplot(price_data,aes(x = category, y=price_usd, fill=category)) + 
  geom_boxplot(alpha=0.5) +
  xlab("Category") +
  ylab("Price in USD") +
  ggtitle("Pattern prices distributions in each category")

# Outliers cut away
ggplot(price_data) + 
  geom_histogram(aes(x = price_usd), fill='Blue', alpha=0.5, binwidth=0.5) +
  scale_x_continuous(limits = c(0, 20), breaks = round(seq(0, 20, by = 1), 1)) +
  xlab("Pattern price in USD") +
  ylab("Number of patterns") +
  ggtitle("Distribution of knitting patterns prices")

plot(sort(price_data$nbr_projects))

# not sure looking at correlation between price and nbr projects is useful
# nbr projects should only weakly depend on price point, many outliers will skew distribution
# => exclude outliers ? 
# or make other post with decreasing nbr of projects plot to check what the median nbr of projects is
