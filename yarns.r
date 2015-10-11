# Explore which yarn colors are most stashed and used in knitting projects

library(httr)
library(tm)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(reshape2)

# Trying different yarn stash searches to get more chances to have the "color" field filled
# on the maximum number of stash items entries
# Ignore handspun yarn because its properties are quite different from commercial yarns
# (ie non commercial, multicolor, time consuming)
# In the end this data is discarded in favor of direct reading from the yarn homepage on Ravelry
# because sampling 5000 items is not as accurate as the Ravelry report, which reflect the whole database
# naming convention:
# S: search sorting ("best match" or "most faved"), P: photo = T or F
yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=no&sort=favorited&photo=yes",
             config=config("token"=ravelry.token))
# SF_PT <- content(yarns)
# yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=no&sort=best&photo=yes",
#              config=config("token"=ravelry.token))
# SB_PT <- content(yarns)
# yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=no&sort=favorited&photo=no",
#              config=config("token"=ravelry.token))
# SF_PF <- content(yarns)
# yarns <- GET("https://api.ravelry.com/stash/search.json?page_size=5000&handspun=no&sort=best&photo=no",
#              config=config("token"=ravelry.token))
# SB_PF <- content(yarns)
yarns <- content(yarns)
#rm(yarns)

# Replace NULL by NA (Ravelry API returns json object with NULLs)
null2NA <- function(value) ifelse(is.null(value), NA, value)

# # Check how many stash items have the color family field filled (there are 5000 items for each search query)
# hasColor <- sapply(SB_PF$stashes, function(x) sapply(list(x$color_family_name), null2NA) )
# sum(is.na(hasColor))
# # Resultst for each search: SF_PT: 2326, SB_PT: 2163, SF_PF:1936, SB_PF: 2248
# # Numbers of favorites in SF_PF is low (a few units for most items):
# favedNoPhoto <- sort(sapply(SF_PF$stashes, function(x) x$favorites_count), decreasing=T)
# plot(favedNoPhoto)

# Get some properties of stashed yarns: list of vectors
yarnData <- lapply(yarns$stashes, function(x) sapply(list(x$color_family_name,
                                                          x$yarn$yarn_company$permalink,
                                                          x$yarn$rating_average,
                                                          x$yarn$yarn_weight$name,
                                                          x$favorites_count,
                                                          x$name), null2NA) )
nrows <- length(yarnData)
# reshape into data frame
yarnData <- data.frame(matrix(unlist(yarnData), nrow=nrows, byrow=T))
names(yarnData) <- c("color", "company", "rating","weight","favorites","name")
summary(yarnData)

# Most stashed yarn companies
wordcloud(yarnData$company, max.words=100, colors=brewer.pal(8,"Dark2"), rot.per=0)

## Use direct reading (entered by hand) of the Ravlery yarn homepage

# Colors selected among the 140 html supported color names
ravColors <- c("Black"="#000000","Blue"="#0000ff","Blue-green"="#008080","Blue-purple"="#6A5ACD","Brown"="#A52A2A",
                "Gray"="#808080","Green"="#008000","Natural/Undyed"="#e0d8c8","Orange"="#FFA500","Pink"="#FFC0CB",
                "Purple"="#800080","Red"="#ff0000","Red-orange"="#FF4500","Red-purple"="#C71585","White"="#ffffff",
                "Yellow"="#ffff00","Yellow-green"="#ADFF2F","Yellow-orange"="#FFD700")

# Color breakdown (august) from project and stash search page info: 
yarnColors <- data.frame("color"=c("Blue","Green","Purple","Brown","Gray","Blue-green",
                                    "Pink","Red","Natural/Undyed", "Black","Red-purple",
                                    "White","Orange","Yellow","Blue-purple","Red-orange",
                                    "Yellow-green","Yellow-orange"),
                          "project"=c(702934,572417,420128,384078,495350,368599,
                                      412316,362362,268711,237424,148006,
                                      254850,170679,168060,143248,92505,
                                      65680,53732),
                          "stash"=c(923548,753213,600723,534547,506782,493983,
                                    561139,433341,304647,261388,204782,
                                    241738,228724,194401,198696,127643,
                                    82374,65809))

yarnWeights = data.frame("weight"=c("bulky","aran"),"count"=c(10,15))

# Scale down to compare to stash data, and sort
#project_colors$value <- project_colors$value*max(table(yarndata$color))/max(project_colors$value)*4/3
#stash_colors$value <- stash_colors$value*max(table(yarndata$color))/max(stash_colors$value)*4/3

# Sort yarn colors by increasing number of items in each color
yarnColors <- arrange(yarnColors, stash)

# Plot best data
ggplot(data=yarnColors)+
  geom_bar(aes(x=seq_along(color), y=stash, fill=factor(color)),
           stat="identity")+
  geom_point(aes(x=seq_along(color), y=project, fill=factor(color)),
             colour="white", pch=21, size=5)+
  guides(fill=guide_legend(title="colors\nbars: stash\ndots: projects"))+
  theme(axis.text.x=element_blank())+
  theme(axis.ticks.x=element_blank())+
  scale_fill_manual(values=rav_colors)+
  xlab("Ravelry color family")+
  ylab("Number of stash items / projects")+
  ggtitle("Color distribution of stashed yarns and yarns used in projects")

# Plot estimated data from the API queries; the distribution is slightly different
# ??? plot y-axis is shifted downwards by approx 10
ggplot() +
  geom_bar(data=na.omit(yarndata),
           aes(x=reorder(color,color,length),fill=factor(reorder(color,color,length) ) ),
           stat="bin")+
  geom_point(data=stash_colors,
             aes(x=color,y=value,fill=factor(color)),
             colour="black",pch=21,size=5)+
  guides(fill=guide_legend(title="\nbars: stash items\ndots: projects (normalized)"))+
  theme(axis.text.x=element_blank())+
  theme(axis.ticks.x=element_blank())+
  scale_fill_manual(values=ravColors)+
  xlab("Ravelry color family")+
  ylab("Number of stash items")+
  ggtitle("Color distribution of stashed yarns (5000 from API) with project use comparison")

# Plot yarn weight distribution
ggplot(data=na.omit(yarndata), aes(x=reorder(weight,weight,length) )) +
  geom_bar(stat="bin",fill="MediumSlateBlue ")+
  guides(fill=guide_legend(title="Ravelry weight category"))+
  xlab("")+
  ylab("Number of stash items")+
  ggtitle("Weight distribution of stashed yarns")

