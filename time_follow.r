# time series of pattern fvaes, comments etc as function of time

library(ggplot2)


pattern_time <- read.csv("pattern_follow.txt",header=F,sep=" ")
#pattern_names <- c("barley-2","hitofude-cardigan","honey-cowl")
names(pattern_time) <- c("permalink","queued_projects_count","projects_count","favorites_count","comments_count","time")

pattern_time2 <- scale(pattern_time[,c(2:5)], center=as.numeric(pattern_time[1,c(2:5)]), scale=F)



pattern_time3 <- as.data.frame(cbind("permalink"=pattern_time$permalink,
                                     pattern_time2,
                                     "time"=pattern_time$time, stringsAsFactors=F), stringsAsFactors=F)

ggplot(pattern_time3) + 
  geom_point(aes(x = time, y=favorites_count, color=permalink), size=5) +
  xlab("Time") +
  ylab("") +
  ggtitle("")
