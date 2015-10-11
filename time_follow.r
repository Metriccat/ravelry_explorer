# time series of pattern fvaes, comments etc as function of time

library(ggplot2)
library(scales)
library(plyr)
library(TTR)

#pattern_time <- read.csv("pattern_follow.txt",header=F,sep=" ")
BT_time <- read.csv("BT_follow.txt",header=F,sep=" ")
#pattern_names <- c("barley-2","hitofude-cardigan","honey-cowl")
#names(pattern_time) <- c("permalink","queued_projects_count","projects_count","favorites_count","comments_count","date")
names(BT_time) <- c("pattern","queued_projects_count","projects_count","favorites_count","comments_count","day","time")
names_BT <- c("ashland-2","bannock","birch-bay-2","cascades","deschutes-2","copse-2",
             "fletching-3","lander","lolo","mcloughlin","nehalem-2","riverbend-2",
             "sauvie","trailhead-2","willamette-7")
names_full <- c("Ashland","Bannock","Birch Bay","Cascades","Deschutes","Copse",
                "Fletching","Lander","Lolo","McLoughlin","Nehalem","Riverbend",
                "Sauvie","Trailhead","Willamette")
BT_colors <- c("#C4BA93",
               "#BD57C9",
               "#D2493C",
               "#77D64C",
               "#52395D",
               "#414F3A",
               "#C6467F",
               "#7179C8",
               "#71D3A0",
               "#81B6C3",
               "#D1CE4D",
               "#CD97B2",
               "#C2813A",
               "#5F8639",
               "#74392F")
names(names_full) <- names_BT
BT_time <- subset(BT_time,subset=(pattern %in% names_BT))
BT_time$pattern <- droplevels(BT_time$pattern)
# use pattern name instead of permalink
BT_time$pattern <- revalue(BT_time$pattern, names_full)                                                
# egt full date from day and hour/min, in BT local time (PDT)
BT_time$date <- as.POSIXct(paste(BT_time$day, BT_time$time))
attributes(BT_time$date)$tzone <- "America/Los_Angeles" 
  
# pattern_time2 <- scale(pattern_time[,c(2:5)], center=as.numeric(pattern_time[1,c(2:5)]), scale=F)
# pattern_time3 <- as.data.frame(cbind("permalink"=pattern_time$permalink,
#                                      pattern_time2,
#                                      "time"=pattern_time$time, stringsAsFactors=F), stringsAsFactors=F)


# corelation between queuing and favoriting
ddply(BT_time, 
      .(pattern), 
      function(data) data.frame(correl = cor(data$favorites_count,
                                             data$queued_projects_count)))

n=dim(BT_time)[1]
lastdate = BT_time$date[n]

ggplot(BT_time) + 
  geom_line(aes(x=date, y=favorites_count, color=pattern, group=pattern)) +
  geom_text(data=BT_time[BT_time$date ==lastdate,], 
            aes(x=date,
                y=favorites_count,
                color=pattern,
                label=pattern),
            vjust = -0.2) +
  xlab("Time") +
  ylab("Number of favorites") +
  theme(legend.position="none") +
  scale_colour_manual(values=BT_colors) +
  ggtitle("Evolution of number of favorites on BT Fall 2015 patterns")


# nbr of favorites per unit time
dfav_dt <- function(data){
  n <- dim(data)[1] # nbr lines
  favdiff <- c(0, diff(data$favorites_count)) # length n
  timediff <- difftime(data$date, c(data$date[1], data$date[1:n-1]),units="hours")
  # div by 0 on first item, but ggplot will just ignore those points
  timediff <- as.numeric(timediff) 
  favderiv <- SMA(favdiff/timediff)
  return(data.frame(deriv = favderiv, 
                    date = data$date))
}

m <- 3000
rates <- ddply(BT_time[1:m,],.(pattern), dfav_dt)

norm_col <- function(data){
  maxd <- max(data$deriv, na.rm=T)
  return(cbind(data,norm_deriv=data$deriv/maxd))
}

rates <- ddply(rates,.(pattern), norm_col)

ggplot(rates) + 
  geom_line(aes(x=date, y=norm_deriv, color="red", group=pattern)) +
  xlab("Time") +
  ylab("Normalized favoriting rate") +
  theme(legend.position="none") +
  ggtitle("Evolution of favoriting rate on BT Fall 2015 patterns")

#smooth_rates <- SMA(rates[rates$pattern=="Cascades","deriv"])

plot(smooth_rates)
