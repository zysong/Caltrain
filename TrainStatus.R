library(twitteR)
library(dplyr)
library(stringr)

consumer_key <- "WrvZpySST9ozDztvldEBi8Ibl"
consumer_secret <- "0pGP603uWcbthQTdM8arOF34YjhPCYwblx3oj2rYbjAuuBLSpK"
access_token <- "47530544-VMIwUqvqULzcdBe6vEO3lePiL4YYEq2LswpJUYP2C"
access_secret <- "w5708WDaVK6mATGshYJNwFupAJ7AxZkCJ9XbzHGRvvV6f"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
timeline<-userTimeline("CaltrainStatus", n=3000)
tw.df<-twListToDF(timeline)
tw.df<-select(tw.df, c(text, created))
tw.df$created<-as.POSIXlt(tw.df$created, tz="America/Los_Angeles")
tw.df$hour<-(tw.df$created)$hour
tw.df$date<-as.Date(tw.df$created)
tw.df$weekday<-weekdays(tw.df$date)
tw.df<-select(tw.df, -created)
incidents_byDate<-tw.df %>% group_by(date) %>% summarise(n_tw = n())
plot(incidents_byDate, type="l")
incidents_byWeekday<-tw.df %>% group_by(weekday) %>% summarise(n_tw = n())
plot(incidents_byWeekday)
incidents_byHour<-tw.df %>% group_by(hour) %>% summarise(n_tw = n())
plot(incidents_byHour)
