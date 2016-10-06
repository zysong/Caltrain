library(twitteR)
library(dplyr)
library(tidyr)
require(bit64)
library(stringr)
library(zoo)
library(forecast)
require(tseries)
library(geoR)
library(ggplot2)
library(caret)
library(tm)
library(SnowballC)
library(wordcloud)

consumer_key <- "WrvZpySST9ozDztvldEBi8Ibl"
consumer_secret <- "0pGP603uWcbthQTdM8arOF34YjhPCYwblx3oj2rYbjAuuBLSpK"
access_token <- "47530544-VMIwUqvqULzcdBe6vEO3lePiL4YYEq2LswpJUYP2C"
access_secret <- "w5708WDaVK6mATGshYJNwFupAJ7AxZkCJ9XbzHGRvvV6f"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Retrieve the timeline of tweets by @CaltrainStatus
timeline<-function(userID="CaltrainStatus", nQuery=3200, sinceDate="2014-08-17"){
  tl<-userTimeline(userID, n=nQuery)
  tl.df<-twListToDF(tl)
  while (as.Date(tail(tl.df$created, 1))>sinceDate) {
    tl<-userTimeline(userID, n=nQuery, maxID = as.integer64(tail(tl.df$id, 1))-1)
    if (length(tl)==0) break
    tl.df<-rbind(tl.df, twListToDF(tl))
  }
  return(tl.df)
}
timeline.df<-timeline()
write.csv(timeline.df, "timeline.csv")
#timeline.df<-read.csv("timeline.csv")
DateHour<-round(timeline.df$created, units="hours")

tw.df<-select(timeline.df, c(text, created))

#Extract train ID
tw.df$trainID<-NA
text.split<-str_split_fixed(tw.df$text, "#Caltrain", 2)
tw.df$text<-text.split[,1]
tw.df$text<-gsub("[0-9]{4,}", "Numbers", x=tw.df$text)
id.rows<-grep("[12348][0-9]{2}", tw.df$text)
id.position<-regexpr("[12348][0-9]{2}", tw.df$text)
id.train<-regmatches(tw.df$text, id.position)
tw.df<-tw.df[id.rows,] #keep only the rows with train IDs
tw.df$trainID<-id.train


#derive train direction (nb or sb)
tw.df$direction<-NA
tw.df$direction<-ifelse(as.integer(tw.df$trainID)%%2==0, "sb", "nb")

#derive train types (local, limited or bullet)
tw.df$trainType<-NA
local.rows<-grep("^[14]", tw.df$trainID)
limited.rows<-grep("^2", tw.df$trainID)
bullet.rows<-grep("^[38]", tw.df$trainID)
tw.df$trainType[local.rows]<-"local"
tw.df$trainType[limited.rows]<-"limited"
tw.df$trainType[bullet.rows]<-"bullet"

#Extract time information
created<-round(as.POSIXlt(tw.df$created, tz="America/Los_Angeles"), units="hours")
tw.df$hour<-created$hour
tw.df$date<-as.Date(created)
tw.df$weekday<-as.factor(weekdays(tw.df$date))
tw.df<-select(tw.df, -created)

#Count words and plot wordcloud
tw.text<-str_replace_all(tw.df$text,"[^[:graph:]]", " ")
twCorpus<-Corpus(VectorSource(tw.text))
twCorpus<-tm_map(twCorpus, PlainTextDocument)
twCorpus<-tm_map(twCorpus, removeWords, stopwords('english'), mc.cores=1)
twCorpus<-tm_map(twCorpus, content_transformer(tolower))
twCorpus<-tm_map(twCorpus, removePunctuation)
twCorpus<-tm_map(twCorpus, stemDocument)
wordcloud(twCorpus, max.words = 100, random.order = FALSE)

#Aggregate incidents by day
incidents_byDate<-tw.df %>% group_by(date) %>% summarise(n_tw = n())
alldays<-data.frame(date=seq(incidents_byDate$date[1], 
             length=as.integer(max(incidents_byDate$date)-min(incidents_byDate$date)+1), 
             by="1 day"))
incidents_alldays<-left_join(alldays, incidents_byDate, by="date")
incidents_alldays$n_tw[is.na(incidents_alldays$n_tw)]<-0
incidents_alldays$weekday<-as.factor(weekdays(incidents_alldays$date))
plot(incidents_alldays[,1:2], type="l", xlab="Date", ylab="Incidents")
hist(incidents_alldays$n_tw, breaks=40, xlab = "Incidents", ylab = "Days", main = "Histogram of incidents per day")

#Aggregate incidents by week
incidents_alldays$week<-rep(1:ceiling(nrow(incidents_alldays)/7), each=7, length.out=nrow(incidents_alldays))
incidents_byWeek<-incidents_alldays %>% group_by(week) %>% summarise(incidents=sum(n_tw))
incidents_byWeek<-incidents_byWeek[-nrow(incidents_byWeek),]
hist(incidents_byWeek$incidents)
hist(log1p(incidents_byWeek$incidents), breaks = 20)
firstWeek<-as.integer(difftime(incidents_alldays$date[1], as.Date("2014-01-01"), units = "week"))
ts.week<-ts(incidents_byWeek$incidents, start=1, frequency = 1)
plot(ts.week)
adf.test(ts.week) #It is stationary
ets.week<-ets(ts.week)
pred.ets.week<-forecast(ets.week, lower=0)
plot(pred.ets.week)

ts.incidents<-ts(incidents_alldays$n_tw, start=1, frequency = 7)
msts.incidents<-msts(incidents_alldays$n_tw, c(7, 30), 7)
plot(ts.incidents, xlab="Time (week)", ylab="Incidents")
plot(msts.incidents, xlab="Time (week)", ylab="Incidents")
ndiffs(ts.incidents) #return 0
adf.test(ts.incidents) #it is stationary
stl.ts<-stl(ts.incidents, s.window = "period")
plot(stl.ts)
fit.ets<-ets(ts.incidents)
pred.ets<-forecast(fit.ets, 5)
plot(pred.ets)
Acf(ts.incidents)
Pacf(ts.incidents)
fit.arima<-auto.arima(ts.incidents)
plot(forecast(fit.arima, 5))
arimafcasts<-rollapplyr(ts.incidents, width=28, function(x) forecast(auto.arima(x), 1)$mean)

mape<-function(act, pred){
  mean(abs(1-pred/act))
}

incidents_byWeekday<-tw.df %>% group_by(weekday) %>% summarise(n_tw = n())
plot(incidents_byWeekday)
#Tuesday tends to have more incidents. Let's test the difference
lm.weekday<-lm(n_tw~weekday, data=incidents_alldays)
summary(lm.weekday)
hist(subset(incidents_alldays, weekday=="Tuesday")$n_tw, breaks = 40, xlab = "Incidents on Tuesdays", main="")

incidents_byHour<-tw.df %>% group_by(hour) %>% summarise(n_tw = n())
plot(incidents_byHour)

incidents_byDirection<-tw.df %>% group_by(direction) %>% summarise(n_tw = n())
barplot(incidents_byDirection$n_tw, names.arg = incidents_byDirection$direction, xlab = "Direction", ylab = "Incidents")

incidents_byTrainType<-tw.df %>% group_by(trainType) %>% summarise(n_tw = n())
barplot(incidents_byTrainType$n_tw, names.arg = incidents_byTrainType$trainType, xlab = "Train type", ylab = "Incidents")
