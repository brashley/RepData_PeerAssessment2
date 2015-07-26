library("tidyr")
library("dplyr")
library("lubridate")
library(data.table)
library(ggplot2)



if (!file.exists("StormData.csv.bz2")) {
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(fileUrl, destfile="StormData.csv.bz2", mode="wb")
}

StormData <- suppressWarnings(read.csv("StormData.csv.bz2",stringsAsFactors=FALSE))

glimpse(StormData)
head(StormData)

StormData$BGN_DATE <- mdy_hms(StormData$BGN_DATE)

storm_year <- StormData %>% mutate(YEAR=year(BGN_DATE)) %>% group_by(YEAR) %>% summarize(rows=n())
# decided to use the last 10 years of data as representative # of events rises by factor of 5
storm_year[35:52,]


sd_new <- StormData %>% mutate(YEAR=year(BGN_DATE)) %>% 
                 filter(YEAR >= "2001") %>%
                 filter(FATALITIES > 0 | INJURIES > 0)

# patch up most of the big Event Types
sd_new$EVTYPE <- gsub("/", " ", sd_new$EVTYPE) 
sd_new$EVTYPE[grep("WINTER WEATHER MIX|WINTER WEATHER/MIX", sd_new$EVTYPE)] <- "WINTER WEATHER"
sd_new$EVTYPE[grep("^TSTM", sd_new$EVTYPE)] <- "TROPICAL STORM"
sd_new$EVTYPE[grep("^THUNDERSTORM", sd_new$EVTYPE)] <- "THUNDERSTORM WIND"
sd_new$EVTYPE[grep("^STRONG WIND", sd_new$EVTYPE)] <- "STRONG WIND"
sd_new$EVTYPE[grep("^STORM SURGE", sd_new$EVTYPE)] <- "STORM SURGE TIDE"
sd_new$EVTYPE[grep("^RIP CURRENT", sd_new$EVTYPE)] <- "RIP CURRENT"
sd_new$EVTYPE[grep("^HURRICANE", sd_new$EVTYPE)] <- "HURRICANE"
sd_new$EVTYPE[grep("^HEAVY SURF", sd_new$EVTYPE)] <- "HIGH SURF"
sd_new$EVTYPE[grep("^EXTREME COLD|^EXTREME WINDCHIL", sd_new$EVTYPE)] <- "EXTREME COLD"
sd_new$EVTYPE[grep("^COLD WEATHERL", sd_new$EVTYPE)] <- "COLD WIND CHILL"



event_counts <- sd_new %>% group_by(EVTYPE) %>% 
                  summarize(Deaths=sum(FATALITIES), Injuries=sum(INJURIES))

top_events <- event_counts %>% top_n(5) %>% 
                  arrange(desc(Injuries)) %>% 
                  rename(Event = EVTYPE) %>% 
                  gather(Impact, QTY, -Event) 

gather(top_events, Impact, QTY) %>% rename(Event = EVTYPE)
top_events <- transform(top_events, 
                          Event = reorder(Event, order(QTY, decreasing = TRUE)))

g <- ggplot(data=top_events, aes(x=Event, y=QTY, fill=Impact)) + geom_bar(stat="identity")   #, fill=QTY) stat="sum"
g <- g + ggtitle("Most Harmfull Weather Event In the US from 2001 to 2011\n")
g + theme(plot.title = element_text(lineheight=.8, face="bold"))

levels(sd$PROPDMGEXP)
sd_new %>% group_by(PROPDMGEXP) %>% summarize(qty=n())


FATALITIES INJURIES PROPDMG CROPDMG
# 1996 
#  >0 fatalities or >0 injuries or >0 proprieties damages or >0 crop damages
# gsub("/", " ", df$EVTYPE) and gsub("  ", " ", df$EVTYPE)
