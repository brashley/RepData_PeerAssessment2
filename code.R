library("tidyr")
library("dplyr")
library("lubridate")
library(data.table)
library(ggplot2)
library(scales)



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
                 filter(YEAR >= "2001") 

event_counts <- sd_new %>% group_by(EVTYPE) %>% 
    summarize(Deaths=sum(FATALITIES), Injuries=sum(INJURIES))


nrow(event_counts)

# patch up most of the big Event Types for the newer data
sd_new$EVTYPE <- gsub("/", " ", sd_new$EVTYPE) 
sd_new$EVTYPE[grep("WINTER WEATHER MIX|WINTER WEATHER/MIX", sd_new$EVTYPE)] <- "WINTER WEATHER"
sd_new$EVTYPE[grep("^WILD", sd_new$EVTYPE)] <- "WILDFIRE"
sd_new$EVTYPE[grep("^MARINE TS", sd_new$EVTYPE)] <- "MARINE THUNDERSTORM WIND"
sd_new$EVTYPE[grep("^TSTM", sd_new$EVTYPE)] <- "TROPICAL STORM"
sd_new$EVTYPE[grep("^THUNDERSTORM", sd_new$EVTYPE)] <- "THUNDERSTORM WIND"
sd_new$EVTYPE[grep("^STRONG WIND", sd_new$EVTYPE)] <- "STRONG WIND"
sd_new$EVTYPE[grep("^STORM SURGE", sd_new$EVTYPE)] <- "STORM SURGE TIDE"
sd_new$EVTYPE[grep("^RIP CURRENT", sd_new$EVTYPE)] <- "RIP CURRENT"
sd_new$EVTYPE[grep("^HURRICANE", sd_new$EVTYPE)] <- "HURRICANE"
sd_new$EVTYPE[grep("^HEAVY SURF", sd_new$EVTYPE)] <- "HIGH SURF"
sd_new$EVTYPE[grep("^EXTREME COLD|^EXTREME WINDCHIL", sd_new$EVTYPE)] <- "EXTREME COLD"
sd_new$EVTYPE[grep("^COLD WEATHERL", sd_new$EVTYPE)] <- "COLD WIND CHILL"
sd_new$EVTYPE[grep("^COASTAL FLOOD", sd_new$EVTYPE)] <- "COASTAL FLOOD"
sd_new$EVTYPE[grep("^MUD", sd_new$EVTYPE)] <- "MUD SLIDE"
sd_new$EVTYPE[grep("^LAKE EFFECT SNOW", sd_new$EVTYPE)] <- "LAKE-EFFECT SNOW"
sd_new$EVTYPE[grep("^GUSTY WIND", sd_new$EVTYPE)] <- "GUSTY WINDS"
sd_new$EVTYPE[grep("^HIGH SURF", sd_new$EVTYPE)] <- "HIGH SURF"



sd_health <- sd_new %>% filter(FATALITIES > 0 | INJURIES > 0)

event_counts <- sd_health %>% group_by(EVTYPE) %>% 
                  summarize(Deaths=sum(FATALITIES), Injuries=sum(INJURIES))

nrow(event_counts)

top_events <- event_counts %>% top_n(5) %>% 
                  arrange(desc(Injuries)) %>% 
                  rename(Event = EVTYPE) %>% 
                  gather(Impact, QTY, -Event) 


top_events <- transform(top_events, 
                          Event = reorder(Event, order(QTY, decreasing = TRUE)))

g <- ggplot(data=top_events, aes(x=Event, y=QTY, fill=Impact)) + geom_bar(stat="identity")   #, fill=QTY) stat="sum"
g <- g + ggtitle("Most Harmfull Weather Event to Health In the U.S. from 2001 to 2011\n")
g + theme(plot.title = element_text(lineheight=.8, face="bold"))

##############################################

sd_damage <- sd_new %>% filter(PROPDMG > 0 | CROPDMG > 0)

event_counts <- sd_damage %>% group_by(EVTYPE) %>% 
    summarize(Deaths=sum(FATALITIES), Injuries=sum(INJURIES))

nrow(event_counts)


levels(sd_damage$PROPDMGEXP)
sd_damage %>% group_by(PROPDMGEXP) %>% summarize(qty=n())
sd_damage %>% group_by(CROPDMGEXP) %>% summarize(qty=n())

# convert to leters to Unit multipliers
sd_damage$PROPDMGX[sd_damage$PROPDMGEXP=="B"] <- 10^9
sd_damage$PROPDMGX[sd_damage$PROPDMGEXP=="M"] <- 10^6
sd_damage$PROPDMGX[sd_damage$PROPDMGEXP=="K"] <- 10^3
sd_damage$PROPDMGX[is.na(sd_damage$PROPDMGX)] <- 1
sd_damage$PROPDMGCOST <- sd_damage$PROPDMG * sd_damage$PROPDMGX

sd_damage$CROPDMGX[sd_damage$CROPDMGEXP=="B"] <- 10^9
sd_damage$CROPDMGX[sd_damage$CROPDMGEXP=="M"] <- 10^6
sd_damage$CROPDMGX[sd_damage$CROPDMGEXP=="K"] <- 10^3
sd_damage$CROPDMGX[is.na(sd_damage$CROPDMGX)] <- 1
sd_damage$CROPDMGCOST <- sd_damage$CROPDMG * sd_damage$CROPDMGX


event_costs <- sd_damage %>% group_by(EVTYPE) %>% 
    summarize(Property=sum(PROPDMGCOST), Crops=sum(CROPDMGCOST))

nrow(event_counts)

top_events <- event_costs %>% top_n(5,Property) %>% 
    arrange(desc(Property)) %>% 
    rename(Event = EVTYPE) %>% 
    gather(Impact, QTY, -Event) 


top_events <- transform(top_events, 
                        Event = reorder(Event, order(QTY, decreasing = TRUE)))

g <- ggplot(data=top_events, aes(x=Event, y=QTY, fill=Impact)) + geom_bar(stat="identity")   #, fill=QTY) stat="sum"
g <- g + ggtitle("Most Harmfull Weather Event to Damage Costs In the U.S. from 2001 to 2011\n")
g <- g + theme(plot.title = element_text(lineheight=.8, face="bold")) 
g + labs( y = "Damage in $") + scale_y_continuous(labels=dollar)


