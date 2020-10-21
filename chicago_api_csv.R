#
# R script to analyze the homicide rate in Chicago. Data is provided via the API of data.cityofchicago.org
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
# library(dplyr)
# library(ggplot2)
# library(readr)
# library(reshape2)

#
# import via web API
# $app_token=2OghfIz6RILPCh3eJQ5XI4ZZQ obtained via OpenData by Socrata
#
crime_file = "https://data.cityofchicago.org/resource/6zsd-86xi.csv?primary_type=HOMICIDE&$where=year>2000&$limit=10000&$$app_token=2OghfIz6RILPCh3eJQ5XI4ZZQ"
chicagocrime <- read_csv(crime_file, 
                          col_types = cols(
                            date = col_datetime(format = "%Y-%m-%dT%H:%M:%S"), 
                            location = col_skip(), 
                            updated_on = col_skip(), 
                            x_coordinate = col_skip(), 
                            y_coordinate = col_skip()
                          )
 ) 

#
# add some useful date-based modifiers for further analysis
#
#chicagocrime$month = month(chicagocrime$date)
#chicagocrime$day = day(chicagocrime$date)
#chicagocrime$hour = hour(chicagocrime$date)
#chicagocrime$dow = weekdays(chicagocrime$date)
#chicagocrime$yday = yday(chicagocrime$date)

# and in dplyr style
 chicagocrime <- chicagocrime %>% mutate(month = month(date)) %>%
                  mutate(day = day(date)) %>%
                  mutate(hour = hour(date)) %>%
                  mutate(dow = weekdays(date)) %>%
                  mutate(yday = yday(date))
                  



#
# summarize homicide data by month/year
#
homicide <- chicagocrime %>% 
  group_by(primary_type,year,month) %>% 
  summarise(count=n()) 

homicide$yearcode=homicide$year+homicide$month/12
ggplot(homicide)+aes(yearcode,count)+geom_point()#+geom_smooth(method="loess")

#
# Summary homicide data on year-to-date, down to the last reported month
#
#this_month = month(today())
#homicide_YTD <- homicide %>% filter(month < this_month) %>% group_by(year) %>% summarise(total=sum(count))
#homicide_YTD %>%  ggplot + aes(year,total)+geom_bar(stat="identity")

#
# Summary homicide data on year-to-date, down to the last reported yearday
#
homicide <- chicagocrime %>% 
  group_by(primary_type,year,yday) %>% 
  summarise(count=n())

homicide$yearsum <- ave(homicide$count,homicide$year,FUN=cumsum)

maxyear <- max(homicide$year)
minyear <- min(homicide$year)
yday_thisyear <- homicide %>% filter(year==maxyear)
maxyday <- max(yday_thisyear$yday)
homicide_YTDay <- homicide %>% filter(yday <= maxyday )  %>% group_by(year) %>% summarise(total=sum(count))

maxdate <- max(chicagocrime$date)
maxday <- day(maxdate)
maxmonth <- month(maxdate, label=TRUE)
title = paste("Chicago homicides year-to-date as of ", maxmonth ," ",maxday,sep="")

max_y_value = (max(homicide_YTDay$total) %/% 50 + 1)* 50

homicide_YTDay %>%  ggplot + aes(year,total)+geom_bar(stat="identity") +
                             labs(title=title, x="Year", y="Homicide count") +
                             geom_text(aes(label=total),vjust=-1) + 
                             ylim(0,max_y_value)
                             


#
# data file write
#
lastdate <- as.Date(paste(toString(maxyear),"-01-01",sep=""))+maxyday-1
outfile <- paste("chicagohomicide-",lastdate,".csv",sep="")
goutfile <- paste("chicagohomicide-",lastdate,".png",sep="")
#write.csv(homicide_YTDay, file = outfile)
write.csv(homicide, file = outfile)
#ggsave(goutfile)

title = paste("Chicago cumulative homicides by day-of-year as of ", maxmonth ," ",maxday,sep="")
homicide %>% filter( year >= 2015)  %>% mutate(ydate = as.Date(paste("2017-01-01",sep=""))+yday-1) %>%
                                          ggplot+aes(ydate,yearsum, color=factor(year)) +  geom_line()+
                                          labs(title=title, x="Time of year", y="Cumulative homicide count") +
                                          scale_x_date(date_breaks = "1 month", date_labels =  "%b")   +
                                          labs(color="By Year")+ theme(legend.position = c(0.05,0.851)) +
                                          ylim(0,800)  



#rm(outfile,yday_thisyear,maxyear,maxyday,this_month)

#chicagocrime %>% group_by(month) %>% summarise(count=n()) %>%  ggplot + aes(month,count) + geom_bar(stat="identity")