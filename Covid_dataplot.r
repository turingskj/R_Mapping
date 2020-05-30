library(sf)
#library(raster)
library(dplyr)
library(spData)
library(tigris)
#library(spDataLarge)

library(tmap)    # for static and interactive maps
library(tmaptools)
library(usmap)
#library(leaflet) # for interactive maps
#library(mapview)
library(ggplot2)


# add Covid-19 data
fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
head(Alldata)  # check if county names include numeric values
#mydate <- "2020-05-26"
#mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
#dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
#Alldata$date <-as.factor(Alldata$date)

covid_bardata <- tapply(Alldata$cases, Alldata$date, FUN=sum)
covid_bardata <- as.data.frame(covid_bardata)
names(covid_bardata)[1] <- "cases"
covid_bardata$date <- rownames(covid_bardata)
covid_bardata$date <- as.factor(covid_bardata$date)

#location1 <- grep("2020-01-01", covid_bardata$date)
#covid_bardata <- covid_bardata[location1:129,]

covid_bardata$dailycase[1] <- covid_bardata$cases[1]
#covid_bardata$average5[1] <- covid_bardata$dailycase[1]

for (myvar in seq(1:nrow(covid_bardata))) {   

  if (myvar > 1) {
 
    covid_bardata$dailycase[myvar] <- (covid_bardata$cases[myvar] -  covid_bardata$cases[myvar-1])
    
  }
}

for (myvar in seq(1:nrow(covid_bardata))) {   
  if (myvar ==1){
    covid_bardata$average5[myvar] <- mean(covid_bardata$dailycase[myvar:(myvar+4)])
  }
  if (myvar == 2){
    covid_bardata$average5[myvar] <- mean(covid_bardata$dailycase[(myvar-1):(myvar+3)])
  }
  if ((myvar > 2) & (myvar <(nrow(covid_bardata)-1))) {
    
    covid_bardata$average5[myvar] <- mean(covid_bardata$dailycase[(myvar-2):(myvar+2)])
  }
  if (myvar == (nrow(covid_bardata)-1))  {
    covid_bardata$average5[myvar] <- mean(covid_bardata$dailycase[(myvar-2):(myvar)])
  }
  if (myvar == (nrow(covid_bardata)))  {
    covid_bardata$average5[myvar] <- mean(covid_bardata$dailycase[(myvar-3):(myvar)])
  }

}

ggplot(data=covid_bardata, aes(x=date, y=dailycase))+
  geom_bar(stat="identity", width= 0.8) + scale_x_discrete(breaks = covid_bardata$dailycase[c(T,F,F)])

ggplot(data=covid_bardata, aes(x=date, group=1 ))+
  geom_bar(aes(y=dailycase), stat="identity", width= 1, color="white", fill="blue") + 
  geom_line(aes(y = average5), stat="identity", color="red", size=1.2) +
  scale_x_discrete(breaks = covid_bardata["date"][seq(1, nrow(covid_bardata), 15),])


