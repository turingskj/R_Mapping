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
library(plotly)

# add Covid-19 data
fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
head(Alldata)  # check if county names include numeric values
mydate <- "2020-03-01"
#mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
#dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
#Alldata$date <-as.factor(Alldata$date)

covid_bardata <- tapply(Alldata$cases, Alldata$date, FUN=sum)
covid_bardata <- as.data.frame(covid_bardata)
names(covid_bardata)[1] <- "cases"
covid_bardata$date <- rownames(covid_bardata)
covid_bardata$date <- as.factor(covid_bardata$date)

location1 <- grep(mydate, covid_bardata$date)
covid_bardata <- covid_bardata[location1:nrow(covid_bardata),]

covid_bardata$dailycase[1] <- 0
#covid_bardata$average5[1] <- covid_bardata$dailycase[1]
for (myvar in seq(1:nrow(covid_bardata))) {   
  
  if (myvar > 1) {
    
    covid_bardata$dailycase[myvar] <- (covid_bardata$cases[myvar] -  covid_bardata$cases[myvar-1])
    
  }
}

for (myvar in seq(1:nrow(covid_bardata))) {   
  if (myvar < 7) {
    covid_bardata$average7[myvar] <- mean(covid_bardata$dailycase[1:myvar])
  }
  else {
    covid_bardata$average7[myvar] <- mean(covid_bardata$dailycase[(myvar-6):(myvar)])
  }
  
}

myplot2<-ggplot(data=covid_bardata, aes(x=date, y=dailycase))+
  geom_bar(stat="identity", width= 0.8) + scale_x_discrete(breaks = covid_bardata$dailycase[c(T,F,F)])

myplot<-ggplot(data=covid_bardata, aes(x=date, group=1 ))+
  geom_bar(aes(y=dailycase), stat="identity", width= 0.5, color="darkorange", fill="pink") + 
  geom_line(aes(y = average7), stat="identity", color="red", size=1.2) +
  scale_x_discrete(breaks = covid_bardata["date"][seq(1, nrow(covid_bardata), 20),])
myplot
myplot2

grid.arrange(myplot, myplot2, ncol=2) # use "grid" package to arrange plots.

library(gridExtra) # use this pacakge to arrange plots into one plot of grid

myplotboth <- arrangeGrob(myplot, myplot2)
grid::grid.draw(myplotboth)  # if the pacakge "grid" is not loaded, we can still acess its function
                             # grid.draw() using "::" operator

myplot
# putting interactive charts in one plot
a1<-ggplotly(myplot)
a2<-ggplotly(myplot2)
subplot(a1, a2)

#############
#############


fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
head(Alldata)  # check if county names include numeric values

# mydate <- "2020-05-28"
mddata <- subset(Alldata, state=="Maryland")
mddata$date <-as.factor(mddata$date)
#dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
#Alldata$date <-as.factor(Alldata$date)

mdcovid_bardata <- tapply(mddata$cases, mddata$date, FUN=sum)

mdcovid_bardata <- as.data.frame(mdcovid_bardata)
names(mdcovid_bardata)[1] <- "cases"
mdcovid_bardata$date <- rownames(mdcovid_bardata)
mdcovid_bardata$date <- as.factor(mdcovid_bardata$date)
mdcovid_bardata <- na.omit(mdcovid_bardata)
#location1 <- grep("2020-01-01", covid_bardata$date)
#covid_bardata <- covid_bardata[location1:129,]

mdcovid_bardata$dailycase[1] <- 0
#covid_bardata$average5[1] <- covid_bardata$dailycase[1]

for (myvar in seq(1:nrow(mdcovid_bardata))) {   
  
  if (myvar > 1) {
    
    mdcovid_bardata$dailycase[myvar] <- (mdcovid_bardata$cases[myvar] -  mdcovid_bardata$cases[myvar-1])
    
  }
}

for (myvar in seq(1:nrow(mdcovid_bardata))) {   
  if (myvar < 7) {
    mdcovid_bardata$average7[myvar] <- mean(mdcovid_bardata$dailycase[1:myvar])
  }
  else {
    mdcovid_bardata$average7[myvar] <- mean(mdcovid_bardata$dailycase[(myvar-6):(myvar)])
  }
  
}

ggplot(data=mdcovid_bardata, aes(x=date, y=dailycase))+
  geom_bar(stat="identity", width= 0.8) + scale_x_discrete(breaks = mdcovid_bardata$dailycase[c(T,F,F)])

ggplot(data=mdcovid_bardata, aes(x=date, group=1 ))+
  geom_bar(aes(y=dailycase), stat="identity", width= 1, color="white", fill="blue") + 
  geom_line(aes(y = average7), stat="identity", color="red", size=1.2) +
  scale_x_discrete(breaks = mdcovid_bardata["date"][seq(1, nrow(mdcovid_bardata), 15),])



