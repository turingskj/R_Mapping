
library(ggplot2)
library(plotly)

# add Covid-19 data from NY Times Github repository
cat('Downloading data from the NYT data repository...\n')
fileconnect1 <-url("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
cat('Done downloading.\n')

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


myplot<-ggplot(data=covid_bardata, aes(x=date, group=1 ))+
  geom_bar(aes(y=dailycase), stat="identity", width= 0.5, color="darkorange", fill="pink") + 
  geom_line(aes(y = average7), stat="identity", color="red", size=1.2) +
  scale_x_discrete(breaks = covid_bardata["date"][seq(1, nrow(covid_bardata), 20),]) +
  theme(aspect.ratio= 0.5)





myplot
ggplotly(myplot)






