#library(sf)
#library(raster)
#library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
#library(leaflet) # for interactive maps
#library(mapview)
library(ggplot2)

#library(shiny)

# dev.cur() # shows the current graphic devices
# dev.off() # delete current output graphic device
# png("filename.png") or jpeg(...) or windows()... redirect the graphic output
# pdf(...)
# dev.off() will clear the graphic output device


# using tmap library

tmap_mode("plot")


us_states_map <- tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map

# adding user data for plot. Note that the data just needs to be match the number of category

my_usstates <- us_states[c("GEOID", "NAME")]
visited <- runif(nrow(my_usstates), min=0, max=20)
my_usstates$visited = floor(visited+0.5)
tm_shape(my_usstates, projection = 2163) + tm_polygons("visited")
tm_shape(my_usstates, projection = 2163) + tm_polygons()


# rgdal package --> download the polygon data from http://www.personal.psu.edu/users/a/c/acr181/election.html
library(GWmodel)
library(rgdal)
US3 <- readOGR(dsn="2004_Election", layer = "2004_Election_Counties")
plot(US3)





# usmap package

library(usmap)
library(ggplot)
mdcountypop<-usmap::countypop[which(usmap::countypop$abbr=="MD"),]

fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
# or
# Alldata <- read.csv(fileconnect1, sep =",", header = TRUE, encoding="UTF-8")

head(Alldata)
namelength <- sapply(as.character(Alldata$county), nchar) # find the lenght of county name
longestName <- which.max(namelength) # give a integer value with a name (a longest county name)
names(longestName)

# library(readr)
# tt<-parse_number(countynames)

#library(tidyr)
#tempnumeric <- extract_numeric(countynames) # extrac numbers from string-numeric mix. If no numeric value, "NA"
#temp1 <- as.numeric(is.na(tempnumeric)) # temp1 will be ones if all NAs in tempnueric (no numeric in county names)
#temp1 <- sum(temp1) 
# if nrow(Alldata) is the same as temp1, then no county names have numeric value

mydate <- "2020-05-24"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
dedata <- subset(Alldata, state=="Delaware" & date ==mydate)

plot_usmap(include=c("MD"), data= mddata, value="cases") +  
  scale_fill_continuous(low = "white", high = "red", name = paste("MD Covid-19 cases:", mydate), label = scales::comma, 
                        limit = c(0, 20000)) +  theme(legend.position = "right" ) 


plot_usmap(data=statepop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 State Pop", mydate), label = scales::comma) +
  theme(legend.position = "right" )

plot_usmap(data=countypop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 County Pop", mydate), label = scales::comma, 
                        trans = "log")


# two different maps with two different colors
mdstate <- plot_usmap(include=c("MD"), data= mddata, value="cases", color="black", size=1.5)  

destate <- plot_usmap(include=c("DE"), data= dedata, value="cases", color="blue", size =1.5)  

ggplot() + mdstate$layers[[1]] + destate$layers[[1]] + mdstate$theme +  
  scale_fill_continuous(low = "white", high = "red", name = paste("Covid-19 cases:", mydate), label = scales::comma, 
                        limit = c(0, 15000)) +  theme(legend.position = "right" )
