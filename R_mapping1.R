#library(sf)
#library(raster)
#library(dplyr)
#library(spData)
#library(spDataLarge)

library(tmap)    # for static and interactive maps
#library(leaflet) # for interactive maps
#library(mapview)
library(ggplot2)

#library(shiny)

tmap_mode("plot")

us_states_map <- tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map


library(usmap)
mdcountypop<-usmap::countypop[which(usmap::countypop$abbr=="MD"),]

fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
# or
Alldata <- read.csv(fileconnect1, sep =",", header = TRUE, encoding="UTF-8")

head(Alldata)

mddata <- subset(Alldata, state=="Maryland" & date =="2020-05-23")


plot_usmap("counties", include=c("MD"), data= mddata, value="cases") +  
  scale_fill_continuous(low = "white", high = "red", name = "MD Covid-19 cases", label = scales::comma)
