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

tmap_mode(c("plot", "view"))

us_states_map <- tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map



library(usmap)
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


plot_usmap("counties", include=c("MD"), data= mddata, value="cases") +  
  scale_fill_continuous(low = "white", high = "red", name = paste("MD Covid-19 cases:", mydate), label = scales::comma)
