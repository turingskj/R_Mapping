library(sf)
#library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
library(tmaptools)
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
us_hawaii_map = tm_shape(hawaii, projection = 2163) + tm_polygons()
us_hawaii_map
us_alaska_map = tm_shape(alaska, projection = 2163) + tm_polygons()
us_alaska_map



library(tigris)
library(dplyr)

# using tmap and tigris package (census data map) # see the manual for tigris data
us_geomap <- states(class="sf") # import feature data only
tm_shape(us_geomap, projection = 2163) + tm_polygons()

visited <- runif(nrow(my_usstates), min=0, max=20)
visited = floor(visited+0.5)

#plot county map
us_geomap <- counties(class="sf") # import feature data only
tm_shape(us_geomap, projection = 2163) + tm_polygons()



# get the names of continent us states + DC from spData set
my_usstates <- us_states$NAME
us_geomap_48 <- subset(us_geomap, us_geomap[["NAME"]] %in% my_usstates)  

us_geomap_48$visited <- visited
tm_shape(us_geomap_48, projection = 2163) + tm_polygons("visited")

# add Covid-19 data
fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
head(Alldata)  # check if county names include numeric values
mydate <- "2020-05-26"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
alldata <-subset(Alldata, date ==mydate)

statecase <- tapply(alldata$cases, alldata$state, FUN=sum)
statecase <- as.data.frame(statecase)
statecase$NAME <- rownames(statecase)
statecase <- subset(statecase, NAME %in% my_usstates)  
us_geomap48_covid <- merge(us_geomap_48, statecase, by="NAME")

maxcase <- max(statecase$statecase)
maxcase <- (ceiling(maxcase/100000))*100000

tm_shape(us_geomap48_covid, projection = 2163) +  
  tm_fill("statecase", title="Covid19 5-26-2020", breaks = seq(from=0, to = maxcase, by=100000)) +
  tm_borders("black")

tmap_mode("view")
qtm(us_geomap48_covid, projection = 2163, fill="statecase", fill.title="Covid19-state", 
    fill.style="fixed", fill.breaks = seq(from=0, to = maxcase, by=50000), 
    fill.palette=brewer.pal(8, "Reds"), text="statecase")


# using Census Bureau map
uscount_cbmap <- readOGR(dsn="mapdata/cb_2019_us_all_5m", layer = "cb_2019_us_state_5m")
tm_shape(uscount_cbmap, projection =2163) + tm_polygons()



# adding user data for plot. Note that the data just needs to be match the number of category
library(spData)
my_usstates <- us_states[c("GEOID", "NAME")]
visited <- runif(nrow(my_usstates), min=0, max=20)
my_usstates$visited = floor(visited+0.5)
tm_shape(my_usstates, projection = 2163) + tm_polygons("visited")
tm_shape(my_usstates, projection = 2163) + tm_polygons()




# rgdal package --> download the polygon data from http://www.personal.psu.edu/users/a/c/acr181/election.html
library(GWmodel)
library(rgdal)
US3 <- readOGR(dsn="mapdata/2004_Election", layer = "2004_Election_Counties")
plot(US3)





# usmap package

library(usmap)
library(ggplot)
library(ggplot2)

plot_usmap("counties")
plot_usmap("states") 

# read data (population data)
mdcountypop<-usmap::countypop[which(usmap::countypop$abbr=="MD"),]

fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
# or
# Alldata <- read.csv(fileconnect1, sep =",", header = TRUE, encoding="UTF-8")

head(Alldata)  # check if county names include numeric values
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

mydate <- "2020-05-25"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
alldata <-subset(Alldata, date ==mydate)


plot_usmap(include=c("MD"), data= mddata, value="cases", projection = 2163) +  
  scale_fill_continuous(low = "white", high = "red", name = paste("MD Covid-19 cases:", mydate), label = scales::comma, 
                        limit = c(0, 20000)) +  theme(legend.position = "right" ) 

# try to find a state sum
statecase <- tapply(alldata$cases, alldata$state, FUN=sum)
statecase <- as.data.frame(statecase)
statecase$state <- rownames(statecase)
maxcase <- max(statecase$statecase)
maxcase <- (ceiling(maxcase/100000))*100000

plot_usmap("states", data=statecase, value="statecase", color="red", size=1, labels = FALSE) +  
scale_fill_continuous(low = "white", high = "red", name = paste("Covid19 cases"), 
                        breaks = seq(from=0, to = maxcase, by=100000), label = scales::comma) +  theme(legend.position = "right" ) 


plot_usmap(data=statepop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 State Pop", mydate), label = scales::comma) +
  theme(legend.position = "right" )

plot_usmap(data=countypop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 County Pop", mydate), label = scales::comma, 
                        trans = "log") + theme(legend.position = "right")


# two different maps with two different colors
mdstate <- plot_usmap(include=c("MD"), data= mddata,  value="cases", color="black", size=1)  
destate <- plot_usmap(include=c("DE"), data= dedata,  value="cases", color="blue", size =1)  

ggplot() + mdstate$layers[[1]] + destate$layers[[1]] + mdstate$theme +  
  scale_fill_continuous(low = "white", high = "red", name = paste("Covid-19 cases:", mydate), label = scales::comma, 
                        limit = c(0, 20000)) +  theme(legend.position = "right" )
