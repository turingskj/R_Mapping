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

#library(shiny)

# dev.cur() # shows the current graphic devices
# dev.off() # delete current output graphic device
# png("filename.png") or jpeg(...) or windows()... redirect the graphic output
# pdf(...)
# dev.off() will clear the graphic output device


# using tmap library

tmap_mode("plot")

# us spData set (us_states...)
# basic map plotting using spData us_states data set using tmap package

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
us_geomap <- states(class="sf") # import feature data only using states() of the tigris package
tm_shape(us_geomap, projection = 2163) + tm_polygons()  # this will show Alaska and Hawaii together


#plot county map

my_usstates <- us_states[c("GEOID", "NAME")]  # us_states is the dataset of spData package
                                              # Hawaii and Alaska are seperate datasets of the package
us_geomap_ct <- counties(class="sf", cb=TRUE) # import feature data only
us_geomap_ct <- subset(us_geomap_ct, us_geomap_ct[["STATEFP"]] %in% my_usstates$GEOID)  
#select the continuous us states (excluding Hawaii, Alaska, and other islands.)

# if you want MD only..
#us_geomap_ct <- subset(us_geomap_ct, us_geomap_ct[["STATEFP"]] %in% "24")  

visited <- runif(nrow(us_geomap_ct), min=0, max=20)
visited = floor(visited+0.5)
us_geomap_ct$visited <- visited

tm_shape(us_geomap_ct, projection = 2163) + tm_polygons("visited") #+ tm_text("NAME", size = 9/10)

#plot county map using tigris package function counties()
#us_geomap <- counties(class="sf") # import feature data only including Alaska and Hawaii
#tm_shape(us_geomap, projection = 2163) + tm_polygons()


# get the names of continent us states + DC from spData set
#my_usstates <- us_states$NAME
#my_usstates <- us_states[c("GEOID", "NAME")]

#us_geomap_48 <- subset(us_geomap, us_geomap[["STATEFP"]] %in% my_usstates$GEOID)  
#visited <- runif(nrow(us_geomap_48), min=0, max=20)
#visited = floor(visited+0.5)

#us_geomap_48$visited <- visited
#tm_shape(us_geomap_48, projection = 2163) + tm_polygons("visited")



# add Covid-19 data
fileconnect1 <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
Alldata <- read.table(fileconnect1, sep =",", header = TRUE, encoding="UTF-8", quote = "\"")
head(Alldata)  # check if county names include numeric values
mydate <- "2020-05-30"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
alldata <-subset(Alldata, date ==mydate)
alldata$NAME <- alldata$county


my_usstates <- us_states[c("GEOID", "NAME")]  # us_states is the dataset of spData package
statecase <- tapply(alldata$cases, alldata$state, FUN=sum)  # calculate total case by state
statecase <- as.data.frame(statecase)
statecase$NAME <- rownames(statecase)
statecase <- subset(statecase, NAME %in% my_usstates$NAME)  
my_usstates_covid19 <- merge(my_usstates, statecase, by="NAME")

maxcase <- max(statecase$statecase)
maxcase <- (ceiling(maxcase/100000))*100000

tm_shape(my_usstates_covid19, projection = 2163) +  
  tm_fill("statecase", title="Covid19 5-26-2020", breaks = seq(from=0, to = maxcase, by=50000)) +
  tm_borders("black") + tm_text("statecase", size = 3/5)


# Add covid 19 data to county maps

us_geomap <- counties(class="sf", cb=TRUE, resolution="5m") # import feature data only; higher resolution (500k) is default
my_usstates <- us_states$NAME  # from spData
my_usstates <- us_states[c("GEOID", "NAME")]
us_geomap_48 <- subset(us_geomap, us_geomap[["STATEFP"]] %in% my_usstates$GEOID)  

us_geomap48_covid <- merge(us_geomap_48, alldata, by="NAME")

maxcase <- max(us_geomap48_covid$cases)
maxcase <- (ceiling(maxcase/100000))*100000


tmap_mode("plot")
# do not plot. takes too long in a slow computer
starttime<-Sys.time()
tm_shape(us_geomap48_covid, projection = 2163) +  
  tm_fill("cases", title="Covid19 5-28-2020", breaks = seq(from=0, to = maxcase, by=5000)) +
  tm_borders("black")
endtime<-Sys.time()
endtime - starttime



library(RColorBrewer)
tmap_mode("view")
starttime <-Sys.time()
qtm(us_geomap48_covid, projection = 2163, fill="cases", fill.title="Covid19-county", 
    fill.style="fixed", fill.breaks = seq(from=0, to = maxcase, by=5000), 
    fill.palette=brewer.pal(8, "Reds"))#, text="cases")
endtime <-Sys.time()
endtime - starttime

#library(RColorBrewer)
#tmap_mode("plot")
#qtm(us_geomap48_covid, projection = 2163, fill="cases", fill.title="Covid19-county", 
#    fill.style="fixed", fill.breaks = seq(from=0, to = maxcase, by=1000), 
#    fill.palette=brewer.pal(8, "Reds"), text="cases")





# Add covid 19 county data MD map only.

us_geomap <- counties(class="sf") # import feature data only

mydate <- "2020-05-28"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
mddata <-na.omit(mddata)
mddata$COUNTYFP <- gsub("24", "", mddata$fips)

md_geomap <- subset(us_geomap, us_geomap[["STATEFP"]] == "24" )  
md_geomap_covid <- merge(md_geomap, mddata, by="COUNTYFP")

maxcase <- max(md_geomap_covid$cases)
maxcase <- (ceiling(maxcase/10000))*10000

tmap_mode("plot")

tm_shape(md_geomap_covid, projection = 2163) +  
  tm_fill("cases", title="Covid19 5-28-2020", breaks = seq(from=1, to = maxcase, by=500)) +
  tm_borders("gray") + tm_text("cases", size=1, col="blue", fontface="bold") +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))

tm_shape(md_geomap_covid, projection = 2163) +  
  tm_fill("cases", title="Covid19 5-28-2020", breaks = c(1, 50, 100, 200, 500, 1000, 5000, 10000, 15000, 20000)) +
  tm_borders("gray") + tm_text("cases", size=1, col="navy", fontface="bold") +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))


#tmap_mode("view")
#tm_shape(md_geomap_covid, projection = 2163) +  
  #tm_fill("cases", title="Covid19 5-28-2020", breaks = seq(from=1, to = maxcase, by=2000)) +
  #tm_borders("gray") + tm_text("cases", size=1, col="blue", fontface="bold") +
  #tm_view(view.legend.position=c("LEFT", "BOTTOM"))

tm_shape(md_geomap_covid, projection = 2163) +  
  tm_fill("cases", title="Covid19 5-28-2020", breaks = seq(from=1, to = maxcase, by=2000)) +
  tm_borders("gray") + tm_text("cases", size=1, col="blue", fontface="bold") +
  tm_layout(legend.position = c("LEFT", "BOTTOM"))


tmap_mode("view")
tm_shape(md_geomap_covid, projection = 2163) +  
  tm_fill("cases", title="Covid19 5-28-2020", breaks = seq(from=1, to = maxcase, by=2000)) +
  tm_borders("gray") + tm_text("NAME", size=1, col="blue", fontface="bold") +
  tm_view(view.legend.position=c("LEFT", "BOTTOM"))




#########################
#########################
# using Census Bureau map
#########################
library(rgdal)
uscount_cbmap <- readOGR(dsn="mapdata/cb_2019_us_all_5m", layer = "cb_2019_us_state_5m")
tm_shape(uscount_cbmap, projection =2163) + tm_polygons()


# Adding user data for plot. Note that the data just needs to be match the number of category
library(spData)
my_usstates <- us_states[c("GEOID", "NAME")]
visited <- runif(nrow(my_usstates), min=0, max=20)
my_usstates$visited = floor(visited+0.5)
tm_shape(my_usstates, projection = 2163) + tm_polygons("visited")
#tm_shape(my_usstates, projection = 2163) + tm_polygons()



# rgdal package --> download the polygon data from http://www.personal.psu.edu/users/a/c/acr181/election.html
library(GWmodel)
library(rgdal)
US3 <- readOGR(dsn="mapdata/2004_Election", layer = "2004_Election_Counties")
plot(US3)


# usmap package

library(usmap)
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

mydate <- "2020-05-28"
mddata <- subset(Alldata, state=="Maryland" & date ==mydate)
dedata <- subset(Alldata, state=="Delaware" & date ==mydate)
alldata <-subset(Alldata, date ==mydate)


maxcase <- max(mddata$cases)
maxcase <- (ceiling(maxcase/10000))*10000

plot_usmap(include=c("MD"), data= mddata, value="cases", color="black", labels=TRUE) +  
  scale_fill_continuous(low = "white", high = "red", name = paste("MD Covid-19 cases:"), 
                        breaks = seq(from=0, to = maxcase, by=5000)) +
                      theme(legend.position = "right" )

# try to find a state sum
statecase <- tapply(alldata$cases, alldata$state, FUN=sum)
statecase <- as.data.frame(statecase)
statecase$state <- rownames(statecase)
maxcase <- max(statecase$statecase)
maxcase <- (ceiling(maxcase/100000))*100000

plot_usmap("states", data=statecase, value="statecase", color="gray", size=1, labels = FALSE) +  
scale_fill_continuous(low = "white", high = "red", name = paste("Covid19 cases"), 
                        breaks = seq(from=0, to = maxcase, by=100000), label = scales::comma) +  
  theme(legend.position = "right" ) 


plot_usmap(data=statepop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 State Pop", mydate), label = scales::comma) +
  theme(legend.position = "right" )

plot_usmap(data=countypop, value="pop_2015") +
  scale_fill_continuous(low = "white", high = "red", name = paste("2015 County Pop", mydate), label = scales::comma, 
                        trans = "log") + theme(legend.position = "right")


# two different maps with two different colors
mdstate <- plot_usmap(include=c("MD"), data= mddata,  value="cases", color="black", size=1)  
destate <- plot_usmap(include=c("DE"), data= dedata,  value="cases", color="blue", size =1)  

ggplot(projection = 2163) + mdstate$layers[[1]] + destate$layers[[1]] + mdstate$theme +  
  scale_fill_continuous(low = "white", high = "red", name = paste("Covid-19 cases:", mydate), label = scales::comma, 
                        limit = c(0, 20000)) +  theme(legend.position = "right" )
