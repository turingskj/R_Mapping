library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)

library(tmap)    # for static and interactive maps
#library(leaflet) # for interactive maps
library(mapview)
library(ggplot2)

library(shiny)

tmap_mode("plot")

us_states_map <- tm_shape(us_states, projection = 2163) + tm_polygons() + 
  tm_layout(frame = FALSE)
us_states_map


library(usmap)
mdcountypop<-usmap::countypop[which(usmap::countypop$abbr=="MD"),]

plot_usmap("counties", include=c("MD"), data= mdcountypop, value="pop_2015") +  
  scale_fill_continuous(low = "white", high = "red", name = "Population (2015)", label = scales::comma)
