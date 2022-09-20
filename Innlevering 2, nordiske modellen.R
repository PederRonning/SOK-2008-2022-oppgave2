 # HELP SCRIPT FILE  Utfording 2.3 - sok-2008

library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package


union<- read_csv("https://uit-sok-2008-h22.github.io/Assets/union_unempl.csv") #This loads the data with information about the variables of interest

union$country <- gsub("United Kingdom", "UK", union$country)

names(union)[names(union) == "country"] <- "region"
View(union) 

mapdata <- map_data("world")
view(mapdata)

mapdata <- left_join(mapdata, union, by= "region")
view(mapdata)

mapdata1 <- mapdata %>% filter(!is.na(mapdata$unempl))
view(mapdata1)


mapdata1$excesscoverage <- (mapdata1$coverage - mapdata1$density)

#Arbeidsledighet
map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = unempl), col = "black")

map1.1 <- map1 + scale_fill_gradient(name = "Arbeidsledighet", low = "green", high = "red", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map1.1

#Fagforeningsdensitet
map2 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = density), col = "black")

map2.1 <- map2 + scale_fill_gradient(name = "Fagforeningsdensitet", low = "red", high = "green", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map2.1

#coverage - density
#Excess coverage
map3 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = excesscoverage), col = "black")

map3.1 <- map3 + scale_fill_gradient(name = "Excess Coverage", low = "red", high = "green", na.value = "grey50") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map3.1

#Coord
#koordinering av lønnsfastsettelse
map4 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = coord), col = "black") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map4

#hva forventer du deg å se, gitt økonomisk teori om fagforeninger på et perfekt arbiedsmarked?
#er dette det du ser? om ikke, hva kan eventuelt forklare dette?