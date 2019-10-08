### GEOG 418/518 ###
### LAB 3 - SPATIAL AUTOCORRELATION ###
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("spdep")
# install.packages("GISTools")
# install.packages("raster")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("tmap")
# install.packages("BAMMtools")
# install.packages("shinyjs")

library(plyr)
library(dplyr)
library(spdep)
library(GISTools)
library(raster)
library(maptools)
library(rgdal)
library(tmap)
library(BAMMtools)


#######################
dir <- 
setwd(dir)


tracts <- readOGR(dsn =, layer = "")


census.16 <- read.csv("")



crd.data <- merge(tracts, census.16, by = "GUID")

crd.data <- crd.data[!is.na(crd.data$MInc),]


class(crd.data)
summary(crd.data)
########################

########################
tmaptools::palette_explorer() #Tool for selecting pallettes

map_PopDens <- tm_shape(crd.data) + 
  tm_polygons(col = "MdInc", 
              title = "Median Income", 
              style = "jenks", 
              palette = "viridis", n = 6)

map_PopDens
########################


########################
crd.nb <- poly2nb(crd.data)
crd.net <- nb2lines(crd.nb,coords=coordinates(crd.data))


tm_shape(crd.data) + tm_borders(col='lightgrey') + 
  tm_shape(crd.net) + tm_lines(col='red')


crd.nb2 <- poly2nb(crd.data, queen = FALSE)
crd.net2 <- nb2lines(crd.nb2,coords=coordinates(crd.data))


tm_shape(crd.data) + tm_borders(col='lightgrey') + 
  tm_shape(crd.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(crd.net2) + tm_lines(col='yellow', lwd = 2)
########################

crd.lw <- nb2listw(crd.nb, zero.policy = TRUE, style = "W")
print.listw(crd.lw, zero.policy = TRUE)

########################
crd.data$IncLagMeans = lag.listw(crd.lw, crd.data$MdInc, zero.policy = TRUE)


map_LagMean <- tm_shape(crd.data) + 
  tm_polygons(col = "IncLagMeans", 
              title = "Median Income\nLagged Means", 
              style = "jenks", 
              palette = "viridis", n = 6) 
  

map_LagMean


########################
mi <- moran.test(crd.data$MdInc, crd.lw, zero.policy = TRUE)
mi


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(crd.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <-
  
########################  
  
lisa.test <- localmoran(crd.data$MdInc, crd.lw)

crd.data$Ii <- lisa.test[,1]
crd.data$E.Ii<- lisa.test[,2]
crd.data$Var.Ii<- lisa.test[,3]
crd.data$Z.Ii<- lisa.test[,4]
crd.data$P<- lisa.test[,5]
########################

map_LISA <- tm_shape(crd.data) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I", 
              style = "jenks", 
              palette = "viridis", n = 6) 


map_LISA
########################


moran.plot(crd.data$MdInc, crd.lw, zero.policy=NULL, spChk=NULL, labels=NULL, xlab="Population Density", 
           ylab="Spatially Lagged Population Density", quiet=NULL)
########################