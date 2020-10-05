### GEOG 418/518 ###
### LAB 3 - SPATIAL AUTOCORRELATION ###
# install.packages("spdep")
# install.packages("raster")
# install.packages("rgdal")
# install.packages("tmap")
# install.packages("shinyjs")

library(rgdal)
library(tmap)
library(spdep)
library(raster)

#######################
dir <- 
setwd(dir)


VRI <- readOGR(dsn = , layer = )
VRI <- spTransform(VRI, CRS("+init=epsg:")) #Find EPSG for NAD83 UTM Zone 10N


head(VRI@data)

vriCleanCols <- c("FID_VEG_CO", "POLYGON_ID", "PROJ_AGE_1",
                  "SITE_INDEX", "SPECIES__4", "SPECIES__5",
                  "PROJ_HEI_1", "SPECIES_PC", "SPECIES__6",
                  "VRI_LIVE_S", "BASAL_AREA", "WHOLE_STEM",
                  "CROWN_CL_1")

vriClean <- VRI[,vriCleanCols]

#Meta Data (https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/data-management/standards/vegcomp_poly_rank1_data_dictionaryv5_2019.pdf)
# FID = Field ID
# PolyID = VRI Polygon ID
# Stand_Age = Estimated stand age projected to 2020 from estimated establishment date
# Site_Index = A value to estimate site quality. This describes the height that the stand could grow to by age 50 in meters.
# CoDom_Sp = The species code for the co-dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Dom_Sp = The species code for the dominant tree species. Full list of codes: https://www.for.gov.bc.ca/hfp/publications/00026/fs708-14-appendix_d.htm
# Stand_HT = The estimated height for the stand
# DomSP_Perc = The estimated percentage of the dominent species
# CDomSP_Perc = The estimated percentage of the co-dominent species
# Stand_Dens = Estimated density of stand (Stems per hectare)
# Stand_BA = Estimated Basal area of the stand (square meters)
# Stand_StemBio = Estimated stand level stem biomass (tonnes per hectare)
# Stand_CrownCl = The percentage of ground area covered by tree crowns

newNames <- c("FID", "PolyID", "Stand_Age", "Site_Index",
              "CoDom_Sp", "Dom_Sp", "Stand_HT", "DomSP_Perc", 
              "CDomSP_Perc", "Stand_Dens", "Stand_BA", "Stand_StemBio", "Stand_CrownCl")

colnames(vriClean@data) <- newNames
head(vriClean@data)

#Choose a Variable
vriClean <- vriClean[!is.na(vriClean@data$VARIABLE), ]

########################

########################
tmaptools::palette_explorer() #Tool for selecting pallettes

map_StdHT <- tm_shape(vriClean) + 
  tm_polygons(col = "VARIABLE", 
              title = "VARIABLE", 
              style = "jenks", 
              palette = "viridis", n = 6)

map_StdHT
########################


########################
vri.nb <- poly2nb(vriClean)
vri.net <- nb2lines(vri.nb, coords=coordinates(vriClean))
crs(vri.net) <- crs(vriClean)

tm_shape(vriClean) + tm_borders(col='lightgrey') + 
  tm_shape(vri.net) + tm_lines(col='red')


vri.nb2 <- poly2nb(vriClean, queen = FALSE)
vri.net2 <- nb2lines(vri.nb2, coords=coordinates(vriClean))
crs(vri.net2) <- crs(vriClean)

tm_shape(vriClean) + tm_borders(col='lightgrey') + 
  tm_shape(vri.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(vri.net2) + tm_lines(col='yellow', lwd = 2)
########################

vri.lw <- nb2listw(vri.nb, zero.policy = TRUE, style = "W")
print.listw(vri.lw, zero.policy = TRUE)

########################

########################
mi <- moran.test(vriClean$VARIABLE, vri.lw, zero.policy = TRUE)
mi


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(vri.lw)


mI <- mi$estimate[[1]]
eI <- mi$estimate[[2]]
var <- mi$estimate[[3]]

z <-
  
########################  
lisa.test <- localmoran(vriClean$Stand_HT, vri.lw)

vriClean$Ii <- lisa.test[,1]
vriClean$E.Ii<- lisa.test[,2]
vriClean$Var.Ii<- lisa.test[,3]
vriClean$Z.Ii<- lisa.test[,4]
vriClean$P<- lisa.test[,5]
########################
map_LISA <- tm_shape(vriClean) + 
  tm_polygons(col = "Ii", 
              title = "Local Moran's I", 
              style = "jenks", 
              palette = "viridis", n = 6) 


map_LISA
########################
moran.plot(vriClean$Stand_HT, vri.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Some Variable", 
           ylab="Spatially Lagged Variable", quiet=NULL)
########################