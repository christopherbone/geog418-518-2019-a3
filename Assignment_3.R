### GEOG 418/518 ###
### LAB 3 - SPATIAL AUTOCORRELATION ###
# install.packages("knitr")
# install.packages("rgdal")
# install.packages("tmap")
# install.packages("spdep")
# install.packages("raster")
# install.packages("shinyjs")
# install.packages("e1071")

library(knitr)
library(rgdal)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(e1071)
#######################
dir <- 
setwd(dir)


csv <- read.csv() 


shp <- readOGR(dsn = , layer = , verbose = FALSE) 



cols <- c()


colnames(csv) <- cols


csv$len <- nchar(csv$)


csv_clean <- subset(csv, csv$)


census_DAs <- merge(shp, csv_clean, 
                    by.x = , 
                    by.y = , 
                    all.x = TRUE)


Municp <- subset(census_DAs, census_DAs$CMANAME == )


Income_noNA <- Municp[which(!is.na(Municp$)),]


########################

########################
tmaptools::palette_explorer() #Tool for selecting pallettes

map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "VARIABLE", 
              title = "VARIABLE", 
              style = "jenks", 
              palette = "viridis", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

map_Income
########################


########################
Income.nb <- poly2nb(Income_noNA)
Income.net <- nb2lines(Income.nb, coords=coordinates(Income_noNA))
crs(Income.net) <- crs(Income_noNA)


Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=coordinates(Income_noNA))
crs(Income.net2) <- crs(Income_noNA)

IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='red')

IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)

IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
  tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)


tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)
########################

Income.lw <- nb2listw(Income.nb2, zero.policy = TRUE, style = "W")

########################

########################
miIncome <- moran.test(Income_noNA$, Income.lw, zero.policy = TRUE)


mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]


moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}


range <- moran.range(Income.lw)

z <-
  
########################  
lisa.testIncome <- localmoran(Income_noNA$, Income.lw)


Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]
########################
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Ii",
              title = "Local Moran's I",
              style = "jenks",
              border.alpha = 0.1,
              palette = "viridis", n = 6)
########################
moran.plot(Income_noNA$, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, 
           xlab="Some Variable", 
           ylab="Spatially Lagged Variable", quiet=NULL)
########################