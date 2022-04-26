#######################################################
##########   fire metrics pre-processing  #############
##########  made by Kevin Yomi  #######################
##########      22.03.2020      #######################
#######################################################

# .... setting -----

outputDir = "Graphs/"
inputDIR = "Data/"
data_crs = data_crs

# .... loading packages ----

pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("lubridate","rgdal","raster","sf","tmap","dplyr", "ggplot2")
for (package in neededPackages){pkgTest(package)}

library(sf)
library(raster)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tmap)
library()
# ... data projection 

data_crs <- "+proj=tmerc +lat_0=4 +lon_0=12.5 +k=0.99975 +x_0=1110369.7
            +y_0=0 +ellps=clrk80 +units=m +no_defs" 

# ... load data

border <- st_read("../Data/shapefile_lga/borno_lga.shp")

modis <- st_read("...Data/modis_data/subset_borno.shp")%>%
  st_transform(., data_crs)

# ... removing outliers

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(0.05, 0.95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
# ... sub-setting the data (2003) is the most recommended year to start the analysis with ----
modis %>% 
  subset (modis, CONFIDENCE > 30)%>%
  mutate(Date = as.Date(ACQ_DATE, format = "%Y/%m/%d"))%>%
  filter(acq_date >"2002-12-31")

# .... add temporality to the date field ----

modis$year <- year(modis$ACQ_DATE)
modis$month <- month(modis$ACQ_DATE)
modis$day  <- day(modis$ACQ_DATE)

# ... applying our function to the data ----

modis %>%
  mutate(outlier = remove_outliers(modis$FRP),
         na_check = is.na(outlier))

# casting multipoint into point feature ---- 

modis <- st_cast(modis, 'POINT')
