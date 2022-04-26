#######################################################
##########   fire metrics       #######################
########## made by Kevin Yomi   #######################
############## 22.03.2020       #######################
#######################################################

# ... setting -----

outputDir = "Graphs/"
inputDIR = "Data/modis_data/subset_borno.shp"
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

data_crs <- "+proj=tmerc +lat_0=4 +lon_0=12.5 +k=0.99975 +x_0=1110369.7
            +y_0=0 +ellps=clrk80 +units=m +no_defs" 

# .... loading data ----

modis = st_read("Data/modis_data/subset_borno.shp")

# .... setting for fire metrics -----

res = 1000 # for active fire 1km resolution

study_ras <- raster(extent(modis), resolution = res, crs = st_crs(modis)$proj4string)

# ---- visualizing active fire distribution in Borno state ----

# --------- plotting our data -----

tm_shape(modis)+
  tm_dots(col="red",alpha=0.15)+
  tm_shape(border,bbox=border)+
  tm_borders(lwd=2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title="Active fires between 2003 and 2020")

# ----- fire frequency ----

ras_freq = rasterize(modis, study_ras, field = 1, fun = "count") # using our template raster 
d = as.numeric(length(unique(modis$year))) # frequency based on yearly occurrence
ras_freq <- ras_freq/d # divide by number of year

# --------------- Plot ----

tm_shape(ras_freq)+          
  tm_raster(alpha=0.7,
            style="fisher",
            n=10,
            title="",
            palette="YlOrRd")+
  tm_shape(border,bbox=border)+
  tm_borders(lwd=2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title="Fire frequence per km2", legend.outside=TRUE)

# ---- fire intensity

fire_int <- rasterize(modis, study_ras, field = "FRP", fun = mean)

# ------------- plot ----

tm_shape(fire_int)+          
  tm_raster(alpha=0.6,
            style="quantile",
            n=5,
            labels = c("very low","low","moderate", "high", "very high"),
            title="Fire Radiative Power(mw)",
            palette="YlOrRd")+
  tm_shape(border,bbox=border)+
  tm_borders(lwd = 2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title=" Mean Fire Intensity (2003-2020)", legend.outside=TRUE)

# ------ seasonality metrics -----
# ---- setting ----

list_modis<-function(df,column){ # function to create a list divided by the value corresponding to months
  
  list_df<-list()
  
  for (i in unique(as.character(df[[column]]))){
    nam<-paste("modis",i,sep=".")
    output<-assign(nam,df[df[[column]]==i,])
    list_df[[i]]<-output
  }
  return(list_df)
}

stack_layer <- function(lis){ # this function stacks the above-created lists
  r_stack <- stack()
  study_ras <- raster(extent(modis), resolution = res, crs = st_crs(modis)$proj4string)
  for (i in lis){
    output <- rasterize(i,study_ras, field = 1, fun = "count")
    r_stack <- addLayer(r_stack,output)
  }
  
  names(r_stack)<-names(lis)
  return(r_stack)
  
}

# ---- months with the peak of fires occurrence -----
# Add a column with the month name for each detection 

modis = modis %>% 
  mutate(ACQ_DATE = ymd(ACQ_DATE)) %>% 
  mutate_at(vars(ACQ_DATE), funs(year, month)) %>%
  mutate(month_name = month.name[month])

# monthly raster stacks

list_month<-list_modis(modis,"month_name")
stack_month<-stack_layer(list_month)

# convert the stack into data frame and replace NAs by O 

df_month<-as.data.frame(stack_month)
df_month[is.na(df_month)]<-0

# Add a value with the peak month and assign NA value to row with no fires

y<-as.numeric(ncol(df_month))

for (i in 1:nrow(df_month)){
  if (sum(df_month[i,1:y])==0){
    df_month[i,y+1]<-NA
  }else {
    df_month[i,y+1]<-colnames(df_month[which.max(df_month[i,])])
  }
}

# ----------- create the column into factor and assign levels

factor_month<-as.factor(df_month[,13])
factor_month<-factor(factor_month,levels=c("January",'February',"March","April","May",
                                           "June","July","August",
                                           "September","October",
                                           "November","December"))
# Assign these value to a raster 
fire_peak <-setValues(study_ras,factor_month)
# Plot with a personalized scale of colors
co<-c("#9B2226","#370617","#03071e","#F3F8F2","#DCEAEB","#C4DBE4","#94BDD5","#fff1e6","#098DFF","#E9D8A6","#EE9B00","#BB3E03")

tm_shape(fire_peak)+
  tm_raster(alpha=0.7,palette=co,title="")+
  tm_shape(border,bbox=border)+
  tm_borders(lwd=2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title="Peak month for fire count", legend.outside=TRUE)

# ---- Fore season length

#Use the rasterstack to have a dataframe of the count of fire every month in each pixels 

df_month<-as.data.frame(stack_month)

# Replace NA value of  column and do rowsum
df_month[is.na(df_month)]<-0
df_month$total<-rowSums(df_month)

# Create a function returning the number of months since the moment 10% of the AF and 90% of the AF have been detected on a pixel

len<-function(df){
  
  val1=0            # Initializing all the value
  val2=0
  step1=0
  step2=0
  a=df[13]
  beg=0.1*a
  end=0.9*a
  
  
  if (df[13]==0){  #Return NA when there is no fires
    output<-NA
    return (output)
    break
  }
  
  for (i in df){   # Compute start date 
    if
    (val1<beg){
      val1=val1+i
      step1=step1+1
    }  
    else{
      mon_beg<-step1
    }
  }
  
  
  for (i in df){  # Compute end date
    if(val2<end){
      val2=val2+i
      step2=step2+1
    } 
    else{
      mon_end<-step2
    }
  }
  
  
  output<-(step2-step1)+1 # Return final value 
  return(output)
  
  
}

# apply the len function ------------
len_mon<-apply(df_month,1,len)

# assign value to a raster file
fire_season <- setValues(study_ras, len_mon)

# ------------  Plot ---- 
tm_shape(fire_season)+
  tm_raster(alpha=0.7,palette="YlOrRd",
            title="",
            breaks=c(1,2,3,4,6,10,12))+
  tm_shape(border,bbox=border)+
  tm_borders(lwd=2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title=" Fire season length (months)", legend.outside=TRUE)

# --------------------------------------------------------------------------
# ------------ Internannual variability

#Create a raster stack with the different years

list_year<-list_modis(modis,"year")
years<-stack_layer(list_year)

# Convert to dataframe, replace na by 0 and create a column with standard deviation
years<-as.data.frame(years)
years[is.na(years)]<-0
years<-transform(years, sd=apply(years,1, sd, na.rm = TRUE))

# Reset 0 value by NA
years[years==0]<-NA


# Let's select only one column
sd<-years[,"sd"]

# Assign these value to a raster 
fire_var<-setValues(study_ras,sd)


# Plot
tm_shape(fire_var)+
  tm_raster(alpha=0.7,palette="YlOrRd",title="",style="quantile",n=5)+
  tm_shape(border,bbox=border)+
  tm_borders(lwd=2)+
  tm_compass(type = "8star", position = c("left", "top"))+
  tm_scale_bar(breaks=c(0,50,100),text.size=1,position=c("right","bottom"))+
  tm_layout(main.title="Interannual fire variability", legend.outside="TRUE")