###### extracting the environmental data


##### General: point coordinates of the all sites #####

sites_points<-read.csv("Data/coordinates.csv", header = TRUE)%>%as_tibble()%>%dplyr::select(-Plant_year)#read the APROXIMATED locations of the sites in subset. THIS IS ONLY FOR TRYOUT-CODE. Double-checked inventory of all treedivnet sites needed
coordinates(sites_points)<- ~ Long+Lat #this code will convert the sites_subset into a spatialPoint layer

####### Climatic variables: CRU TS4.04 ######

# CRU TS4.04: Climatic Research Unit (CRU) Time-Series (TS) version 4.04 
# High-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2019)
# Repository URL: http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data
# Creating a user account on the CEDA website is needed to access the data. Data is freely available


# Download data for climatic period 1991 - 2019 for temperate and precipitation
# Has to be done per response variable and per decade

#common folder for all CRUTS 4.04 data. Pick one. Beware: large datafiles, so in my case on storage drive (E:)
dest_path <-"E:/Users/hsblonde/CAMBIO/CAMBIO_gridded_data/CRUTS_4.04" #create a folder in this path. Beware: not on e.g. OneDrive, the sync messes up the .nc reading


#### Monthly Precipitation #####

#### Download and unzip the data (put code block in comments once done)
#________________________________

#Download the files with same names as below. Store the gzip files in the dest_path folder
# Catalogue URL (password protected): http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/pre/
#filenames with "...pre.dat.nc" 
# not needed to re-run the zip files code below. Unzipping manually is also an option. Uncomment to run on first time

  #This code unzips the gzip items in the destination folder. The .nc raster file will be viewable after this.
# gunzip(paste(dest_path, "/cru_ts4.04.1991.2000.pre.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE) #don't remove the zip file
# gunzip(paste(dest_path, "/cru_ts4.04.2001.2010.pre.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)
# gunzip(paste(dest_path, "/cru_ts4.04.2011.2019.pre.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)



##### re-run this code below to get the maps in the R environment
#__________________________________________________________________

#This code unfolds the .nc files into a collections of gridded rasters. Keep the variable "pre" (precipitation). 
#Bricks are multi-layered rasters. Each band represents an average precipitation (mm) value per cell per month (timestamp included)

pre_1991.2000_brick<- brick(paste(dest_path, "/cru_ts4.04.1991.2000.pre.dat.nc", sep  =""), var= "pre")
pre_2001.2010_brick<- brick(paste(dest_path, "/cru_ts4.04.2001.2010.pre.dat.nc", sep  =""), var= "pre")
pre_2011.2019_brick<- brick(paste(dest_path, "/cru_ts4.04.2011.2019.pre.dat.nc", sep  =""), var= "pre")

pre_1991.2019_stack<-stack(pre_1991.2000_brick,pre_2001.2010_brick,pre_2011.2019_brick)#create one big stack that contains the whole precip period


###### Sample the precipitation values (raster) using the point location of the site
#______________________________________________________________________________

precipitation_sampled_all<-raster::extract(pre_1991.2019_stack, sites_points)%>%#order matters for sampling: first RasterStack, then spatialPoints
  as_tibble()%>% #make tibble. "extract" function already created a dataframe (not point layer)
  cbind(sites_points)%>%
  as_tibble()%>%
  dplyr::select(- Remark,-optional, -Lat, -Long)%>%#remove these unnecessary variables
  dplyr::select(Site, everything())%>%#order it correctly
  gather(X1991.01.16:last_col(), key = Timestamp, value = pre)%>% #select the first month (Jan 1991) till the last
  mutate(Timestamp = substring(Timestamp, 2))%>% #remove the first character "X" in the timestamp
  separate(Timestamp, into = c("Year", "Month","Day"), remove = FALSE)%>% #split the timestamp in the constituent part, but keep the original
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day))%>% #into numerics instead of chr
  mutate(Date = chartr(old = ".", new = "-", Timestamp))%>% #substitute "." with "-" to be a Date type
  mutate(Date = as.Date(Date))%>% #convert the "Date" variable in a date format which can be plotted nicely without problems
  mutate(Time = as.integer(Date)) #integer form of date is needed for the GAM modelling

write.csv(precipitation_sampled_all, "Data/data_products/precipitation_sites_all.csv")

##### Monthly temperature ######


#### Download and unzip the data (put code block in comments once done)
#________________________________

#Download the files with same names as below. Store the gzip files in the dest_path folder
# Catalogue URL (password protected): http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/tmp/
#filenames with "...pre.dat.nc" 
# not needed to re-run the zip files code below. Unzipping manually is also an option. Uncomment to run on first time

#This code unzips the gzip items in the destination folder. The .nc raster file will be viewable after this.
# gunzip(paste(dest_path, "/cru_ts4.04.1991.2000.tmp.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE) #don't remove the zip file
# gunzip(paste(dest_path, "/cru_ts4.04.2001.2010.tmp.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)
# gunzip(paste(dest_path, "/cru_ts4.04.2011.2019.tmp.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)


##### re-run this code below to get the maps in the R environment
#__________________________________________________________________

#This code unfolds the .nc files into a collections of gridded rasters. Keep the variable "tmp" (temperature). 
#Bricks are multi-layered rasters. Each band represents an average temperature (°C) value per cell per month (timestamp included)

tmp_1991.2000_brick<- brick(paste(dest_path, "/cru_ts4.04.1991.2000.tmp.dat.nc", sep  =""), var= "tmp")
tmp_2001.2010_brick<- brick(paste(dest_path, "/cru_ts4.04.2001.2010.tmp.dat.nc", sep  =""), var= "tmp")
tmp_2011.2019_brick<- brick(paste(dest_path, "/cru_ts4.04.2011.2019.tmp.dat.nc", sep  =""), var= "tmp")

tmp_1991.2019_stack<-stack(tmp_1991.2000_brick,tmp_2001.2010_brick,tmp_2011.2019_brick)#create one big stack that contains the whole  period

###### Sample the precipitation values (raster) using the point location of the site
#______________________________________________________________________________


temperature_sampled_all<-raster::extract(tmp_1991.2019_stack, sites_points)%>%#order matters for sampling: first RasterStack, then spatialPoints
  as_tibble()%>% #make tibble. "extract" function already created a dataframe (not point layer)
  cbind(sites_points)%>%
  as_tibble()%>%
  dplyr::select(-Remark, - optional, -Lat, -Long)%>%#remove these unnecessary variables
  dplyr::select(Site, everything())%>%#order it correctly
  gather(X1991.01.16:last_col(), key = Timestamp, value = tmp)%>% #select the first month (Jan 1991) till the last
  mutate(Timestamp = substring(Timestamp, 2))%>% #remove the first character "X" in the timestamp
  separate(Timestamp, into = c("Year", "Month","Day"), remove = FALSE)%>% #split the timestamp in the constituent part, but keep the original
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day))%>% #into numerics instead of chr
  mutate(Date = chartr(old = ".", new = "-", Timestamp))%>% #substitute "." with "-" to be a Date type
  mutate(Date = as.Date(Date))%>% #convert the "Date" variable in a date format which can be plotted nicely without problems
  mutate(Time = as.integer(Date)) #integer form of date is needed for the GAM modelling

write.csv(temperature_sampled_all, "Data/data_products/temperature_sites_all.csv")


####### Maximal temperature #####


#### Download and unzip the data (put code block in comments once done)
#________________________________

#Download the files with same names as below. Store the gzip files in the dest_path folder
# Catalogue URL (password protected): http://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.04/data/tmx/
#filenames with "...pre.dat.nc" 
# not needed to re-run the zip files code below. Unzipping manually is also an option. Uncomment to run on first time

#This code unzips the gzip items in the destination folder. The .nc raster file will be viewable after this.
# gunzip(paste(dest_path, "/cru_ts4.04.1991.2000.tmx.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE) #don't remove the zip file
# gunzip(paste(dest_path, "/cru_ts4.04.2001.2010.tmx.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)
# gunzip(paste(dest_path, "/cru_ts4.04.2011.2019.tmx.dat.nc.gz", sep  =""),remove = FALSE, overwrite = TRUE)


##### re-run this code below to get the maps in the R environment
#__________________________________________________________________

#This code unfolds the .nc files into a collections of gridded rasters. Keep the variable "tmx" ( max temperature). 
#Bricks are multi-layered rasters. Each band represents a max temperature (°C) value per cell per month (timestamp included)

tmx_1991.2000_brick<- brick(paste(dest_path, "/cru_ts4.04.1991.2000.tmx.dat.nc", sep  =""), var= "tmx")
tmx_2001.2010_brick<- brick(paste(dest_path, "/cru_ts4.04.2001.2010.tmx.dat.nc", sep  =""), var= "tmx")
tmx_2011.2019_brick<- brick(paste(dest_path, "/cru_ts4.04.2011.2019.tmx.dat.nc", sep  =""), var= "tmx")

tmx_1991.2019_stack<-stack(tmx_1991.2000_brick,tmx_2001.2010_brick,tmx_2011.2019_brick)#create one big stack that contains the whole  period

###### Sample the precipitation values (raster) using the point location of the site
#______________________________________________________________________________


max_temperature_sampled_all<-raster::extract(tmx_1991.2019_stack, sites_points)%>%#order matters for sampling: first RasterStack, then spatialPoints
  as_tibble()%>% #make tibble. "extract" function already created a dataframe (not point layer)
  cbind(sites_points)%>%
  as_tibble()%>%
  dplyr::select(-Remark, - optional, -Lat, -Long)%>%#remove these unnecessary variables
  dplyr::select(Site, everything())%>%#order it correctly
  gather(X1991.01.16:last_col(), key = Timestamp, value = tmx)%>% #select the first month (Jan 1991) till the last
  mutate(Timestamp = substring(Timestamp, 2))%>% #remove the first character "X" in the timestamp
  separate(Timestamp, into = c("Year", "Month","Day"), remove = FALSE)%>% #split the timestamp in the constituent part, but keep the original
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day))%>% #into numerics instead of chr
  mutate(Date = chartr(old = ".", new = "-", Timestamp))%>% #substitute "." with "-" to be a Date type
  mutate(Date = as.Date(Date))%>% #convert the "Date" variable in a date format which can be plotted nicely without problems
  mutate(Time = as.integer(Date)) #integer form of date is needed for the GAM modelling

write.csv(max_temperature_sampled_all, "Data/data_products/max_temperature_sites_all.csv")

