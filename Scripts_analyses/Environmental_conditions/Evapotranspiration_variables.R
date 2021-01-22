#evapotranspiration

#### General: point coordinates of the subsetted sites #####

sites_points<-read.csv("Data/coordinates.csv", header = TRUE)%>%as_tibble()%>%dplyr::select(-Plant_year)#read the APROXIMATED locations of the sites in subset. THIS IS ONLY FOR TRYOUT-CODE. Full coordinate inventory of all treedivnet sites needed
coordinates(sites_points)<- ~ Long+Lat #this code will convert the sites_subset into a spatialPoint layer

####### Evapotranspiration variables: GLEAM V3.3a dataset ######

# GLEAM Global Land Evapotranspiration Amsterdam Model
# High-resolution gridded data of month-by-month variation in evapotranspiration (1980 - 2018)
# Repository URL: https://www.gleam.eu/#downloads
# Applying for a auto-generated user account  is needed to access the data. 
# Data is freely available. A SFTP server connection is needed to transfer the files. I used FileZilla


# Metadata

#E = Evaporative flux (mm day-1) #pick this variable
#Eb = Bare soil evaporation
#Ei = interception loss
#Ep = Potential Evapotranspiration (mm day-1)
#Es = Snow sublimation
#Et = Transpiration
#Ew = Open water evaporation
#S = Evaporative Stress factor
#SM_root = Soil moisture roots (m³/m³) #pick soil moisture in root area
#SMsurf =  Soil moisture surface (m³/m³)


#common folder for all GLEAMdata. Pick one. Beware: large datafiles, so in my case on storage drive (E:)
dest_path_ET <-"E:/Users/hsblonde/CAMBIO/CAMBIO_gridded_data/GLEAM" #create a folder in this path. Beware: not on e.g. OneDrive, the sync messes up the .nc reading




##### re-run this code below to get the maps in the R environment. Beware, this is heavy for cache memory
#__________________________________________________________________

#This code unfolds the .nc files into a collections of gridded rasters.
#Bricks are multi-layered rasters. Each band represents an average value per cell per month (timestamp included)

#evapotranspiration
  #load in the .nc files
E_1980.2018_brick<- brick(paste(dest_path_ET, "/E_1980_2018_GLEAM_v3.3a_MO.nc", sep  =""),crs = " +proj=longlat +datum=WGS84 +no_defs" ) #seems rotated...
E_1980.2018_rotated<-flip(t(E_1980.2018_brick),direction='x')#workaround code to flip the values on the map
names(E_1980.2018_rotated)<-names(E_1980.2018_brick)#give the ET values the original name and not the generic 'layer.1' after inversion with flip and transpose


  #check first map in the .nc list to see if rotation has worked
leaflet() %>% 
  addTiles()%>%
  addRasterImage(E_1980.2018_rotated$X1980.01.16) #map looks normal!


#Soil moisture content in root area
  #load in the .nc files
SMroot_1980.2018_brick<- brick(paste(dest_path_ET, "/SMroot_1980_2018_GLEAM_v3.3a_MO.nc", sep  =""),crs = " +proj=longlat +datum=WGS84 +no_defs")#seems rotated..
SMroot_1980.2018_rotated<-flip(t(SMroot_1980.2018_brick),direction='x')#workaround code to flip the values on the map
names(SMroot_1980.2018_rotated)<-names(SMroot_1980.2018_brick)#give the soil moisture values the original name and not the generic 'layer.1' after inversion with flip and transpose


  #check first map in the .nc list to see if rotation has worked
leaflet() %>% 
  addTiles()%>%
  addRasterImage(SMroot_1980.2018_rotated$X1980.01.16) #map looks normal!

###### Extract the evaporation variables ########

#Sample the site points 
#____________________________

  #extraction of the evapotranspiration variable for the three sites
evapotranspiration_sampled_all<-raster::extract(E_1980.2018_rotated, sites_points)%>%#order matters for sampling: first RasterStack, then spatialPoints
  as_tibble()%>%#make tibble. "extract" function already created a dataframe (not point layer)
  cbind(sites_points)%>%
  as_tibble()%>%
  dplyr::select(-Remark, - optional, -Lat, -Long)%>%#remove these unnecessary variables
  mutate(ET_variable = "Actual_evaporation")%>%
  dplyr::select(Site,ET_variable, everything())


  #extraction of the soil moisture in root zone variable for the three sites
soil_moisture_root_sampled_all<-raster::extract(SMroot_1980.2018_rotated, sites_points)%>%#order matters for sampling: first RasterStack, then spatialPoints
  as_tibble()%>%#make tibble. "extract" function already created a dataframe (not point layer)
  cbind(sites_points)%>%
  as_tibble()%>%
  dplyr::select(-Remark, - optional, -Lat, -Long)%>%#remove these unnecessary variables
  mutate(ET_variable = "Soil_moisture_root")%>%
  dplyr::select(Site,ET_variable, everything())


#Combine the data in a single frame
#_________________________________________

Actual_evaporation_soil_moisture_sampled_all<-evapotranspiration_sampled_all%>%
  rbind(soil_moisture_root_sampled_all)%>%
  as_tibble()%>%
  gather(X1980.01.16:last_col(), key = Timestamp, value = ET_value)%>% #select the first month (Jan 1980) till the last and gather in a generic evapotranspiration value. ET_variable show the difference between the two
  spread(ET_variable, value = ET_value)%>% #spread the ET_variable. This creates two columns: Actual evaporation and soil moisture in root zone
  mutate(Timestamp = substring(Timestamp, 2))%>% #remove the first character "X" in the timestamp
  separate(Timestamp, into = c("Year", "Month","Day"), remove = FALSE)%>% #split the timestamp in the constituent part, but keep the original
  mutate(Year = as.numeric(Year), Month = as.numeric(Month), Day = as.numeric(Day))%>% #into numerics instead of chr
  mutate(Date = chartr(old = ".", new = "-", Timestamp))%>% #substitute "." with "-" to be a Date type
  mutate(Date = as.Date(Date))%>% #convert the "Date" variable in a date format which can be plotted nicely without problems
  mutate(Time = as.integer(Date))%>% #integer form of date is needed for the GAM modelling
  dplyr::select(Site, Actual_evaporation, Soil_moisture_root, everything())
  
write.csv(Actual_evaporation_soil_moisture_sampled_all, "Data/data_products/Evaporation_soilmoisture_sites_all.csv")


