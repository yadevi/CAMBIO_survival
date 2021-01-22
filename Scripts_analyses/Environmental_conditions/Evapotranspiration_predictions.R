# Code for calculating extreme events from long-term evapotranspiration data using general additive models (GAM)

# run the code in this section to load in the datasets  

# Metadata #
#___________

#Site = sitename. Sites included in this run are FORBIO Gedinne, IDENT Cloquet and Satakunta. These three datasets have multiple years of survival data
#Actual_evaporation = Actual evaporation per month (mm/day)
#Soil_moisture_root = Root-zone soil moisture content (m³/m³)
#Timestamp = original timestamp of monthly observations from the CRUTS 4.04 dataset
#Year = year of observation
#Month = month of observation
#Day = day of observation. This is always halfway the month because these are monthly averages
#Date = Date in as.Date() format. This makes dates plottable in ggplot
#Time = number of days since 1970-01-01. Implied in the Date format. Needed for modelling time


#Dataset#
#___________


#Evapotranspiration month for 1980-2018 
E_SM_samples<-read.csv("Data/data_products/Evaporation_soilmoisture_sites_all.csv")%>% 
  dplyr::select(-X)%>%#first column X is rownumber, remove this
  mutate(Date = as.Date(Date))%>%#this should be in a date format
  dplyr::rename(Obs_E = Actual_evaporation, Obs_SM = Soil_moisture_root)%>%#rename into Observed Evaporation(Obs_E) and Observed Soim moisture root (Obs_SM)
  as_tibble() 
  
  
  
##### Mean Evaporation  (E) #####

#create a GAM per site and include predictions
E_gam_collection<-E_SM_samples%>%#creates the dataframe of tibbles for Evaporation (E) response variable
  filter(Date >= "1991-01-16")%>% #the same timeseries as for the climatic data
  group_by(Site)%>% #one model per site
  nest()%>% #keep the dataset tucked away
  dplyr::rename(subset_sites = data)%>% #rename the nested datasets variable to avoid confusion
  mutate(gam_cycl = purrr::map(subset_sites, ~ gam(data =.x, Obs_E ~ s(Month, bs = 'cc', k = 12) + s(Time), method= "REML")))%>%# map to each site a gam with cyclical smoother that has 12 knots, one for each month. "Time" gives the long-term trend (default smoother = tin plate regression splines)
  mutate(Pred_E = purrr::map(.x = gam_cycl, ~ predict(., type = "response"))) #predict from the dataset included

#transform GAM collection into  dataset for figure
E_gam_predictions<-E_gam_collection%>%
  unnest(c(subset_sites, Pred_E))%>%#unnest the original dataset and the prediction
  dplyr::select(-gam_cycl)%>%#remove the gam object
  ungroup()%>% #ungroup the sites for now. Can always be re-grouped if needed
  mutate(delta_E = Obs_E - Pred_E) #absolute value of difference between observed and predicted temperature

#GAM diagnostics
E_gam_diagnostics<-E_gam_collection%>%
  mutate(sum_gam = purrr::map(gam_cycl, summary))%>% #full summary output
  mutate(tidied_gam_output = purrr::map(gam_cycl, tidy))%>% #tidy summary
  unnest(tidied_gam_output)%>% #show the diagnostics
  mutate(R_squared = purrr::map_dbl(sum_gam,"r.sq")) #show the R²

#make the figure
E_gam_predictions%>%
  ggplot(data = .,aes(x = Date, y = Obs_E))+ #Observed value of actual evaporation
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = Pred_E, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("Mean Evaporation (mm/day)")+
  ylab("mean E (mm/day)")


##### Mean Soil moisture in root zone  (SM) #####

#create a GAM per site and include predictions
SM_gam_collection<-E_SM_samples%>%#creates the dataframe of tibbles for Evaporation (E) response variable
  filter(Date >= "1991-01-16")%>% #the same timeseries as for the climatic data
  group_by(Site)%>% #one model per site
  nest()%>% #keep the dataset tucked away
  dplyr::rename(subset_sites = data)%>% #rename the nested datasets variable to avoid confusion
  mutate(gam_cycl = purrr::map(subset_sites, ~ gam(data =.x, Obs_SM ~ s(Month, bs = 'cc', k = 12) + s(Time), method= "REML")))%>%# map to each site a gam with cyclical smoother that has 12 knots, one for each month. "Time" gives the long-term trend (default smoother = tin plate regression splines)
  mutate(Pred_SM = purrr::map(.x = gam_cycl, ~ predict(., type = "response"))) #predict from the dataset included

#transform GAM collection into  dataset for figure
SM_gam_predictions<-SM_gam_collection%>%
  unnest(c(subset_sites, Pred_SM))%>%#unnest the original dataset and the prediction
  dplyr::select(-gam_cycl)%>%#remove the gam object
  ungroup()%>% #ungroup the sites for now. Can always be re-grouped if needed
  mutate(delta_SM = Obs_SM - Pred_SM) #absolute value of difference between observed and predicted temperature

#GAM diagnostics
SM_gam_diagnostics<-SM_gam_collection%>%
  mutate(sum_gam = purrr::map(gam_cycl, summary))%>% #full summary output
  mutate(tidied_gam_output = purrr::map(gam_cycl, tidy))%>% #tidy summary
  unnest(tidied_gam_output)%>% #show the diagnostics
  mutate(R_squared = purrr::map_dbl(sum_gam,"r.sq")) #show the R²

#make the figure
SM_gam_predictions%>%
  ggplot(data = .,aes(x = Date, y = Obs_SM))+ #Observed soil moisture content in roo zone
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = Pred_SM, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("Mean Soil Moisture content in root zone (m³/m³)")+
  ylab("mean SM (m³/m³)")



  
