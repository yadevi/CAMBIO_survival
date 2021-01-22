# Code for calculating extreme events from long-term climate data using general additive models (GAM)


###### climate datasets of sites#####
# run the code in this section to load in the datasets  

# Metadata #
#___________

#Site = sitename. Sites included in this run are FORBIO Gedinne, IDENT Cloquet and Satakunta. These three datasets have multiple years of survival data
#Timestamp = original timestamp of monthly observations from the CRUTS 4.04 dataset
#Year = year of observation
#Month = month of observation
#Day = day of observation. This is always halfway the month because these are monthly averages
#Date = Date in as.Date() format. This makes dates plottable in ggplot
#Time = number of days since 1970-01-01. Implied in the Date format. Needed for modelling time

#Datasets#
#___________


 #mean temperature per month for 1991-2019 
tmp_samples<-read.csv("Data/data_products/temperature_sites_all.csv")%>% #tmp (°C) is abbreviation for temperature. See also Climate_variables script
  dplyr::select(-X)%>% #first column X is rownumber, remove this
  mutate(Date = as.Date(Date))%>%#this should be in a date format
  dplyr::rename(Obs_tmp = tmp)%>%#rename temperature (tmp) into Observed temperature (Obs_tmp)
  as_tibble() 

  #max temperature per month for 1991-2019
tmx_samples<-read.csv("Data/data_products/max_temperature_sites_all.csv")%>% #tmx (°C) is abbreviation for max temperature. See also Climate_variables script
  dplyr::select(-X)%>% #first column X is rownumber, remove this
  mutate(Date = as.Date(Date))%>%#this should be in a date format
  dplyr::rename(Obs_tmx = tmx)%>%#rename max temperature (tmx) into Observed max temperature (Obs_tmx)
  as_tibble()

  #mean precipitation per month for 1991-2019
pre_samples<-read.csv("Data/data_products/precipitation_sites_all.csv")%>% #pre (mm) is abbreviation for precipitation See also Climate_variables script
  dplyr::select(-X)%>% #first column X is rownumber, remove this
  mutate(Date = as.Date(Date))%>%#this should be in a date format
  dplyr::rename(Obs_pre = pre)%>%#rename precipitation  (pre) into Observed precipitation (Obs_pre)
  as_tibble()

##### Mean monthly temperature (tmp) #####

  #create a GAM per site and include predictions
tmp_gam_collection<-tmp_samples%>%#creates the dataframe of tibbles for temperature (tmp) response variable
  group_by(Site)%>% #one model per site
  nest()%>% #keep the dataset tucked away
  dplyr::rename(subset_sites = data)%>% #rename the nested datasets variable to avoid confusion
  mutate(gam_cycl = purrr::map(subset_sites, ~ gam(data =.x, Obs_tmp ~ s(Month, bs = 'cc', k = 12) + s(Time), method= "REML")))%>%# map to each site a gam with cyclical smoother that has 12 knots, one for each month. "Time" gives the long-term trend (default smoother = tin plate regression splines)
  mutate(Pred_tmp = purrr::map(.x = gam_cycl, ~ predict(., type = "response"))) #predict from the dataset included

  #transform GAM collection into  dataset for figure
tmp_gam_predictions<-tmp_gam_collection%>%
  unnest(c(subset_sites, Pred_tmp))%>%#unnest the original dataset and the prediction
  dplyr::select(-gam_cycl)%>%#remove the gam object
  ungroup()%>% #ungroup the sites for now. Can always be re-grouped if needed
  mutate(delta_tmp = Obs_tmp - Pred_tmp) #absolute value of difference between observed and predicted temperature

  #GAM diagnostics
tmp_gam_diagnostics<-tmp_gam_collection%>%
  mutate(sum_gam = purrr::map(gam_cycl, summary))%>% #full summary output
  mutate(tidied_gam_output = purrr::map(gam_cycl, tidy))%>% #tidy summary
  unnest(tidied_gam_output)%>% #show the diagnostics
  mutate(R_squared = purrr::map_dbl(sum_gam,"r.sq")) #show the R²

  #make the figure
tmp_gam_predictions%>%
  ggplot(data = .,aes(x = Date, y = Obs_tmp))+ #tmp is mean temperature
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = Pred_tmp, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("Mean monthly Temperature (°C)")+
  ylab("mean temperature (°C)")
  


##### Maximum monthly temperature (tmx) #####

#create a GAM per site and include predictions
tmx_gam_collection<-tmx_samples%>%#creates the dataframe of tibbles for max temperature (tmx) response variable
  group_by(Site)%>% #one model per site
  nest()%>% #keep the dataset tucked away
  dplyr::rename(subset_sites = data)%>% #rename the nested datasets variable to avoid confusion
  mutate(gam_cycl = purrr::map(subset_sites, ~ gam(data =.x, Obs_tmx ~ s(Month, bs = 'cc', k = 12) + s(Time), method= "REML")))%>%# map to each site a gam with cyclical smoother that has 12 knots, one for each month. "Time" gives the long-term trend (default smoother = tin plate regression splines)
  mutate(Pred_tmx = purrr::map(.x = gam_cycl, ~ predict(., type = "response"))) #predict from the dataset included

#GAM diagnostics
tmx_gam_diagnostics<-tmx_gam_collection%>%
  mutate(sum_gam = purrr::map(gam_cycl, summary))%>% #full summary output
  mutate(tidied_gam_output = purrr::map(gam_cycl, tidy))%>% #tidy summary
  unnest(tidied_gam_output)%>% #show the diagnostics
  mutate(R_squared = purrr::map_dbl(sum_gam,"r.sq")) #show the R²

#transform GAM collection into  dataset for figure
tmx_gam_predictions<-tmx_gam_collection%>%
  unnest(c(subset_sites, Pred_tmx))%>%#unnest the original dataset and the prediction
  dplyr::select(-gam_cycl)%>%#remove the gam object
  ungroup()%>% #ungroup the sites for now. Can always be re-grouped if needed
  mutate(delta_tmx = Obs_tmx - Pred_tmx) #absolute value of difference between observed and predicted temperature


#make the figure
tmx_gam_predictions%>%
  ggplot(data = .,aes(x = Date, y = Obs_tmx))+ #tmx is mean temperature
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = Pred_tmx, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("Max monthly Temperature (°C)")+
  ylab("Temperature (°C)")





##### Monthly precipitation (pre). This still needs refinement as not completely seasonal unlike temperature #####

#create a GAM per site and include predictions. Not sure whether precipitation needs to be cyclical...
pre_gam_collection<-pre_samples%>%#creates the dataframe of tibbles for precipitation (pre) response variable
  group_by(Site)%>% #one model per site
  nest()%>% #keep the dataset tucked away
  dplyr::rename(subset_sites = data)%>% #rename the nested datasets variable to avoid confusion
  mutate(gam_cycl = purrr::map(subset_sites, ~ gam(data =.x, Obs_pre ~  s(Month, bs = "cc", k = 12)+s(Time), method= "REML")))%>%# map to each site a gam with cyclical smoother that has 12 knots, one for each month. "Time" gives the long-term trend (default smoother = tin plate regression splines)
  mutate(Pred_pre = purrr::map(.x = gam_cycl, ~ predict(., type = "response"))) #predict from the dataset included

#GAM diagnostics
pre_gam_diagnostics<-pre_gam_collection%>%
  mutate(sum_gam = purrr::map(gam_cycl, summary))%>% #full summary output
  mutate(tidied_gam_output = purrr::map(gam_cycl, tidy))%>% #tidy summary
  unnest(tidied_gam_output)%>% #show the diagnostics
  mutate(R_squared = purrr::map_dbl(sum_gam,"r.sq")) #show the R²

#transform GAM collection into  dataset for figure
pre_gam_predictions<-pre_gam_collection%>%
  unnest(c(subset_sites, Pred_pre))%>%#unnest the original dataset and the prediction
  dplyr::select(-gam_cycl)%>%#remove the gam object
  ungroup()%>% #ungroup the sites for now. Can always be re-grouped if needed
  mutate(delta_pre = Obs_pre - Pred_pre) #absolute value of difference between observed and predicted precipitation


#make the figure
pre_gam_predictions%>%
  ggplot(data = .,aes(x = Date, y = Obs_pre))+ #pre is mean precipitation
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = Pred_pre, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("monthly precipitation (°C)")+
  ggtitle("Max monthly precipitation (°C)")+
  ylab("precipitation (°C)")

