####### Need to run Evapotranspiration_predictions.R and Climate_predictions.R to get the input objects first#####

##### Seperate objects #####

#climate variables
tmp_dat<-tmp_gam_predictions%>% #predictions of mean temperature, object from other Climate_predictions.R
  dplyr::select(Site,Year,Month,Day,Date,Time,Obs_tmp,Pred_tmp)%>%
  dplyr::rename(observed_value = Obs_tmp, predicted_value = Pred_tmp)%>%
  mutate(Variable = "mean_temperature")

tmx_dat<-tmx_gam_predictions%>% #predictions of max temperature, object from other Climate_predictions.R 
  dplyr::select(Site,Year,Month,Day,Date,Time,Obs_tmx,Pred_tmx)%>%
  dplyr::rename(observed_value = Obs_tmx, predicted_value = Pred_tmx)%>%
  mutate(Variable = "max_temperature")
  
  
pre_dat<-pre_gam_predictions%>% #predictions of precipitation, object from other Climate_predictions.R
  dplyr::select(Site,Year,Month,Day,Date,Time,Obs_pre,Pred_pre)%>%
  dplyr::rename(observed_value = Obs_pre, predicted_value = Pred_pre)%>%
  mutate(Variable = "precipitation")



#Evapotranspiration variables
E_dat<-E_gam_predictions%>% #predictions of total evapotransiration, object from Evapotranspiration_predictions.R
  dplyr::select(Site,Year,Month,Day,Date,Time,Obs_E,Pred_E)%>%
  dplyr::rename(observed_value = Obs_E, predicted_value = Pred_E)%>%
  mutate(Variable = "mean_evaporation")

SM_dat<-SM_gam_predictions%>% #predictions of total soil moisture content in root zone, object from Evapotranspiration_predictions.R
 dplyr::select(Site,Year,Month,Day,Date,Time,Obs_SM,Pred_SM)%>%
  dplyr::rename(observed_value = Obs_SM, predicted_value = Pred_SM)%>%
  mutate(Variable = "soil_moisture_rootzone")

#### #combine all data ####


plant_years<-read.csv("Data/coordinates.csv", header = TRUE)%>% #these are the years of planting the trees in each site
  as_tibble()%>% #coordinates has the plant year
  dplyr::select(Site,Plant_year)

df_survival_plot<-read.csv("Data/data_products/df_survival_plot.csv", header = TRUE, row.names = 1)%>%as_tibble()

survey_years<-df_survival_plot%>%
  filter(yearfilter_5y == 1)%>% #keep the fiveyears data
  group_by(exp,year)%>%
  group_keys()%>%
  dplyr::rename(Survey_year = year, Site = exp)
  
environment_predictions_all<-tmp_dat%>%
  rbind(tmx_dat)%>%
  rbind(pre_dat)%>%
  rbind(E_dat)%>%
  rbind(SM_dat)%>% #combine all the above dfs
  merge(plant_years, by = "Site")%>%
  merge(survey_years,by ="Site")%>%
  as_tibble()%>%
  dplyr::select(Site,Variable, Plant_year,everything())%>%
  mutate(delta_value = observed_value - predicted_value)%>%
  mutate(site_window = ifelse(Year >= Plant_year & Year <= (Plant_year + Survey_year),1,0))%>% #filter out everything before plant year up until latest survey year
  mutate(overall_window = ifelse(Year >= min(Plant_year), 1,0)) #filter for the earliest plant year

write.csv(environment_predictions_all, "Data/data_products/environment_predictions_all.csv")


#an exploration figure for now
environment_predictions_all%>% 
  filter(Variable == "soil_moisture_rootzone")%>%
  filter(site_window == 1)%>%
  ggplot(data = .,aes(x = Date, y = observed_value))+ #Observed soil moisture content in roo zone
  geom_line(alpha = 0.3,aes(colour = "observed"))+
  geom_line(aes(x= Date, y = predicted_value, colour = "prediction"))+#predictions from the cyclical GAM
  geom_point(alpha = 0.6)+
  facet_wrap( ~Site, scales = "free")+ #one plot for each site with free scales
  scale_x_date(guide = guide_axis(n.dodge = 2))+ #offset the date labels a bit to avoid overlapping
  theme_bw(base_size = 12)+
  ggtitle("Mean Soil Moisture content in root zone (m³/m³)")+
  ylab("mean SM (m³/m³)")
