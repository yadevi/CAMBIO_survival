###### Data wrangling of the original tree div data#####

#read csv file with Simon's data
data_simon<-read.csv("Data/data_original/data_simon.csv", header = TRUE)%>%as_tibble() #data_simon.csv not on github because way to heavy to upload (160 MB). See sharepoint under "Survival_Analysis"

#Make a dummy dataset that holds the survival of trees at the start of experiment. Create that initial dataset here
df_initial<-data_simon%>% #start from full dataset 
  dplyr::select(-X, - year,-survival)%>% #remove dummy integer column "x", re-add year and survival later
  distinct()%>% #keep the original distinct records
  mutate(year = 0,  survival = 1)%>% #default survival state at time of planting (t = 0) was alive (survival = 1)
  dplyr::select(ID, year, survival, everything()) #change order of columns

#Create a complete survival dataset
df_survival<-data_simon%>% #start from all recordings to create a complete survival dataset
  dplyr::select(ID, year, survival, everything(), -X)%>% #junk column X removed, now order is the same as df_initial. There as 167,571 distinct IDs
  full_join(df_initial)#full join with the dummy initial conditions dataframe. df_initial can be removed from environment after done to save memory space


# create data products for further use
### Subset for trying out computation heavy analyses #####

#create a subset of the survival dataset with three sites that have numerous early-stage survival data

df_subset_survival<-df_survival%>%
  filter(exp %in% c("FORBIO_Ged", "IDENT.cloquet", "Satakunta"))


#create a dataframe from the subsettes sites with percentage of survival per species per plot per year

df_subset_survival_plot<-df_subset_survival%>%
  group_by(exp,block, plot,SR, SpComp,year)%>%
  filter(!is.na(survival))%>%#make sure that "NA" is not treated as zero (dead) by filtering them
  dplyr::summarise(sum_alive = sum(survival), n_initial = n())%>%#count the number of alive trees and the number of initial trees
  mutate(proportion_alive = (sum_alive/n_initial))%>%
  as_tibble()
  

write.csv(df_subset_survival_plot,"Data/data_products/df_subset_survival_plot.csv") #### write this subsetted data oiut for further use

#make sure to clean the global environment at the end of the session. It can take a lot of memory space
  

### Proportional data on all sites ####


df_survival_plot<-df_survival%>%
  group_by(exp,block, plot,SR, SpComp,year)%>%
  filter(!is.na(survival))%>%#make sure that "NA" is not treated as zero (dead) by filtering them
  dplyr::summarise(sum_alive = sum(survival), n_initial = n())%>%#count the number of alive trees and the number of initial trees
  mutate(proportion_alive = (sum_alive/n_initial))%>%
  as_tibble()%>%
  group_by(exp,year)%>% #group by year and experiment
  nest()%>% #nest the other data away to easily view what will happen with the following codes
  group_by(exp)%>% #group only by experiment
  mutate(yearfilter_max = ifelse(year == max(year),1,0))%>% #create a yearfilter on the the maximum year in the dataset
  mutate(yearfilter_5y = ifelse( abs(5-year) == min(abs(5-year)),1,0))%>% #create a yearfilter on the year closest to year 5 per experiment
  unnest(cols = c(data))%>%
  ungroup()

write.csv(df_survival_plot,"Data/data_products/df_survival_plot.csv") #write out the data


