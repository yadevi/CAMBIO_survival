#### running the beta regression
library(tidyverse)
library(brms)
library(tidybayes)

#read in all data
df_survival_plot<-read.csv("Data/data_products/df_survival_plot.csv", header = TRUE, row.names = 1)%>%
  as_tibble()%>%
  mutate(surv_sc = ifelse(sum_alive == n_initial, 0.99, proportion_alive))%>% #0 and 1 cannot be entered in beta family
  mutate(surv_sc = ifelse(sum_alive == 0, 0.01, surv_sc))%>%
  mutate(SR_sc = SR - 1) %>% #rescale richness to have intercept at richness = 1 (i.e not ZERO)
  as_tibble() 

#yearfilter is the variable to filter: yearfilter_max is the last survey year in each experiment, yearfilter_5y is the survey year closest to 5y
#This is a binary variable: 0 or 1. To keep the values, filter on 1

survival_max<-df_survival_plot%>%
  filter(yearfilter_max == 1)%>% #keeps the values of the latest survey year
  dplyr::select(exp,SpComp, year,SR_sc,surv_sc)

survival_5y<-df_survival_plot%>%
  filter(yearfilter_5y == 1)%>% #keeps the values of the survey year closest to year 5
  dplyr::select(exp,SpComp, year,SR_sc,surv_sc)


###### running the model on all data ######

results_sites_5y<-survival_5y%>%
  group_by(exp,year)%>%
  nest()%>%#tuck the dataset away
  ungroup()%>%
  mutate(model_sites = purrr::map(data, ~brm(data = .x, bf(surv_sc ~ 1 + SR_sc + (1 |SpComp),
                                         phi ~ 1 + SR_sc),
                                      family = Beta(link = "logit", link_phi = "log"),
                                      iter = 2000, warmup = 1000, chains = 2, cores = 4,
                                      control = list(adapt_delta = 0.99))
                                                     
                                                     )) #create one model per site


#saveRDS(results_sites_5y,"Model_objects/results_sites_5y.RDS") #be carefull: don't commit this file. it is way too big!





results_sites_5y_noSpcomp<-survival_5y%>%
  group_by(exp,year)%>%
  nest()%>%#tuck the dataset away
  ungroup()%>%
  mutate(model_sites = purrr::map(data, ~brm(data = .x, bf(surv_sc ~ 1 + SR_sc,
                                                           phi ~ 1 + SR_sc),
                                             family = Beta(link = "logit", link_phi = "log"),
                                             iter = 2000, warmup = 1000, chains = 2, cores = 4,
                                             control = list(adapt_delta = 0.99))
                                  
  )) #create one model per site


saveRDS(results_sites_5y_noSpcomp,"Model_objects/results_sites_5y_noSpcomp.RDS") #be carefull: don't commit this file, it is way too big!



