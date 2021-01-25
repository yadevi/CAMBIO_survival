#### running the beta regression
library(tidyverse)
library(brms)
library(tidybayes)


#Reading in data read in all data
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





###### Checking the model output on all data ####


# output from models run in other script
out_sites <- readRDS("Model_objects/results_sites_5y_noSpcomp.RDS")

# checking traceplots===========================
pdf("Figures/Diagnostics/traceplots_noSpComp.pdf")
for(i in 1:21){
  print(rstan::traceplot(out_sites$model_sites[[i]]$fit, pars = c("b_Intercept", "b_phi_Intercept", "b_SR_sc", "b_phi_SR_sc"))+
          ggtitle(out_sites$exp[[i]]))
}
dev.off()



# explore patterns per experiment================================

# plot raw data with predictions as lines
pdf("Figures/Survival/predicted_survival_noSpComp.pdf")
for(i in 1:21){
  newdat <- data.frame(SR_sc = seq(0, max(out_sites$data[[i]]$SR_sc), 1) )
  
  preds <- predict(out_sites$model_sites[[i]], newdata = newdat, re_formula = NA)
  
  plot(surv_sc ~ jitter(SR_sc, factor = 0.05), data = out_sites$data[[i]], 
       ylim = c(0, 1), main  = out_sites$exp[[i]])
  lines(newdat$SR_sc, preds[,1])
  lines(newdat$SR_sc, preds[,3], lty = 2)
  lines(newdat$SR_sc, preds[,4], lty = 2)  
}
dev.off()



# summary of predictions across experiments#================================


# predictions of monocultures versus highest-diversity levels
predictions <- out_sites %>%
  mutate(preds = map2(model_sites, data, ~predict(.x, 
                                                  newdata = data.frame(SR_sc = c(0, max(.y$SR_sc))),
                                                  re_formula = NA)
  )) %>%
  mutate(SR_sc = map(data, function(x) c(0, max(x$SR_sc)))) %>%
  mutate(out_preds = map2(preds, SR_sc, ~as_tibble(cbind(.x, .y)))) %>%
  dplyr::select(exp, out_preds) %>%
  unnest(cols = c(out_preds)) %>%
  dplyr::rename(SR_sc = .y)%>%
  mutate(SR_fc = ifelse(SR_sc == 0, "monoculture", "species mixture"))



# graph of predictions
p1 <- ggplot(predictions, aes(exp, Estimate, col = SR_fc)) + 
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = Q2.5, ymax = Q97.5), width = 0.2, position = position_dodge(width = .5)) +
  scale_y_continuous("Plot-level survival percentage") +
  scale_x_discrete("") +
  scale_color_discrete("Tree richness") +
  coord_flip() +
  theme_bw()
p1
ggsave("Figures/Survival/survival_mono_mix.png")

# meta regression =============
# meta regression model
priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

meta_reg <- brm(bf(Estimate | se(Est.Error) ~ SR_fc + (1 | exp)),
                data = predictions,
                control=list(adapt_delta = 0.99, 
                             max_treedepth=15),
                iter = 8000, warmup = 1000, chains = 2, cores = 2)

plot(meta_reg) #problems with mixing chains, needs to be fixed before looking deeper into this model

