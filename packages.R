#### loading packages #####

library(tidyverse) #tidy data handling
library(broom) #tidy model outputs
library(broom.mixed) #tidy mixed and (bayesian) model outputs
library(lme4) #linear mixed-effects models
library(R.utils) #unzip rasters
library(sp) #spatial analysis
library(sf) #shapefiles
library(ncdf4) #reading netCDF rasters
library(RNetCDF) #.nc files
library(raster) #general raster tools
library(rasterVis) #visualising rasters
library(leaflet) #plotting maps, will remove previous redunant libraries later
library(mgcv) #gam
library(rstan) #to check whether there are any system issues when running brms
library(brms) #bayesian statistics with STAN running in the background
library(tidybayes)#tidy the brms output
library(bayesplot)#easy bayesian diagnostic plots
library(viridis) #nice colourschemes
library(ggrepel)
