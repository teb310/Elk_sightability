# Elk Sightability Analysis
# Step 2: Modified Horvitz-Thompson Analysis

# NOTE: WE EXPECT THIS ESTIMATE TO BE LESS ACCURATE AND PRECISE THAN THE BAYESIAN (JAGS) ESTIMATE.
# YOU CAN SKIP THIS SCRIPT IF YOU DON'T WANT THESE ESTIMATES FOR COMPARISON

list.of.packages <-
  c(
    "tidyverse",
    "lubridate",
    "chron",
    "bcdata",
    "bcmaps",
    "sf",
    "rgdal",
    "readxl",
    "Cairo",
    "rjags",
    "coda",
    "OpenStreetMap",
    "ggmap",
    "SightabilityModel",
    "truncnorm",
    "doParallel",
    "nimble",
    "xtable",
    "statip",
    "R2jags"
  )
# Check you have them and load them

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

setwd("C:/Users/TBRUSH/R/SightabilityModels/input")
name <- "2020_to_23"
load(paste0(name, "_mHT_input.rdata"))
load(paste0(name, "_other_inputs.rdata"))

# mHT is run individually by year
# Repeat below for each year you want new estimates from

# Set year
y <- 2022

# Press Source

tau.hats <- matrix(NA, nrow = length(eff$ID[eff$year == y]), ncol = 9)
samp.y <- sampinfo[sampinfo$year == y,]
obs.y <- obs[obs$year == y,]
# 95% intervals
for (i in 1:nrow(samp.y)) {
  tempsamp <- samp.y[i,]
  tempobs <- obs.y[obs.y$stratum == tempsamp$stratum,]
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.05,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[i, 1:5] <- temp$est
  tau.hats[i, 6] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 7] <- as.numeric(gsub(",", "", temp.summary$ucl))
  
  temp <-
    Sight.Est(observed ~ voc, odat = tempobs, sdat = exp, tempsamp,
              alpha = 0.5,
              Vm.boot = TRUE,
              nboot = 10000)
  temp.summary <- summary(temp)
  tau.hats[i, 8] <- as.numeric(gsub(",", "", temp.summary$lcl))
  tau.hats[i, 9] <- as.numeric(gsub(",", "", temp.summary$ucl))
}
colnames(tau.hats) <- c(names(temp$est), "lcl_95", "ucl_95", "lcl_50", "ucl_50")
tau.hats <- round(tau.hats, 0)

results <- as.data.frame(tau.hats) %>%
  bind_cols(samp.y %>% select(stratum)) %>%
  inner_join(eff[eff$year == y,] %>% select(EPU, ID), by=c("year","stratum"="ID")) %>%
  select(EPU, tau.hat, lcl_95:ucl_50, everything()) %>%
  mutate(sd = sqrt(VarTot),
         cv = sd/tau.hat) %>%
  arrange(EPU)

setwd(output)
save(results, file=paste0("mHT_",y,".rdata"))

rm(list=ls())

# Next script: 03_Bayesian_analysis.R
# If you don't want to run the Bayesian analysis, skip to 04_Results.R
