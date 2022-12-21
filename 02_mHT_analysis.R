# Elk Sightability Analysis
# Step 2: Modified Horowitz-Thompson Analysis

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

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")
load("mHT_input.Rdata")


# Repeat below for each year you want estimates from

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
  inner_join(eff[eff$year == y,] %>% select(Unit, ID), by=c("stratum"="ID")) %>%
  select(EPU=Unit, tau.hat, lcl_95:ucl_50, everything()) %>%
  mutate(sd = sqrt(VarTot),
         cv = sd/tau.hat) %>%
  arrange(EPU)

setwd("C:/Users/TBRUSH/R/Elk_sightability/out")
save(results, file=paste0("mHT_",y,".RData"))
