# Elk Sightability Analysis
# Step 3: Bayesian Analysis 

setwd("C:/Users/TBRUSH/R/SightabilityModels/input")
load(paste0(name, "_jags_input.rdata"))
load(paste0(name, "_other_inputs.rdata"))

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# Visualize voc-detection relationship
sight.sum <- sight.dat %>%
  mutate(x_class = cut(x.tilde, breaks = seq(0,1,0.1), include.lowest=TRUE)) %>%
  group_by(x_class) %>%
  summarize(n = n(),
            prob = mean(z.tilde))

ggplot(sight.sum, mapping = aes(x_class, prob)) +
  geom_col()

# 3.1 RUN MODEL ####

# specify initial values
inits <-  function() list(bo=runif(1), bvoc=runif(1))

# Parameters monitored
params <- c("bo", "bvoc", "tau.hat")

# MCMC settings
ni <- 40000 # build to 40000
nt <- 2     # 50% thinning rate (discard every 2nd iteration)
nb <- 20000 # half of ni
nc <- 3

# Bundle data
bundle.dat <- list(x.tilde=sight.dat$x.tilde, z.tilde=sight.dat$z.tilde, #sight.dat
                   x=oper.dat$x, ym1=oper.dat$ym1, h=oper.dat$h, q=oper.dat$q, z=oper.dat$z, yr=oper.dat$yr, subunits=oper.dat$subunits, # oper.dat
                   h.plots=plot.dat$h.plots, yr.plots=plot.dat$yr.plots, # plot_dat
                   R=scalar.dat$R, Ngroups=scalar.dat$Ngroups, Nsubunits.yr=scalar.dat$Nsubunits.yr, scalars=scalar.sums, #scalar.dat
                   years=length(unique(plot.dat$yr.plots)), stratums=length(unique(plot.dat$h.plots)))
# Run model
jags_output <- jags(bundle.dat, inits, params, "beta_binom_model_elk2022.txt", nc, ni, nb, nt)

setwd(output)

save("jags_output", "scalar.dat", file=paste0(name, "_jags_output.rdata"))

rm(list = ls())

