# Elk Sightability Analysis
# Step 4: Plotting and Reporting

# LOAD PACKAGES ####

list.of.packages <- c("bayesplot", "tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# LOAD DATA ####
# Get inputs
setwd("C:/Users/TBRUSH/R/SightabilityModels/input")
load("other_inputs.rdata")

# Get summary data (i.e. standard estimates) from your excel file
standard <- compile_sheets(file, "Summary") %>%
  rename(Standard = estimate)

# Get model outputs
setwd(output)

# jags output already includes all years
load("jags_output.rdata")

# UNCOMMENT BELOW IF YOU WANT TO INCLUDE mHT ESTIMATES
# # for mHT, first get list of mHT outputs in output folder
# mHT_files <- list.files(pattern = "mHT") 
# mHT_files <- subset(mHT_files, str_detect(mHT_files, ".rdata")==T)
# 
# # then load in all mHT output files
# load(mHT_files[1])
# mHT_output <- results %>%
#   rename(mHT = tau.hat,
#          mHT_lcl_50 = lcl_50,
#          mHT_ucl_50 = ucl_50,
#          mHT_lcl_95 = lcl_95,
#          mHT_ucl_95 = ucl_95)
# 
# # run below if length(mHT_files) > 1
# for(i in 2:length(mHT_files))
# {
#   load(mHT_files[i])
#   results <- results %>% rename(mHT = tau.hat,
#                                 mHT_lcl_50 = lcl_50,
#                                 mHT_ucl_50 = ucl_50,
#                                 mHT_lcl_95 = lcl_95,
#                                 mHT_ucl_95 = ucl_95)
#   mHT_output <- bind_rows(mHT_output, results)
#   rm("results")
# }


# CLEAN ####

## Bayesian ####

jags.summary <- as.data.frame(jags_output$BUGSoutput$summary)

tau.jags <- matrix(NA,(nrow(jags.summary)-3),9)
tau.jags <- as.data.frame(tau.jags)
tau.jags[,1] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=h)[:digit:]{1,2}"))
tau.jags[,2] <- as.numeric(str_extract(colnames(scalar.dat)[1:length(jags_output$BUGSoutput$median$tau.hat)], "(?<=y)[:digit:]{1,2}"))
tau.jags[,3] <- round(jags.summary$`50%`[4:nrow(jags.summary)])
tau.jags[,4] <- round(jags.summary$`2.5%`[4:nrow(jags.summary)])
tau.jags[,5] <- round(jags.summary$`97.5%`[4:nrow(jags.summary)])
tau.jags[,6] <- round(jags.summary$`25%`[4:nrow(jags.summary)])
tau.jags[,7] <- round(jags.summary$`75%`[4:nrow(jags.summary)])
tau.jags[,8] <- round(jags.summary$Rhat[4:nrow(jags.summary)], 3)
tau.jags[,9] <- round(jags.summary$sd[4:nrow(jags.summary)]/jags.summary$`50%`[4:nrow(jags.summary)], 3)

rm(jags_output)

colnames(tau.jags) <- c("ID", "year.ID", "JAGS","JAGS_lcl_95", "JAGS_ucl_95", "JAGS_lcl_50", "JAGS_ucl_50", "Rhat", "cv") 
jags_output <- left_join(tau.jags, year.ID, by="year.ID") %>%
  left_join(EPU.list, by="ID") %>%
  select(-year.ID, -ID)

# COMBINE ####

# create a dataframe that combines the important elements of all dataframes
results.all <- left_join(jags_output %>% select(year, EPU, JAGS, JAGS_lcl_95, JAGS_ucl_9, JAGS_lcl_50, JAGS_ucl_50),
                         standard, 
                         by=c("EPU", "year"))

# UNCOMMENT BELOW IF YOU'RE INCLUDING mHT ESTIMATES
# results.all <- left_join(results.all,
#                          mHT_output %>% select(year, EPU, mHT, mHT_lcl_50, mHT_ucl_50, mHT_lcl_95, mHT_ucl_95),
#                          by=c("EPU", "year"))

# uncomment any commented sections below if you're including mHT estimates
results.long <- pivot_longer(results.all,
                             c(JAGS,
                               # mHT,
                               Standard),
                             names_to = "model",
                             values_to = "estimate") %>%
  mutate(lcl_50 =
           # if_else(model=="mHT", mHT_lcl_50,
           if_else(model == "JAGS", JAGS_lcl_50, as.double(NA))) %>%
# ) %>%

  mutate(ucl_50 =
           # if_else(model=="mHT", mHT_ucl_50,
           if_else(model == "JAGS", JAGS_ucl_50, as.double(NA))) %>%
# ) %>%
  mutate(lcl_95 =
           # if_else(model=="mHT", mHT_lcl_95,
           if_else(model == "JAGS", JAGS_lcl_95, as.double(NA))) %>%
  # ) %>%
  
  mutate(ucl_95 =
           # if_else(model=="mHT", mHT_ucl_95,
           if_else(model == "JAGS", JAGS_ucl_95, as.double(NA))) %>%
  # ) %>%
  select(-JAGS_lcl_50,-JAGS_ucl_50, -JAGS_lcl_95,-JAGS_ucl_95)
# %>% select(-mHT_lcl_50, -mHT_ucl_50, -mHT_lcl_95, -mHT_ucl_95)

write.csv(results.all, "Results_wide.csv", row.names = F)
write.csv(results.long, "Results_long.csv", row.names = F)

# FIGURES ####

setwd(output)

## By year ####
# One figure per year, showing all EPUs

results_by_year = ggplot(results.long, aes(x = EPU, y=estimate, fill=model))+
  # add in confidence intervals as dotted lines
  geom_linerange(aes(EPU, ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.7)) +
  # uncomment the line below if you want 50% confidence intervals as well
  # geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.7) ) +
  # add in point estimates
  geom_point(shape=21, size=2.5, position = position_dodge(width = 0.7))+
  # set theme elements
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=11)) +
  # set y axis title
  scale_y_continuous("Population Estimate") +  
  # use greyscale for fill colors
  scale_fill_grey(start = 0, end = 1) +
  # create a new graph for each year
  facet_wrap(vars(year), scales="free")

# view
results_by_year
# save
ggsave("Elk_estimates_yearly.jpeg", width = 20, height = 6, units="in")


## By EPU ####
# One figure per EPU, showing all years

results_by_EPU = ggplot(results.long, aes(x = as.integer(year), y=estimate, fill=model))+
  # add in confidence intervals as dotted lines
  geom_linerange(aes(as.integer(year), ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.3)) +
  # uncomment the line below if you want 50% confidence intervals as well
  # geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.3) ) +
  # Add a horizontal dotted line at the target population value
  geom_hline(aes(yintercept=target), linetype = 2, color = 'red', size=0.5) +
  # Add a trendline for each set of points (colored by method)
  geom_smooth(aes(color=model), method=lm, se=FALSE, size=0.5, position = position_dodge(width=0.3)) +
  # Add point estimates
  geom_point(shape=21, size=2.5, position = position_dodge(width = 0.3))+
  # ADd theme elements
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=11), ) +
  theme(legend.key.size = unit(1, 'in'), ) +
  # name x and y axes and make sure there's only one label per year on x axis
  scale_y_continuous("Population Estimate") +
  scale_x_continuous("Year", breaks=c(unique(results.long$year))) +
  # use greyscale for point fill & color
  scale_fill_grey(start = 0, end = 0.7) +
  scale_color_grey(start = 0, end = 0.7) +
  # separate by EPU
  facet_wrap(vars(EPU), scales="free")

# view (keep in mind scale will be wonky)
results_by_EPU
# save (set size big enough to see everything)
ggsave("Elk_estimates_EPU.jpeg", width = 20, height = 20, units="in")


# EXTRAS ####

## Agreement: mHT vs. Bayesian vs. Standard ####

# UNCOMMENT BELOW IF YOU'RE INCLUDING mHT ESTIMATES
# # mHT vs Bayesian (JAGS)
# 
# agree.mB = agree_test(x = results.all$mHT,
#                       y = results.all$JAGS, 
#                       delta = 1)
# print(agree.mB) # 70%
# agree.mB_plot = plot(agree.mB) +
#   scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
# agree.mB_plot
# 
# 
# # mHT vs Expert
# 
# agree.mS = agree_test(x = results.all$mHT,
#                       y = results.all$Standard, 
#                       delta = 1)
# print(agree.mS) # 52%
# agree.mS_plot = plot(agree.mS) +
#   scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
# agree.mS_plot


# JAGS vs Expert

agree.BS = agree_test(x = results.all$JAGS,
                      y = results.all$Standard, 
                      delta = 1)
print(agree.BS) # 76%
agree.BS_plot = plot(agree.BS) +
  scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
agree.BS_plot

# Agreement Table
Agreement <- as.data.frame(matrix(NA, 3, 2))
Agreement[1,] <- c("mHT vs. Bayesian (JAGS)", agree.mB$ccc.xy[1])
Agreement[2,] <- c("mHT vs. Standard", agree.mS$ccc.xy[1])
Agreement[3,] <- c("Bayesian vs. Standard", agree.BS$ccc.xy[1])

colnames(Agreement) <- c("Test", "CCC")

write.csv(Agreement,"Agreement.csv", row.names = FALSE)

