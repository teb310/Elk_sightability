# Elk Sightability Analysis
# Step 4: Plotting and Reporting

# LOAD PACKAGES ####

list.of.packages <- c("bayesplot", "tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags", "SimplyAgree")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# LOAD DATA ####
setwd("C:/Users/TBRUSH/R/Elk_sightability/out")

load("jags_output.RData")
load("mHT_output.RData")
standard <- read_excel("C:/Users/TBRUSH/R/Elk_sightability/input/SurveyData_ SPRING_2022.xls", 
                          sheet = "2022 Summary", range = "A2:AH29")

# Expert estimates ####

standard <- standard %>%
  select(EPU=`...1`, y1=`Est., Population April 2021`, y2=`Est., Population April 2022`) %>%
  arrange(EPU)

# mHT ####

## Clean output ####
# Combine dataframes
mHT <- bind_rows(mHT_2021 %>% mutate(year = 1), mHT_2022 %>% mutate(year = 2))

## CV stats ####
mHT.cv <- matrix(NA, 2, 3)
mHT.cv[,1] <- c(2021, 2022)
mHT.cv[,2] <- c(median(mHT_2021$cv), median(mHT_2022$cv))
mHT.cv[,3] <- c(mean(mHT_2021$cv), mean(mHT_2022$cv))

colnames(mHT.cv) <- c("year", "median_cv", "mean_cv")
mHT.cv <- as.data.frame(mHT.cv)


# JAGS ####

load("jags_effort.RData")

## Clean output ####

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

colnames(tau.jags) <- c("ID", "year", "tau.hat","lcl_95", "ucl_95", "lcl_50", "ucl_50", "Rhat", "cv") 
tau.jags <- tau.jags %>%
  mutate(year = if_else(year == 1, 2021, 2022)) %>%
  left_join(eff[,c(1,4:5)], by=c("ID", "year")) %>%
  select(EPU = Unit, year:cv) %>%
  arrange(EPU, year)

tau.jags_y1 <- tau.jags %>% filter(year == 2021) %>% select(-c(year, Rhat))
tau.jags_y2 <- tau.jags %>% filter(year == 2022) %>% select(-c(year, Rhat))

## CV stats ####

jags.cv <- matrix(NA, 2, 3)
jags.cv[,1] <- c(2021, 2022)
jags.cv[,2] <- c(median(tau.jags$cv[tau.jags$year == 2021]), median(tau.jags$cv[tau.jags$year == 2022]))
jags.cv[,3] <- c(mean(tau.jags$cv[tau.jags$year == 2021]), mean(tau.jags$cv[tau.jags$year == 2022]))
colnames(jags.cv) <- c("year", "median_cv", "mean_cv")
jags.cv <- as.data.frame(jags.cv)


# COMBINE RESULTS ####


## 2021 ####

mHT_y1 <- mHT_2021 %>%
  select(-c(VarTot:VarMod)) %>%
  mutate(model = "mHT")
jags_y1 <- tau.jags_y1 %>%
  mutate(model = "Bayesian")
standard_y1<- standard %>%
  filter(EPU %in% jags_y1$EPU) %>%
  select(EPU, tau.hat=y1) %>%
  mutate(model = "Standard")
results_y1 <- bind_rows(mHT_y1, jags_y1, standard_y1) %>%
  mutate(year = 2021)
row.names(results_y1) <- 1:nrow(results_y1)


## 2022 ####

mHT_y2 <- mHT_2022 %>%
  select(-c(VarTot:VarMod)) %>%
  mutate(model = "mHT")
jags_y2 <- tau.jags_y2 %>%
  mutate(model = "Bayesian")
standard_y2<- standard %>%
  filter(EPU %in% jags_y2$EPU) %>%
  select(EPU, tau.hat=y2) %>%
  mutate(model = "Standard")
results_y2 <- bind_rows(mHT_y2, jags_y2, standard_y2) %>%
  mutate(year = 2022)
row.names(results_y2) <- 1:nrow(results_y2)


# 
# ## Repeated EPUs ####
# results_2x <- bind_rows(results_y1 %>% filter(EPU %in% results_y2$EPU), results_y2 %>% filter(EPU %in% results_y1$EPU)) %>%
#   arrange(EPU, model, year)
# 
# results_2x %>%
#   group_by(EPU, model) %>%
#   summarize(pct_change = ((tau.hat[year == 2022]-tau.hat[year == 2021])/tau.hat[year == 2021])*100) %>%
#   # write.csv("Results_2x.csv", row.names = F)
# 
# results_2x_plot = ggplot(results_2x, aes(x = as.factor(year), y=tau.hat, color=model))+
#   geom_point(shape=15, size=3, position = position_dodge(width = 0.7)) +
#   facet_wrap(vars(EPU), ncol = 3) +
#   labs(y = "Population Estimate", size=12) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_linerange(aes(as.factor(year), ymin = lcl, ymax = ucl), position = position_dodge(width = 0.7)) +
#   theme(axis.text.x=element_text(size=12, angle = 90, vjust = .4), axis.title.x = element_blank()) +
#   theme(axis.text.y = element_text(size=12)) +
#   scale_colour_grey(start = 0.2, end = 0.8) +
#   facet_wrap(vars(EPU), scales = "free_y",)
# # For palette choices: 
# #   RColorBrewer::display.brewer.all()
# results_2x_plot
# ggsave("Results_2x_plot.jpeg")



## All results ####
results_all <- bind_rows(results_y1, results_y2) %>%
  write.csv("Results_all.csv", row.names = F)

tau_transposed <- bind_rows(results_y1, results_y2) %>%
  arrange(year, EPU) %>%
  select(EPU, year, model, tau.hat) %>%
  pivot_wider(names_from = model, values_from = tau.hat)

results_final <- bind_rows(results_y1, results_y2) %>%
  arrange(EPU) %>%
  mutate(CI_95 = if_else(is.na(lcl_95), as.character(NA),
                         paste(lcl_95, ucl_95, sep = ", ")),
         CI_50 = if_else(is.na(lcl_50), as.character(NA),
                         paste(lcl_50, ucl_50, sep = ", "))) %>%
  select(EPU, year, model, tau.hat, CI_95, CI_50, cv) %>%
  write.csv("Results_final.csv", row.names = F)

## Agreement: mHT vs. Bayesian vs. Standard ####

# mHT vs Bayesian

agree.mB = agree_test(x = tau_transposed$mHT,
                      y = tau_transposed$Bayesian, 
                      delta = 1)
print(agree.mB) # 70%
agree.mB_plot = plot(agree.mB) +
  scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
agree.mB_plot


# mHT vs Expert

agree.mS = agree_test(x = tau_transposed$mHT,
                      y = tau_transposed$Standard, 
                      delta = 1)
print(agree.mS) # 52%
agree.mS_plot = plot(agree.mS) +
  scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
agree.mS_plot


# JAGS vs Expert

agree.BS = agree_test(x = tau_transposed$Bayesian,
                      y = tau_transposed$Standard, 
                      delta = 1)
print(agree.BS) # 76%
agree.BS_plot = plot(agree.BS) +
  scale_y_continuous(breaks = c(seq(-300, 400, by = 100)), limits = c(-300, 400))
agree.BS_plot

# Agreement Table
Agreement <- as.data.frame(matrix(NA, 3, 2))
Agreement[1,] <- c("mHT vs. Bayesian", agree.mB$ccc.xy[1])
Agreement[2,] <- c("mHT vs. Standard", agree.mS$ccc.xy[1])
Agreement[3,] <- c("Bayesian vs. Standard", agree.BS$ccc.xy[1])

colnames(Agreement) <- c("Test", "CCC")

write.csv(Agreement,"Agreement.csv", row.names = FALSE)


## CV stats ####
cv.all <- bind_rows("mHT" = mHT.cv, "Bayesian" = jags.cv, .id = "Model") %>%
  arrange(year)
write.csv(cv.all, "CV.csv", row.names = F)

# PLOT RESULTS ####

## 2021 ####
results_plot1 = ggplot(results_y1, aes(x = EPU, y=tau.hat, fill=model))+
  geom_linerange(aes(EPU, ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.7)) +
  geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.7) ) +
  geom_point(shape=21, size=2.5, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate") +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=11)) +
  scale_fill_grey(start = 0, end = 1)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot1
ggsave("Results_2021.jpeg", width = 9, height = 5, units="in")

# Bayesian and Standard only
results_BS_plot1 = ggplot(results_y1[results_y1$model != "mHT",], aes(x = EPU, y=tau.hat, fill=model))+
  geom_linerange(aes(EPU, ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.7)) +
  geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.7) ) +
  geom_point(shape=21, size=3, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate") +
  theme(axis.text.x=element_text(size=11, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=10)) +
  scale_fill_grey(start = 0, end = 1)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_BS_plot1
ggsave("Results_BS_2021.jpeg",width = 9, height = 5, units="in")

## 2022 ####
results_plot2 = ggplot(results_y2, aes(x = EPU, y=tau.hat, fill=model))+
  geom_linerange(aes(EPU, ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.7)) +
  geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.7) ) +
  geom_point(shape=21, size=3, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate") +
  theme(axis.text.x=element_text(size=10, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=11)) +
  scale_fill_grey(start = 0, end = 1)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_plot2
ggsave("Results_2022.jpeg", width = 9, height = 5, units="in")

# Bayesian and Standard only
results_BS_plot2 = ggplot(results_y2[results_y2$model != "mHT",], aes(x = EPU, y=tau.hat, fill=model))+
  geom_point(shape=21, size=3, position = position_dodge(width = 0.7))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous("Population Estimate") +
  geom_linerange(aes(EPU, ymin = lcl_95, ymax = ucl_95), linetype = 2, position = position_dodge(width = 0.7)) +
  geom_linerange(aes(EPU, ymin = lcl_50, ymax = ucl_50), position = position_dodge(width = 0.7) ) +
  theme(axis.text.x=element_text(size=11, angle = 90, vjust = .4)) +
  theme(axis.text.y = element_text(size=10)) +
  scale_fill_grey(start = 0, end = 1)
# For palette choices: 
#   RColorBrewer::display.brewer.all()
results_BS_plot2
ggsave("Results_BS_2022.jpeg", width = 9, height = 5, units="in")
  
# # Check Distributions ###
# 
# mcmc_violin(jags_output$BUGSoutput$sims.array,
#             probs = c(0.025, 0.05, 0.5, 0.95, 0.975)) +
#   scale_y_continuous("Population Estimate") +
#   theme(axis.text.y = element_text(size=12))
#   
# mcmc_areas(jags_output$BUGSoutput$sims.array,
#            prob = 0.90,
#            prob_outer = 0.95, area_method = "equal height")
# 
# mcmc_areas(jags_output$BUGSoutput$sims.array,
#            pars = vars(tau.nh2y2, tau.nh4y2, tau.nh5y2, tau.nh6y2, tau.nh12y2, tau.nh13y2, tau.nh14y2, tau.nh15y2, tau.nh18y2, tau.nh20y2),
#            prob = 0.80,
#            prob_outer = 0.95, area_method = "equal height")
