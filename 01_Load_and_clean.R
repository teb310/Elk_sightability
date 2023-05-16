# Elk Sightability Analysis
# Step 1: Load and clean

# 1.1 LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1.2 Create functions ####
# Name fixer function converts misspelled or abbreviated EPU names to standard names
name_fixer <- function(x){
output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrow", x, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Desert", x, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Cheh", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sech", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathko",
    grepl("Hasl", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quat", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lillooet", x, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Van", x, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squam", x, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", x, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", x, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", x, ignore.case = TRUE) ~ "Theo",
    grepl("Mcnab", x, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", x, ignore.case = TRUE) ~ "Bear",
    TRUE ~ x
  )
return(output)
}

# Standard_survey standardizes survey types
standard_survey <- function(x){
output <- case_when(
    grepl("incidental", x, ignore.case = TRUE) ~ "Incidental",
    grepl("telemetry", x, ignore.case = TRUE) ~ "Telemetry",
    grepl("transect", x, ignore.case = TRUE) ~ "Inventory",
    grepl("inventory", x, ignore.case = TRUE) ~ "Inventory",
    grepl("capture", x, ignore.case = TRUE) ~ "Capture",
    TRUE ~ "Other")
return(output)
}

# compile_sheets binds all sheets in a file (filepath) that follow a certain naming patter (type)
# E.g. to bind survey data sheets from all years, type should be "Data"
compile_sheets <- function(filepath,type){
  sheets <- excel_sheets(filepath)
  sheets <- subset(sheets, str_detect(sheets, paste0(type)) ==T)
  
  output <- data.frame(matrix(ncol = 0, nrow = 0))
  
  for(i in 1:length(sheets))
  {
    output <- bind_rows(output, read_excel(filepath, sheet = paste0(sheets[i])))
  }
  return(output)
}

# 1.3 LOAD DATA ####

# create input and output folders if you haven't already
dir.create("C:/Users/TBRUSH/R/SightabilityModels/input")
dir.create("C:/Users/TBRUSH/R/SightabilityModels/output")

# Set your working directory paths and survey data file path

input <- "C:/Users/TBRUSH/R/SightabilityModels/input"
output <- "C:/Users/TBRUSH/R/SightabilityModels/output"

file <- "example_data.xlsx"

setwd(input)

# Extract observations from all years
# If you didn't name your survey data sheets with "Data", replace below
obs.all <- compile_sheets(file, "Data")

# fix EPU names & survey types
obs.all$EPU <- name_fixer(obs.all$EPU)
obs.all$survey.type <- standard_survey(obs.all$survey.type) 


# Bring in surveyed area from each year
surveyed.areas <- compile_sheets(file, "Effort")

# Get total EPU areas and save EPU names
EPU.areas <- read_csv("EPU_areas.csv")
EPU.list <- as.character(EPU.areas$EPU)

# 1.4 SIGHTABILITY DATA ####

# This script assumes that all surveys include sightability trials
# i.e. during survey, surveyors keep track of observed collars
# after survey, surveyors turn on telemetry to track missed collars
# If this is not true, be sure to remove any other surveys below

sight <- obs.all

# filter out incidental observations
sight <- sight %>%
  filter(survey.type == "Telemetry" | survey.type == "Inventory",
# filter out observations with no collars
  collars>0)

# duplicate observations with >1 collars
sight.dup <- as.data.frame(matrix(NA, 0, ncol(sight)))
colnames(sight.dup) <- colnames(sight)

for(i in 1:nrow(sight))
{
  if(sight$collars[i]>1){
    sight.dup <- rbind(sight.dup, rep(sight[i,], sight$collars[i]-1))
    
  }
}
sight <- bind_rows(sight, sight.dup)

# 1.5 EFFORT ####

eff <- surveyed.areas %>%
  group_by(year, EPU) %>%
  summarise(area_surveyed = sum(area_surveyed)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000)

setdiff(eff$EPU, EPU.list) # If nothing is returned, names match

# Amend EPU.list to only include surveyed EPUs, then assign ID numbers
EPU.list <- data.frame(EPU = unique(eff$EPU)) %>%
  mutate(ID = seq(1,length(EPU),1))

eff <- left_join(eff, EPU.list, by="EPU")

sampinfo <- left_join(eff, EPU.areas, by="EPU") %>%
  mutate(stratum = ID, 
         Nh = area, 
         nh = as.integer(if_else(area_surveyed_km > area, area, area_surveyed_km))) %>%
  select(year, stratum, Nh, nh)

# 1.6 OBSERVATIONAL DATASET ####

# make sure we're only keeping data from EPUs with effort data that year
obs <- inner_join(obs.all, eff %>% select(year, EPU, ID), by=c("EPU","year")) %>%
  mutate(
    stratum = ID,
    subunit = EPU,
    total = if_else(is.na(total), 0, total),
    voc = voc*100,
    .keep="unused") %>%
# only keep inventory & capture surveys
  filter(survey.type=="Inventory" | survey.type=="Capture") %>%
  select(-survey.type)


# get rid of NAs in voc
## add mean voc by EPU
avg.voc <- obs %>%
  group_by(subunit) %>%
  summarize(mean_voc = mean(voc, na.rm=T))
avg.voc.overall <- mean(obs$voc, na.rm=T)

obs.voc <- left_join(obs, avg.voc, by="subunit") %>%
  mutate(mean_voc = if_else(is.na(mean_voc), avg.voc.overall, mean_voc),
         voc = if_else(is.na(voc), mean_voc, voc))

obs <- obs.voc %>%
  arrange(year, stratum)

# Make all numeric fields integers
# obs <- obs %>%
#   transmute(year = as.integer(year),
#          stratum = as.integer(stratum),
#          subunit = as.integer(stratum),
#          total = as.integer(total),
#          cows = as.integer(cows),
#          calves = as.integer(calves),
#          spikes = as.integer(spikes),
#          bulls = as.integer(bulls),
#          unclass = as.integer(unclass),
#          voc = as.integer(voc),
#          grpsize = as.integer(grpsize)) %>%
#   arrange(year, stratum)


# make sure totals = sum of cows, calves, etc
obs %>%
  filter(total != (cow+calf+spike+bull+UC)) %>%
  glimpse()

# If records show up, use code below to add unclassified individuals & re-check
# obs <- obs %>%
#   mutate(
#     unclass = if_else(
#       total > (cows+calves+spikes+bulls+unclass), total-(cows+calves+spikes+bulls),
#       unclass),
#     total = (cows+calves+spikes+bulls+unclass))
# obs %>%
#   filter(total != (cows+calves+spikes+bulls+unclass)) %>%
#   glimpse() # all good now

# Before saving, check moose dataset to ensure your data has the same format
# data(exp.m)
# data(obs.m)
# data(sampinfo.m)

# 1.7 mHT DATA ####

# slight tweaks to sight dataframe
exp <- sight %>%
  transmute(
    year = year,
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    voc = as.integer(voc*100),
    grpsize = as.integer(total)
  )

# save it for the next script
save(list = c("eff", "exp", "obs", "sampinfo"), file = "mHT_input.rdata")

# 1.8 BAYESIAN DATA ####
# Tweaks for Bayesian entry
## Main differences between bayesian and mHT datasets:
## 1. VOC is a decimal in Bayesian
## 2. subunit is numbered like stratum in Bayesian

## 1.8.1 SIGHT DAT ####

# s = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (0 if bedded, 1 if standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- sight %>%
  mutate(
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    grpsize = as.integer(total)
  )  %>%
# standardize habitat
  mutate(
    # 1 - rock / other (gravel, landfill, road, slide, other)
    # 2 - meadow / riparian (field, meadow, riparian, wetland, river)
    # 3 - cutblock / powerline (block, powerline, NSR, FTG)
    # 4 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ 4,
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ 3,
      grepl("field|meadow|riparian|wetland|river", habitat, ignore.case = TRUE) ~ 2,
      grepl("gravel|landfill|road|wtp|other|slide", habitat, ignore.case = TRUE) ~ 1
    ),
    # standardize activity
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ 1,
      grepl("bed", activity, ignore.case = TRUE) ~ 0),
    a = as.double(activity),
    s = as.double(habitat),
    t = as.double(grpsize),
    x.tilde = as.double(voc),
    z.tilde = as.double(observed)) %>%
  select(a, s, t, x.tilde, z.tilde)

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### 1.8.1.1 test correlations ####
# sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))
# 
# x.z <- cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method="pearson")
# a.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method="pearson")
# s.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$s)], sight.dat$s[!is.na(sight.dat$s)], method="pearson")
# t.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$t)], sight.dat$t[!is.na(sight.dat$t)], method="pearson")
# 
# Correlation <- as.data.frame(matrix(NA, 4, 3))
# Correlation[1,] <- c("VOC", x.z$estimate, x.z$p.value)
# Correlation[2,] <- c("Activity", a.z$estimate, a.z$p.value)
# Correlation[3,] <- c("Habitat", s.z$estimate, s.z$p.value)
# Correlation[4,] <- c("Group size", t.z$estimate, t.z$p.value)
# colnames(Correlation) <- c("Variable", "Correlation", "p")
# 
# write.csv(Correlation, "C:/Users/TBRUSH/R/Elk_sightability/out/Correlation.csv", row.names = FALSE)

### 1.5.1.2 finish sight.dat ####
# voc is most significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## 1.8.2 OPER DAT ####

# Get year ID
year.ID <- as.data.frame(matrix(NA, length(unique(obs$year)), 2))
colnames(year.ID) <- c("year", "year.ID")

year.ID[,1] <- unique(obs$year) %>% sort()
year.ID[,2] <- seq(1,length(unique(obs$year)))

# join to oper.dat
oper.dat <- left_join(obs, year.ID, by="year")

# get non-augmented data organized
oper.dat <- oper.dat %>%
  transmute(x = round(as.double(voc*.01), 2),
            ym1 = total-1,
            h = as.double(stratum),
            q = 1,
            z = 1,
            yr = year.ID,
            subunits = as.double(stratum)) %>%
  glimpse()

# augmented data
# need to determine max m of each h
aug <- oper.dat %>%
  group_by(yr, h) %>%
  summarize(m = n()) %>%
  ungroup() %>%
  group_by(h) %>%
  reframe(yr = yr,
            m = m,
            m.max = max(m)) %>%
  ungroup() %>%
  mutate(b = 5*m.max,
         aug = b-m)

oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug),] %>%
  mutate(x = NA, ym1 = NA, h = h, q = NA, z = 0, yr = yr, subunits = h, .keep="none") %>%
  ungroup()

oper.dat <- rbind(oper.dat, oper.dat.aug) %>%
  arrange(yr, h, q)

glimpse(oper.dat) # check

## 1.8.3 PLOT DAT ####

plot.dat <- oper.dat %>%
  select(yr, h) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots) %>%
  arrange(yr.plots, h.plots)
glimpse(plot.dat)

## 1.8.4 SCALAR DAT ####

scalar.dat <- as.data.frame(matrix(NA, 1, 3))
colnames(scalar.dat) <- c("R", "Ngroups", "Nsubunits.yr")

scalar.dat <- as.data.frame(matrix(NA, 1, (nrow(plot.dat))))
i <- 1
for(i in 1:nrow(plot.dat)){
  scalar.dat[,i] <- as.double(nrow(oper.dat %>% filter(yr == plot.dat$yr.plots[i], h == plot.dat$h.plots[i])))
  colnames(scalar.dat)[i] <- paste("h", plot.dat$h.plots[i], "y", plot.dat$yr.plots[i], sep = "")
  }

scalar.dat <- scalar.dat %>%
  mutate(R = as.double(nrow(sight.dat)),
         Ngroups = as.double(nrow(oper.dat)),
         Nsubunits.yr = as.double(nrow(plot.dat)))

# Need scalar.sums to ease modelling
scalar.sums <- matrix(NA, nrow(plot.dat), 2)
for (i in 1:nrow(plot.dat)){
  t <- i-1
  scalar.sums[i, 1] <- sum(scalar.dat[,0:t], 1)
  scalar.sums[i, 2] <- sum(scalar.dat[,0:i])
}
  
## 1.8.5 SAVE BAYESIAN DATA ####
save(list = c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums"), file = "jags_input.rdata")

# 1.9 SAVE OTHER USEFUL DATA
save(list=c("compile_sheets","input","output","file","EPU.list","year.ID", "eff"), file="other_inputs.rdata")

rm(list = ls())

