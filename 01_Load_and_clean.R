# Elk Sightability Analysis
# Step 1: Load and clean

setwd("C:/Users/TBRUSH/R/Elk_sightability/input")

# 1.1 LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "rgdal", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1.2 Create functions ####
name_fixer <- function(x){
output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Gray",
    grepl("Narrows", x, ignore.case = TRUE) ~ "Tzoonie-Narrows",
    grepl("Deserted", x, ignore.case = TRUE) ~ "Deserted-Stakawus",
    grepl("Chehalis", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sechelt", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathko",
    grepl("Haslam", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quatum", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lillooet", x, ignore.case = TRUE) ~ "Lower Lillooet",
    grepl("Vancouver", x, ignore.case = TRUE) ~ "Vancouver",
    grepl("Squamish", x, ignore.case = TRUE) ~ "Squamish",
    grepl("Indian", x, ignore.case = TRUE) ~ "Indian",
    grepl("Stave", x, ignore.case = TRUE) ~ "Stave",
    grepl("Theo", x, ignore.case = TRUE) ~ "Theo",
    grepl("Mcnab", x, ignore.case = TRUE) ~ "McNab",
    grepl("Bear", x, ignore.case = TRUE) ~ "Bear",
    TRUE ~ x
  )
return(output)
}

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

# 1.3 LOAD DATA ####
dat.2021 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2021 Survey Data", range = "A1:O136", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "text", "text", "numeric", "text", 
                                     "text"))
dat.2022 <- read_excel("SurveyData_ SPRING_2022.xls", 
                       sheet = "2022 Survey Data", range = "A1:P102", 
                       col_types = c("numeric", "text", "text", 
                                     "numeric", "numeric", "numeric", 
                                     "skip", "numeric", "numeric", "numeric", 
                                     "skip", "text", "text", "numeric", 
                                     "text", "text"))
EPU.areas <- read_csv("Effort/EPU_areas.csv")

EPU.list <- as.character(EPU.areas$Unit)
print(EPU.list)

# 1.4 mHT DATA ####

## 1.4.1 SIGHTABILITY ####

# clean survey types and filter out incidental observations
# 2 options:
## 1. 1 for spotting collared elk, 0 for searching for collar
## 2. 1 for spotting any elk (collared or not), 0 for searching for collar

exp.tmp <- bind_rows(dat.2021, dat.2022) %>%
  mutate(
    subunit = EPU,
    total = `Elk Obs.`,
    survey.type = `Survey type`,
    voc = `% cover`,
    .keep = "unused"
  )
exp.tmp$survey.type <- standard_survey(exp.tmp$survey.type) 
exp.tmp$subunit <- name_fixer(exp.tmp$subunit)

exp.tmp <- exp.tmp %>%
  filter(survey.type == "Telemetry" | survey.type == "Inventory")

# Separate collar observations from non-collar
exp.tmp <- exp.tmp %>%
  mutate(collar =
           case_when(
             grepl("no collar", `Notes:`, ignore.case = TRUE) ~ "No Collar",
             grepl("dart", `Notes:`, ignore.case = TRUE) ~ "No Collar",
             str_detect(`Notes:`, "15\\d.\\d\\d\\d") ~ as.character(
               str_extract_all(`Notes:`, "15\\d.\\d\\d\\d"), sep=", "),
             str_detect(`Notes:`, "\\d\\d\\d\\d\\d") ~ as.character(
               str_extract_all(`Notes:`, "\\d\\d\\d\\d\\d"), sep=", "),
             str_detect(`Notes:`, "2 yellow collar") ~ "Collar, Collar",      
             grepl("yellow collar", `Notes:`, ignore.case = TRUE) ~ "Collar",
             grepl("Telemetry", `survey.type`) ~ "Collar",
             TRUE ~ "No Collar"
           )
  ) %>%
  filter(collar != "No Collar") %>%
  arrange(collar)

# duplicate observations with 2 collars, then clean

exp.tmp <- rbind(exp.tmp, exp.tmp[rep(str_detect(exp.tmp$collar, ","), 1),])

exp <- exp.tmp %>%
  transmute(
    year = as.integer(Year),
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    voc = as.integer(voc*100),
    grpsize = as.integer(total)
  )

## 1.4.2 EFFORT ####

area.2021 <- read_csv("Effort/areas_2021_INV.csv")
eff.2021 <- area.2021 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2021)

area.2022 <- read_csv("Effort/areas_2022_INV.csv")
eff.2022 <- area.2022 %>%
  group_by(Unit) %>%
  summarise(area_surveyed = sum(Shape_Area)) %>%
  mutate(area_surveyed_km = area_surveyed/1000000, year = 2022)

setdiff(bind_rows(eff.2021, eff.2022)$Unit, EPU.list) # Names match

# Check that all EPUs in eff have sightability data in exp
# setdiff(eff.2021$Unit, exp.tmp$subunit[exp.tmp$Year==2021]) # No exp for mcnab ->
# eff.2021 <- eff.2021 %>%
#   filter(Unit != "McNab")
# 
# setdiff(eff.2022$Unit, exp.tmp$subunit[exp.tmp$Year==2022]) # No exp for Brittain, Tzoonie ->
# eff.2022 <- eff.2022 %>%
#   filter(Unit != "Brittain",
#          Unit != "Tzoonie-Narrows")

eff <- bind_rows(eff.2021, eff.2022)

# Amend EPU.list to only include surveyed EPUs
EPU.list <- data.frame(Unit = unique(eff$Unit)) %>%
  mutate(ID = seq(1,length(Unit),1))

eff <- left_join(eff, EPU.list, by="Unit")

sampinfo <- left_join(eff, EPU.areas, by="Unit") %>%
  mutate(stratum = ID, 
         Nh = as.integer(LIW_Cap_sqkm), 
         nh = as.integer(if_else(area_surveyed_km > LIW_Cap_sqkm, LIW_Cap_sqkm, area_surveyed_km)), 
         year = as.integer(year)) %>%
  select(year, stratum, Nh, nh)

## 1.4.3 OBSERVATIONAL DATASET ####

# 2021
obs.2021 <- dat.2021 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  ) %>%
  transmute(
    year = 2021,
    stratum = 0,
    subunit = name_fixer(dat.2021$EPU),
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    voc = `% cover`*100,
    habitat = Habitat,
    activity = Activity,
    notes = `Notes:`,
    survey.type = `Survey type`,
    date = Date
  ) %>%
    filter(subunit %in% eff$Unit[eff$year == 2021])

obs.2021$survey.type <- standard_survey(obs.2021$survey.type)

# 2022
obs.2022 <- dat.2022 %>%
  mutate(
    Cow = if_else(is.na(Cow), 0, Cow),
    Calf = if_else(is.na(Calf), 0, Calf),
    Spike = if_else(is.na(Spike), 0, Spike),
    Bull = if_else(is.na(Bull), 0, Bull),
    Unclass. = if_else(is.na(Unclass.), 0, Unclass.)
  ) %>%
  transmute(
    year = 2022,
    stratum = 0,
    subunit = name_fixer(dat.2022$EPU),
    total = if_else(is.na(`Elk Obs.`), 0, `Elk Obs.`),
    cows = Cow,
    calves = Calf,
    spikes = Spike,
    bulls = Bull,
    unclass = `Unclass.`,
    grpsize = `Elk Obs.`,
    voc = `% cover`*100,
    habitat = Habitat,
    activity = Activity,
    notes = `Notes:`,
    survey.type = `Survey type`,
    date = Date
  ) %>%
filter(subunit %in% eff$Unit[eff$year == 2022])
obs.2022$survey.type <- standard_survey(obs.2022$survey.type)


# bind together & only keep inventory & capture surveys
obs.inv <- bind_rows(obs.2021, obs.2022) %>%
  filter(survey.type=="Inventory" | survey.type=="Capture") %>%
  select(-survey.type)

# stratum field
obs <- inner_join(obs.inv, EPU.list, by=c("subunit"="Unit")) %>%
  mutate(stratum = as.integer(ID), .keep = "unused") %>%
  filter(!is.na(stratum))

# get rid of NAs in voc
## add mean voc by EPU
avg.voc <- obs %>%
  group_by(subunit) %>%
  summarize(mean_voc = mean(voc, na.rm=T))
avg.voc.overall <- mean(obs$voc, na.rm=T)

obs.voc <- left_join(obs, avg.voc, by="subunit") %>%
  mutate(mean_voc = if_else(is.na(mean_voc), avg.voc.overall, mean_voc),
         voc = if_else(is.na(voc), mean_voc, voc))

obs <- obs.voc

# Make all numeric fields integers
obs <- obs %>%
  transmute(year = as.integer(year),
         stratum = as.integer(stratum),
         subunit = as.integer(stratum),
         total = as.integer(total),
         cows = as.integer(cows),
         calves = as.integer(calves),
         spikes = as.integer(spikes),
         bulls = as.integer(bulls),
         unclass = as.integer(unclass),
         voc = as.integer(voc),
         grpsize = as.integer(grpsize)) %>%
  arrange(year, stratum)
obs %>%
# make sure totals = sum of cows, calves, etc
  filter(total != (cows+calves+spikes+bulls+unclass)) %>%
  glimpse()
# If records show up, use code below to add unclassified individuals & re-check
obs <- obs %>%
  mutate(
    unclass = if_else(
      total > (cows+calves+spikes+bulls+unclass), total-(cows+calves+spikes+bulls),
      unclass),
    total = (cows+calves+spikes+bulls+unclass))
obs %>%
  filter(total != (cows+calves+spikes+bulls+unclass)) %>%
  glimpse() # all good now

# Before saving, check moose dataset to ensure your data has the same format
data(exp.m)
data(obs.m)
data(sampinfo.m)

## 1.4.4 SAVE mHT DATA ####
save(list = c("eff", "exp", "obs", "sampinfo"), file = "mHT_input.Rdata")

# 1.5 BAYESIAN DATA ####

## Main differences between bayesian and mHT datasets:
## 1. VOC is a decimal in Bayesian
## 2. subunit is numbered like stratum in Bayesian


## 1.5.1 SIGHT DAT ####

# Sightability survey data:  64 records
# s = habitat indicator ()
# x = visual obstrcution measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (0 if bedded, 1 if standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- exp.tmp %>%
  transmute(
    year = as.integer(Year),
    observed = as.integer(if_else(survey.type=="Inventory", 1, 0)),
    voc = as.integer(voc*100),
    grpsize = as.integer(total),
    activity = Activity,
    habitat = Habitat
  )  %>%
# standardize habitat
  mutate(
    # 1 - rock / other (gravel, landfill, road, WTP, other)
    # 2 - meadow / riparian (field, meadow, riparian, wetland, river, slide)
    # 3 - cutblock / powerline (block, powerline, NSR, FTG)
    # 4 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ 4,
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ 3,
      grepl("field|meadow|riparian|wetland|river|slide|out", habitat, ignore.case = TRUE) ~ 2,
      grepl("gravel|landfill|road|wtp|other", habitat, ignore.case = TRUE) ~ 1
    ),
    # standardize activity
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ 1,
      grepl("bed", activity, ignore.case = TRUE) ~ 0),
    a = as.double(activity),
    s = as.double(habitat),
    t = as.double(grpsize),
    x.tilde = as.double(voc*.01),
    z.tilde = as.double(observed)) %>%
  select(a, s, t, x.tilde, z.tilde)

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### 1.5.1.1 test correlations ####
sight.dat %>% group_by(z.tilde) %>% summarize(mean = mean(x.tilde))

x.z <- cor.test(sight.dat$z.tilde, sight.dat$x.tilde, method="pearson")
a.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$a)], sight.dat$a[!is.na(sight.dat$a)], method="pearson")
s.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$s)], sight.dat$s[!is.na(sight.dat$s)], method="pearson")
t.z <- cor.test(sight.dat$z.tilde[!is.na(sight.dat$t)], sight.dat$t[!is.na(sight.dat$t)], method="pearson")

Correlation <- as.data.frame(matrix(NA, 4, 3))
Correlation[1,] <- c("VOC", x.z$estimate, x.z$p.value)
Correlation[2,] <- c("Activity", a.z$estimate, a.z$p.value)
Correlation[3,] <- c("Habitat", s.z$estimate, s.z$p.value)
Correlation[4,] <- c("Group size", t.z$estimate, t.z$p.value)
colnames(Correlation) <- c("Variable", "Correlation", "p")

write.csv(Correlation, "C:/Users/TBRUSH/R/Elk_sightability/out/Correlation.csv", row.names = FALSE)

### 1.5.1.2 finish sight.dat ####
# voc is most significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde)

## 1.3.4 OPER DAT ####

# Operational survey data:  4380 records
# (includes observed and augmented data for the annual surveys in  2006 and 2007 combined).
# Augmented data records have NA (missing) for x, y, q.
# x = visual obstruction measurements
# ym1 = y-1, where y = observed group size
# h = stratum identifier (all same stratum)
# q = indicator variable that represents whether the group belongs to the study population (equal to 1 for all observed groups and NA for all augmented groups).
# z = detection indicator (equal to 1 if the group was observed during the operational survey and 0 otherwise)
# subunits = unique plot identifier (for all sampled plots).
# yr = year of observation (1 = 2006, 2= 2007)

# non-augmented data

oper.dat <- obs %>%
  transmute(x = round(as.double(voc*.01), 2),
            ym1 = total-1,
            h = as.double(stratum),
            q = 1,
            z = 1,
            yr = if_else(year == 2021, 1, 2),
            subunits = as.double(stratum)) %>%
  glimpse()

# augmented data
# need to determine max m of each h
aug <- oper.dat %>%
  group_by(yr, h) %>%
  summarize(m = n()) %>%
  ungroup() %>%
  group_by(h) %>%
  summarize(yr = yr,
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

## 1.3.5 PLOT DAT ####

# Plot-level information data: 77 records (one for each of the plots sampled in either 2006 or 2007)
# h.plots = stratum to which the plot belonged (1, 2, 3 correspond to low, medium and high density strata)
# yr.plots = year the plot was sampled (1 = 2006, 2= 2007)
plot.dat <- oper.dat %>%
  select(yr, h) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr) %>%
  select(h.plots, yr.plots) %>%
  arrange(yr.plots, h.plots)
glimpse(plot.dat)

## 1.3.6 SCALAR DAT ####

#   Scalars:
#   R = number of sightability trials (64)
# 
#   Ngroups = number of observed and augmented groups for the 2006 and 2007 annual operational surveys (7290)
# 
#   Nsubunits.yr = total number of plots sampled in 2006 and 2007 combined = 81 
# 
#   ny1 = number of groups associated with the annual survey in 2006 (year 1) = 960

scalar.dat <- as.data.frame(matrix(NA, 1, 3))
colnames(scalar.dat) <- c("R", "Ngroups", "Nsubunits.yr")

unique(oper.dat$h[oper.dat$yr==1])
unique(oper.dat$h[oper.dat$yr==2])


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
  
## 1.3.7 SAVE DATA ####
save(list = c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums"), file = "jags_input.Rdata")
rm(list = ls())

