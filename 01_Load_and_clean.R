# Elk Sightability Analysis
# Step 1: Load and clean

# 1.1 LOAD PACKAGES ####

list.of.packages <- c("tidyverse", "lubridate","chron","bcdata", "bcmaps","sf", "readxl", "Cairo", "rjags","coda","OpenStreetMap", "ggmap", "SightabilityModel","truncnorm", "doParallel", "nimble", "xtable", "statip", "R2jags")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# 1.2 CREATE FUNCTIONS ####
# Name fixer function converts misspelled or abbreviated EPU names to standard names
name_fixer <- function(x){
output <- case_when(
    grepl("Rainy", x, ignore.case = TRUE) ~ "Rainy-Grey",
    grepl("Narrow", x, ignore.case = TRUE) ~ "Narrows",
    grepl("Desert", x, ignore.case = TRUE) ~ "Deserted",
    grepl("Cheh", x, ignore.case = TRUE) ~ "Chehalis",
    grepl("Sech", x, ignore.case = TRUE) ~ "Sechelt Peninsula",
    grepl("Homa", x, ignore.case = TRUE) ~ "Homathco",
    grepl("Lois", x, ignore.case = TRUE) ~ "Lois",
    grepl("Eldr", x, ignore.case = TRUE) ~ "Eldred",
    grepl("Hasl", x, ignore.case = TRUE) ~ "Haslam",
    grepl("Dani", x, ignore.case = TRUE) ~ "Powell-Daniels",
    grepl("Quat", x, ignore.case = TRUE) ~ "Quatam",
    grepl("Lill", x, ignore.case = TRUE) ~ "Lower Lillooet",
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
# E.g. to bind survey data sheets from all years (sheets with "Data" in name), type should be "Data"
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

file <- "2023_data.xlsx"

# give a name to this dataset that will be used as a prefix to your input and output files
# I suggest naming by year
name <- "2020_to_23"

setwd(input)

# Extract observations from all years
# If you didn't name your survey data sheets with "Data", replace below
obs.all <- compile_sheets(file, "\\d{4} Data")

# fix EPU names & survey types
obs.all$EPU <- name_fixer(obs.all$EPU)
obs.all$survey.type <- standard_survey(obs.all$survey.type) 

# Bring in surveyed area from each year
# If running mHT model, you need an effort sheet for each year:
surveyed.areas <- compile_sheets(file, "\\d{4} Effort")
# If only running Bayesian model, you can just use the summary sheet:
# surveyed.areas <- compile_sheets(file, "\\d{4} Summary")
  # filter(!is.na(estimate))

surveyed.areas$EPU <- name_fixer(surveyed.areas$EPU)

# Get total EPU areas and save EPU names
EPU.areas <- read_csv("EPU_areas.csv")
EPU.list <- name_fixer(EPU.areas$EPU)

# 1.4 SIGHTABILITY DATA ####

sight <- obs.all %>%
  group_by(year) %>%
  # we'll create a field to tell us whether each year counts toward sightability data
  mutate(sight = if_else(all(
    c("Inventory", "Telemetry") %in% unique(survey.type)
  ), TRUE, FALSE)) %>%
  # only keep telemetry and inventory observations
  filter(survey.type == "Telemetry" | survey.type == "Inventory",
         # only keep years where sightability trials were done
         sight == TRUE,
         # filter out observations with no collars
         collars > 0,
         # remove any rows that didn't pass the name check
         EPU != "")

# duplicate observations with >1 collars
sight.dup <- head(sight, 0)

for (i in seq_along(sight$collars)) {
  if (sight$collars[i] > 1) {
    for (k in 2:sight$collars[i]) {
      sight.dup <- rbind(sight.dup, sight[i, ])
    }
  }
}

if (nrow(sight.dup) > 0) {
  sight <- bind_rows(sight, sight.dup)
}

# 1.5 EFFORT ####

eff <- surveyed.areas %>%
  group_by(year, EPU) %>%
  summarise(area_surveyed_m = sum(area_surveyed_m)) %>%
  mutate(area_surveyed_km = area_surveyed_m/1000000)

# Amend EPU.list to only include surveyed EPUs, then assign ID numbers
EPU.list <- data.frame(EPU = unique(eff$EPU)) %>%
  filter(EPU %in% EPU.areas$EPU) %>%
  mutate(ID = seq(1,length(EPU),1))

eff <- inner_join(eff, EPU.list, by="EPU")

sampinfo <- inner_join(eff, EPU.areas, by="EPU") %>%
  mutate(stratum = ID,
         Nh = area,
         nh = as.integer(if_else(area_surveyed_km > area, area, area_surveyed_km))) %>%
  select(year, stratum, Nh, nh)

# 1.6 OBSERVATIONAL DATASET ####

# make sure we're only keeping data from EPUs with effort data that year
obs <- inner_join(obs.all, eff %>% select(year, EPU, ID), by=c("EPU","year")) %>%
  mutate(
    subunit = ID,
    stratum = ID,
    # make sure there are no NAs in observation counts
    mutate(across(
      c(cow, calf, spike, bull, UC, total), ~ if_else(is.na(.), 0, .)
    )),
    # voc in percentage for mHT
    voc = voc*100,
    .keep="unused") %>%
  # remove incidental observations
  filter(survey.type == "Inventory" |
           survey.type == "Capture" | survey.type == "Telemetry") %>%
  # then turn any "capture" to "inventory"
  mutate(survey.type = if_else(survey.type == "Capture", "Inventory", survey.type))

# make sure all totals = sum of cows, calves, etc

obs <- obs %>%
  mutate(
    UC = if_else(
      total > (cow + calf + spike + bull + UC),
      as.numeric(total - (cow + calf + spike + bull)),
      as.numeric(UC)
    ),
    total = as.numeric(cow + calf + spike + bull + UC)
  )

## 1.6.1 Observed dataset ####

observed <- obs %>%
  group_by(year, EPU) %>%
  summarize(cows_observed = sum(cow)-sum(spike),
            bulls_observed = sum(bull),
            yearlings_observed = sum(spike)*2,
            calves_observed = sum(calf),
            unclassified_observed = sum(UC),
            total_observed = sum(total))

## 1.6.2 Telem adjustments ####
#including telemetry obs helps some EPUs but hurts others -> depends on effect on average group size

group <- obs %>%
  group_by(subunit, stratum) %>%
  summarize(avg_group = mean(total))

# if keeping telems brings avg groupsize closer to EPU's total average, then keep them
telem.stats <- obs %>%
  ungroup() %>%
  group_by(year, subunit, stratum, survey.type) %>%
  summarize(n = n(),
            count = sum(total)) %>%
  ungroup() %>%
  pivot_wider(names_from = survey.type,
              values_from = c(n, count)) %>%
  mutate(
    n_telem = replace_na(n_Telemetry, 0),
    n_nontelem = replace_na(n_Inventory, 0),
    count_telem = replace_na(count_Telemetry, 0),
    count_nontelem = replace_na(count_Inventory, 0),
    .keep = "unused"
  ) %>%
  filter(count_telem > 0) %>%
  mutate(
    avg_group_nontelem = (count_nontelem / n_nontelem),
    avg_group_telem = (count_telem + count_nontelem) / (n_telem + n_nontelem)
  ) %>%
  left_join(group, by = c("subunit", "stratum")) %>%
  mutate(keep_telem = if_else(
    abs(avg_group_telem - avg_group) >= abs(avg_group_nontelem - avg_group),
    F,
    T
  ))

# add telem direction to obs
obs <-
  left_join(
    obs,
    telem.stats %>% select(year, subunit, stratum, keep_telem),
    by = c("year", "subunit", "stratum")
  ) %>%
  filter(!(survey.type == "Telemetry" & keep_telem == F))

## 1.6.3 Final adjustments ####
obs <- obs %>%
  mutate(grpsize = total) %>%
  select(year, stratum, subunit, total, cows = cow, calves = calf, bulls = bull, unclass = UC, voc, grpsize)

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
save(list = c("eff", "exp", "obs", "sampinfo"), file = paste0(name, "_mHT_input.rdata"))

# 1.8 BAYESIAN DATA ####
# Tweaks for Bayesian entry
## Main differences between bayesian and mHT datasets:
## 1. VOC is a decimal in Bayesian
## 2. subunit is numbered like stratum in Bayesian

## 1.8.1 Sight.dat ####

# s = habitat indicator (Mature foest, young forest, non-forest)
# x = visual obstruction measurements associated with the test trial data used to develop the sightability model
# a = activity indicator (bedded, standing/moving)
# z = detection indicator (1 if the group was observed, 0 otherwise)
# t = group size

sight.dat <- sight %>%
  mutate(observed = as.integer(if_else(survey.type == "Inventory", 1, 0)), grpsize = as.integer(total))  %>%
  # standardize habitat
  mutate(
    # HABITAT KEY WORDS
    # 1 - rock / meadow / riparian (field, meadow, riparian, wetland, river)
    # 2 - cutblock / powerline (block, powerline, NSR, FTG)
    # 3 - mature forest (mature, old)
    habitat = case_when(
      grepl("mature|old|conifer", habitat, ignore.case = TRUE) ~ "Mature Forest",
      grepl("block|powerline|nsr|ftg", habitat, ignore.case = TRUE) ~ "Young Forest",
      grepl(
        "field|meadow|riparian|wetland|river|gravel|landfill|road|wtp|other|slide",
        habitat,
        ignore.case = TRUE
      ) ~ "No Forest",
    ),
    # standardize activity
    activity = case_when(
      grepl("standing|moving|run", activity, ignore.case = TRUE) ~ "Standing/Moving",
      grepl("bed", activity, ignore.case = TRUE) ~ "Bedded"
    ),
    a = as.factor(activity),
    s = factor(habitat, levels = c(
      "No Forest", "Young Forest", "Mature Forest"
    )),
    t = as.double(grpsize),
    x.tilde = as.double(voc),
    z.tilde = as.double(observed)
  ) %>%
  select(a, s, t, x.tilde, z.tilde)

glimpse(sight.dat) # check - looks the same as Fieberg's sight_dat csv

### 1.8.1.1 test correlations ####
# uncomment this section if you want to test and/or plot the correlation of group size, habitat, activity, voc with sightability
# cor.sum <- sight.dat %>%
#   group_by(z.tilde) %>%
#   summarize(median.x = median(x.tilde, na.rm=T), mean.x = mean(x.tilde, na.rm=T), sd.x = sd(x.tilde, na.rm=T), se.x = sd(x.tilde, na.rm=T)/sqrt(length(x.tilde[!is.na(x.tilde)])),
#             median.t = median(t, na.rm=T), mean.t = mean(t, na.rm=T), sd.t = sd(t, na.rm=T), se.t = sd(t, na.rm = T)/sqrt(length(t[!is.na(t)])))
# 
# # VOC plot
# ggplot(sight.dat %>% filter(!is.na(x.tilde)) %>% rename ("Visual Obstruction" = x.tilde) %>% mutate(z.tilde = if_else(z.tilde==1, "Seen", "Missed")), aes(as.factor(z.tilde), `Visual Obstruction`)) +
#   geom_boxplot(aes(fill = z.tilde)) +
#   stat_summary(fun = "mean", geom = "point", shape = 8, size = 3)+
#   ylim(c(0,1)) +
#   scale_fill_brewer(palette = "Paired") +
#   xlab("") +
#   theme_classic() +
#   theme(legend.position="none",
#         axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 14, color="black"))
# 
# # Group size plot
# ggplot(sight.dat %>% filter(!is.na(t)) %>% rename ("Group size" = t) %>% mutate(z.tilde = if_else(z.tilde==1, "Seen", "Missed")), aes(as.factor(z.tilde), `Group size`)) +
#   geom_boxplot(aes(fill = z.tilde)) +
#   stat_summary(fun = "mean", geom = "point", shape = 8, size = 3)+
#   ylim(c(0,50)) +
#   xlab("") +
#   scale_fill_brewer(palette = "Paired") +
#   theme_classic() +
#   theme(legend.position="none",
#         axis.title.y = element_text(size = 14, margin = margin(0,10,0,0)),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 14, color="black"))
# 
# # Habitat plot
# 
# sight.prop <- sight.dat %>%
#   filter(!is.na(s)) %>%
#   group_by(z.tilde) %>%
#   summarize(n.z = n())
# sight.prop.s <- sight.dat %>%
#   filter(!is.na(s)) %>%
#   group_by(z.tilde, s) %>%
#   summarize(n.s = n()) %>%
#   inner_join(sight.prop, by="z.tilde") %>%
#   mutate(prop = n.s/n.z,
#          z.tilde = if_else(z.tilde == 1, "Seen", "Missed")) %>%
#   rename("Habitat" = s)
# 
# ggplot(sight.prop.s, mapping = aes(as.factor(z.tilde), prop)) +
#   geom_col(aes(fill = Habitat),
#            position = "dodge",
#            color = "grey30") +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "", y = "Percentage of Observations") +
#   scale_fill_brewer(palette = "Greens") +
#   theme_classic() +
#   theme(
#     axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 14, color = "black"),
#     legend.text = element_text(size = 12, color = "grey30"),
#     legend.title = element_text(size = 14)
#   )
# 
# # Activity plot
# 
# sight.prop <- sight.dat %>%
#   filter(!is.na(a)) %>%
#   group_by(z.tilde) %>%
#   summarize(n.z = n())
# sight.prop.a <- sight.dat %>%
#   filter(!is.na(a)) %>%
#   group_by(z.tilde, a) %>%
#   summarize(n.a = n()) %>%
#   inner_join(sight.prop, by="z.tilde") %>%
#   mutate(prop = n.a/n.z,
#          z.tilde = if_else(z.tilde == 1, "Seen", "Missed")) %>%
#   rename("Activity" = a)
# 
# ggplot(sight.prop.a, mapping = aes(as.factor(z.tilde), prop)) +
#   geom_col(aes(fill=Activity), color = "grey30", position = "dodge") +
#   scale_y_continuous(labels = scales::percent) +
#   labs(x = "", y = "Percentage of Observations") +
#   theme_classic() +
#   scale_fill_brewer(palette = "RdYlBu", direction = -1) +
#   theme(axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0)),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 14, color = "black"),
#         legend.text = element_text(size = 12, color = "grey30"),
#         legend.title = element_text(size = 14))
# 
# x.z <- t.test(sight.dat$x.tilde[sight.dat$z.tilde==0], sight.dat$x.tilde[sight.dat$z.tilde==1])
# t.z <- t.test(sight.dat$t[sight.dat$z.tilde==0 & !is.na(sight.dat$t)], sight.dat$t[sight.dat$z.tilde==1 & !is.na(sight.dat$t)])
# a.z <- chisq.test(table(sight.dat$a, sight.dat$z.tilde))
# s.z <- chisq.test(table(sight.dat$s, sight.dat$z.tilde))
# 
# Correlation <- as.data.frame(matrix(NA, 4, 5))
# Correlation[1,] <- c("VOC", x.z$method, x.z$statistic, x.z$parameter, x.z$p.value)
# Correlation[2,] <- c("Group size", t.z$method, t.z$statistic, t.z$parameter, t.z$p.value)
# Correlation[3,] <- c("Activity", a.z$method, a.z$statistic, a.z$parameter, a.z$p.value)
# Correlation[4,] <- c("Habitat", s.z$method, s.z$statistic, s.z$parameter, s.z$p.value)
# colnames(Correlation) <- c("Variable", "Method", "Statistic", "DF", "p")
# 
# write.csv(Correlation, paste0("C:/Users/TBRUSH/R/SightabilityModels/output/", name, "_Correlation.csv"), row.names = FALSE)
# write.csv(sight.dat, paste0("C:/Users/TBRUSH/R/SightabilityModels/output/", name, "_Sightability.csv"), row.names = FALSE)

### 1.5.1.2 finish sight.dat ####
# voc is most significantly correlated with sightability -> select only voc
sight.dat <- sight.dat %>% select(x.tilde, z.tilde) %>%
  filter(!is.na(x.tilde)) %>%
  ungroup()

## 1.8.2 Oper.dat ####
### 1.8.2.1 non-augmented data ####

# Get year ID
year.ID <- as.data.frame(matrix(NA, length(unique(obs$year)), 2))
colnames(year.ID) <- c("year", "year.ID")

year.ID[,1] <- unique(obs$year) %>% sort()
year.ID[,2] <- seq(1,length(unique(obs$year)))

# join to oper.dat
oper.dat <- left_join(obs, year.ID, by="year")

# organize non-augmented data
oper.dat <- oper.dat %>%
  transmute(x = round(as.double(voc*.01), 2),
            ym1 = total-1,
            h = as.double(stratum),
            q = 1,
            z = 1,
            yr = year.ID,
            subunits = as.double(stratum),
            survey.type = NULL) %>%
  # model won't accept x = 0 or 1, fix below
  mutate(x = if_else(x==1, 0.999,
                     if_else(x==0, 0.001, x))) %>%
  glimpse()

# augmented data
# need to determine max annual m of each h
aug <- oper.dat %>%
  group_by(yr, h, subunits) %>%
  summarize(m = n()) %>%
  ungroup() %>%
  group_by(h) %>%
  reframe(yr = yr,
          subunits = subunits,
            m = m,
            m.max = max(m)) %>%
  ungroup() %>%
  mutate(b = 2*m.max) %>%
  # b needs to be greater than the max number of groups in the population -> add 10 to b if b<10
  mutate(b = if_else(b<10, b+10, b),
         aug = b - m)

# create augmented dataframe
oper.dat.aug <- aug[rep(1:nrow(aug), aug$aug), ] %>%
  mutate(
    x = NA,
    ym1 = NA,
    h = h,
    q = NA,
    z = 0,
    yr = yr,
    subunits = h,
    .keep = "none"
  ) %>%
  ungroup()

# combine dataframes
oper.dat <- rbind(oper.dat, oper.dat.aug) %>%
  arrange(yr, subunits, h, desc(z))

glimpse(oper.dat) # check

## 1.8.3 Plot.dat ####

plot.dat <- oper.dat %>%
  select(yr, h, subunits) %>%
  distinct() %>%
  mutate(h.plots = h, 
         yr.plots = yr,
         subunits.plots = subunits
         ) %>%
  select(h.plots, yr.plots, subunits.plots) %>%
  arrange(yr.plots, subunits.plots, h.plots)
glimpse(plot.dat)

## 1.8.4 Scalar.dat ####

scalar.dat <- as.data.frame(matrix(NA, 1, (nrow(plot.dat))))
i <- 1
for(i in 1:nrow(plot.dat)) {
  scalar.dat[, i] <- as.double(nrow(
    oper.dat %>% filter(yr == plot.dat$yr.plots[i], 
                        h == plot.dat$h.plots[i],
                        subunits == plot.dat$subunits.plots[i]
                        )
  ))
  colnames(scalar.dat)[i] <- paste0("h",
                                    plot.dat$h.plots[i],
                                    "y",
                                    plot.dat$yr.plots[i],
                                    "sub",
                                    plot.dat$subunits.plots[i])
}

scalar.dat <- scalar.dat %>%
  mutate(R = as.double(nrow(sight.dat)),
         Ngroups = as.double(nrow(oper.dat)),
         Nsubunits.yr = as.double(nrow(plot.dat)))

# Need scalar.sums to ease modelling
# tells us how many rows belong to each year/stratum/subunit combo
scalar.sums <- matrix(NA, nrow(plot.dat), 2)
for (i in 1:nrow(plot.dat)){
  t <- i-1
  scalar.sums[i, 1] <- sum(scalar.dat[,0:t], 1)
  scalar.sums[i, 2] <- sum(scalar.dat[,0:i])
}
  
## 1.8.5 Save Bayesian data ####
save(list = c("sight.dat", "oper.dat", "plot.dat", "scalar.dat", "eff", "scalar.sums"), file = paste0(name, "_jags_input.rdata"))

# 1.9 SAVE OTHER INPUTS ####
save(list=c("name", "compile_sheets", "name_fixer", "input","output","file","EPU.list","year.ID", "eff"), file=paste0(name, "_other_inputs.rdata"))

rm(list = ls())

# Next script: 02_mHT_analysis.R
# If you're not doing the mHT analysis, skip forward to 03_Bayesian_analysis.R

