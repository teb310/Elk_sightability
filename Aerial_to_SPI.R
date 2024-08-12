# Script to convert aerial observation data to SPI format

library(tidyverse)
library(readxl)

# set up name fixer
fix_names <- function(x, EPU) {
  output <- x %>%
  mutate(EPU = case_when(
    grepl("deserted", EPU, ignore.case = T) ~ "Deserted",
    grepl("chehalis", EPU, ignore.case = T) ~ "Chehalis",
    grepl("clowhom", EPU, ignore.case = T) ~ "Clowhom",
    grepl("dani", EPU, ignore.case = T) ~ "Powell-Daniels",
    grepl("homat", EPU, ignore.case = T) ~ "Homathco",
    grepl("narrows", EPU, ignore.case = T) ~ "Narrows",
    grepl("rainy", EPU, ignore.case = T) ~ "Rainy-Grey",
    grepl("sechelt", EPU, ignore.case = T) ~ "Sechelt Peninsula",
    grepl("theo", EPU, ignore.case = T) ~ "Theo",
    grepl("mcn", EPU, ignore.case = T) ~ "McNab",
    grepl("vanc", EPU, ignore.case = T) ~ "Vancouver",
    .default = EPU
  ))  
  return(output)
}
  
year <- 2021
data_file <- "C:/Users/TBRUSH/R/SightabilityModels/old_shiny/input/2021_data.xlsx"
locs <- read_csv(paste0("//SFP.IDIR.BCGOV/U109/TBRUSH$/GIS/Aerial_elk_inventory/",year,"/obs_",year,"_locs.txt"))
est <- read_xlsx("//SFP.IDIR.BCGOV/S140/S40073/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/5. Reporting/Reports/Transitory/Sightability_Report_2023_results_table.xlsx") %>%
  filter(Year == year)
est <- fix_names(est)

sum <- read_xlsx(data_file, sheet = paste(year,"Summary"))
obs <- read_xlsx(data_file, sheet = paste(year,"Data"))

sort(unique(est$EPU))
sort(unique(obs$EPU))
# locs$EPU <- locs$Unit
sort(unique(locs$EPU))

obs <- fix_names(obs)
obs <- obs %>% filter(EPU %in% est$EPU)
sort(unique(obs$EPU))

locs <- fix_names(locs)
sort(unique(locs$EPU))

locs$Date <- as.character(locs$Date)
obs$date <- as.character(obs$date)

obs_new <- left_join(obs, locs, by=c("EPU", "date"="Date", "total"="Group_Size"), multiple = "first") %>%
  mutate(date = as.POSIXct(date)) %>%
  arrange(date, EPU)

obs <- obs_new %>%
  mutate(
    across(cow:UC, ~replace_na(.x, 0)),
    study_area_name = "South Coast",
    block_label = EPU,
    date = format(date, "%d-%b-%Y"),
    time = NA,
    surveyor = NA,
    species = "M-CEEL",
    count = if_else(total >= cow+bull+spike+calf+UC, total,
                    cow+bull+spike+calf+UC),
    photos = NA,
    animal_ID = NA,
    WLH_ID = NA,
    UTM_zone = NA,
    easting = NA,
    northing = NA,
    longitude = Easting,
    latitude = Northing,
    comments = NA,
    predef_survey = NA,
    predef_mark = NA,
    group_label = NA,
    adult_males = bull,
    adult_females = cow,
    adults_uc = NA,
    juveniles = calf,
    yearling_males = spike,
    yearling_females = NA,
    yearling_uc = NA,
    uc_male = NA,
    uc_female = NA,
    unclassified = if_else(count > bull+cow+calf+spike+UC, count-(bull+cow+calf+spike), UC),
    male_1 = NA,
    male_2 = NA,
    male_3 = NA,
    male_4 = NA,
    male_1_2 = NA,
    male_3_4 = NA,
    male_3p = NA,
    male_3p_4p = NA,
    male_4p = NA,
    male_5p = NA,
    male_6p = NA,
    male_10p = NA,
    activity = case_when(
      grepl("stand", activity, TRUE) ~ "SA",
      grepl("bed", activity, TRUE) ~ "BE",
      grepl("run", activity, TRUE) ~ "TR"
    ),
    sign_type = NA,
    sign_age = NA,
    sign_count = NA,
    snow_cover = NA,
    veg_cover = voc*100,
    habitat = NA,
    survey_type = case_when(
      grepl("inc", survey.type, TRUE) ~ "IC",
      grepl("cap", survey.type, TRUE) ~ "IC",
      grepl("telem", survey.type, TRUE) ~ "TL",
      grepl("inv", survey.type, TRUE) ~ "IV"
    ),
    collars,
    .keep = "none",
  ) %>%
  mutate(across(c(adult_males:unclassified, collars), ~if_else(.x == 0, NA, .x))) %>%
  select(
    study_area_name,
    block_label,
    date,
    time:male_10p,
    activity,
    sign_type:veg_cover,
    habitat,
    survey_type,
    collars
  )

write.csv(obs, paste0("output/SPI/Obs_",year,".csv"), row.names = F, na = "")

# Copy and paste from CSV into SPI sheet

est <- read_xlsx("//SFP.IDIR.BCGOV/S140/S40073/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/5. Reporting/Reports/Transitory/Sightability_Report_2023_results_table.xlsx") %>%
  filter(Year == year)
est <- fix_names(est)
sum <- fix_names(sum)

est_new <- est %>%
  mutate(`95% CI` = str_replace_all(`95% CI`, ",", " -")) %>%
  separate(`95% CI`, into = c("LCL", "UCL"), sep = " - ", fill="right") %>%
  select(-`50% CI`) %>%
  mutate(across(c(LCL, CV), ~if_else(.x == "-", NA, .x))) %>%
  left_join(sum[,c("EPU", "min_count")], by="EPU")

est <- est_new %>%
  mutate(
    study_area_name = "South Coast",
    block_label = EPU,
    sum_year = Year,
    sum_month = NA,
    sum_day = NA,
    survey_time = NA,
    survey_time_units = NA,
    km_surveyed = NA,
    sqm_surveyed = NA,
    comments = NA,
    species = "M-CEEL",
    param = "individuals",
    param_method = case_when(
      Method == "Standard" ~ "MC-EK",
      .default = "MC"),
    param_value = Estimate,
    param_denom = NA,
    best_param = case_when(
      Method == "Bayesian" ~ "Y",
      .default = "N"),
    LCL = LCL,
    UCL = UCL,
    Conf = case_when(
      Method == "Standard" ~ NA,
      .default = 95),
    SE = NA,
    CV = round(as.numeric(CV), 3),
    SCF = round(min_count/Estimate, digits = 2),
    param_comments = case_when(
      Method == "Standard" ~ "Standard method: Sightability class is selected for each sex based on expert knowledge. Correction factors are then applied accordingly.",
      Method == "Bayesian" ~ "Bayesian data augmentation model",
      Method == "mHT" ~ "Modified Horvitz-Thompson frequentist model"),
    .keep = "none"
  ) %>%
  mutate(
    SCF = if_else(SCF > 1, 1, SCF)
  ) %>%
  select(
    study_area_name:best_param,
    LCL:UCL,
    Conf:SE,
    CV,
    SCF:param_comments
  ) %>%
  arrange(block_label)

write.csv(est, paste0("output/SPI/Summary_", year,".csv"), row.names = F, na = "")


# Block info

blocks <- read.csv("//SFP.IDIR.BCGOV/U109/TBRUSH$/GIS/Aerial_elk_inventory/EPU_centroids.txt")

blocks <- rename(blocks, "EPU" = "Unit")
blocks <- fix_names(blocks)

blocks <- blocks %>%
  filter(EPU %in% est$block_label) %>%
  mutate(
    study_area_name = "South Coast",
    study_area_photos = NA,
    parent_label = NA,
    block_label = EPU,
    UTM_zone = NA,
    easting = NA,
    northing = NA,
    longitude = Center_Long,
    latitude = Center_Lat,
    area = Shape_Area,
    comments = NA,
    photos = NA,
    predef = NA,
    stratum = NA,
    stratum_desc = NA,
    .keep = "none"
  ) %>%
  arrange(block_label)

write.csv(blocks, paste0("output/SPI/Block_Info_", year,".csv"), row.names = F, na = "")

