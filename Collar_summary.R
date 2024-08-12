# Extracting demographics of collared elk

library(tidyverse)
library(readxl)

info <- read_excel("I:/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Collar_data/Capture and Telemetry_DATABASE_April 17_2023.xls", 
                   sheet = "Animal Capture or Marking ", 
                   col_types = c("text", "text", "text", 
                                 "text", "text", "text", "text", "date", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "numeric"), skip = 10)

info <- info %>%
  filter(Make=="Vectronics") %>%
  mutate(ID = as.numeric(`Serial No.`))

# 2021

cols_2021 <- read_csv("I:/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Collar_data/Spring2021_Collars.csv") %>%
  left_join(info, by= c("Collar ID" = "ID"))
# Check - did all collars match with info?

# Display locations to make sure we're not including any at the office
library(leaflet)
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircles(lng = cols_2021$`Longitude[deg]`, lat = cols_2021$`Latitude[deg]`, color = "red", label = cols_2021$`Collar ID`)

cols_2021 <- cols_2021 %>%
  select(`Collar ID`, `Age Class`, Sex, `Population Unit (release)`)

summary(as.factor(cols_2021$Sex))
summary(as.factor(cols_2021$`Population Unit (release)`))

# 2022

cols_2022 <- read_csv("I:/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Collar_data/Spring2022_Collars.csv") %>%
  left_join(info, by= c("Collar ID" = "ID"))
# Check - did all collars match with info?

# Display locations to make sure we're not including any at the office

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircles(lng = cols_2022$`Longitude[deg]`, lat = cols_2022$`Latitude[deg]`, color = "red", label = cols_2022$`Collar ID`)

cols_2022 <- cols_2022 %>%
  select(`Collar ID`, `Age Class`, Sex, `Population Unit (release)`)

summary(as.factor(cols_2022$Sex))
summary(as.factor(cols_2022$`Population Unit (release)`))

# 2023

cols_2023 <- read_csv("I:/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/Sightability Project/4. Technical/Data/Collar_data/Spring2023_Collars.csv") %>%
  left_join(info, by= c("Collar ID" = "ID"))
# Check - did all collars match with info?

# Display locations to make sure we're not including any at the office

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircles(lng = cols_2023$`Longitude[deg]`, lat = cols_2023$`Latitude[deg]`, color = "red", label = cols_2022$`Collar ID`)

cols_2023 <- cols_2023 %>%
  select(`Collar ID`, `Age Class`, Sex, `Population Unit (release)`)

summary(as.factor(cols_2023$Sex))
summary(as.factor(cols_2023$`Population Unit (release)`))

