# 00_Create_EPU_areas
# This script converts "BEI_by_EPU.csv" (made by intersecting BEI data with EPUs
# in ArcMap) to a table summarizing capable elk winter habitat area per EPU

library(readr)

setwd("C:/Users/TBRUSH/R/Elk_sightability")

dat <- read_csv("input/BEI_by_EPU.txt") %>%
  # pare down to necesary columns
  select(EPU = Unit, LIW_Cap = M_CEEL_RO1, area_m = Shape_Area) %>%
  # convert area to km
  mutate(area_km = area_m/1000000) %>%
  # filter out incapable areas
  filter(LIW_Cap < 4) %>%
  # sum areas by EPU
  group_by(EPU) %>%
  summarize(area = sum(area_km))

write.csv(dat, "input/Effort/EPU_areas.csv", row.names = F)
