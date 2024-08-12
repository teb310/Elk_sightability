library(tidyverse)
library(readxl)

# TEM
hab <- read.csv("//SFP.IDIR.BCGOV/U109/TBRUSH$/GIS/elk_habitat/TEM_EPU_Join.txt") 
hab_LIW <- hab %>%
  select(Rank = MCEEL_LIW_6C_WA, Area_sqm = Shape_Area, EPU = Unit) %>%
  filter(Rank <= 3,
         EPU == "Brittain" | EPU == "Rainy-Grey" | EPU == "Sechelt Peninsula") %>%
  group_by(EPU, Rank) %>%
  summarize(Area_sqkm = sum(Area_sqm)/1000000)

hab_LIG <- hab %>%
  select(Rank = MCEEL_GFD_6C_WA, Area_sqm = Shape_Area, EPU = Unit) %>%
  filter(Rank <= 3,
         EPU == "Brittain" | EPU == "Rainy-Grey" | EPU == "Sechelt Peninsula") %>%
  group_by(EPU, Rank) %>%
  summarize(Area_sqkm = sum(Area_sqm)/1000000)

# BEI
hab <- read.csv("//SFP.IDIR.BCGOV/U109/TBRUSH$/GIS/elk_habitat/BEI_EPU_Join.txt") 
hab_LIW <- hab %>%
  select(Rank = M_CEEL_RO_WLI6WTSUI, Area_sqm = SHAPE_Area, EPU = Unit) %>%
  filter(Rank <= 3,
         EPU == "Indian" | EPU == "McNab" | EPU == "Pitt" | EPU == "Skwawka" | EPU == "Squamish" | EPU == "Stave") %>%
  group_by(EPU, Rank) %>%
  summarize(Area_sqkm = sum(Area_sqm)/1000000)

hab_LIG <- hab %>%
  select(Rank = M_CEEL_RO_SLI6WTSUI, Area_sqm = SHAPE_Area, EPU = Unit) %>%
  filter(Rank <= 3,
         EPU == "Indian" | EPU == "McNab" | EPU == "Pitt" | EPU == "Skwawka" | EPU == "Squamish" | EPU == "Stave") %>%
  group_by(EPU, Rank) %>%
  summarize(Area_sqkm = sum(Area_sqm)/1000000)

hab <- read_xlsx("//SFP.IDIR.BCGOV/S140/S40073/FLNR RM/!Terrestrial Wildlife Section/1_Species & Values/Roosevelt Elk/SBOT/Habitat_objectives.xlsx")
hab$Value <- if_else(hab$Variable == "X", hab$Value*100, hab$Value)

# LIG

hab_LIG <- hab %>%
  filter(Season=="LIG" | is.na(Season))

x <- as.numeric(NA)
P <- as.numeric(NA)
h <- matrix(NA, nrow = 3, ncol = 9)
H <- as.numeric(NA)

for(i in 1:3){
  for(j in 1:9){
    x[j] <- hab_LIG$Value[hab_LIG$Variable=="Tau" & hab_LIG$EPU==j]/hab_LIG$Value[hab_LIG$Variable=="X" & hab_LIG$EPU==j & hab_LIG$Rank==i]
    x <- subset(x, x!=Inf)
  }
  P[i] <- sum(x)/length(x)
  for(j in 1:9){
    h[i,j] <- hab_LIG$Value[hab_LIG$Variable=="K" & hab_LIG$EPU==j]/P[i]
  }
}

for (j in 1:9){
  H[j] <- sum(h[1:3,j])
}
H_LIG <- H

# LIW

hab_LIW <- hab %>%
  filter(Season=="LIW" | is.na(Season))

x <- as.numeric(NA)
P <- as.numeric(NA)
h <- matrix(NA, nrow = 3, ncol = 9)
H <- as.numeric(NA)

for(i in 1:3){
  for(j in 1:9){
    x[j] <- hab_LIW$Value[hab_LIW$Variable=="Tau" & hab_LIW$EPU==j]/hab_LIW$Value[hab_LIW$Variable=="X" & hab_LIW$EPU==j & hab_LIW$Rank==i]
    x <- subset(x, x!=Inf)
  }
  P[i] <- sum(x)/length(x)
  for(j in 1:9){
    h[i,j] <- hab_LIW$Value[hab_LIW$Variable=="K" & hab_LIW$EPU==j]/P[i]
  }
}

for (j in 1:9){
  H[j] <- sum(h[1:3,j])
}
H_LIW <- H
