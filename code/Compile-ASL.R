# Compile-ASL.R
# Compile and filter age, sex, and length data for Arctic Report Card salmon essay
# Erik Schoen
# 11-2023

rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/ArcticReportCard-Salmon")

#### Load packages and read in data ####

library(tidyverse)

# Read in Alaska salmon Age-Sex-Length data from Clark et al. 2018
# https://knb.ecoinformatics.org/view/doi:10.5063/F1707ZTM

lengthAKSalmon <- read_csv("https://cn.dataone.org/cn/v2/resolve/urn:uuid:478e0569-a61b-4339-86d7-7ab3849dc8d4")

### Filter data ###
# Filter this large dataset down to only include Western Alaska salmon from focal stocks for ARC essay 
lengthWAKSalmon <- lengthAKSalmon %>%
  filter(SASAP.Region == "Yukon" | SASAP.Region == "Kuskokwim" | SASAP.Region == "Bristol Bay" | SASAP.Region == "Norton Sound",
         Species == "chum" | Species == "chinook" | Species == "sockeye",
         sampleYear > 1969, 
         Length.Measurement.Type == "mid-eye to fork of tail") %>%
  # Only include fish sampled with minimally size-selective gear (e.g., no gill nets)
  # Gear == "weir" | Gear == "handpicked or carcass" | Gear == "trap" | Gear == "seine") %>%
  mutate(Region = factor(SASAP.Region),
         ASLProjectType = factor(ASLProjectType),
         Gear = factor(Gear),
         Location = factor(LocationUnique)) %>%
  filter(is.na(Flag), !is.na(Length)) %>% # Remove questionable data flagged by Clark et al. and missing data
  select(Year = sampleYear, Region, ASLProjectType, Gear, Location, Species, Length)


### Write data to RDS ###

saveRDS(lengthWAKSalmon, "./data/lengthWAKSalmon.rds")
