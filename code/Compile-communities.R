# Compile-communities.R
# Compile community location data for map in Arctic Report Card salmon essay
# Erik Schoen
# 11-2023

rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/ArcticReportCard-Salmon")

#### Load packages and read in data ####

library(tidyverse)

# Read in Alaska community data
AK.all <- read_csv("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A7fc6f6db-c5ea-426a-a743-1f2edafb43b8")

# Read in list of communities in YT, BC, and NWT from Statistics Canada
# https://doi.org/10.25318/9810000201-eng
CAN.census <- read_csv("./data/can_communities.csv", col_types = "ccfi")

# Remove communities with population < 5 or NA
CAN.census.filtered <- CAN.census %>%
  filter(!is.na(Population),
         Population > 0) %>%
  mutate(Community = str_remove(Community, "[:digit:]"), # remove numbers from community names
         Community = str_trim(Community, side = "both")) %>% 
  select(Region = Jurisdiction, Community, Population) 

# Read in BC, YT, NWT point locations used by SNAP (these include communities and other locs such as ferry terminals and mines)
BC.locs <- read_csv("./data/british_columbia_point_locations.csv")
YT.locs <- read_csv("./data/yukon_point_locations.csv")
NWT.locs <- read_csv("./data/northwest_territories_point_locations.csv")
CAN.locs <- rbind(BC.locs, YT.locs, NWT.locs) %>%
  select(Region = region, Community = name, Lat = latitude, Lon = longitude) %>%
  # correct names for matching with census data
  mutate(Community = ifelse(Community == "Dawson City", "Dawson", Community),
         Community = ifelse(Community == "Mount Lorne", "Mt. Lorne", Community),
         Community = ifelse(Community == "Upper Laberge", "Lake Laberge", Community),
         Community = ifelse(Community == "Behchokǫ̀", "Behchokò", Community),
         Community = ifelse(Community == "Deline", "Déline", Community),
         Community = ifelse(Community == "Łutselkʼe", "Lutselk'e", Community),
         Community = ifelse(Community == "Hay River Reserve", "Hay River Dene", Community)
  )

# # Intersect the Canadian point locations with the Canadian census data
CAN.comm <- inner_join(CAN.census.filtered, CAN.locs, by = c("Region", "Community"))

# Check for mis-matches 
CAN.mismatches <- anti_join(CAN.census.filtered, CAN.locs, by = c("Region", "Community"))

# Filter and combine
AK.comm <- AK.all %>%
  filter(Year == 2015,
         !is.na(lat),
         !is.na(lng),
         total > 0) %>%
  mutate(Region = "Alaska") %>%
  select(Region, Community = city, Population = total, Lat = lat, Lon = lng)

identical(names(AK.comm), names(CAN.comm))

AK.CAN.communities <- rbind(AK.comm, CAN.comm)
# Save as csv
write_csv(AK.CAN.communities, "./data/AK_CAN_communities.csv")
