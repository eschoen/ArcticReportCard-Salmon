# ARC-plots.R
# Compile and plot data for Arctic Report Card salmon essay
# Erik Schoen
# 9-2023

rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/ArcticReportCard-Salmon")

# Load packages and read in data

library(tidyverse)
library(scales)

theme_set(theme_bw(12))


harvest <- read_csv("./data/harvest.csv")
runSize <- read_csv("./data/runSize.csv")

# Summarize harvest data for plotting
harvestLong <- harvest %>%
  rowwise() %>%
  mutate(TotalCommercial = sum(Commercial, CommercialRelated, TestFishery),
         Other = sum(PersonalUse_Domestic, Sport_PublicAngling)) %>%
  group_by(Region, Species, Year) %>%
  summarize(Subsistence = sum(Subsistence),
            Commercial = sum(TotalCommercial),
            Other = sum(Other)) %>%
  pivot_longer(cols = Subsistence:Other, names_to = "Fishery", values_to = "Harvest") %>%
  # convert fishery, species, and region to factors and reorder for plotting
  mutate(Region = factor(Region, levels = c("Yukon", "Kuskokwim", "Bristol Bay")),
         Species = factor(Species, levels = c("Chinook", "Chum", "Sockeye")),
         Fishery = factor(Fishery, levels = c("Subsistence", "Commercial", "Other")))


# Make plots
harvest.plot <- ggplot(data = harvestLong, aes(x = Year, y = Harvest, color = Fishery)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_grid(rows = vars(Species), cols = vars(Region), scales = "free") +
  scale_x_continuous(limits = c(1988,2024)) + # provide a little white space btw regions
  scale_y_continuous(name = "Salmon Harvest", labels = label_comma()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # hide gridlines

harvest.plot
ggsave("./figures/harvest.png")

## Figure 2: Trends in abundance of key salmon stocks in Western Alaska

## Figure 3: Trends of body size (panel A) and fecundity (panel B) for selected salmon populations in Western Alaska

## Figure 4: Harvest trends in major Western Alaska salmon fisheries

