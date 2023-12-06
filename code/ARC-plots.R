# ARC-plots.R
# Compile and plot data for Arctic Report Card salmon essay
# Erik Schoen
# 9-2023

rm(list=ls(all=TRUE)) # clear the working environment
setwd("~/Development/ArcticReportCard-Salmon")

#### Load packages and read in data ####

library(tidyverse)
library(scales)
library(GGally)
library(patchwork)

theme_set(theme_bw(12))

# Read in data provided directly by Katie Howard at ADFG
runSize <- read_csv("./data/runSize.csv") # Total run size (spawning escapement + harvest)
harvest <- read_csv("./data/harvest.csv")

# Read in Yukon Chinook length and fecundity estimates from Ohlberger et al 2020 (provided by J. Ohlberger)
lengthYukonChk <- read_csv("./data/Ohlberger-etal-2020-Fig4-lengths.csv")
fecundityYukonChk <- read_csv("./data/Ohlberger-etal-2020-Fig5-fecundity.csv")
eggMassYukonChk <- read_csv("./data/Ohlberger-etal-2020-Fig5-eggmass.csv")

# Read in Bristol Bay sockeye length data from Ohlberger et al 2023
# https://github.com/janohlberger/SockeyeSize/blob/main/output/data_used_by_run_year_all.csv
# lengthBBSockeye <- read_csv("./data/data_used_by_run_year_all.csv")[-1]

# Read in Alaska salmon Age-Sex-Length data from Clark et al. 2018
# These data compiled from https://knb.ecoinformatics.org/view/doi:10.5063/F1707ZTM by "Compile-ASL.R" script
lengthWAKSalmon <- readRDS("./data/lengthWAKSalmon.rds")

#### Wrangle data and summarize for plotting ####

# Summarize abundance data for plotting
runSizeLong <- runSize %>%
  mutate(YukonChum = YukonSummerChum + YukonFallChum) %>%

         # TotalBBSockeye = KvichakRiverSockeye+AlagnakRiverSockeye+NaknekRiverSockeye+EgegikRiverSockeye+
         #   UgashikRiverSockeye+WoodRiverSockeye+IgushikRiverSockeye+TogiakRiverSockeye+NushagakRiverSockeye) %>%
  select(Year, 
         YukonChinook = TotalYukonChinook,
         KuskokwimChinook,
         NushagakChinook,
         YukonChum,
         KuskokwimChum = KuskokwimChumIndex_CPUE,
         BBSockeye = TotalBBSockeye) %>%
  pivot_longer(cols = YukonChinook:BBSockeye, names_to = "Stock", values_to = "AbundanceIndex") %>%
  mutate(Region = case_when(
                            str_detect(Stock, "Yukon") ~ "Yukon",
                            str_detect(Stock, "Kuskokwim") ~ "Kuskokwim",
                            str_detect(Stock, "Nushagak") ~ "Bristol Bay",
                            str_detect(Stock, "BB") ~ "Bristol Bay"),
        Species = case_when(
                            str_detect(Stock, "Chinook") ~ "Chinook",
                            str_detect(Stock, "Chum") ~ "Chum",
                            str_detect(Stock, "Sockeye") ~ "Sockeye"),
        Region = factor(Region, levels = c("Yukon", "Kuskokwim", "Bristol Bay")),
        Species = factor(Species, levels = c("Chinook", "Chum", "Sockeye"))
        )

runSizeMean <- runSizeLong %>%
  filter(Year < 2021) %>% # filter out the 2021 and 2022 data to calculate a 1991-2020 average
  group_by(Region, Species, Stock) %>%
  summarize(meanAbundanceIndex = mean(AbundanceIndex))

runSizeDeviations <- runSizeLong %>%
  left_join(runSizeMean, by = c("Region", "Species", "Stock")) %>%
  mutate(runSizeDeviation = (AbundanceIndex - meanAbundanceIndex) / meanAbundanceIndex *100) %>%
  select(Region, Species, Stock, Year, AbundanceIndex, meanAbundanceIndex, runSizeDeviation)

# Set color palettes (colorblind friendly)
salmonColor <- c("gold", "blue", "red")
harvestColor <- c("#D55E00", "#009E73", "#56B4E9") 


# Summarize body size and fecundity data for plotting
# Yukon Chinook
lengthYukonChk1 <- lengthYukonChk %>%
  mutate(Stock = "YukonChinook",
         Species = "Chinook") %>%
  select(Stock, Species, Year, Length, Lower, Upper)

# fecundityYukonChk1 <- fecundityYukonChk %>%
#   rename(Estimate = Fecundity) %>%
#   mutate(Stock = "YukonChinook",
#          Trait = "Fecundity") %>%
#   select(Stock, Year, Trait, Estimate, Lower, Upper)
# 
# eggMassYukonChk1 <- eggMassYukonChk %>%
#   rename(Estimate = `Egg mass`) %>%
#   mutate(Stock = "YukonChinook",
#          Trait = "Egg mass") %>%
#   select(Stock, Year, Trait, Estimate, Lower, Upper)
# 
# bodySizeFecundityYukonChk <- rbind(lengthYukonChk1, fecundityYukonChk1, eggMassYukonChk1)

# Western Alaska chum
lengthWAKChum <- lengthWAKSalmon %>%
  filter(Species == "chum")

# Examine the data
# ggpairs(select(lengthWAKSalmon, -Location))
summary(lengthWAKChum)

# Select only measurements from minimally selective gear types (weirs or "handpicked or carcass")
lengthWAKChum <- lengthWAKChum %>%
  filter(Gear == "weir" | Gear == "handpicked or carcass")

lengthWAKChum1 <- lengthWAKChum %>%
  mutate(Stock = "Yukon & Kuskokwim Chum",
         Species = "Chum",
         Lower = NA,
         Upper = NA) %>%
  select(Stock, Species, Year, Length, Lower, Upper)

# # Bristol Bay sockeye
# lengthBBSockeye1 <- lengthBBSockeye %>%
#   rename(Year = year,
#          Length = length) %>%
#   mutate(Stock = "Bristol Bay Sockeye",
#          Species = "Sockeye",
#          Lower = NA,
#          Upper = NA) %>%
#   select(Stock, Species, Year, Length, Lower, Upper)

# # Combine all focal stocks for multipanel figure
# lengthFocalStocks <- rbind(lengthYukonChk1, lengthWAKChum1, lengthBBSockeye1)

# Examine all Western Alaska Chinook, chum, sockeye using filtered Clark et al. 2018 dataset
lengthWAKSalmon <- lengthWAKSalmon %>%
  # Only include fish sampled with minimally size-selective gear (e.g., no gill nets)
  filter(Gear == "weir" | Gear == "handpicked or carcass" | Gear == "seine") %>%
  mutate(Region = factor(Region, levels = c("Norton Sound", "Yukon", "Kuskokwim", "Bristol Bay")),
         ASLProjectType = factor(ASLProjectType),
         Gear = factor(Gear),
         Location = factor(Location),
         Species = case_when(
           Species == "chinook" ~ "Chinook", 
           Species == "chum" ~ "Chum",
           Species == "sockeye" ~ "Sockeye"),
         Species = factor(Species)) %>%
  filter(!is.na(Length)) %>% # Remove records with missing lengths
  select(Year, Region, ASLProjectType, Gear, Location, Species, Length)

# Summarize length data for plotting
lengthWAKSalmon.summary <- lengthWAKSalmon %>%
  group_by(Species, Year) %>%
  summarize(n = n(),
            meanLength = mean(Length),
            sdLength = sd(Length),
            seLength = sd(Length)/sqrt(n))

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

#### Make plots ####

## Figure 2: Trends in abundance of key salmon stocks in Western Alaska
abundance.plot <- ggplot(data = runSizeDeviations, aes(x = Year, y = runSizeDeviation, fill = Species)) +
  geom_col(position = "dodge") + 
  geom_hline(yintercept = 0) +
  facet_grid(rows = vars(Region), scales = "free")  + 
  scale_x_continuous(minor_breaks = seq(1990, 2022, 2)) +
  scale_y_continuous(name = "Salmon abundance anomaly (%)") +
  expand_limits(y = c(-100, 100)) + # show from -100% to +100% in all panels
  scale_fill_manual(values = salmonColor) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
abundance.plot
ggsave("./figures/Figure2_abundance.png", width = 6, height = 5)
ggsave("./figures/Figure2_abundance.eps", width = 6, height = 5)
ggsave("./figures/Figure2_abundance.pdf", width = 6, height = 5)

# abundance.3by3.plot <- ggplot(data = runSizeDeviations, aes(x = Year, y = runSizeDeviation)) +
#   geom_col(position = "dodge") + 
#   geom_hline(yintercept = 0) +
#   facet_grid(rows = vars(Species), cols = vars(Region), scales = "free") +
#   scale_x_continuous(minor_breaks = seq(1990, 2022, 2)) +
#   scale_y_continuous(name = "Salmon abundance anomaly (%)") +
#   expand_limits(y = c(-100, 100)) + # show from -100% to +100% in all panels
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# abundance.3by3.plot
# ggsave("./figures/Figure2_abundance_3by3.png", width = 6, height = 5)

## Figure 3: Trends of body size (panel A) and fecundity (panel B) for selected salmon populations in Western Alaska
# # Yukon Chinook
# lengthYukonChinook.plot <- ggplot(data = lengthYukonChk, aes(x = Year, y = Length)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthYukonChinook.plot
# ggsave("./figures/length_YukonChinook.png", width = 5, height = 3)
# 
# lengthYukonChinook.plot.withPoints <- ggplot(data = lengthYukonChk, aes(x = Year, y = Length)) +
#   geom_smooth(se = F) +
#   geom_point() + 
#   geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)", limits = c(750, 900)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthYukonChinook.plot.withPoints
# ggsave("./figures/length_YukonChinook_withPoints.png", width = 5, height = 3)
# 
# fecundityYukonChinook.plot <- ggplot(data = fecundityYukonChk, aes(x = Year, y = Fecundity)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Fecundity (eggs / female)", labels = label_comma()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# fecundityYukonChinook.plot
# ggsave("./figures/fecundity_YukonChinook.png", width = 5, height = 3)
# 
# fecundityYukonChinook.plot.withPoints <- ggplot(data = fecundityYukonChk, aes(x = Year, y = Fecundity)) +
#   geom_smooth(se = F) +
#   geom_point() +
#   geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
#   scale_y_continuous(name = "Fecundity (eggs / female)", labels = label_comma(), limits = c(6000, 9000)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# fecundityYukonChinook.plot.withPoints
# ggsave("./figures/Figure3b_fecundity_YukonChinook_withPoints.png", width = 3, height = 3)
# 
# # Western Alaska Chum
# lengthWAKChum.plot <- ggplot(data = lengthWAKChum, aes(x = Year, y = Length)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthWAKChum.plot
# ggsave("./figures/length_WAKChum.png", width = 5, height = 3)
# 
# # BBay Sockeye
# lengthBBSockeye.plot <- ggplot(data = lengthBBSockeye, aes(x = year, y = length)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthBBSockeye.plot
# ggsave("./figures/length_BBSockeye.png", width = 5, height = 3)
# 
# # Make multi-panel plot using patchwork
# length.plot <- lengthYukonChinook.plot + lengthWAKChum.plot + lengthBBSockeye.plot +
#   plot_layout(ncol = 1)
# length.plot
# ggsave("./figures/length_focalStocks_3panels.png", width = 5, height = 6)
# 
# # Length of focal stocks (one panel, species = color)
# lengthFocalStocks.plot <- ggplot(data = lengthFocalStocks, aes(x = Year, y = Length, color = Species)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   scale_color_manual(values = salmonColor) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthFocalStocks.plot
# ggsave("./figures/length_focalStocks_1panelColor.png", width = 6, height = 5)
# 
# # Length of focal stocks (3 panels, species = facets)
# lengthFocalStocks.plot <- ggplot(data = lengthFocalStocks, aes(x = Year, y = Length)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   facet_grid(rows = vars(Species), scales = "free") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthFocalStocks.plot
# ggsave("./figures/Figure3a_length_3focalStocks.png", width = 4, height = 6)
# 
# # WAK salmon 3 spp x 4 regions
# lengthWAKSalmon.faceted.plot <- ggplot(data = lengthWAKSalmon, aes(x = Year, y = Length)) +
#   geom_smooth() +
#   scale_x_continuous(name = "Year", limits = c(1970, 2020))+
#   scale_y_continuous(name = "Body Length (mm)") +
#   facet_grid(rows = vars(Species), cols = vars(Region), scales = "free") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthWAKSalmon.faceted.plot
# ggsave("./figures/Figure3a_length_WAKSalmon_faceted.png", width = 6, height = 5)

# WAK salmon all species and areas one plot
lengthWAKSalmon.plot <- ggplot(data = lengthWAKSalmon, aes(x = Year, y = Length, color = Species)) +
  geom_smooth(se = F) +
  scale_x_continuous(name = "Year", limits = c(1970, 2020))+
  scale_y_continuous(name = "Body Length (mm)") +
  scale_color_manual(values = salmonColor) +
  # facet_grid(rows = vars(Species), cols = vars(Region), scales = "free") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthWAKSalmon.plot
# ggsave("./figures/Figure3a_length_WAKSalmon.png", width = 6, height = 4)

# Alternate version of length plot showing annual means +/- SDs
lengthWAKSalmon.withpoints.sd <- ggplot(data = lengthWAKSalmon.summary, aes(x = Year, y = meanLength,
                                                                         ymin = meanLength - sdLength, 
                                                                         ymax = meanLength + sdLength,
                                                                         color = Species)) +
  # geom_smooth(se = F, method = "gam") +
  geom_smooth(se = F, span = 0.45) +
  geom_pointrange(fatten = 2) +
  geom_point(color = "black", shape = 21, size = 2) +
  scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
  scale_y_continuous(name = "Body Length (mm)") +
  scale_color_manual(values = salmonColor) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthWAKSalmon.withpoints.sd

# Third version showing annual means only
lengthWAKSalmon.withpoints <- ggplot(data = lengthWAKSalmon.summary, aes(x = Year, y = meanLength,
                                                                         color = Species, fill = Species)) +
  # geom_smooth(se = F, method = "gam") +
  geom_smooth(se = F, span = 0.45) +
  # geom_pointrange(fatten = 2) +
  geom_point(color = "black", shape = 21, size = 2) +
  scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
  scale_y_continuous(name = "Body Length (mm)") +
  scale_color_manual(values = salmonColor) +
  scale_fill_manual(values = salmonColor) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# lengthWAKSalmon.withpoints

fecundityYukonChinook.plot.withPoints.color <- ggplot(data = fecundityYukonChk, aes(x = Year, y = Fecundity)) +
  geom_smooth(se = F, color = "black") +
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  geom_point(fill = "gold", color = "gold") +
  scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
  scale_y_continuous(name = "Fecundity (eggs / female)", labels = label_comma(), limits = c(6000, 9000)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())   # hide gridlines 
# fecundityYukonChinook.plot.withPoints.color
# ggsave("./figures/Figure3b_fecundity_YukonChinook_color.png", width = 3, height = 3)

# Make two-panel plot using patchwork

# First remove x-axis label and ticks from top panel, and add letters to each panel
lengthWAKSalmon.panel.a <- lengthWAKSalmon.plot +
  theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank())  +
  annotate("text", x = 2018, y = 840, label = "a")
# lengthWAKSalmon.panel.a

lengthWAKSalmon.panel.a2 <- lengthWAKSalmon.withpoints.sd +
  theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank())  +
  annotate("text", x = 2018, y = 1100, label = "a")
# lengthWAKSalmon.panel.a2

lengthWAKSalmon.panel.a3 <- lengthWAKSalmon.withpoints +
  theme(axis.title.x = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank())  +
  annotate("text", x = 2018, y = 910, label = "a")
# lengthWAKSalmon.panel.a3

fecundityYukonChinook.panel.b <- fecundityYukonChinook.plot.withPoints.color +
  annotate("text", x = 2018, y = 8900, label = "b")
# fecundityYukonChinook.panel.b

fecundityYukonChinook.panel.b2 <- ggplot(data = fecundityYukonChk, aes(x = Year, y = Fecundity,
                                                                       ymin = Lower, ymax = Upper)) +
  geom_smooth(se = F, color = "gold") +
  geom_pointrange(fatten = 2, color = "gold") +
  geom_point(color = "black", shape = 21, size = 2) +
  scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
  scale_y_continuous(name = "Fecundity (eggs / female)", labels = label_comma(), limits = c(6000, 9000)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + # hide gridlines 
  annotate("text", x = 2018, y = 8900, label = "b")
# fecundityYukonChinook.panel.b2

fecundityYukonChinook.panel.b3 <- ggplot(data = fecundityYukonChk, aes(x = Year, y = Fecundity)) +
  geom_smooth(se = F, color = "gold") +
  # geom_pointrange(fatten = 2, color = "gold") +
  geom_point(color = "black", fill = "gold", shape = 21, size = 2) +
  scale_x_continuous(name = "Year", limits = c(1970, 2020)) +
  scale_y_continuous(name = "Fecundity (eggs / female)", labels = label_comma(), limits = c(6000, 8500)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + # hide gridlines 
  annotate("text", x = 2018, y = 8400, label = "b")
# fecundityYukonChinook.panel.b3

length.fecundity.plot <- lengthWAKSalmon.panel.a + fecundityYukonChinook.panel.b +
  plot_layout(ncol = 1)
# length.fecundity.plot
# ggsave("./figures/Figure3_length_fecundity.png", width = 6, height = 6)
# ggsave("./figures/Figure3_length_fecundity.eps", width = 6, height = 6)
# ggsave("./figures/Figure3_length_fecundity.pdf", width = 6, height = 6)

# version 2 with annual means +/- SD (or SE) on both plots
length.fecundity.plot2 <- lengthWAKSalmon.panel.a2 + fecundityYukonChinook.panel.b2 +
  plot_layout(ncol = 1)
# length.fecundity.plot2
# ggsave("./figures/Figure3_length_fecundity_with_points_whiskers.png", width = 6, height = 6)

# # version 3 with annual means on both plots
length.fecundity.plot3 <- lengthWAKSalmon.panel.a3 + fecundityYukonChinook.panel.b3 +
  plot_layout(ncol = 1)
length.fecundity.plot3
ggsave("./figures/Figure3_length_fecundity_with_points.png", width = 6, height = 6)
ggsave("./figures/Figure3_length_fecundity_with_points.eps", width = 6, height = 6)
ggsave("./figures/Figure3_length_fecundity_with_points.pdf", width = 6, height = 6)


## Figure 4: Harvest trends in major Western Alaska salmon fisheries
harvest.plot <- ggplot(data = harvestLong, aes(x = Year, y = Harvest, color = Fishery)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_grid(rows = vars(Species), cols = vars(Region), scales = "free") +
  scale_x_continuous(limits = c(1988,2024)) + # provide a little white space btw regions
  scale_y_continuous(name = "Annual harvest (numbers of salmon)", labels = label_comma()) +
  scale_color_manual(values = harvestColor) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  + # hide gridlines 
  theme(legend.position = c(0.17, 0.17))

harvest.plot
ggsave("./figures/Figure4_harvest.png", width = 6, height = 5)
ggsave("./figures/Figure4_harvest.eps", width = 6, height = 5)
ggsave("./figures/Figure4_harvest.pdf", width = 6, height = 5)

