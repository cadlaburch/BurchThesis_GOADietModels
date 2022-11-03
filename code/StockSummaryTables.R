#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))

#creating sample size table
stomach_contents_2021 <- raw_stomach_contents2021 %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN, sep = "_"))

sample_size2021 <- stomach_contents_2021 %>%
  select(uniqueID, Pred_common, Year) %>% 
  group_by(Pred_common, Year) %>% 
  summarize(n = length(unique(uniqueID)))

Barnes <- filter(sample_size2021, Pred_common == "Arrowtooth flounder" | Pred_common == "Pacific halibut")

#why does this not match the tables that Cheryl Barnes made?