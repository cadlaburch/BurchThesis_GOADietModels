#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))

#creating sample size table
stomach_contents_2021 <- raw_stomach_contents2021 %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"))

sample_size2021 <- stomach_contents_2021 %>%
  filter(PRED_FULL != 1) %>% 
  select(uniqueID, Pred_common, Year) %>% 
  group_by(Pred_common, Year) %>% 
  summarize(n = length(unique(uniqueID)))

filter(sample_size2021, Pred_common == "Big skate")

Barnes <- filter(sample_size2021, Pred_common == "Arrowtooth flounder" | Pred_common == "Pacific halibut")

#why does this not match the tables that Cheryl Barnes made?

#General Diet tables
#Arrowtooth Flounder
A_Flounder <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder") %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Arrowtooth flounder")

P_Halibut <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut") %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific halibut")

P_Cod <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod") %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific cod")

W_Pollock <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock") %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Walleye pollock")

Sablefish <- stomach_contents_2021 %>%
  filter(Pred_common == "Sablefish") %>% 
  group_by(Prey_Name) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Sablefish")

raw_diet_table <- rbind(A_Flounder, P_Halibut, P_Cod, W_Pollock, Sablefish)

write.csv(raw_diet_table, here("output/raw_diet_table.csv"), row.names = F)
