library(readr) 
library(tidyverse)
library(here)

#Load Data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"),
         Len_bin = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 80, 280)))

#--------------------------------
#SAMPLE SIZES
#This table shows the number of predator stomachs samples by year, and INPFC area. 
  #It also shows the avg, min, max, and sd lengths by year and region.
Pred_n_by_Year.INPFC <- sc_groupings %>% 
  select(Year, uniqueID, Pred_Species, Pred_common, INPFC_AREA, PRED_LEN, PRED_FULL) %>% 
  distinct(Year, uniqueID, Pred_Species, Pred_common, INPFC_AREA, PRED_LEN, PRED_FULL) %>% 
  group_by(Year, Pred_common, INPFC_AREA) %>% 
  mutate(empty_stomachs = sum(PRED_FULL == 1),
         full_stomachs = sum(PRED_FULL != 1),
         total_stomachs = length(unique(uniqueID)),
         length_min = min(PRED_LEN),
         length_max = max(PRED_LEN),
         length_avg = mean(PRED_LEN),
         length_sd = sd(PRED_LEN)) %>% 
  distinct(Year, Pred_Species, Pred_common, INPFC_AREA, total_stomachs, empty_stomachs, full_stomachs, length_avg, length_min, length_max, length_sd)

Focal_Pred_n_by_Year.INPFC <- Pred_n_by_Year.INPFC %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder")) %>% 
  select(Year, Pred_common, INPFC_AREA,
         empty_stomachs, full_stomachs, total_stomachs,
         length_min, length_max, length_avg, length_sd)

write.csv(Focal_Pred_n_by_Year.INPFC, file = here("output/summary_tables/focalsamplesizeINPFCYear.csv"), row.names = F)

#This table shows the same data as above, but not broken down by INPFC area just by year
Pred_n_Year <- sc_groupings %>% 
  select(Year, uniqueID, Pred_Species, Pred_common, PRED_LEN, PRED_FULL) %>% 
  distinct(Year, uniqueID, Pred_Species, Pred_common, PRED_LEN, PRED_FULL) %>% 
  group_by(Year, Pred_common) %>% 
  mutate(empty_stomachs = sum(PRED_FULL == 1),
         full_stomachs = sum(PRED_FULL != 1),
         total_stomachs = length(unique(uniqueID)),
         length_avg = mean(PRED_LEN),
         length_min = min(PRED_LEN),
         length_max = max(PRED_LEN),
         length_sd = sd(PRED_LEN)) %>% 
  distinct(Year, Pred_Species, Pred_common, empty_stomachs, full_stomachs, total_stomachs, length_avg, length_min, length_max, length_sd)

#This is a simplified table for the manuscript, it shows the total number of predators sampled.
Pred_sample <- sc_groupings %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder")) %>%
  filter(Year >= 1990) %>% 
  select(uniqueID, Pred_common, PRED_LEN, PRED_FULL) %>% 
  distinct(uniqueID, Pred_common, PRED_LEN, PRED_FULL) %>% 
  group_by(Pred_common) %>% 
  mutate(empty_stomachs = sum(PRED_FULL == 1),
         full_stomachs = sum(PRED_FULL != 1),
         total_stomachs = length(unique(uniqueID)),
         length_min = min(PRED_LEN),
         length_max = max(PRED_LEN),
         length_avg = mean(PRED_LEN),
         length_sd = sd(PRED_LEN)) %>% 
  distinct(Pred_common, empty_stomachs, full_stomachs, total_stomachs, length_avg, length_min, length_max, length_sd)

write.csv(Pred_sample, file = here("output/summary_tables/focalsamplesize.csv"), row.names = F)


#--------------------------------
#DIET PROPORTIONS
#This table shows the percent weight and percent occurrence of prey items in the stomachs of all predators sampled
  #by year Pred length and INPFC area
  #I used 30cm length bins with 60+ grouping the largest predators together

diet_proportions_table <- sc_groupings %>% 
  filter(PRED_FULL != 1) %>% #removing empty stomachs
  group_by(Pred_common, Year, INPFC_AREA, Len_bin) %>% 
  mutate(TotalWeight = sum(PREY_TWT), #calculating the total stomach weight for each predator
         TotalPredN = length(unique(uniqueID))) %>%  #calculating the number of predator stomachs 
  group_by(Pred_common, Prey_Name_Clean, Year, INPFC_AREA, Len_bin) %>% 
  mutate(NStomachs = length(unique(uniqueID)), 
         Percent_Occur = NStomachs/TotalPredN*100,
         PreyWeight = sum(PREY_TWT),
         Percent_Weight = (PreyWeight/TotalWeight)*100) %>% 
  select(Year, INPFC_AREA, Pred_Species, Pred_common, Len_bin, Prey_Name_Clean,
         TotalPredN, NStomachs, Percent_Occur,
         TotalWeight, PreyWeight, Percent_Weight)

#This table shows the general diet summary for predators broken down by length
general_diets_focal_len <- sc_groupings %>% 
  filter(PRED_FULL != 1) %>% #removing empty stomachs
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder")) %>%
  group_by(Pred_common, Len_bin) %>% 
  mutate(TotalWeight = sum(PREY_TWT), #calculating the total stomach weight for each predator
         TotalPredN = length(unique(uniqueID))) %>%  #calculating the number of predator stomachs 
  group_by(Pred_common, Prey_Name_Clean, Len_bin) %>% 
  mutate(NStomachs = length(unique(uniqueID)), 
         Percent_Occur = NStomachs/TotalPredN*100,
         PreyWeight = sum(PREY_TWT),
         Percent_Weight = (PreyWeight/TotalWeight)*100) %>% 
  distinct(Pred_common, Prey_Name_Clean, Len_bin, NStomachs, Percent_Occur, Percent_Weight)

write.csv(general_diets_focal_len, file = here("output/summary_tables/general_diets_focal_len.csv"), row.names = F)

#This table shows the general diet summary for the predators not broken down by length
general_diets_focal <- sc_groupings %>% 
  filter(PRED_FULL != 1) %>% #removing empty stomachs
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder")) %>%
  group_by(Pred_common) %>% 
  mutate(TotalWeight = sum(PREY_TWT), #calculating the total stomach weight for each predator
         TotalPredN = length(unique(uniqueID))) %>%  #calculating the number of predator stomachs 
  group_by(Pred_common, Prey_Name_Clean) %>% 
  mutate(NStomachs = length(unique(uniqueID)), 
         Percent_Occur = NStomachs/TotalPredN*100,
         PreyWeight = sum(PREY_TWT),
         Percent_Weight = (PreyWeight/TotalWeight)*100) %>% 
  distinct(Pred_common, Prey_Name_Clean, NStomachs, Percent_Occur, Percent_Weight)

#Select top 50 prey items for each predator
general_diets_focal <- general_diets_focal %>% 
  group_by(Pred_common) %>% 
  arrange(desc(Percent_Weight)) %>% 
  filter(Percent_Weight >= 1)

write.csv(general_diets_focal, file = here("output/summary_tables/general_diets_focal.csv"), row.names = F)

#This table shows the number of focal prey for each predator and each size bin used in models



