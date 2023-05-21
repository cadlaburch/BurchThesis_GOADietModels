library(readr) 
library(tidyverse)
library(here)

#Load Data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021
groupings <- read_csv(here("output/groupings.csv"))

#Filter the data to only include observations used in modeling
#create unique haul identifier
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#remove empty stomachs and create unique stomach ID
data <- data %>% 
  filter(Prey_Name != "Empty") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

#change year to factor
data$Year <- factor(data$Year)

#Remove deep hauls and data entry error
data <- data %>% 
  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

#Join data with cleaned names
sc_groupings <- left_join(data,groupings,by="Prey_Name")


#################################
# TABLE 2
WP <- sc_groupings %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod")) %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000)))
levels(WP$Len_bin) = c("1", "2", "3", "4")

PH <- sc_groupings %>% 
  filter(Pred_common %in% c("Pacific halibut", "Arrowtooth flounder")) %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) 
levels(PH$Len_bin) = c("1", "2", "3", "4")


#creating dataframe with all predators
sc_groupings <- rbind(WP, PH)


Pred_sample <- sc_groupings %>% 
  select(uniqueID, Pred_common, PRED_LEN, Len_bin) %>% 
  distinct(uniqueID, Pred_common, PRED_LEN, Len_bin) %>%
  group_by(Pred_common, Len_bin) %>% 
  mutate(Len_bin_n = length(Len_bin)) %>% 
  ungroup() %>% 
  group_by(Pred_common) %>% 
  mutate(total_stomachs = length(unique(uniqueID)),
         length_min = min(PRED_LEN),
         length_max = max(PRED_LEN),
         length_avg = mean(PRED_LEN),
         length_sd = sd(PRED_LEN)) %>% 
  distinct(Pred_common, Len_bin, Len_bin_n, total_stomachs, length_avg, length_min, length_max, length_sd)

Table2 <- pivot_wider(Pred_sample, names_from = Len_bin, values_from = Len_bin_n) 

write.csv(Table2, file = here("output/summary_tables/Table2.csv"), row.names = F)


#################################
#TABLE 3
Table3 <- sc_groupings %>%
  group_by(Pred_common) %>% 
  mutate(TotalWeight = sum(PREY_TWT), #calculating the total stomach weight for each predator
         TotalPredN = length(unique(uniqueID))) %>%  #calculating the number of predator stomachs 
  group_by(Pred_common, gam_grouping) %>% 
  mutate(NStomachs = length(unique(uniqueID)), 
         Percent_Occur = NStomachs/TotalPredN*100,
         PreyWeight = sum(PREY_TWT),
         Percent_Weight = (PreyWeight/TotalWeight)*100) %>% 
  distinct(Pred_common, gam_grouping, NStomachs, Percent_Occur, Percent_Weight) %>% 
  filter(gam_grouping != "NA")

Table3 <- Table3 %>% 
  pivot_longer(cols = NStomachs:Percent_Weight, names_to = 'Metric') %>% 
  pivot_wider(names_from = gam_grouping, values_from = value)

write.csv(Table3, file = here("output/summary_tables/Table3.csv"), row.names = F)
