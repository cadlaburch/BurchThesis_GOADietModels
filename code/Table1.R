library(readr) 
library(tidyverse)
library(here)

#Load Data
Tdata <- read_csv(here("data/data.csv"))

#################################
#TABLE 1
Table1 <- Tdata %>%
  group_by(Pred_common) %>% 
  mutate(TotalWeight = sum(PREY_TWT), #calculating the total stomach weight for each predator
         TotalPredN = length(unique(uniqueID))) %>%  #calculating the number of predator stomachs 
  ungroup() %>% 
  group_by(Pred_common, gam_grouping) %>% 
  mutate(NStomachs = length(unique(uniqueID)), #number of stomachs from each predator that have prey item present
         Percent_Occur = (NStomachs/TotalPredN)*100,
         PreyWeight = sum(PREY_TWT),
         Percent_Weight = (PreyWeight/TotalWeight)*100) %>% 
  distinct(Pred_common, gam_grouping, TotalPredN, NStomachs, Percent_Occur, Percent_Weight) %>% 
  filter(gam_grouping != "NA") %>% 
  rename('N Pred' = 'TotalPredN', 'N Prey' = 'NStomachs', "%O" = Percent_Occur, "%W" = Percent_Weight)


Table1 <- Table1 %>% 
  pivot_longer(cols = 'N Pred':'%W', names_to = 'Metric') %>% 
  pivot_wider(names_from = gam_grouping, values_from = value)

write.csv(Table1, file = here("output/summary_tables/Table1.csv"), row.names = F)
