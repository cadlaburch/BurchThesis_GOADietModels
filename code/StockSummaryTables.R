#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

#Joining stomach contents with prey groupings
#methods: groupings was done manually. clean names are all taxonomic classifications
sc_groupings <- left_join(raw_stomach_contents2021,groupings,by="Prey_Name")

#creating sample size table
#Create length bins
stomach_contents_2021 <- sc_groupings %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN,  sep = "_"),
         Len_bin_20 = cut(PRED_LEN, breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200,
                                             220, 240, 260, 280)),
         Len_bin_10 = cut(PRED_LEN, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 
                                               120, 130, 140, 150, 160, 170, 180, 190, 200, 210,
                                               220, 230, 240, 250, 260, 270, 280)))

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
#Methods: I'm removing the empty stomachs
A_Flounder <- stomach_contents_2021 %>%
  filter(Pred_common == "Arrowtooth flounder", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Arrowtooth flounder")

P_Halibut <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific halibut", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific halibut")

P_Cod <- stomach_contents_2021 %>%
  filter(Pred_common == "Pacific cod", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Pacific cod")

W_Pollock <- stomach_contents_2021 %>%
  filter(Pred_common == "Walleye pollock", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Walleye pollock")

Sablefish <- stomach_contents_2021 %>%
  filter(Pred_common == "Sablefish", Prey_Name != "Empty") %>% 
  group_by(Prey_Name_Clean) %>% 
  summarise(TotalWt = sum(PREY_TWT), N = n()) %>% 
  mutate(PW = (TotalWt/sum(TotalWt))*100, PN = (N/sum(N))*100, Pred_common = "Sablefish")

raw_diet_table <- rbind(A_Flounder, P_Halibut, P_Cod, W_Pollock, Sablefish)

write.csv(raw_diet_table, here("output/raw_diet_table.csv"), row.names = F)

#Creating summary figures which includes predator length
#recalculating PW including length bins
Arrowtooth_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Arrowtooth flounder", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_10, stock_arrowtooth) %>% 
  summarise(TotalWt = sum(PREY_TWT))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Halibut_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific halibut", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_10, stock_arrowtooth) %>% 
  summarise(TotalWt = sum(PREY_TWT))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Pollock_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Walleye pollock", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_10, stock_arrowtooth) %>% 
  summarise(TotalWt = sum(PREY_TWT))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Cod_len <- stomach_contents_2021 %>% 
  filter(Pred_common == "Pacific cod", stock_arrowtooth != "empty") %>%
  group_by(Len_bin_10, stock_arrowtooth) %>% 
  summarise(TotalWt = sum(PREY_TWT))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Plotting
colorlist<-c('#e6194b', '#ffe119', '#bcf60c', '#3cb44b', '#46f0f0', #'#f58231',
             '#4363d8', '#911eb4', '#f032e6', '#800000', '#9A6324', 
             '#ffd8b1', '#808000', '#fffac8', '#000075', '#008080',   
             '#aaffc3', '#e6beff', '#a9a9a9', '#fabebe')

Arrow_Stock <- ggplot(Arrowtooth_len, aes(x = Len_bin_10, y = PW, fill = stock_arrowtooth)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Arrowtooth Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

Halibut_Stock <- ggplot(Halibut_len, aes(x = Len_bin_10, y = PW, fill = stock_arrowtooth)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Halibut Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

Pollock_Stock <- ggplot(Pollock_len, aes(x = Len_bin_10, y = PW, fill = stock_arrowtooth)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Pollock Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

Cod_Stock <- ggplot(Cod_len, aes(x = Len_bin_10, y = PW, fill = stock_arrowtooth)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Cod Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

ggsave("Arrow_stock_10.jpg", plot = Arrow_Stock, 
       path = here("output"), device = "jpg")

ggsave("Halibut_stock_10.jpg", plot = Halibut_Stock, 
       path = here("output"), device = "jpg")
