#Load libraries
library(readr) 
library(tidyverse)
library(here)

#load data
raw_stomach_contents <- read_csv(here("data/GOA_Raw_StomachContents.csv"))
groupings <- read_csv(here("output/groupings.csv"))

#Calculate percent weight for all years
PW <- raw_stomach_contents %>% 
  filter(Pred_nodc == 8791030701) %>% #filter Walleye Pollock
  group_by(Prey_Name) %>%  #group by prey 
  summarise(TotalWt = sum(Prey_twt), N = n()) %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100),  PN = (N/sum(N))*100)

PW <- raw_stomach_contents %>% 
  filter(Pred_nodc == 8791030401) %>% #filter Pacific Cod
  group_by(Prey_Name) %>%  #group by prey 
  summarise(TotalWt = sum(Prey_twt), N = n()) %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100),  PN = (N/sum(N))*100)

#Filter to prey that are >=1% composition of predator diet
PW_group <- PW %>% 
  filter(PW >= 1)
#I used this information to create a unique prey grouping structure for Walleye Pollock

#join grouping with main data
sc_groupings <- left_join(raw_stomach_contents,groupings,by="Prey_Name")

Pollock <- sc_groupings %>% 
  filter(Pred_nodc == 8791030701)
Cod <- sc_groupings %>% 
  filter(Pred_nodc == 8791030701)

#Binning predator lengths
range(Pollock$Pred_len)

Pollock <- Pollock %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 75)))

#Recalculate PW for each year and length class
Poll_PW <- Pollock %>% 
  filter(Pred_nodc == 8791030701) %>%
  group_by(Year, Len_bin, pollock_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Plot
colorlist<-c('#e6194b', '#f58231', '#ffe119', '#bcf60c', '#46f0f0', 
             '#4363d8', '#911eb4', '#f032e6', '#800000', 
             '#ffd8b1', '#808000', '#fffac8', '#000075',   
             '#aaffc3', '#e6beff', '#a9a9a9', '#fabebe')

ggplot(Poll_PW, aes(x = Len_bin, y = PW, fill = pollock_grouping)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

length(unique(Pollock$pollock_grouping))

ggsave("WalleyePollockByYear.jpg", plot = WalleyePollockByYear, 
       path = here("output"), device = "jpg")


