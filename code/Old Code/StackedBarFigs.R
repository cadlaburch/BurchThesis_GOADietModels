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

PW <- raw_stomach_contents %>% 
  filter(Pred_nodc == 8827020101) %>% #filter Sablefish
  group_by(Prey_Name) %>%  #group by prey 
  summarise(TotalWt = sum(Prey_twt), N = n()) %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100),  PN = (N/sum(N))*100)

PW <- raw_stomach_contents %>% 
  filter(Pred_nodc == 8857041901) %>% #filter Halibut
  group_by(Prey_Name) %>%  #group by prey 
  summarise(TotalWt = sum(Prey_twt), N = n()) %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100),  PN = (N/sum(N))*100)

#Filter to prey that are >=1% composition of predator diet
PW_group <- PW %>% 
  filter(PW >= 2)
#I used this information to create a unique prey grouping structure for Walleye Pollock

#join grouping with main data
sc_groupings <- left_join(raw_stomach_contents,groupings,by="Prey_Name")

Pollock <- sc_groupings %>% 
  filter(Pred_nodc == 8791030701)
Cod <- sc_groupings %>% 
  filter(Pred_nodc == 8791030401)
Sablefish <- sc_groupings %>% 
  filter(Pred_nodc == 8827020101)
Halibut <- sc_groupings %>% 
  filter(Pred_nodc == 8857041901)

#Binning predator lengths
quantile(Pollock$Pred_len)
quantile(Cod$Pred_len)
quantile(Sablefish$Pred_len)
quantile(Halibut$Pred_len)

Pollock <- Pollock %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 37, 47, 54, 75)))
Pollock <- Pollock %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 75)))
Cod <- Cod %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 45, 55, 62, 105)))
Cod <- Cod %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 20, 30, 40, 50, 60, 75)))
Sablefish <- Sablefish %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 52, 59, 65, 101)))
Sablefish <- Sablefish %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(20, 40, 60, 80, 101)))
Halibut <- Halibut %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 46, 59, 72, 167)))
Halibut <- Halibut %>%
  mutate(Len_bin = cut(Pred_len, breaks = c(0, 30, 60, 90, 120, 150, 167)))

#Recalculate PW for each year and length class
Pollock_year <- Pollock %>% 
  filter(pollock_grouping != "Empty") %>%
  group_by(Year, Len_bin, pollock_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Pollock_sum <- Pollock %>% 
  filter(pollock_grouping != "Empty") %>%
  group_by(Len_bin, pollock_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Cod_year <- Cod %>% 
  filter(cod_grouping != "Empty") %>%
  group_by(Year, Len_bin, cod_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Cod_sum <- Cod %>% 
  filter(cod_grouping != "Empty") %>%
  group_by(Len_bin, cod_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Sablefish_year <- Sablefish %>% 
  filter(sablefish_grouping != "Empty") %>%
  group_by(Year, Len_bin, sablefish_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Sablefish_sum <- Sablefish %>% 
  filter(sablefish_grouping != "Empty") %>%
  group_by(Len_bin, sablefish_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Halibut_year <- Halibut %>% 
  filter(halibut_grouping != "Empty") %>%
  group_by(Year, Len_bin, halibut_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

Halibut_sum <- Halibut %>% 
  filter(halibut_grouping != "Empty") %>%
  group_by(Len_bin, halibut_grouping) %>% 
  summarise(TotalWt = sum(Prey_twt))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#Plot

colorlist<-c('#e6194b', '#f58231', '#ffe119', '#bcf60c', '#3cb44b', '#46f0f0', 
             '#4363d8', '#911eb4', '#f032e6', '#800000', '#9A6324', 
             '#ffd8b1', '#808000', '#fffac8', '#000075', '#008080',   
             '#aaffc3', '#e6beff', '#a9a9a9', '#fabebe')

PollockStackedByYear <- ggplot(Pollock_sum, aes(x = Len_bin, y = PW, fill = pollock_grouping)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

PollockStacked <- ggplot(Pollock_sum, aes(x = Len_bin, y = PW, fill = pollock_grouping)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Walleye Pollock Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

length(unique(Pollock$pollock_grouping))

ggsave("PollockStackedByYearQ.jpg", plot = PollockStackedByYear, 
       path = here("output"), device = "jpg")

ggsave("PollockStacked.jpg", plot = PollockStacked, 
       path = here("output"), device = "jpg")

CodStackedByYear <- ggplot(Cod, aes(x = Len_bin, y = PW, fill = cod_grouping)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "P Cod By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

CodStacked <- ggplot(Cod_sum, aes(x = Len_bin, y = PW, fill = cod_grouping)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "P Cod Diet", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

ggsave("CodStackedByYearQ.jpg", plot = CodStackedByYear, 
       path = here("output"), device = "jpg")

ggsave("CodStacked.jpg", plot = CodStacked, 
       path = here("output"), device = "jpg")

SablefishStackedByYear <- ggplot(Sablefish_year, aes(x = Len_bin, y = PW, fill = sablefish_grouping)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Sablefish By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

SablefishStacked <- ggplot(Sablefish_sum, aes(x = Len_bin, y = PW, fill = sablefish_grouping)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = colorlist) +
  labs(title = "Sablefish By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

ggsave("SablefishStackedByYearQ.jpg", plot = SablefishStackedByYear, 
       path = here("output"), device = "jpg")

ggsave("SablefishStacked.jpg", plot = SablefishStacked, 
       path = here("output"), device = "jpg")

HalibutStackedByYear <- ggplot(Halibut_year, aes(x = Len_bin, y = PW, fill = halibut_grouping)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~Year) +
  scale_fill_manual(values = colorlist) +
  labs(title = "Halibut By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

HalibutStacked <- ggplot(Halibut_sum, aes(x = Len_bin, y = PW, fill = halibut_grouping)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colorlist) +
  labs(title = "Halibut By Year", y = "percent weight", 
       x = "predator length") +
  theme_minimal()

ggsave("HalibutStackedByYearQ.jpg", plot = HalibutStackedByYear, 
       path = here("output"), device = "jpg")

ggsave("HalibutStacked.jpg", plot = HalibutStacked, 
       path = here("output"), device = "jpg")
