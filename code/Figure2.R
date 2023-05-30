#Author: Catalina Burch
#Date Modified: 5/21/23
#Description: This file creates Figure 2 stacked bar graph

#Load Libraries
library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)

#Load data
f2data <- read_csv(here("data/data.csv"))

#Calculate Percent weight by species
f2data <- f2data %>% 
  group_by(Pred_common, Len_bin_SB) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Pred_common, Len_bin_SB, stock_groupings) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = length(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

#ordering the length bins
f2data$Len_bin_SB <- factor(f2data$Len_bin_SB, 
                          levels = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80",">70", "81-90", ">80", "91-100", "101-110", "111-120", ">120"))

#Colors for plot
colorlist<-c('#8A0000', "#C80000", '#e34a33', '#fc8d59', '#fdcc8a', '#fef0d9', 'white', 
                      '#f1eef6', '#d0d1e6', '#a6bddb', '#74a9cf', '#3690c0', '#0570b0', '#034e7b')
                      
#Ordering the prey items
f2data$stock_groupings <- factor(f2data$stock_groupings, 
                                 levels = c("Arthropoda", "commercial crab", "copepod", "benthic invertebrate", "euphausiids",
                                            "zooplankton", "other","cod", "flatfish",
                                            "walleye pollock","salmon","rockfish", "forage fish", "other fish"))

#Figure 2 Plot
Figure2 <- f2data %>% 
  ggplot( aes(x = Len_bin_SB, y = PW, fill = stock_groupings)) +
  geom_bar(stat = "identity", show.legend = T) +  #geom_text(aes(x = Len_bin_AF, y = 103, label = n), color = '#b30000') +
  facet_wrap(~Pred_common, scales = "free_x") +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = Figure2, device = png, path = here("output/Figures"), filename = "Figure2.png", dpi = 300,
       height = 6, width = 9)












#Messing around with different visualization
f2data <- read_csv(here("data/data.csv"))

f2data <- f2data %>% 
  group_by(Pred_common, Len_bin_SB) %>% 
  mutate(n = length(unique(uniqueID))) %>% 
  ungroup() %>% 
  group_by(Pred_common, Len_bin_SB, gam_grouping) %>% 
  summarise(TotalWt = sum(PREY_TWT), n = length(n))  %>% 
  mutate(PW = (TotalWt/(sum(TotalWt))*100))

colorlist<-c("#C80000", '#fc8d59','#fdcc8a', '#fef0d9', 'white',
                      '#a6bddb', '#0570b0', '#034e7b')
                      
#ordering the length bins
f2data$Len_bin_SB <- factor(f2data$Len_bin_SB, 
                            levels = c("<20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80",">70", "81-90", ">80", "91-100", "101-110", "111-120", ">120"))
                      
#Ordering the prey items
f2data$gam_grouping <- factor(f2data$gam_grouping, 
                                 levels = c("Tanner crab","Pandalidae", "Euphausiacea", "Paguridae", "Crangonidae",
                                            "other", "Walleye pollock", "Osmeridae", "Clupeidei"))


Figure2Alternative <- f2data %>% 
  ggplot( aes(x = Len_bin_SB, y = PW, fill = gam_grouping)) +
  geom_bar(stat = "identity", show.legend = T) +  #geom_text(aes(x = Len_bin_AF, y = 103, label = n), color = '#b30000') +
  facet_wrap(~Pred_common, scales = "free_x") +
  scale_fill_manual(values = colorlist, name = "Prey Groupings") +
  labs(x = "Predator length (cm)", y = "Percent weight") +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  scale_y_continuous(expand = c(0,2)) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        strip.text = element_text(size = 12, face = "bold"))

#Export figure
ggsave(plot = Figure2Alternative, device = png, path = here("output/Figures"), filename = "Figure2Alternative.png", dpi = 300,
       height = 6, width = 9)
