#Author: Catalina Burch
#Date Modified: 5/30/23
#Description: This file creates Figure 3 gear temp partial effects plots

library(here) #for finding working directory
library(readr) #for loading CSV
library(writexl) #for exporting 
library(tidyverse)
library(mgcv) #for running gams
library(MuMIn) #for the dredge summary table
library(patchwork) #for combining plots
library(gridExtra) #for laying out plots
library(visreg) #for visualizing partial effects
library(mapdata) #for partial effects map
options(na.action = "na.fail") 

#load data
Mdata <- read_csv(here("data/data.csv")) #this data includes empty stomachs

#change year to factor
Mdata$Year <- factor(Mdata$Year)

#Change to binary wide format data
Mdata <- Mdata %>% 
  mutate(pres_absent = 1) %>% #create binary presence for each prey item
  distinct(Year, uniqueID, Haul_Join, RLAT, RLONG, GEAR_DEPTH,
           GEAR_TEMP, PRED_LEN, Pred_common, Prey_Name, pres_absent) %>% #remove redundancies, i.e. the same prey species listed twice for the same stomach with different life history stages
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% #create wide dataframe with a column for each prey type
  rename(Walleyepollock = 'Walleye pollock', TannerCrab = 'Tanner Crab',
         Crangonidae = 'Crangonidae (shrimp)', Pandalidae = `Pandalidae (shrimp)`) %>%  #rename (this was an issue for running the prey models below because the model doesn't like a space in the column name)
  group_by(uniqueID) %>% 
  mutate(forage = sum(Osmerid, Clupeoidei),
         forage = ifelse(forage > 0, 1, 0))

#create separate dataframes for each predator
WP <- Mdata %>% 
  filter(Pred_common == "Walleye pollock") 

PH <- Mdata %>% 
  filter(Pred_common == "Pacific halibut") 

PC <- Mdata %>% 
  filter(Pred_common == "Pacific cod") 

AF <- Mdata %>% 
  filter(Pred_common == "Arrowtooth flounder") 

#------------
#PREY: Euphausiid
#Pred: Walleye Pollock
Euph_WP_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = WP,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

Euph_WP <- visreg(Euph_WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
                gg = TRUE, line=list(col="black"), xlab = "", 
                ylab = "Euphausiacea") +
  theme_classic() +
  ggtitle("Walleye pollock")+
  theme(text = element_text(family = "Times New Roman"))


#Pacific Cod
Euph_PC_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

Euph_PC <- visreg(Euph_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                gg = TRUE, line=list(col="black"), xlab = "", ylab = "Euphausiacea") +
  theme_classic() +
  ggtitle("Pacific cod")+
  theme(text = element_text(family = "Times New Roman"))


#Arrowtooth Flounder
Euph_AF_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

Euph_AF <- visreg(Euph_AF_M, "GEAR_TEMP",type = "conditional", scale = "response",
                  gg = TRUE, line=list(col="black"), xlab = "", ylab = "Euphausiacea") +
  theme_classic() +
  ggtitle("Arrowtooth flounder")+
  theme(text = element_text(family = "Times New Roman"))


#------------
#PREY: Pandalidae
#Pacific Cod
Pand_PC_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

Pand_PC <- visreg(Pand_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                gg = TRUE, line=list(col="black"), xlab = "", 
                ylab = "Pandalidae") +
  theme_classic() +
  ggtitle("Pacific cod")+
  theme(text = element_text(family = "Times New Roman"))



#------------
#PREY: Tanner Crab
#Pacific Halibut
TC_PH_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

TC_PH <- visreg(TC_PH_M, "GEAR_TEMP",type = "conditional", scale = "response",
                  gg = TRUE, line=list(col="black"), xlab = "", 
                ylab = "Tanner Crab") +
  theme_classic() +
  ggtitle("Pacific halibut")+
  theme(text = element_text(family = "Times New Roman"))

#Pacific Cod
TC_PC_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

TC_PC <- visreg(TC_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                gg = TRUE, line=list(col="black"), xlab = "", ylab = "Tanner Crab") +
  theme_classic() +
  ggtitle("Pacific cod")+
  theme(text = element_text(family = "Times New Roman"))


#------------
#PREY: Paguridae
#Pacific Cod
Pag_PC_M <- gam(Paguridae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

Pag_PC <- visreg(Pag_PC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                gg = TRUE, line=list(col="black"), xlab = "", 
                ylab = "Paguridae ") +
  theme_classic() +
  ggtitle("Pacific cod")+
  theme(text = element_text(family = "Times New Roman"))

Figure3 <- (Euph_PC + Euph_WP + Euph_AF) /
  (Pand_PC + Pag_PC + plot_spacer()) /
  (TC_PC + TC_PH + plot_spacer()) 

ggsave(plot = Figure3, device = png, path = here("output/Figures"), filename = "Figure3.png", dpi = 300,
       height = 8, width = 7.5)
