library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)
library(mgcv) #for running gams
library(MuMIn) #for the dredge summary table
library(remotes)
install_version("MuMIn", "1.47.1")


#DATA ASSEMBLY
#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
#?????????? I'm not sure why she created another haul identifier when there was already a HaulJoin Column?
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))


#remove empty stomachs
data <- data %>% 
  filter(Prey_Name != "Empty")

#I think I need to do a bit of data manipulation to get my data to look more like cheryls which is in a wide format
#I believe that her data is formatted so each line is a predator and the prey items are listed as columns within the pred stomach
#I need to create a unique identifier for each predator
wide_data <- data %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "", #create unique pred id
         pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>%
  distinct(uniqueID, Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% 
  rename(Walleyepollock = 'Walleye pollock')


#check that the each stomach is it's own unique line
length(unique(wide_data$uniqueID))

#-------------------------------

#-------------------------------------
#PREY:  EUPHAUSIACEA
  #PRED: Walleye Pollock

WP <- wide_data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  na.omit()

PH <- wide_data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  na.omit()

PC <- wide_data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  na.omit()

AF <- wide_data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  na.omit()

#Euphausiid Function
Model_Euph <- function (Euphausiacea, data) {
  Model1 <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                data = data,
                family = binomial(link = logit),
                method = "GCV.Cp")
  return(Model1)
}

#PRED: Walleye Pollock
Euph_WP_M <- Model_Euph(Euphausiacea, WP)
summary(Euph_WP_M)

#PRED: Pacific Halibut
Euph_PH_M <- Model_Euph(Euphausiacea, PH)
summary(Euph_PH_M)

  #PRED: Pacific cod
Euph_PC_M <- Model_Euph(Euphausiacea, PC)
summary(Euph_PC_M)

  #PRED: Arrowtooth flounder
Euph_AF_M <- Model_Euph(Euphausiacea, AF)
summary(Euph_AF_M)


#-------------------------------
#PREY: Sand Lance
Model_SL <- function (Ammodytidae, data) {
  Model1 <- gam(Ammodytidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                data = data,
                family = binomial(link = logit),
                method = "GCV.Cp")
  return(Model1)
}

#PRED: Walleye Pollock
SL_WP_M <- Model_SL(Ammodytidae, WP)
summary(SL_WP_M)

#PRED: Pacific Halibut
SL_PH_M <- Model_SL(Ammodytidae, PH)
summary(SL_PH_M)

#PRED: Pacific cod
SL_PC_M <- Model_SL(Ammodytidae, PC)
summary(SL_PC_M)

#PRED: Arrowtooth flounder
SL_AF_M <- Model_SL(Ammodytidae, AF)
summary(SL_AF_M)

#-------------------------------
#PREY: Walleye Pollock
Model_WP <- function (Walleyepollock, data) {
  Model1 <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH)+ s(GEAR_TEMP, k = 4) + PRED_LEN,
                data = data,
                family = binomial(link = logit),
                method = "GCV.Cp")
  return(Model1)
}

#PRED: Walleye Pollock
WP_WP_M <- Model_WP(Walleyepollock, WP)
summary(WP_WP_M)

#PRED: Pacific Halibut
WP_PH_M <- Model_WP(Walleyepollock, PH)
summary(WP_PH_M)

#PRED: Pacific cod
WP_PC_M <- Model_WP(Walleyepollock, PC)
summary(WP_PC_M)

#PRED: Arrowtooth flounder
WP_AF_M <- Model_WP(Walleyepollock, AF)
summary(WP_AF_M)


