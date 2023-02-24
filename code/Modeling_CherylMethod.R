library(here) #for finding working directory
library(readr) #for loading CSV
library(tidyverse)

#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
#?????????? I'm not sure why she created another haul identifier when there was already a HaulJoin Column?
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Next she manually assigned INPFC areas to strata areas, but I don't think I have to do this because there already is a INPFC column in my data
strata <- data %>% distinct(STRATUM, INPFC_AREA)
  #when I compare this to Cheryl's INPFC areas they don't match. For example my INPFC Area she grouped stratum 10, 11, 12, 13 into area 610. My data shows that stratum 10, 11 includes 610 and 519, and stratum 12 includes 620 and 610. 

#Rename INPFC areas. Not sure if I need to do this

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#limit data to size classes of interest... how to deal with this...

#set 10cm fork length bins
data <- data %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 29, 39, 49, 59, 69, 1000)))
levels(data$Len_bin) = c("<30", "30-39", "40-49", "50-59", "60-69", ">70")

#remove empty stomachs
data <- data %>% 
  filter(Prey_Name != "Empty")

#I think I need to do a bit of data manipulation to get my data to look more like cheryls which is in a wide format
#I believe that her data is formatted so each line is a predator and the prey items are listed as columns within the pred stomach
#I need to create a unique identifier for each predator
wide_data <- data %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "",
         pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>% 
  distinct(uniqueID, Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
           SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This big chunk is to get rid of redundancies
  pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) 

#check that the each stomach is it's own unique line
length(unique(wide_data$uniqueID))


#Next I need to model the prey occurrence for each focal pred/prey combo

#Model 1 will look at euphausiids in walleye pollock stomachs
Model1 <- wide_data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  select(uniqueID, Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR, 
         SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Euphausiacea)

#remove any NAs
Model1 <- na.omit(Model1)

test <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin,
            data = Model1,
            family = binomial(link = logit), 
            method="GCV.Cp")
 
summary(test)

test_select <- dredge(test, beta = F, evaluate = T, rank = "AIC", trace = F)
print(test_select, abbrev.names = F, warnings = T)
summary(test_select)





