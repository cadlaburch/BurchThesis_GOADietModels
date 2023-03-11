#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(broom)

#Load Data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
groupings <- read_csv(here("output/groupings.csv"))

glm.data <- raw_stomach_contents2021 %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

#linear models
lm <- lm(formula = PRED_LEN ~ PRED_WT, data = raw_stomach_contents2021)
print(lm)
summary(lm)
tidy(lm)
coef(lm) #coefficient values
confint(lm) #confidence intervals
predict(lm) #predicts values based on model, add , new_data argument to predict new data based on model


#-------------------
#This chunk is to practice running one glm on binary data
#Eupausiids in Walleye Pollock stomachs
pred_poll_prey_euph <- glm.data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(PA = ifelse(Prey_Name == "Euphausiacea", 1, 0)) %>% 
  group_by(uniqueID) %>% 
  mutate(PA = sum(PA)) %>% 
  distinct(Year, Month, Day, HAUL, RLAT, RLONG, GEAR_DEPTH, GEAR_TEMP, INPFC_AREA, uniqueID, PRED_LEN, PA) %>%
  mutate(PA = ifelse(PA == 0, 0, 1))

#check that I don't have any reoccuring stomachs
length(unique(pred_poll_prey_euph$uniqueID)) #this matches the number of rows in the data

#Run a logistic regression
binary.glm <- glm(PA ~ GEAR_TEMP, data = pred_poll_prey_euph, family = 'binomial')
summary(binary.glm)


#Create a loop that will run a glm for each predator and prey in the data
wp.vector <- raw_stomach_contents2021 %>% 
  select(Prey_Name) %>% 
  distinct(Prey_Name) %>% 
  mutate(Pred_common = "Walleye pollock")

ph.vector <- raw_stomach_contents2021 %>% 
  select(Prey_Name) %>% 
  distinct(Prey_Name) %>% 
  mutate(Pred_common = "Pacific halibut")

af.vector <- raw_stomach_contents2021 %>% 
  select(Prey_Name) %>% 
  distinct(Prey_Name) %>% 
  mutate(Pred_common = "Arrowtooth flounder")

pc.vector <- raw_stomach_contents2021 %>% 
  select(Prey_Name) %>% 
  distinct(Prey_Name) %>% 
  mutate(Pred_common = "Pacific cod")

names.loop <- rbind(wp.vector, ph.vector, af.vector, pc.vector)

pred.names <- names.loop %>% 
  select(Pred_common)
prey.names <- names.loop %>% 
  select(Prey_Name)

pred.names <- pred.names$Pred_common
prey.names <- prey.names$Prey_Name


glm.list <- vector('list', 360)

          start.time <- Sys.time()

for(i in 1:length(pred.names)){
  for(j in 1:length(prey.names)){
    pred_prey_data <- glm.data %>% 
      filter(Pred_common == pred.names[i]) %>% 
      mutate(PA = ifelse(Prey_Name == prey.names[j], 1, 0)) %>% 
      group_by(uniqueID) %>% 
      mutate(PA = sum(PA)) %>% 
      distinct(Year, Month, Day, HAUL, RLAT, RLONG, GEAR_DEPTH, GEAR_TEMP, INPFC_AREA, uniqueID, PRED_LEN, PA) %>%
      mutate(PA = ifelse(PA == 0, 0, 1))
    
    
    #Run a logistic regression
    glm.list[[i]] <- glm(PA ~ GEAR_TEMP, data = pred_prey_data, family = 'binomial')
    
  }
}

          end.time <-Sys.time()
          time.taken <- round(end.time - start.time, 2)
          time.taken 
#
          
          
          

