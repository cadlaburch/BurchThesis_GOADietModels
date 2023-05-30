#title: "GAM Models Diagnostics"
#author: "Catalina Burch"
#date: "2023-05-24"

#Load libraries
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
library(ggpubr) #for testing normality
library(PerformanceAnalytics) #for testing correlation
library(arm) #for diagnostic binned residual plot
library(faraway) #for hat values
library(modEvA) #for % deviance explained
library(prclust) #for calculating GCV
options(na.action = "na.fail")

#load data
Ddata <- read_csv(here("data/data.csv")) #this data includes empty stomachs

#change year to factor
Ddata$Year <- factor(Ddata$Year)

#Change to binary wide format data
Ddata <- Ddata %>% 
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
WP <- Ddata %>% 
  filter(Pred_common == "Walleye pollock") 

PH <- Ddata %>% 
  filter(Pred_common == "Pacific halibut") 

PC <- Ddata %>% 
  filter(Pred_common == "Pacific cod") 

AF <- Ddata %>% 
  filter(Pred_common == "Arrowtooth flounder") 



#---------------------
## Testing Parameter Normality

#Walleye Pollock
ggdensity(WP$GEAR_DEPTH)
ggqqplot(WP$GEAR_DEPTH)

ggdensity(WP$GEAR_TEMP)
ggqqplot(WP$GEAR_TEMP)

ggdensity(WP$RLONG)
ggqqplot(WP$RLONG)

ggdensity(WP$RLAT)
ggqqplot(WP$RLAT)
#None of the parameters are normal

#Pacific Cod
ggdensity(PC$GEAR_DEPTH)
ggqqplot(PC$GEAR_DEPTH)

ggdensity(PC$GEAR_TEMP)
ggqqplot(PC$GEAR_TEMP)

ggdensity(PC$RLONG)
ggqqplot(PC$RLONG)

ggdensity(PC$RLAT)
ggqqplot(PC$RLAT)

#Arrowtooth flounder
ggdensity(AF$GEAR_DEPTH)
ggqqplot(AF$GEAR_DEPTH)

ggdensity(AF$GEAR_TEMP)
ggqqplot(AF$GEAR_TEMP)

ggdensity(AF$RLONG)
ggqqplot(AF$RLONG)

ggdensity(AF$RLAT)
ggqqplot(AF$RLAT)

#Pacific halibut
ggdensity(PH$GEAR_DEPTH)
ggqqplot(AF$GEAR_DEPTH)

ggdensity(PH$GEAR_TEMP)
ggqqplot(PH$GEAR_TEMP)

ggdensity(PH$RLONG)
ggqqplot(PH$RLONG)

ggdensity(PH$RLAT)
ggqqplot(PH$RLAT)


#--------------------------
#Testing for parameter correlation
#Walleye Pollock
WP_cor <- WP[ , c(4, 5, 6, 7, 8)]
WP_cor_plot <- chart.Correlation(WP_cor, histogram = T, pch = 19, method = "kendall")

#Pacific cod
PC_cor <- PC[ , c(4, 5, 6, 7, 8)]
chart.Correlation(PC_cor, histogram = T, pch = 19, method = "kendall")

#Arrowtooth flounder
AF_cor <- AF[ , c(4, 5, 6, 7, 8)]
chart.Correlation(AF_cor, histogram = T, pch = 19, method = "kendall")

#Pacific halibut
PH_cor <- PH[ , c(4, 5, 6, 7, 8)]
chart.Correlation(PH_cor, histogram = T, pch = 19, method = "kendall")


#--------------------------------------
######### PREY: Euphausiacea #############
#--------------------------------------

#PRED: Walleye pollock
Model <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = WP,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                 fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                 extra = c("adjR^2", "Dsquared"))

WP_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Euphausiacea",
         Predator = "Walleye pollock")

#-------------
#PRED: Pacific Cod
Model <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PC_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Euphausiacea",
         Predator = "Pacific Cod")

#-------------
#PRED: Arrowtooth Flounder
Model <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2","Dsquared"))

AF_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Euphausiacea",
         Predator = "Arrowtooth Flounder")

Euph_AIC <- rbind(WP_AIC, PC_AIC, AF_AIC)






#--------------------------------------
######### PREY: Walleye Pollock #############
#--------------------------------------

#PRED: Pacific halibut
Model <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PH_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Walleye pollock",
         Predator = "Pacific halibut")

#-------------
#PRED: Pacific Cod
Model <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PC_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Walleye pollock",
         Predator = "Pacific cod")

#-------------
#PRED: Arrowtooth flounder
Model <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

AF_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Walleye pollock",
         Predator = "Arrowtooth flounder")

Walleye_AIC <- rbind(PH_AIC, PC_AIC, AF_AIC)


#--------------------------------------
######### PREY: Pandalidae #############
#--------------------------------------

#PRED: Walleye Pollock
Model <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = WP,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

WP_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Pandalidae",
         Predator = "Walleye pollock")

#-------------
#PRED: Pacific Cod
Model <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PC_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Pandalidae",
         Predator = "Pacific cod")

#-------------
#PRED: Arrowtooth flounder
Model <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

AF_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Pandalidae",
         Predator = "Arrowtooth flounder")

pand_AIC <- rbind(WP_AIC, PC_AIC, AF_AIC)





#--------------------------------------
######### PREY: Clupeoid #############
#--------------------------------------

#PRED: Pacific halibut
Model <- gam(Clupeoidei ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PH_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Clupeidei",
         Predator = "Pacific halibut")

#---------------
#PRED: Arrowtooth flounder
Model <- gam(Clupeoidei ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

AF_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Clupeidei",
         Predator = "Arrowtooth flounder")

Clup_AIC <- rbind(PH_AIC, AF_AIC)

#--------------------------------------
######### PREY: Osmerid #############
#--------------------------------------

#PRED: Pacific halibut
Model <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PH_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Osmerid",
         Predator = "Pacific halibut")

#---------------
#PRED: Arrowtooth flounder
Model <- gam(Osmerid ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = AF,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

AF_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Osmerid",
         Predator = "Arrowtooth flounder")

Osmerid_AIC <- rbind(PH_AIC, AF_AIC)


#--------------------------------------
######### PREY: Tanner Crab #############
#--------------------------------------

#PRED: Pacific halibut
Model <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PH_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Tanner crab",
         Predator = "Pacific halibut")

#--------------
#PRED: Pacific cod
Model <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PC_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Tanner crab",
         Predator = "Pacific cod")

Tanner_AIC <- rbind(PH_AIC, PC_AIC)


#--------------------------------------
######### PREY: Paguridae #############
#--------------------------------------

#PRED: Pacific halibut
Model <- gam(Paguridae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PH,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PH_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Paguridae",
         Predator = "Pacific halibut")

#--------------------
#PRED: Pacific cod
Model <- gam(Paguridae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH, k = 4)+ s(GEAR_TEMP, k = 4) + s(PRED_LEN, k = 4),
             data = PC,
             family = binomial(link = logit), #logistic scale
             method = "GCV.Cp")
summary(Model)
deviance(Model)
Dsquared(Model)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Model)
par(mfrow= c(1,1))
binnedplot(fitted(Model), residuals(Model))

#Comparing Delta AIC of alternative Models
Model_fit <- dredge(Model, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                    fixed = c("s(RLONG, RLAT)", "s(PRED_LEN, k = 4)"),
                    extra = c("adjR^2", "Dsquared"))

PC_AIC <- as.data.frame(Model_fit) %>% 
  mutate(Prey = "Paguridae",
         Predator = "Pacific cod")

Pag_AIC <- rbind(PH_AIC, PC_AIC)



#-------------------
#Combining AIC into one table (Table 3)

AIC_all <- rbind(Euph_AIC, Walleye_AIC, pand_AIC, Clup_AIC, Osmerid_AIC, Tanner_AIC, Pag_AIC)

AIC_all <- AIC_all[,c(14, 15, 5, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13)]

AIC_best <- AIC_all %>% 
  filter(delta <= 2)

write.csv(AIC_best, here("output/summary_tables/Table3.csv"), row.names = F)
write.csv(AIC_all, here("output/summary_tables/SupplementaryAIC.csv"), row.names = F)

#Calculating parameter weights (Table 4)
#Calculating Parameter weights
AIC_weight <- AIC_all %>% 
  group_by(Predator, Prey, `s(GEAR_TEMP, k = 4)`) %>% 
  mutate(Gear_Temp = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator,Prey, `s(PRED_LEN, k = 4)`) %>% 
  mutate(Predator_Length = sum(weight)) %>% 
  ungroup() %>%  
  group_by(Predator,Prey, `s(GEAR_DEPTH, k = 4)`) %>% 
  mutate(Gear_Depth = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator,Prey, `s(RLONG, RLAT)`) %>% 
  mutate(Lat_Long = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predator,Prey, Year) %>% 
  mutate(YearP = sum(weight)) 

AIC_weight <- AIC_weight %>% 
  drop_na() %>% 
  distinct(Prey, Predator, Lat_Long, Gear_Depth, Gear_Temp, Predator_Length, YearP)

write.csv(AIC_weight, here("output/summary_tables/Table4.csv"), row.names = F)
