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
library(PerformanceAnalytics) #for correlation matrix
library(ggpubr) #for normality testing
options(na.action = "na.fail") 


#DATA ASSEMBLY
#Load food habits data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))
data <- raw_stomach_contents2021

#create unique haul identifier
data$Haul_Join <- paste(data$VESSEL, data$CRUISE, data$HAUL, sep = "")

#Exclude data before 1990
data <- data %>% 
  filter(Year >= 1990)

#Select only predators of interest
data <- data %>% 
  filter(Pred_common %in% c("Walleye pollock", "Pacific cod", "Pacific halibut", "Arrowtooth flounder"))

#remove empty stomachs and create unique stomach ID
data <- data %>% 
  filter(Prey_Name != "Empty") %>% 
  mutate(uniqueID = paste(HAULJOIN, PRED_NODC, PRED_SPECN), sep = "")

#change year to factor
data$Year <- factor(data$Year)
data$Haul_Join <- factor(data$Haul_Join)

#Remove deep hauls and data entry error
data <- data %>% 
  filter(GEAR_DEPTH <= 300 & GEAR_DEPTH > 0)

#create day of year (Julien)
# #Note: not currently using this in my model
# data <- data %>%
#   mutate(date = paste(Month, Day, sep = "-"))
# 
# data$date <- as.Date(data$date, "%m-%d")
# data$julien <- format(data$date, "%j")
# data$julien <- as.numeric(data$julien)
# 
# class(data$julien)

# Set coordinate boundaries for plotting:
lonmin = -172
lonmax = -130
latmin = 52
latmax = 62

#Creating a function that transforms the data into wide format based on haul and predator size class.
haul_wide_fun <- function(data) {
  data %>% 
    mutate(pres_absent = ifelse(PREY_TWT > 0, 1, 0)) %>% #create binary presence absence for each prey item
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR,
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, PRED_LEN, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #remove redundancies, i.e. the same prey species listed twice for the same stomach with different life history stages
    group_by(Pred_common, Len_bin, Haul_Join) %>% 
    distinct(Year, Month, Day, Haul_Join, RLAT, RLONG, GEAR_DEPTH, BOTTOM_DEPTH, START_HOUR,
             SURFACE_TEMP, GEAR_TEMP, INPFC_AREA, START_DATE, Len_bin, Pred_common, Prey_Name, pres_absent) %>% #This is to remove redundancies again. I only want one line for each predator in the haul length bin
    pivot_wider(names_from = Prey_Name, values_from = pres_absent, values_fill = list(pres_absent = 0)) %>% #create wide dataframe with a column for each prey type
    rename(Walleyepollock = 'Walleye pollock', TannerCrab = 'Tanner Crab',
           Crangonidae = 'Crangonidae (shrimp)', Pandalidae = `Pandalidae (shrimp)`) %>% #rename (this was an issue for running the walleyepollock prey model below)
    mutate(forage = sum(Osmerid, Clupeoidei),
           forage = ifelse(forage > 0, 1, 0)) %>% 
    na.omit() #remove missing environmental data
}


#_________________
#Create separate dataframes for each predator species with different length bins based on sampling methods

WP <- data %>% 
  filter(Pred_common == "Walleye pollock") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>%
  haul_wide_fun() 
levels(WP$Len_bin) = c("1", "2", "3", "4")

PH <- data %>% 
  filter(Pred_common == "Pacific halibut") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(PH$Len_bin) = c("1", "2", "3", "4")

#Note: I couldn't find the sampling bins for PC so I made it the same as WP
PC <- data %>% 
  filter(Pred_common == "Pacific cod") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 24, 39, 54, 1000))) %>% 
  haul_wide_fun() 
levels(PC$Len_bin) = c("1", "2", "3", "4")

AF <- data %>% 
  filter(Pred_common == "Arrowtooth flounder") %>% 
  mutate(Len_bin = cut(PRED_LEN, breaks = c(0, 31, 50, 70, 1000))) %>% 
  haul_wide_fun()  
levels(AF$Len_bin) = c("1", "2", "3", "4")


#creating dataframe with all predators
all_pred <- rbind(WP, PH, PC, AF)
#-----
# #Sample sizes

samplesize <- matrix(NA, nrow = 4, ncol = 7)
samplesize[1,1] <- "Arrowtooth flounder"
samplesize[2,1] <- "Pacific halibut"
samplesize[3,1] <- "Pacific cod"
samplesize[4,1] <- "Walleye pollock"
colnames(samplesize) <- c("Predator name", "Total stomachs sampled", "Euphausiacea", "Walleye pollock", "Forage Fish", "Pandalidae", "Tanner Crab")


pred_list <- list(AF, PH, PC, WP)
prey_list <- c("Euphausiacea", "Walleyepollock", "Forage", "Pandalidae", "TannerCrab")

for(i in 1:length(pred_list)) {
  for(j in 1:length(prey_list)) {
    samplesize[i, 2] <- nrow(pred_list[[i]]) #total stomachs
    samplesize[i, 3] <- sum(pred_list[[i]]$Euphausiacea)
    samplesize[i, 4]<- sum(pred_list[[i]]$Walleyepollock)
    samplesize[i, 5]<- sum(pred_list[[i]]$forage)
    samplesize[i, 6]<- sum(pred_list[[i]]$Pandalidae)
    samplesize[i, 7]<- sum(pred_list[[i]]$TannerCrab)
  }
}

samplesize <- as.data.frame(samplesize)

write.csv(samplesize, here("output/summary_tables/samplesizemodels.csv"), row.names = F)

#-------------------------------
#######################################
#-------------------------------------
###################################################
#                                                 #  
#            PREY:  EUPHAUSIACEA
#                                                  #
#                                                 #
################################################### 
#Model with predators included as a predictor
#Full Model

Euph <- all_pred %>% 
  filter(Pred_common != "Pacific halibut") 

#Testing for normality
ggdensity(Euph$GEAR_DEPTH)
ggqqplot(Euph$GEAR_DEPTH)

ggdensity(Euph$GEAR_TEMP)
ggqqplot(Euph$GEAR_TEMP)

ggdensity(Euph$RLONG)
ggqqplot(Euph$RLONG)

ggdensity(Euph$RLAT)
ggqqplot(Euph$RLAT)
#None of the parameters are normal 

#Correlation matrix
Euph_cor <- Euph[ , c(5, 6, 7, 11, 16)]
chart.Correlation(Euph_cor, histogram = T, pch = 19, method = "kendall")
cor(Euph$GEAR_DEPTH, Euph$GEAR_TEMP, method = "kendall")
Euph$Len_bin
#Interaction terms
Euph$Euphausiacea
ggplot(data = Euph, aes(x = Len_bin, fill = as.factor(Euphausiacea))) +
  geom_histogram(stat = "count")
Euph$Haul_Join
class(Euph$Haul_Join)

#Run model
Euph_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + (Len_bin*Pred_common),
              data = Euph,
              family = binomial(link = logit), #logistic scale
              method = "GCV.Cp")

Euph_M <- gam(Euphausiacea ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
              data = Euph,
              family = binomial(link = logit), #logistic scale
              method = "GCV.Cp")

summary(Euph_M)
anova(Euph_M)


binnedplot(fitted(Euph_M),residuals(Euph_M))
logitgof(Euph_M$hatched,fitted(Euph_M))
halfnorm(hats <- hatvalues(Euph_M))

#Comparing Delta AIC of alternative Models
Euph_fit <- dredge(Euph_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                   extra = c("adjR^2", "deviance"))

Euph_AIC <- as.data.frame(Euph_fit) %>% 
  mutate(Response = "Euphausiid ", Predators = "Arrowtooth flounder, Pacific cod, Walleye pollock")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Euph_M)#Checking for concurvity 
concurvity(Euph_M, full = T)
#checking for overdispursion. This value should be close to 1. 
sum(residuals(Euph_M, type = "pearson")^2) / df.residual(Euph_M)

#Plotting partial effects
Euph_Plot1 <- visreg(Euph_M, "Year",type = "conditional", scale = "response", #scale creates plot based on probability not log odds
                     gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial effect on Euphausiacea occurrence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

visreg(Euph_M)

Euph_Plot3 <- visreg(Euph_M, "GEAR_TEMP",type = "conditional", scale = "response",
                     gg = TRUE, line=list(col="black"), xlab = "Gear Temp", ylab = "") +
  theme_classic() 

Euph_Plot4 <- visreg(Euph_M, "Len_bin*Pred_common",type = "conditional", scale = "response",
                        gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

Euph_Plot5 <- visreg(Euph_M, "Pred_common",type = "conditional", scale = "response",
                     gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 

plot.new()
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey")
vis.gam(Euph_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
                       contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
                       main="Euphausiacea", too.far=0.025, n.grid=250, 
                       xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)

map <- ggplot() + 
  geom_sf(data = land, fill = "grey") +
  theme_void() + 
  coord_sf(xlim = c(box$xmin, box$xmax),
           ylim = c(box$ymin, box$ymax),
           expand = TRUE) +
  theme(plot.background = element_rect(fill = "transparent",colour = NA))

map + vis.gam(Euph_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Euphausiacea", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)

+geom_polygon(data = map_data ("world"), aes(x=long, y = lat,group=group),fill=NA,color="red",inherit.aes = F) 

Euph_P <- predict.gam(Euph_M)

class(Euph_P)

Euph_P %>%
  ggplot(aes(x = "RLONG", z = fit)) +
  geom_raster(aes(fill = fit)) +
  geom_contour(colour = "white") +
  scale_fill_continuous(name = "Euphausiacea") +
  theme_minimal() +
  theme(legend.position = "top")

Euph_Plot <- Euph_Plot1 + Euph_Plot3

#------------------------------
################################################
#-------------------------------
#----------------
###################################################
#                                                 #  
#            PREY:  Walleye pollock
#                                                  #
#                                                 #
################################################### 

#ALL PRED MODEL
WPrey <- all_pred %>% 
  filter(Pred_common != "Walleye pollock") 

#Testing for normality
ggdensity(WPrey$GEAR_DEPTH)
ggqqplot(WPrey$GEAR_DEPTH)

ggdensity(WPrey$GEAR_TEMP)
ggqqplot(WPrey$GEAR_TEMP)

ggdensity(WPrey$RLONG)
ggqqplot(WPrey$RLONG)

ggdensity(WPrey$RLAT)
ggqqplot(WPrey$RLAT)
#None of the parameters are normal 

#Correlation matrix
WP_cor <- WPrey[ , c(5, 6, 7, 11, 16)]
chart.Correlation(WP_cor, histogram = T, pch = 19, method = "kendall")
cor(WP$GEAR_DEPTH, WP$GEAR_TEMP, method = "kendall")

#Full Model
WP_M <- gam(Walleyepollock ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
            data = WPrey,
            family = binomial(link = logit), #logistic scale
            method = "GCV.Cp")

summary(WP_M)

anova(WP_M)

#Comparing Delta AIC of alternative Models
WP_fit <- dredge(WP_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                 extra = c("adjR^2", "deviance"))

WP_AIC <- as.data.frame(WP_fit) %>% 
  mutate(Response = "Walleye pollock", Predators = "Arrowtooth flounder, Pacific halibut, Pacific cod")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(WP_M)
#Checking for concurvity 
concurvity(WP_M, full = T)
#checking for overdispursion. This value should be close to 1. 
sum(residuals(WP_M, type = "pearson")^2) / df.residual(WP_M)

#Plotting partial effects
WP_Plot1 <- visreg(WP_M, "Year",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial effect on Pollock occurrence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

WP_Plot2 <- visreg(WP_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic()

WP_Plot3 <- visreg(WP_M, "GEAR_TEMP",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Gear Temp", ylab = "Partial effect on Pollock occurrence") +
  theme_classic() 

WP_Plot4 <- visreg(WP_M, "Len_bin",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

WP_Plot5 <- visreg(WP_M, "Pred_common",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


WP_AF_MainP <- (WP_AF_Plot1 + WP_AF_Plot2) / (WP_AF_Plot3 + WP_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_WP.jpg", plot = WP_AF_MainP, device = "jpg", path = here("output/Models"))

plot.new()
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey")
vis.gam(WP_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pollock Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)

plot.new()
vis.gam(Model, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax))
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)


###################################
#-----------------------------------
###################################
###################################################
#                                                 #  
#            PREY:  Forage Fish
#                                                  #
#                                                 #
################################################### 
#All preds

For <- all_pred %>% 
  filter(Pred_common == "Arrowtooth flounder" | Pred_common == "Pacific halibut")

#Testing for normality
ggdensity(For$GEAR_DEPTH)
ggqqplot(For$GEAR_DEPTH)

ggdensity(For$GEAR_TEMP)
ggqqplot(For$GEAR_TEMP)

ggdensity(For$RLONG)
ggqqplot(For$RLONG)

ggdensity(For$RLAT)
ggqqplot(For$RLAT)
#None of the parameters are normal 

#Correlation matrix
For_cor <- For[ , c(5, 6, 7, 11, 16)]
chart.Correlation(For_cor, histogram = T, pch = 19, method = "kendall")
cor(For$GEAR_DEPTH, For$GEAR_TEMP, method = "kendall")

For_M <- gam(forage ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
             data = For,
             family = binomial(link = logit),
             method = "GCV.Cp")

summary(For_M)

anova(For_M)

#Comparing Delta AIC of alternative Models
For_fit <- dredge(For_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                  extra = c("adjR^2", "deviance"))

For_AIC <- as.data.frame(For_fit) %>% 
  mutate(Response = "Forage Fish", Predators = "Arrowtooth flounder, Pacific halibut")


# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(For_M)
#Checking for concurvity 
concurvity(For_M, full = T)
#checking for overdispursion. This value should be close to 1. 
sum(residuals(For_M, type = "pearson")^2) / df.residual(For_M)


#Plotting partial effects
For_Plot1 <- visreg(For_M, "Year",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial effect on Forage Fish occurrence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

# For_Plot2 <- visreg(For_M, "GEAR_DEPTH",type = "conditional", scale = "response",
#                     gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
#   theme_classic() 

For_Plot3 <- visreg(For_M, "GEAR_TEMP",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Gear Temp", ylab = "Partial effect on Forage Fish occurrence") +
  theme_classic() 

For_Plot4 <- visreg(For_M, "Len_bin",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

For_Plot5 <- visreg(For_M, "Pred_common",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


For_MainP <- (For_AF_Plot1 + For_AF_Plot2) / (For_AF_Plot3 + For_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_For.jpg", plot = For_AF_MainP, device = "jpg", path = here("output/Models"))

plot.new()
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey")
vis.gam(For_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Forage Fish Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)



###################################
#-----------------------------------
###################################
###################################################
#                                                 #  
#            PREY:  Pandalidae
#                                                  #
#                                                 #
################################################### 

#All Predators
Pan <- all_pred %>% 
  filter(Pred_common != "Pacific halibut")

#Testing for normality
ggdensity(Pan$GEAR_DEPTH)
ggqqplot(Pan$GEAR_DEPTH)

ggdensity(Pan$GEAR_TEMP)
ggqqplot(Pan$GEAR_TEMP)

ggdensity(Pan$RLONG)
ggqqplot(Pan$RLONG)

ggdensity(Pan$RLAT)
ggqqplot(Pan$RLAT)
#None of the parameters are normal 

#Correlation matrix
Pan_cor <- Pan[ , c(5, 6, 7, 11, 16)]
chart.Correlation(Pan_cor, histogram = T, pch = 19, method = "kendall")
cor(Pan$GEAR_DEPTH, Pan$GEAR_TEMP, method = "kendall")


#Run model
Pan_M <- gam(Pandalidae ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
             data = Pan,
             family = binomial(link = logit),
             method = "GCV.Cp")

summary(Pan_M)

anova(Pan_M)

#Comparing Delta AIC of alternative Models
Pan_fit <- dredge(Pan_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                  extra = c("adjR^2", "deviance"))

Pan_AIC <- as.data.frame(Pan_fit) %>% 
  mutate(Response = "Pandalidae", Predators = "Arrowtooth flounder, Pacific cod, Walleye pollock")

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(Pan_M)
#Checking for concurvity 
concurvity(Pan_M, full = T)
#checking for overdispursion. This value should be close to 1. 
sum(residuals(Pan_M, type = "pearson")^2) / df.residual(Pan_M)


#Plotting partial effects
Pan_Plot1 <- visreg(Pan_M, "Year",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Pandalidae occurrence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45))

# Pan_Plot2 <- visreg(Pan_M, "GEAR_DEPTH",type = "conditional", scale = "response",
#                     gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
#   theme_classic() 

Pan_Plot3 <- visreg(Pan_M, "GEAR_TEMP",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Gear Temp", ylab = "Partial Effect on Pandalidae occurrence") +
  theme_classic() 

Pan_Plot4 <- visreg(Pan_M, "Len_bin",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 

Pan_Plot4 <- visreg(Pan_M, "Pred_common",type = "conditional", scale = "response",
                    gg = TRUE, line=list(col="black"), xlab = "Pred_common", ylab = "") +
  theme_classic() 


Pan_AF_MainP <- (Pan_AF_Plot1 + Pan_AF_Plot2) / (Pan_AF_Plot3 + Pan_AF_Plot4) + 
  plot_annotation(title = "Predator: Arrowtooth Flounder") 

ggsave("AFlounder_eat_Pan.jpg", plot = Pan_AF_MainP, device = "jpg", path = here("output/Models"))

plot.new()
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey")
vis.gam(Pan_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="heat", xlab="Longitude", ylab="Latitude", 
        main="Pandalidae Prescence in AFlounder Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)



###################################
#-----------------------------------
###################################
###################################################
#                                                 #  
#            PREY:  Tanner Crab
#                                                  #
#                                                 #
################################################### 
#ALL PREDS
TC <- all_pred %>% 
  filter(Pred_common == "Pacific cod" | Pred_common == "Pacific halibut")

#Testing for normality
ggdensity(TC$GEAR_DEPTH)
ggqqplot(TC$GEAR_DEPTH)

ggdensity(TC$GEAR_TEMP)
ggqqplot(TC$GEAR_TEMP)

ggdensity(TC$RLONG)
ggqqplot(TC$RLONG)

ggdensity(TC$RLAT)
ggqqplot(TC$RLAT)
#None of the parameters are normal 

#Correlation matrix
TC_cor <- TC[ , c(5, 6, 7, 11, 16)]
chart.Correlation(TC_cor, histogram = T, pch = 19, method = "kendall")
cor(TC$GEAR_DEPTH, TC$GEAR_TEMP, method = "kendall")

#Full Model
TC_M <- gam(TannerCrab ~ Year + s(RLONG, RLAT) + s(GEAR_DEPTH) + s(GEAR_TEMP, k = 4) + Len_bin + Pred_common,
            data = TC,
            family = binomial(link = logit), #logistic scale
            method = "GCV.Cp")

summary(TC_M)

anova(TC_M)

#Comparing Delta AIC of alternative Models
TC_fit <- dredge(TC_M, beta = "none", evaluate = T, rank = "AIC", trace = F, 
                 extra = c("adjR^2", "deviance"))

TC_AIC <- as.data.frame(TC_fit) %>% 
  mutate(Response = "Tanner Crab", Predators = "Pacific halibut, Pacific cod")

write.csv(TC_fit, here("output/Models/Halibut_eat_TC_AIC.csv"), row.names = F)

# Residudal diagnostics
par(mfrow=c(2,2))
gam.check(TC_M)
#Checking for concurvity 
concurvity(TC_M, full = T)
#checking for overdispursion. This value should be close to 1. 
sum(residuals(TC_M, type = "pearson")^2) / df.residual(TC_M)


#Plotting partial effects
TC_Plot1 <- visreg(TC_M, "Year",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Year", ylab = "Partial Effect on Tanner Crab occurrence") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = -45)) 

TC_Plot2 <- visreg(TC_M, "GEAR_DEPTH",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Gear_Depth", ylab = "") +
  theme_classic()

TC_Plot3 <- visreg(TC_M, "GEAR_TEMP",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Gear Temp", ylab = "Partial Effect on Tanner Crab occurrence") +
  theme_classic() 

TC_Plot4 <- visreg(TC_M, "Len_bin",type = "conditional", scale = "response",
                   gg = TRUE, line=list(col="black"), xlab = "Len_bin", ylab = "") +
  theme_classic() 


TC_MainP <- (TC_Plot1 + TC_Plot2) / (TC_Plot3 + TC_Plot4) + 
  plot_annotation(title = "Predator: P Cod") 

plot.new()
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey")
vis.gam(TC_M, c("RLONG", "RLAT"), plot.type = "contour", type="response", 
        contour.col="black", color="white", xlab="Longitude", ylab="Latitude", 
        main="Tanner Crab Prescence in Halibut Stom", too.far=0.025, n.grid=250, 
        xlim=c(lonmin, lonmax), ylim=c(latmin, latmax), add = T)
map('worldHires', fill=T, xlim=c(-172, -130), ylim=c(52, 62), col="lightgrey", add = T)



############################
#---------------------------
#############################

#Combining AIC Tables

global_AIC <- rbind(Euph_AIC, WP_AIC, For_AIC, Pan_AIC, TC_AIC) %>% 
  mutate(deviance = deviance/100)

global_AIC <- global_AIC[,c(16, 15, 5, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14)]


#Calculating Parameter weights
AIC <- global_AIC %>% 
  group_by(Predators, Response, `s(GEAR_TEMP, k = 4)`) %>% 
  mutate(Gear_Temp = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predators, Response, Len_bin) %>% 
  mutate(Length_Bin = sum(weight)) %>% 
  ungroup() %>%  
  group_by(Predators, Response, `s(GEAR_DEPTH)`) %>% 
  mutate(Gear_Depth = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predators, Response, Pred_common) %>% 
  mutate(Preds = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predators, Response, `s(RLONG, RLAT)`) %>% 
  mutate(Lat_Long = sum(weight)) %>% 
  ungroup() %>% 
  group_by(Predators, Response, Year) %>% 
  mutate(YearP = sum(weight)) %>% 
  ungroup() %>% 
  filter(delta <=2) 

AIC$Pred_common

parameter_weights <- AIC %>% 
  drop_na() %>% 
  distinct(Predators, Response, Gear_Temp, Gear_Depth, Lat_Long, Year, Length_Bin, Preds)

write.csv(AIC, here("output/Models/Global_AIC_PredswDepth.csv"), row.names = F)
write.csv(parameter_weights, here("output/Models/parameter_weights_PredswDepth.csv"), row.names = F)


#Calculating parameter weights
AIC_list <- list(Euph_AF_AIC, Euph_AF_AIC)