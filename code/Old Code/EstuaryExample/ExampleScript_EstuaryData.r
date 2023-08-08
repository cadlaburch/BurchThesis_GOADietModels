###########################################
### Analysis of Juneau Beach Seine Data ###
### Example for Catalina                ###
### Anne Beaudreau                      ###
###########################################

# Set working directory to code source folder
#library(rstudioapi)
#setwd(dirname(getActiveDocumentContext()$path))

# Load R libraries
#source('helper.R')

# Read in data and format date field
setdata <- read.csv(here(("code/EstuaryExample/setdata.csv")))
setdata$Date <- as.Date(setdata$Date, "%Y-%m-%d")
glimpse(setdata)

eventdata <- read.csv(here("code/EstuaryExample/eventdata.csv"))
eventdata$Date <- as.Date(eventdata$Date, "%Y-%m-%d")
glimpse(eventdata)

# Convert year to factor
setdata$Year <- factor(setdata$Year)
eventdata$Year <- factor(eventdata$Year)

# Eliminate Sawmill & Berners (4 and 3 samples, respectively)
setdata <- filter(setdata, Site != "SaC" & Site != "BB")
eventdata <- filter(eventdata, Site != "SaC" & Site != "BB")

# Reorder sites from least to most glacial cover
levels(setdata$Site)
setdata$Site <- factor(setdata$Site, levels = c("SaC","ShC","CC","LC","ER","MR","BB"))
levels(setdata$Site)
levels(setdata$Site) <- c("Sawmill Creek","Sheep Creek","Cowee Creek","Lemon Creek","Eagle River","Mendenhall River","Berners Bay")

levels(eventdata$Site)
eventdata$Site <- factor(eventdata$Site, levels = c("SaC","ShC","CC","LC","ER","MR","BB"))
levels(eventdata$Site)
levels(eventdata$Site) <- c("Sawmill Creek","Sheep Creek","Cowee Creek","Lemon Creek","Eagle River","Mendenhall River","Berners Bay")

# Remove Lemon Creek before modeling
setdata %>%
  filter(!Site %in% c("Lemon Creek")) -> setdata

#######################################
## Generalized additive mixed models ##
#######################################

# GAMMs: https://m-clark.github.io/generalized-additive-models/technical.html
library(mgcv)
library(tidyverse)
library(visreg)
library(MuMIn)

## Use 'dredge' to fit all possible combinations of factors: Run models for SppRichness
# GAMM - full model for species richness
richness.full <- gam(log(SppRichness) ~ -1 + Site + s(Julian, k = 5) + s(Julian, by = Site, k = 5) + 
                       NetReclass + s(Year, bs = "re"), data = setdata, na.action = "na.fail")
# Reparameterized to remove intercept
richness.full <- gam(log(SppRichness) ~ Site + s(Julian, k = 5) + s(Julian, by = Site, k = 5) + 
                       NetReclass + s(Year, bs = "re"), data = setdata, na.action = "na.fail")
fit <- dredge(richness.full)
fit #this gives the full output table with model diagnostics (incl. AICc)

summary(richness.full)

# Residudal diagnostics (DO THIS ON MY MODELS)
par(mfrow=c(2,2))
gam.check(richness.full)

# Visualize partial effects
visreg(richness.full, "Julian",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Day of Year", 
       ylab = "Standardized Taxa Richness (ln(taxa/min))")
visreg(richness.full, "NetReclass",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Net Type", 
       ylab = "Standardized Taxa Richness (ln(taxa/min))")
visreg(richness.full, "Year",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Year", 
       ylab = "Standardized Taxa Richness (ln(taxa/min))")
visreg(richness.full, "Site",type = "conditional", scale = "response",
       gg = TRUE, line=list(col="black"), xlab = "Site", 
       ylab = "Standardized Taxa Richness (ln(taxa/min))")

p <- visreg(richness.full, "Julian", by = "Site", type = "conditional", 
            scale = "response", gg = TRUE, line=list(col="black"), xlab = "Day of Year",
            ylab = "Standardized Taxa Richness (ln(taxa/min))", rug = 1)
p + facet_wrap(vars(Site), nrow = 2)

## https://journal.r-project.org/archive/2017/RJ-2017-046/RJ-2017-046.pdf
