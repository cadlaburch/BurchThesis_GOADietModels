#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(patchwork)
library(igraph)
library(ggraph)

#Load Data
raw_stomach_contents2021 <- read_csv(here("data/GOA_Raw_StomachContents2021.csv"))

#What years are in the data?
unique(raw_stomach_contents2021$Year)

#optional only consider main 4 predators?
raw_stomach_contents2021 <- raw_stomach_contents2021 %>% 
  filter(Pred_common == c("Walleye pollock", "Pacific halibut", "Arrowtooth flounder", "Pacific cod"))

#1981
sc_1981 <- raw_stomach_contents2021 %>% 
  filter(Year == 1981)
sc_1981_w <- raw_stomach_contents2021 %>% 
  filter(Year == 1981, RLONG < -147)
sc_1981_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1981, RLONG >= -147)

#1987
sc_1987 <- raw_stomach_contents2021 %>% 
  filter(Year == 1987)
sc_1987_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1987, RLONG < -147)
sc_1987_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1987, RLONG >= -147)

#1990
sc_1990 <- raw_stomach_contents2021 %>% 
  filter(Year == 1990)
sc_1990_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1990, RLONG < -147)
sc_1990_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1990, RLONG >= -147)

#1993
sc_1993 <- raw_stomach_contents2021 %>% 
  filter(Year == 1993)
sc_1993_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1993, RLONG < -147)
sc_1993_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1993, RLONG >= -147)

#1996
sc_1996 <- raw_stomach_contents2021 %>% 
  filter(Year == 1996)
sc_1996_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1996, RLONG < -147)
sc_1996_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1996, RLONG >= -147)

#1999
sc_1999 <- raw_stomach_contents2021 %>% 
  filter(Year == 1999)
sc_1999_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1999, RLONG < -147)
sc_1999_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1999, RLONG >= -147)

#2001
sc_2001 <- raw_stomach_contents2021 %>% 
  filter(Year == 2001)
sc_2001_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2001, RLONG < -147)
sc_2001_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2001, RLONG >= -147)

#2003
sc_2003 <- raw_stomach_contents2021 %>% 
  filter(Year == 2003)
sc_2003_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2003, RLONG < -147)
sc_2003_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2003, RLONG >= -147)

#2005
sc_2005 <- raw_stomach_contents2021 %>% 
  filter(Year == 2005)
sc_2005_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2005, RLONG < -147)
sc_2005_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2005, RLONG >= -147)

#2007
sc_2007 <- raw_stomach_contents2021 %>% 
  filter(Year == 2007)
sc_2007_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2007, RLONG < -147)
sc_2007_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2007, RLONG >= -147)

#2009
sc_2009 <- raw_stomach_contents2021 %>% 
  filter(Year == 2009)
sc_2009_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2009, RLONG < -147)
sc_2009_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2009, RLONG >= -147)

#2011
sc_2011 <- raw_stomach_contents2021 %>% 
  filter(Year == 2011)
sc_2011_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2011, RLONG < -147)
sc_2011_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2011, RLONG >= -147)

#2013
sc_2013 <- raw_stomach_contents2021 %>% 
  filter(Year == 2013)
sc_2013_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2013, RLONG < -147)
sc_2013_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2013, RLONG >= -147)

#2015
sc_2015 <- raw_stomach_contents2021 %>% 
  filter(Year == 1981)
sc_2015_w<- raw_stomach_contents2021 %>% 
  filter(Year == 1981, RLONG < -147)
sc_2015_e<- raw_stomach_contents2021 %>% 
  filter(Year == 1981, RLONG >= -147)

#2017
sc_2017 <- raw_stomach_contents2021 %>% 
  filter(Year == 2017)
sc_2017_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2017, RLONG < -147)
sc_2017_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2017, RLONG >= -147)

#2019
sc_2019 <- raw_stomach_contents2021 %>% 
  filter(Year == 2019)
sc_2019_w<- raw_stomach_contents2021 %>% 
  filter(Year == 2019, RLONG < -147)
sc_2019_e<- raw_stomach_contents2021 %>% 
  filter(Year == 2019, RLONG >= -147)

#2021
sc_2021 <- raw_stomach_contents2021 %>% 
  filter(Year == 2021)
sc_2021_w <- raw_stomach_contents2021 %>% 
  filter(Year == 2021, RLONG < -147)
sc_2021_e <- raw_stomach_contents2021 %>% 
  filter(Year == 2021, RLONG >= -147)


#2021
edges.df <- raw_stomach_contents2021 %>% 
  filter(Prey_Name != "Empty", Year == 2021) %>% 
  distinct(Pred_Species, Prey_Name) %>% 
  select(Pred_Species, Prey_Name)

el_2021 <- graph.edgelist(as.matrix(edges.df), directed = T)

ggraph(el_2021) +
  geom_edge_link() +
  geom_node_point()

griz.fem <- grizzly$Est_Total_Females

rt_output <- rep(x = NA, times = length(griz.fem))





