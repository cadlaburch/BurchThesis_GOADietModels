#This is my first look at the food habits data
#edit
#load some libraries
library(readr)
library(tidyverse)

#load in the raw data
raw_prey_length <- read_csv("data/1_Gulf Of Alaska_Raw_PreyLength.csv")
raw_stomach_contents <- read_csv("data/1_Gulf Of Alaska_Raw_StomachContents.csv")

#don't know if I need the race data but I found it...
RaceData2015_2019 <- read_csv("data/RaceDatagoa2015_2019.csv")

#which years are included in the data?
unique(raw_prey_length$Year)
  #Answer: 1981 1987 1990 1993 1996 1999 2001 2003 2005 2007 2009 2011 2013 2015 2017 2019
unique(raw_stomach_contents$Year)
  #Answer: 1981 1987 1990 1993 1996 1999 2001 2003 2005 2007 2009 2011 2013 2015 2017 2019 
#They had the same years of collection

#what are the geographic boundaries?
range(raw_prey_length$Rlat)
  #Answer: 52.46, 60.30
range(raw_prey_length$Rlong)
  #Answer: -169.98, -132.68
