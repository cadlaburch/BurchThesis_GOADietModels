#Load libraries
library(readr) 
library(tidyverse)
library(here)
library(taxize)

#load data
raw_stomach_contents <- read_csv(here("data/GOA_Raw_StomachContents.csv"))

#################
#Adding Common Names for Predators
#warning this chunk will take time to run. 
pred_names <- raw_stomach_contents %>% 
  distinct(Pred_name)

pred_list<-(sci2comm(pred_names$Pred_name, simplify = T, db = "ncbi"))
pred_names_df <- rownames_to_column(as.data.frame(do.call(rbind, pred_list)))
colnames(pred_names_df) <- c("Pred_name", "Com_name")

#merging common names with overall dataframe
stomach_contents <- full_join(raw_stomach_contents, pred_names_df, by = "Pred_name")

#There's a couple of missing common names this is QC to correct that


###############
#Creating a table that shows sample sizes for each predator by year
sample_size <- stomach_contents %>%
  select(Pred_name, Com_name, Year) %>% 
  group_by(Pred_name, Com_name, Year) %>% 
  summarize(n = n())

#Plot so that it is easy to visualize
samplesize <- ggplot(sample_size, aes(x = Year, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Com_name, scales = "free_y") +
  theme_minimal() +
  scale_x_continuous(breaks = c(1981, 1987, 1990, 1993, 1996, 1999,
                                2001, 2003, 2005, 2007, 2009, 2011, 
                                2013, 2015, 2017, 2019)) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.minor = element_blank())

ggsave("samplesize.jpg", plot = samplesize, height = 20, width = 30, limitsize = F,
       path = here("output"), device = "jpg")
