library(dplyr) # v0.7.5
library(rlang) # v0.2.1
library(purrr)

pollock_1999 <- raw_stomach_contents %>% 
  filter(Year == 1999, Pred_nodc == 8791030701)

PW_PN <- function(.data) {
  .data %>% 
  group_by(Prey_Name) %>% 
    summarise(TotalWt = sum(Prey_twt), N = n()) %>% 
    mutate("%W" = (TotalWt/(sum(TotalWt))*100), "%N" = (N/sum(N))*100)
}

pollock_1999 <- raw_stomach_contents %>% 
  filter(Year == 1999, Pred_nodc == 8791030701) %>% 
  PW_PN()



pollock <- raw_stomach_contents %>% 
  filter(Year == 1999, Pred_nodc == 8791030701)
pollocks <- list()

for(i in c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019))
  {
  pollocks[[i]] <- pollock %>% 
    filter(Year == i) %>% 
    group_by(Prey_Name) %>% 
    summarize(TotalWt = sum(Prey_twt), N = n())
}



temp<- tibble()

for (i in c(1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019))
     {
    temp[[i]] <- raw_stomach_contents %>% 
       filter(Year == i, Pred_nodc == 8791030701) %>% #filter for year and Walleye Pollock
       group_by(Prey_Name) %>%  #group by prey 
       mutate(TotalWt = sum(Prey_twt), N = n()) %>% 
       mutate("%W" = (TotalWt/(sum(TotalWt))*100), "%N" = (N/sum(N))*100)
}

print(pollock$TotalWt)
     
     F_poll_(i) <- raw_stomach_contents %>% 
       filter(Year == i, Pred_nodc == 8791030701) %>% 
       group_by(Prey_Name) %>% 
       summarise(F = sum(unique(Pred_specn)*0+1)) %>% 
       mutate("%F" = (F/540)*100, year = 2001)
     
     #merge dataframes
     poll_(i) <- full_join(WN_poll_i, F_poll_i, by = "Prey_Name")
     }

summary <- function(summary, n){
  summary <- raw_stomach_contents %>% 
    filter(Year == n, Pred_nodc == 8791030701) %>% #filter for year and Walleye Pollock
    group_by(Prey_Name) %>%  #group by prey 
    mutate(TotalWt = sum(Prey_twt), N = n()) %>% 
    mutate("%W" = (TotalWt/(sum(TotalWt))*100), "%N" = (N/sum(N))*100)
}



