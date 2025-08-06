library(tidyverse)
library(baseballr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(splines)


leadsnew <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024_Lead_Distances_v2.csv")



# BP/IP pitcher metric , BP comes from statcast
leadsf <- leads %>% 
  select(-Pitcher.y, -Catcher.y, -popcount) %>% 
  mutate(Pitcher = Pitcher.x, Catcher = Catcher.x) %>% 
  select(-Pitcher.x, -Catcher.x)

leadsf <- leadsf %>% 
  select(Date, Home, Away, Inning, TopBottom, Runner1B, PrimaryLead1B, sprint_speed, SB1, CS1, PK1, Pitcher, pitch_hand, Threat, Catcher, poptime, outs, Balls, Strikes, everything()) %>% 
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))
# leadsf: leads with pitcher/catcher/runner metrics + rearranged

# leads1b: filtering
leads1b <- leadsf %>% 
  filter(BaseState == "1--") %>% 
  filter(!is.na(Threat)) %>% 
  filter(!is.na(poptime)) %>% 
  filter(!is.na(sprint_speed)) %>% 
  filter(!is.na(PrimaryLead1B))
  

playerleads1b <- (filter(leadsf, !is.na(PrimaryLead1B))) %>% 
  group_by(Runner1B_ID) %>%
  mutate(playeravg1Blead = mean(PrimaryLead1B)) %>%
  filter(n() >= 15) %>% 
  slice_head(n = 1)
  ungroup()
  
newplayerleads1b <- (filter(leads1b, !is.na(PrimaryLead1B))) %>% 
    group_by(Runner1B_ID) %>%
    mutate(playeravg1Blead = mean(PrimaryLead1B)) %>%
    filter(n() >= 15) %>% 
    slice_head(n = 1)
  ungroup()
  
############### EXTRA ###############
  
#leads <- read.csv("C:\\Users\\jackw\\projects\\Moneyball\\data\\2024_Lead_Distances.csv")
  #view(leads)
  #view(leadsnew)
  #view(leadsf)
  #view(leads1b)
  #view(newplayerleads1b)
# LHP average 10.17
#playerleads1b %>% pull(playeravg1Blead) %>% 
# mean()
#leadsf %>% filter(!is.na(PrimaryLead1B)) %>% 
#  pull(PrimaryLead1B) %>% 
#  mean()
#leads1b %>% filter(!is.na(PrimaryLead1B)) %>% 
#  pull(PrimaryLead1B) %>% 
#  mean()
#view(leadsf %>% filter(!is.na(PrimaryLead1B)) %>% 
#  group_by(BaseState) %>% 
#  reframe(M = mean(PrimaryLead1B)) %>% 
#  ungroup())
#colnames(leads)
#leads1b %>%
#  pull(PrimaryLead1B) %>% 
#  mean()
#leads1b %>%
#  pull(sprint_speed) %>% 
#  mean()
#leads1b %>%
#  pull(Threat) %>% 
#  mean()
#leads1b %>%
#  pull(poptime) %>% 
#  mean()
  
