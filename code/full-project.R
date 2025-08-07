library(tidyverse)
library(baseballr)
library(purrr)
library(ggplot2)
library(ggpubr)
library(splines)

### Read in data (Replace with your own file path)

#leadsnew: play-level lead distance data. here "play" means pitch / pickoff att, designated by Play col
leadsnew <- read.csv("data/raw/lead-distances.csv")

#Net Bases Prevented data from Savant (player-level)
nbpdata <- read.csv("data/raw/net-bases-prevented.csv")

#Innings Pitched data from Savant (player-level)
ipdata <- read.csv("data/raw/innings-pitched.csv")

#Sprint Speed data from Savant (player-level)
ssdata <- read.csv("data/raw/sprint-speed.csv")

#Catcher pop time data from Savant (player-level)
ptdata <- read.csv("data/raw/pop-time.csv")

### Filter and left_join Baseball Savant data into leadsnew

nbpdata <- nbpdata %>% 
  reframe(player_id = player_id, player_name = player_name, NBP = net_attr_plus + net_attr_minus)
ipdata <- ipdata %>% 
  left_join(x = ipdata, y = nbpdata, by = c("player_id" = "player_id"))
ipdata <- ipdata %>% 
  reframe(PitcherID = player_id, Pitcher = player_name, NBP = NBP, IP = round(p_formatted_ip, 1))
ipdata <- ipdata %>% 
  mutate(IP = round(IP, 0)) %>% 
  mutate(Threat = 100 * NBP / IP) %>% 
  filter(IP >= 10)
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))
ptdata <- ptdata %>% 
  reframe(Catcher = entity_name, CatcherID = entity_id, poptime = pop_2b_sba, popcount = pop_2b_sba_count)
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))
ssdata <- ssdata %>% 
  reframe(Runner1B_ID = player_id, sprint_speed = sprint_speed)
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))

### Make new filtered datasets

# leadsnewf: fixed/cleaned version of leadsnew with columns rearranged

leadsnewf <- leadsnew %>% 
  select(-Pitcher.y, -Catcher.y, -popcount.x) %>% 
  mutate(Pitcher = Pitcher.x, Catcher = Catcher.x, Threat = Threat.x, sprint_speed = sprint_speed.x, poptime = poptime.x) %>% 
  select(-Pitcher.x, -Catcher.x, -Threat.x, -sprint_speed.x, -poptime.x) %>% 
  select(Date, Home, Away, Inning, TopBottom, 
         Runner1B, PrimaryLead1B, sprint_speed, 
         SB1, CS1, PK1, 
         Pitcher, pitch_hand, Threat, Catcher, poptime, 
         outs, Balls, Strikes, Play, everything()) %>% 
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))

# leadsnew1b: filtered version of leadsnewf to situations with:
# valid catcher, pitcher, runner metrics, runner on 1st only.
# Will serve as the main df from here since we only focus on these game situations

leadsnew1b <- leadsnewf %>% 
  filter(BaseState == "1--") %>% 
  filter(!is.na(Threat)) %>% 
  filter(!is.na(poptime)) %>% 
  filter(!is.na(sprint_speed)) %>% 
  filter(!is.na(PrimaryLead1B)) %>% 
  mutate(pickoffthrow = case_when(
    Play == "Pitch" ~ 0,
    Play == "Pickoff" ~ 1
  ))

# leads1bpkthrow contains only pickoff attempts (Throws to 1B). 127 successful pickoffs, 7439 attempts

leads1bpkthrow <- leadsnew1b %>% 
  filter(pickoffthrow == 1)

# leadsnewer1b filters out outlier situations where 1B is not holding runner on
# for (maybe)? training pickoff throw model

leadsnewer1b <- leadsnew1b %>% 
  filter(!(PrimaryLead1B >= 18 & pickoffthrow == 0))

### Models

# (1) probability of successful SB on given pitch (NOT ASSUMING THE RUNNER GOES)
xsbModelnew <- glm(SB1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
                   family = binomial(link = "logit"), data = leadsnew1b)

# (2) probability of caught stealing on given pitch (NOT ASSUMING THE RUNNER GOES)
xcsModelnew <- glm(CS1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
                   family = binomial(link = "logit"), data = leadsnew1b)

# (3) probability pitcher throws over to 1B, i.e. attempts a pickoff
xThrowOverModel <- glm(pickoffthrow ~ PrimaryLead1B + Threat, 
                       family = binomial(link = "logit"), data = leadsnew1b) # IS THIS THE RIGHT TRAINING DATA?

# (4) given a pickoff throw, probability it's successful given lead distance only
xpksuccessModel <- glm(PK1 ~ PrimaryLead1B, 
                       family = binomial(link = "logit"), data = leads1bpkthrow)

### Functions: model output and optimization

#get_prob inputs game situation (threat, pop time, sprint speed) + lead
#outputs SCALED probabilities of the 3 possible outcomes and corresponding xRuns value
get_prob <- function(PrimaryLead1B, Threat, poptime, sprint_speed) {
  newdata1 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed
  )
  newdata2 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat
  )
  newdata3 <- data.frame(
    PrimaryLead1B = PrimaryLead1B
  )
  xsb = predict(xsbModelnew, newdata1, type = "response")
  xcs = predict(xcsModelnew, newdata1, type = "response")
  xpk = predict(xThrowOverModel, newdata2, type = "response") *
    predict(xpksuccessModel, newdata3, type = "response")
  steal_attempt_prob <- xsb + xcs
  xsb_pct <- ifelse(steal_attempt_prob == 0, 0, xsb / steal_attempt_prob)
  xcs_pct <- ifelse(steal_attempt_prob == 0, 0, xcs / steal_attempt_prob)
  P_PK <- xpk
  P_SB <- (1 - xpk) * xsb_pct
  P_CS <- (1 - xpk) * xcs_pct
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
    data.frame(
    P_PK = P_PK,
    P_SB = P_SB,
    P_CS = P_CS,
    xRuns = xRuns
  )
}
#note on outcomes/scaling:
#the model output probabilities xsb, xcs, xpk do not necessarily add to 1. (Because the models are independent and don't assume the runner intends to go) 
#we fix this by scaling the first two
#specifically:
#since the runner intends to go, there are basically 2 outcomes: pickoff or SB attempt. this is governed by P(Pickoff) = xpk, P(SB attempt) = 1 - xpk
#then SB attempt has 2 sub-outcomes: Steal or Caught. we don't know them, but we know the ratio, which is sb% = xsb / (xsb + xcs)
#so P(SB) = P(SB attempt) * P(Steal | SB Attempt) = (1 - xpk) * (sb%)
#and P(CS) = (1 - xpk) * (1 - sb%). so we get 3 scaled probabilities that add to 1

#this would have to be changed if we wanted to add "nothing happens" outcome. 
#i think this is very hard to do because it necessitates measuring runner intent to steal, which is very unclear 

# example: 30ft lead
# get_prob(30, 3.068454647, 2.01, 29.5)

#get_optimal_lead inputs a given threat, pop time, sprint speedf
# outputs:
# - optimal lead distance (which maximizes xRuns according to get_prob)
# - maximum xRuns value, i.e. the xRuns at that optimal lead distance
get_optimal_lead <- function(Threat, poptime, sprint_speed) {
  xRuns_func <- function(PrimaryLead1B) {
    get_prob(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns
  }
  opt <- optimize(xRuns_func, interval = c(0, 90), maximum = TRUE)
  tibble::tibble(
    PrimaryLead1B = round(opt$maximum, 2),
    xRuns = opt$objective
  )
}

### Adding model predictions to leads data

#warning: this takes a long time
leadsnew1b <- leadsnew1b %>%
  rowwise() %>%
  mutate(
    actualxRuns = get_prob(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optimal = get_optimal_lead(Threat, poptime, sprint_speed)
  ) %>%
  mutate(
    optimalLead1B = optimal$PrimaryLead1B,
    optimalxRuns = optimal$xRuns
  ) %>%
  ungroup() %>%
  mutate(recommendation = if_else(optimalxRuns > 0, "Steal", "Stay"),
         leadChange = optimalLead1B - PrimaryLead1B) %>%
  select(-optimal)

# extra analysis to do: what % of situations is it best to run given real lead? given optimal lead?
# 72% // 86%

# Save all key processed datasets from full project
write.csv(leadsnew, "data/processed/full_leads_with_all_metrics.csv", row.names = FALSE)
write.csv(leadsnew1b, "data/processed/full_leads_1b_final.csv", row.names = FALSE)
write.csv(leadsnewer1b, "data/processed/full_leads_1b_filtered_final.csv", row.names = FALSE)
