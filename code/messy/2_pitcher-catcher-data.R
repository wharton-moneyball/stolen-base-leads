nbpdata <- read.csv("data/raw/net-bases-prevented.csv")
ipdata <- read.csv("data/raw/innings-pitched.csv")
ssdata <- read.csv("data/raw/sprint-speed.csv")
ptdata <- read.csv("data/raw/pop-time.csv")


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

# Save processed datasets
write.csv(nbpdata, "data/processed/net_bases_prevented_processed.csv", row.names = FALSE)
write.csv(ipdata, "data/processed/pitcher_threat_metrics.csv", row.names = FALSE)
write.csv(ptdata, "data/processed/catcher_poptime_metrics.csv", row.names = FALSE)
write.csv(ssdata, "data/processed/sprint_speed_metrics.csv", row.names = FALSE)
write.csv(leadsnew, "data/processed/leads_with_all_metrics.csv", row.names = FALSE)

#view(nbpdata)
#view(ipdata)
#view(ptdata)
#view(ssdata)
