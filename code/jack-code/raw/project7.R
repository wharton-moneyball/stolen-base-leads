leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ipdata, by = c("PitcherID" = "PitcherID"))
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ptdata, by = c("CatcherID" = "CatcherID"))
leadsnew <- leadsnew %>% 
  left_join(x = leadsnew, y = ssdata, by = c("Runner1B_ID" = "Runner1B_ID"))

view(leadsnew)

leadsnewf <- leadsnew %>% 
  select(-Pitcher.y, -Catcher.y, -popcount) %>% 
  mutate(Pitcher = Pitcher.x, Catcher = Catcher.x) %>% 
  select(-Pitcher.x, -Catcher.x)

leadsnewf <- leadsnewf %>% 
  select(Date, Home, Away, Inning, TopBottom, Runner1B, PrimaryLead1B, sprint_speed, SB1, CS1, PK1, Pitcher, pitch_hand, Threat, Catcher, poptime, outs, Balls, Strikes, Play, everything()) %>% 
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))
# leadsf: leads with pitcher/catcher/runner metrics + rearranged

# leads1b: filtering
leadsnew1b <- leadsnewf %>% 
  filter(BaseState == "1--") %>% 
  filter(!is.na(Threat)) %>% 
  filter(!is.na(poptime)) %>% 
  filter(!is.na(sprint_speed)) %>% 
  filter(!is.na(PrimaryLead1B))
leadsnew1b <- leadsnew1b %>% 
  mutate(pickoffthrow = case_when(
    Play == "Pitch" ~ 0,
    Play == "Pickoff" ~ 1
  ))

leads1bpkthrow <- leadsnew1b %>% 
  filter(pickoffthrow == 1)

view(leadsnew1b)
view(leads1bpkthrow)

sum(leadsnew1b$PK1)

leadsnewer1b <- leadsnew1b %>% 
  filter(!(PrimaryLead1B >= 18 & pickoffthrow == 0))
