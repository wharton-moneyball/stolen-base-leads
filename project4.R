colnames(leads1b)

#At-Bat: same Date, Home, Inning, TopBottom, Batter
pkabs <- retro_pickoff_1b_outs_key %>%
  distinct(Date, Inning, TopBottom, Batter)

leadsab <- leads1b %>%
  group_by(Date, Home, Inning, TopBottom, Batter) %>%
  summarise(
    Runner1B = first(Runner1B),
    sprint_speed = first(sprint_speed),
    Threat = first(Threat),
    poptime = first(poptime),
    PrimaryLead1B = mean(PrimaryLead1B),
    pk1_flag = any(PK1 == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pickoff = if_else(
      pk1_flag |
        (paste(Date, Inning, TopBottom, Batter) %in%
           paste(pkabs$Date, pkabs$Inning, pkabs$TopBottom, pkabs$Batter)),
      1L, 0L
    )
  ) %>%
  select(PrimaryLead1B, pickoff, Date, Home, Inning, TopBottom, Batter, Runner1B, sprint_speed, Threat, poptime)

sample_frac <- 89/149
leadsab_filtered <- leadsab %>%
  group_by(pickoff) %>%
  mutate(rowid = row_number()) %>%
  ungroup()
non_pickoff <- leadsab_filtered %>% filter(pickoff == 0)
pickoff <- leadsab_filtered %>% filter(pickoff == 1)
non_pickoff_sampled <- non_pickoff %>% sample_frac(sample_frac)
leadsab_final <- bind_rows(pickoff, non_pickoff_sampled) %>%
  select(-rowid)

view(leadsab)
view(leadsab_final)

view(leads1bpk)

view(leadsnew1b %>% filter(CS1 == 1) %>% select(leadChange, everything()))
