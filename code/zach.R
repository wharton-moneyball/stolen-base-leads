# Load Libraries
library(tidyverse)

##########################
# 1. LOAD DATA
##########################


leads <- read_csv("data/raw/lead-distances.csv")
leadWPK <- read_csv("data/raw/lead-distances.csv")  # Using same file since WPK version not available
ssdata <- read_csv("data/raw/sprint-speed.csv")
nbpdata <- read_csv("data/raw/net-bases-prevented.csv")
ipdata <- read_csv("data/raw/innings-pitched.csv")
ptdata <- read_csv("data/raw/pop-time.csv")

##########################
# 2. CLEAN + PREP DATA
##########################

# Sprint Speed
ssdata <- ssdata %>% select(player_id, sprint_speed)

# Join sprint speed
leads <- leads %>% left_join(ssdata, by = c("Runner1B_ID" = "player_id"))
leadWPK <- leadWPK %>% left_join(ssdata, by = c("Runner1B_ID" = "player_id"))

# Pitcher Net Base Prevention (NBP)
nbpdata <- nbpdata %>%
  mutate(netBP = net_attr_plus + net_attr_minus) %>%
  select(player_id, player_name, netBP)

# Join to ipdata and calculate Threat
ipdata <- ipdata %>%
  left_join(nbpdata, by = "player_id") %>%
  transmute(
    PitcherID = player_id,
    Pitcher = player_name,
    NBP = netBP,
    IP = round(p_formatted_ip, 1)
  ) %>%
  mutate(
    IP = round(IP, 0),
    Threat = 100 * NBP / IP
  ) %>%
  filter(IP > 10)

# Pop time: filter catchers with at least 5 throws
ptdata <- ptdata %>%
  transmute(
    Catcher = entity_name,
    CatcherID = entity_id,
    poptime = pop_2b_sba,
    popcount = pop_2b_sba_count
  ) %>%
  filter(popcount >= 5)

##########################
# 3. JOIN TO MAIN DATASET
##########################

# Join all the data into `leads` and `leadWPK`
join_all <- function(df) {
  df %>%
    left_join(ptdata, by = "CatcherID") %>%
    left_join(ipdata, by = "PitcherID")
}

leads <- join_all(leads)
leadWPK <- join_all(leadWPK)

##########################
# 4. FEATURE ENGINEERING
##########################

# Clean version for PK data
leadsFiltered <- leadWPK %>%
  # Remove redundant .y columns
  select(-Catcher.y, -Pitcher.y, -popcount) %>%
  # Rename .x columns to standard names
  rename(
    Catcher = Catcher.x,
    Pitcher = Pitcher.x
  ) %>%
  # Reorder key columns to front
  select(
    Date, Home, Away, Play, Inning, TopBottom,
    Runner1B, PrimaryLead1B, sprint_speed, SB1, CS1, PK1,
    Pitcher, pitch_hand, Threat, Catcher, poptime,
    outs, Balls, Strikes, everything()
  ) %>%
  # Fix base state naming
  mutate(BaseState = ifelse(BaseState == "--1", "1--", BaseState))

# Filter valid data
leads1bWP <- leadsFiltered %>%
  filter(BaseState == "1--") %>%
  filter(!is.na(Threat), !is.na(sprint_speed), !is.na(poptime), !is.na(PrimaryLead1B)) %>%
  group_by(Runner1B) %>%
  mutate(avg_primary_lead = mean(PrimaryLead1B, na.rm = TRUE)) %>%
  ungroup()

# Pickoff subset
leadsPK1 <- leadsFiltered %>% filter(PK1 == "1")

# =========================
# 5. PLOTS + SUMMARIES
# =========================

# Use cleaned data for summaries
leads_summary <- leads1bWP %>%
  mutate(threat_bin = cut(Threat, breaks = seq(floor(min(Threat)), ceiling(max(Threat)), by = 5))) %>%
  group_by(threat_bin, pitch_hand) %>%
  summarise(AvgLead = mean(PrimaryLead1B, na.rm = TRUE), .groups = "drop")

# Plot
threat_analysis_plot <- ggplot(leads_summary, aes(x = threat_bin, y = AvgLead, group = pitch_hand, color = pitch_hand)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(name = "Pitcher Hand", values = c("R" = "red", "L" = "dodgerblue")) +
  labs(x = "Threat", y = "Average Primary Lead (ft)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 14))

# Save threat analysis plot
ggsave("results/threat_by_pitcher_hand.png", threat_analysis_plot, width = 10, height = 6, dpi = 300)

threat_analysis_plot

# Compare lead distances by hand (cleaned version)
leads_lefty <- leads1bWP %>% filter(pitch_hand == "L")
leads_righty <- leads1bWP %>% filter(pitch_hand == "R")

lefty_mean <- mean(leads_lefty$PrimaryLead1B, na.rm = TRUE)
righty_mean <- mean(leads_righty$PrimaryLead1B, na.rm = TRUE)

# Write out the correct dataset
write_csv(leads1bWP, "data/processed/leads1bWP.csv")