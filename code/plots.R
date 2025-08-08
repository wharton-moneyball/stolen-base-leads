# libraries
library(tidyverse)

# =========================
# 1. LOAD DATA FROM OTHER SCRIPTS
# =========================

# Load data processed by zach.R
leadsnew1b <- read_csv("data/processed/full_leads_1b_final.csv")

# Load pitcher data for threat analysis
ipdata <- read_csv("data/raw/innings-pitched.csv") %>%
  left_join(read_csv("data/raw/net-bases-prevented.csv"), by = "player_id") %>%
  mutate(
    netBP = net_attr_plus + net_attr_minus,
    IP = round(as.numeric(p_formatted_ip), 0),
    Threat = 100 * netBP / IP
  ) %>%
  filter(IP > 10)

# Create player-level summary data for plotting
newplayerleads1b <- leadsnew1b %>%
  filter(!is.na(PrimaryLead1B)) %>%
  group_by(Runner1B_ID) %>%
  mutate(playeravg1Blead = mean(PrimaryLead1B)) %>%
  filter(n() >= 15) %>% 
  slice_head(n = 1) %>% 
  ungroup()

# =========================
# 2. CREATE PLOTS
# =========================

# Create visualization plots
plot1 <- ggplot(leadsnew1b) +
  geom_histogram(aes(x = PrimaryLead1B), 
                 binwidth = 1, 
                 fill = "lightgray", 
                 color = "black") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B") +
  theme_minimal(base_size = 14)

plot2 <- ggplot(subset(leadsnew1b, SB1 == 1 | CS1 == 1)) +
  geom_histogram(aes(x = PrimaryLead1B, fill = factor(CS1)), 
                 binwidth = 1, 
                 color = "black", 
                 position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("forestgreen", "red3"), 
                    labels = c("Safe", "Out"), 
                    name = "Outcome at 2nd Base") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B on Stolen Base Attempts") +
  theme_minimal(base_size = 14) +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))



plot3 <- ggplot(newplayerleads1b) +
  geom_histogram(aes(x = playeravg1Blead), 
                 binwidth = 0.2, 
                 fill = "orange", 
                 color = "black") + 
  labs(x = "Average Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B by Player") +
  theme_minimal(base_size = 15)


plot4 <- ggplot(ipdata) +
  geom_histogram(aes(x = Threat), 
                 binwidth = 1, 
                 fill = "forestgreen", 
                 color = "black") +  
  labs(x = "Pitcher threat", 
       y = "Count", 
       title = "") +
  theme_minimal(base_size = 15)

plot5 <- ggplot(newplayerleads1b, aes(x = sprint_speed, y = playeravg1Blead)) +
  geom_point(color = "firebrick", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = "Sprint Speed (ft/s)",
    y = "Average Primary Lead from 1B (ft)",
    title = "Primary Lead vs. Sprint Speed",
  ) +
  theme_minimal(base_size = 14)

# Create sample for optimal vs actual analysis
leads1bsample <- leadsnew1b[sample(nrow(leadsnew1b), 100),] %>%
  mutate(recommendation = if_else(optimalxRuns > 0, "Steal", "Stay"))

plot6 <- ggplot(leads1bsample, aes(x = actualxRuns, y = optimalxRuns, color = recommendation)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, color = "gray", lty = "dashed", size = 1.2) +
  coord_fixed(ratio = 1) +
  scale_color_manual(values = c("Stay" = "red", "Steal" = "forestgreen")) +
  labs(
    x = "xRuns (Actual)",
    y = "xRuns (Optimal)",
    color = "Recommendation",
    title = "Actual vs. Optimal  - Sample of 100"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

mean_leadchange <- mean((leadsnew1b %>% filter(SB1 == 1))$leadChange, na.rm = TRUE)

plot7 <- ggplot(leadsnew1b %>% filter(SB1 == 1), aes(x = leadChange)) +
  geom_histogram(
    fill = "#40b8c4", 
    color = "black",      
    bins = 30               
  ) +
  geom_vline(
    xintercept = mean_leadchange,
    color = "red",
    size = 1.2
  ) +
  annotate(
    "text",
    x = mean_leadchange - 1.5,
    y = 265,
    label = paste0("Mean: ", round(mean_leadchange, 2)),
    color = "red",
    vjust = -0.5,
    fontface = "bold",
    size = 5
  ) +
  
  theme_minimal(base_size = 15) +
  labs(
    x = "Error: Optimal Lead - Actual Lead (ft)",
    y = "Count",
    title = "Lead Error on SB attempts"
  )

### examples + plots

# Save plots to results folder
ggsave("results/lead_histogram.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("results/steal_attempts_by_lead.png", plot2, width = 10, height = 6, dpi = 300)
ggsave("results/player_average_leads.png", plot3, width = 10, height = 6, dpi = 300)
ggsave("results/pitcher_threat_distribution.png", plot4, width = 10, height = 6, dpi = 300)
ggsave("results/lead_vs_sprint_speed.png", plot5, width = 10, height = 6, dpi = 300)
ggsave("results/actual_vs_optimal_xruns.png", plot6, width = 10, height = 6, dpi = 300)
ggsave("results/steal_lead_error_histogram.png", plot7, width = 10, height = 6, dpi = 300)

# All plots saved via ggsave above