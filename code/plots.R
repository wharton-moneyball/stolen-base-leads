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

# Sample of 100 leads on pitches
leads1bsample <- leadsnew1b[sample(nrow(leadsnew1b), 100),] %>%
  rowwise() %>%
  mutate(
    actualxRunsNew = get_prob_new(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns,
    optimalNew = get_optimal_lead_new(Threat, poptime, sprint_speed)
  ) %>%
  mutate(
    optimalLead1BNew = optimalNew$PrimaryLead1B,
    optimalxRunsNew = optimalNew$xRuns
  ) %>%
  ungroup() %>%
  mutate(recommendationNew = if_else(optimalxRunsNew > 0, "Steal", "Stay"),
         leadChangeNew = optimalLead1BNew - PrimaryLead1B) %>%
  select(-optimalNew)

#Functions to create plots of probability curves. Old model and new model respectively
doPlot <- function(thr, pop, spd) {
  lead_vec <- seq(0, 30, by = 0.1)
  
  results <- sapply(lead_vec, function(x) {
    tmp <- get_prob(x, thr, pop, spd)
    c(xRuns = tmp$xRuns, P_PK = tmp$P_PK, P_SB = tmp$P_SB, P_CS = tmp$P_CS)
  })
  
  df <- data.frame(
    PrimaryLead1B = lead_vec,
    xRuns = results["xRuns", ],
    P_PK = results["P_PK", ],
    P_SB = results["P_SB", ],
    P_CS = results["P_CS", ]
  )
  
  library(tidyr)
  df_long <- pivot_longer(df, cols = c(xRuns, P_PK, P_SB, P_CS),
                          names_to = "metric", values_to = "value")
  
  # Custom colors for each metric
  colormap <- c(
    "xRuns" = "#0072B2",
    "P_PK" = "#D55E00",
    "P_SB" = "#009E73",
    "P_CS" = "#E69F00"
  )
  
  ggplot(df_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
    geom_line(aes(size = (metric == "xRuns")), show.legend = TRUE) +
    scale_size_manual(values = c("TRUE" = 2.3, "FALSE" = 1.1), guide = "none") +  # Thicc line for xRuns
    scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(
      values = colormap,
      labels = c("xRuns" = "xRuns", "P_PK" = "Pickoff Prob.", "P_SB" = "Stolen Base Prob.", "P_CS" = "Caught Stealing Prob.")
    ) +
    labs(
      x = "Primary Lead at 1B (ft)", y = "", title = "Model Results: Given Situation"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", legend.title = element_blank())
}

doPlotNew <- function(thr, pop, spd) {
  lead_vec <- seq(0, 30, by = 0.1)
  
  results <- sapply(lead_vec, function(x) {
    tmp <- get_prob_new(x, thr, pop, spd)
    c(xRuns = tmp$xRuns, P_PK = tmp$P_PK, P_SB = tmp$P_SB, P_CS = tmp$P_CS)
  })
  
  df <- data.frame(
    PrimaryLead1B = lead_vec,
    xRuns = results["xRuns", ],
    P_PK = results["P_PK", ],
    P_SB = results["P_SB", ],
    P_CS = results["P_CS", ]
  )
  
  library(tidyr)
  df_long <- pivot_longer(df, cols = c(xRuns, P_PK, P_SB, P_CS),
                          names_to = "metric", values_to = "value")
  
  # Custom colors for each metric
  colormap <- c(
    "xRuns" = "#0072B2",
    "P_PK" = "#D55E00",
    "P_SB" = "#009E73",
    "P_CS" = "#E69F00"
  )
  
  ggplot(df_long, aes(x = PrimaryLead1B, y = value, color = metric, group = metric)) +
    geom_line(aes(size = (metric == "xRuns")), show.legend = TRUE) +
    scale_size_manual(values = c("TRUE" = 2.3, "FALSE" = 1.1), guide = "none") +  # Thicc line for xRuns
    scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(0, 30), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_color_manual(
      values = colormap,
      labels = c("xRuns" = "xRuns", "P_PK" = "Pickoff Prob.", "P_SB" = "Stolen Base Prob.", "P_CS" = "Caught Stealing Prob.")
    ) +
    labs(
      x = "Primary Lead at 1B (ft)", y = "", title = "Model Results: Given Situation"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top", legend.title = element_blank())
}

# =========================
# 2. CREATE PLOTS
# =========================

# Create visualization plots
plot1 <- ggplot(leadsnew1b) + #originally leads1b
  geom_histogram(aes(x = PrimaryLead1B), 
                 binwidth = 1, 
                 fill = "lightgray", 
                 color = "black") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B") +
  theme_minimal(base_size = 14)

plot2 <- ggplot(subset(leadsnew1b, SB1 == 1 | CS1 == 1)) + #originally leads1b
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

plot6new <- ggplot(leads1bsample, aes(x = actualxRunsNew, y = optimalxRunsNew, color = recommendation)) +
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

mean_leadchange <- 1.75 #only to 2 decimal places, take mean of (leadsnew1b %>% filter(SB1 == 1))$leadChange for true
mean_leadchange_new <- -0.588
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


plot7new <- ggplot(leadsnew1b %>% filter(SB1 == 1), aes(x = leadChangeNew)) +
  geom_histogram(
    fill = "#40b8c4", 
    color = "black",      
    bins = 30               
  ) +
  geom_vline(
    xintercept = mean_leadchange_new,
    color = "red",
    size = 1.2
  ) +
  annotate(
    "text",
    x = mean_leadchange_new - 3.5,
    y = 285,
    label = paste0("Mean: ", round(mean_leadchange_new, 2)),
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
df_long <- leadsnew1b %>%
  filter(SB1 + CS1 >= 1) %>% 
  pivot_longer(
    cols = c(PrimaryLead1B, optimalLead1B),
    names_to = "type", values_to = "lead"
  ) %>%
  mutate(type = recode(type,
                       PrimaryLead1B = "Actual Lead (ft)",
                       optimalLead1B = "Optimal Lead (ft)"
  ))

N <- nrow(leadsnew1b)
plot8 <- ggplot(df_long, aes(x = lead, color = type, fill = type)) +
  geom_density(
    aes(y = after_stat(density * N)),
    size = 1.5,
    alpha = 0.25,
    adjust = 1.4
  ) +
  labs(
    x = "Lead Distance (ft)",
    color = NULL,
    fill = NULL,
    title = "Frequency of leads: Stolen base attempts only"
  ) +
  scale_color_manual(
    values = c("Optimal Lead (ft)" = "#d95f02", "Actual Lead (ft)" = "#1b9e77"),
    breaks = c("Optimal Lead (ft)", "Actual Lead (ft)")
  ) +
  scale_fill_manual(
    values = c("Optimal Lead (ft)" = "#d95f02", "Actual Lead (ft)" = "#1b9e77"),
    breaks = c("Optimal Lead (ft)", "Actual Lead (ft)")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

df_long_new <- leadsnew1b %>%
  filter(SB1 + CS1 >= 1) %>% 
  pivot_longer(
    cols = c(PrimaryLead1B, optimalLead1B, optimalLead1BNew),
    names_to = "type", values_to = "lead"
  ) %>%
  mutate(type = recode(type,
                       PrimaryLead1B = "Actual Lead (ft)",
                       optimalLead1B = "Optimal Lead (ft) [OLD MODEL: RUNNER GOES]",
                       optimalLead1BNew = "Optimal Lead (ft) [NEW MODEL: NO INTENT]"
  ))

plot8new <- ggplot(df_long_new, aes(x = lead, color = type, fill = type)) +
  geom_density(
    aes(y = after_stat(density * N)),
    size = 1.5,
    alpha = 0.25,
    adjust = 1.4
  ) +
  labs(
    x = "Lead Distance (ft)",
    color = NULL,
    fill = NULL,
    title = "Frequency of leads: Stolen base attempts only"
  ) +
  scale_color_manual(
    values = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]" = "#d95f02", "Actual Lead (ft)" = "#1b9e77", "Optimal Lead (ft) [NEW MODEL: NO INTENT]" = "#ca88f3"),
    breaks = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]", "Actual Lead (ft)", "Optimal Lead (ft) [NEW MODEL: NO INTENT]")
  ) +
  scale_fill_manual(
    values = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]" = "#d95f02", "Actual Lead (ft)" = "#1b9e77", "Optimal Lead (ft) [NEW MODEL: NO INTENT]" = "#ca88f3"),
    breaks = c("Optimal Lead (ft) [OLD MODEL: RUNNER GOES]", "Actual Lead (ft)", "Optimal Lead (ft) [NEW MODEL: NO INTENT]")
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plotModelComparison <- ggplot(leadsnew1bSAMPLE, aes(x = optimalLead1B, y = optimalLead1BNew)) +
  geom_point()
plotRecComparison <- with(leadsnew1b, table(recommendation, recommendationNew)) #not really a plot

### examples + plots
doPlot(4.03, 2.09, 30) # the one from presentation
doPlotNew(4.03, 2.09, 30) # same one, with the new multinomial model
plot1
plot2
plot3
plot4
plot5
plot6
plot6new
plot7
plot7new
plot8
plot8new
plotRecComparison
plotModelComparison

# Save plots to results folder
ggsave("results/lead_histogram.png", plot1, width = 10, height = 6, dpi = 300)
ggsave("results/steal_attempts_by_lead.png", plot2, width = 10, height = 6, dpi = 300)
ggsave("results/player_average_leads.png", plot3, width = 10, height = 6, dpi = 300)
ggsave("results/pitcher_threat_distribution.png", plot4, width = 10, height = 6, dpi = 300)
ggsave("results/lead_vs_sprint_speed.png", plot5, width = 10, height = 6, dpi = 300)
ggsave("results/actual_vs_optimal_xruns.png", plot6, width = 10, height = 6, dpi = 300)
ggsave("results/steal_lead_error_histogram.png", plot7, width = 10, height = 6, dpi = 300)
#NOT ALL OF THESE ARE HERE: THE 'NEW' VERSIONS AS WELL AS 8 NEED TO BE ADDED

# All plots saved via ggsave above
