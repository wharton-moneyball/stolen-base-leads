# Load necessary library
library(tidyverse)
library(ggplot2)

# =========================
# 1. LOAD OPTIMAL LEAD DATA
# =========================

# Read in dataset containing actual and optimal primary leads + expected run values
leadoptimal <- read_csv("~/Desktop/Moneyball/Data/leadsoptimal.csv")

# Select relevant columns and reorder them for easier reference
leadoptimal <- leadoptimal %>%  
  select(
    Date, Home, Away, Play, Inning, TopBottom,
    Runner1B, PrimaryLead1B, sprint_speed,
    actualxRuns, optimal.PrimaryLead1B, optimal.xRuns,
    optimalLead1B, optimalxRuns,
    recommendation, leadChange,
    SB1, CS1, PK1, Pitcher, pitch_hand, Threat,
    Catcher, poptime, outs, Balls, Strikes,
    everything()
  )

# View column names (optional step to confirm structure)
colnames(leadoptimal)

# =========================
# 2. CREATE TEAM-LEVEL DATA
# =========================

# Select minimal set of columns for team-based summary
leadTeam <- leadoptimal %>%
  select(Date, Home, Play, Away, TopBottom, Runner1B, SB1, CS1, PK1, leadChange)

# Inspect structure (optional)
view(leadTeam)

# Add team info and limit to pickoff or steal attempt plays
leadTeam <- leadTeam %>%
  mutate(
    # Determine runner's team based on whether it's top or bottom of inning
    RunnerTeam = case_when(
      TopBottom == "T" ~ Away,
      TopBottom == "B" ~ Home
    ),
    # Compute total steal attempts (successful + caught)
    SBAttempts = SB1 + CS1
  ) %>%
  # Keep rows where a pickoff occurred or a steal attempt was made
  filter(Play == "Pickoff" | SBAttempts == 1)

# Inspect cleaned team-level data (optional)
view(leadTeam)

# Compute average lead change error by team
leadTeam <- leadTeam %>% 
  group_by(RunnerTeam) %>%
  # Compute mean absolute lead change error per team
  mutate(TeamAvg = mean(abs(leadChange), na.rm = TRUE)) %>%
  # Keep just one row per team (you only need the avg once)
  slice_head(n = 1) %>%
  ungroup()

# Create final summary table with only team and avg error
leadTeamOnly <- leadTeam %>% 
  select(RunnerTeam, TeamAvg)

# Optional view
view(leadTeamOnly)

# =========================
# 3. PLOT: AVERAGE LEAD ERROR BY TEAM
# =========================

# Define team colors (MLB color scheme for bar fill)
team_colors <- c(
  ARI = "#A71930", ATL = "#CE1141", BAL = "#DF4601", BOS = "#BD3039", 
  CHC = "#0E3386", CIN = "#C6011F", CLE = "#0C2340", COL = "#33006F", 
  CWS = "#27251F", DET = "#0C2340", HOU = "#EB6E1F", KC = "#004687", 
  LAA = "#BA0021", LAD = "#005A9C", MIA = "#00A3E0", MIL = "#12284B", 
  MIN = "#002B5C", NYM = "#002D72", NYY = "#132448", OAK = "#003831", 
  PHI = "#E81828", PIT = "#FDB827", SD = "#2F241D", SEA = "#005C5C", 
  SF = "#FD5A1E", STL = "#C41E3A", TB = "#092C5C", TEX = "#003278", 
  TOR = "#134A8E", WSH = "#AB0003"
)

# Create bar chart showing average lead error per team
ggplot(leadTeamOnly, aes(x = reorder(RunnerTeam, TeamAvg), y = TeamAvg, fill = RunnerTeam)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = team_colors) +
  labs(
    title = "Average Lead Error by Team",
    x = "Team",
    y = "Average Lead Error"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )

# Save team analysis plot
team_lead_error_plot <- ggplot(leadTeamOnly, aes(x = reorder(RunnerTeam, TeamAvg), y = TeamAvg, fill = RunnerTeam)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = team_colors) +
  labs(
    title = "Average Lead Error by Team",
    x = "Team",
    y = "Average Lead Error"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )

ggsave("results/team_lead_error_analysis.png", team_lead_error_plot, width = 12, height = 8, dpi = 300)
