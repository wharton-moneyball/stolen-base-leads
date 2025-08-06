# plots. add sb vs cs, i.e. sbatts.

plot1 <- ggplot(leads1b) +
  geom_histogram(aes(x = PrimaryLead1B), 
                 binwidth = 1, 
                 fill = "lightgray", 
                 color = "black") +  # Adds borders
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B") +
  theme_minimal(base_size = 14)

ggplot(leads1b) +
  geom_histogram(aes(x = PrimaryLead1B, fill = factor(SB1)), 
                 binwidth = 1, 
                 color = "black") +
  scale_fill_manual(values = c("gray70", "forestgreen"), 
                    labels = c("No Steal", "Stolen Base"), 
                    name = "SB Attempt") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B (Stacked by Steal Outcome)") +
  theme_minimal(base_size = 14)

plot2 <- ggplot(subset(leads1b, SB1 == 1 | CS1 == 1)) +
  geom_histogram(aes(x = PrimaryLead1B), 
                 binwidth = 1, 
                 fill = "darkorange", 
                 color = "black") +
  labs(x = "Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B on Stolen Base Attempts") +
  theme_minimal(base_size = 14)

plot2 <- ggplot(subset(leads1b, SB1 == 1 | CS1 == 1)) +
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
plot2
ggarrange(plot1, plot2, ncol = 2, nrow = 1)

sum(leadsf$SB1)

plot3 <- ggplot(newplayerleads1b) +
  geom_histogram(aes(x = playeravg1Blead), 
                 binwidth = 0.2, 
                 fill = "orange", 
                 color = "black") +  # Adds borders
  labs(x = "Average Primary Lead Distance (ft)", 
       y = "Count", 
       title = "Primary Leads from 1B by Player") +
  theme_minimal(base_size = 15)
plot3

plot4 <- ggplot(ipdata) +
  geom_histogram(aes(x = Threat), 
                 binwidth = 1, 
                 fill = "orange", 
                 color = "black") +  # Adds borders
  labs(x = "Pitcher threat", 
       y = "Count", 
       title = "") +
  theme_minimal(base_size = 15)
plot4

plot5 <- ggplot(newplayerleads1b, aes(x = sprint_speed, y = playeravg1Blead)) +
  geom_point(color = "firebrick", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    x = "Sprint Speed (ft/s)",
    y = "Average Primary Lead from 1B (ft)",
    title = "Primary Lead vs. Sprint Speed",
  ) +
  theme_minimal(base_size = 14)# +
 # geom_point(data = subset(newplayerleads1b, Runner1B == "Anthony Volpe"),
 #            aes(x = 28.6, y = 8.95), color = "firebrick", size = 4) +
 # geom_text(data = subset(newplayerleads1b, Runner1B == "Anthony Volpe"),
 #          aes(x = 28.6, y = 8.95, label = "Anthony Volpe"),
 #          hjust = -0.1, vjust = -0.5, size = 4.5, fontface = "bold")

cor(newplayerleads1b$sprint_speed, newplayerleads1b$playeravg1Blead, use = "complete.obs")
# r = 0.54 r^2 = 0.29
# Dan z score = 5.6
plot5

