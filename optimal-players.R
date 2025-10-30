
##Looking at runners 
leadRunners <- leadsnew1b %>%
  select(Runner1B, leadChangeNew) %>%
  group_by(Runner1B) %>%
  summarise(meanChange = mean(leadChangeNew, na.rm = TRUE)) %>%
  arrange(meanChange)
view(leadRunners)


leadRunners<-leadRunners %>%
  mutate(absChange = abs(meanChange)) %>%
  arrange(absChange) 

leadRunners


ggplot(leadRunners %>% slice_head(n = 20),
       aes(x = reorder(Runner1B, absChange), y = absChange)) +
  geom_col(fill = "#40b8c4") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    x = "Runner",
    y = "|Mean Lead Change| (ft)",
    title = "Runners Closest to Optimal Lead Distance"
  )

ggplot(leadRunners %>% slice_min(meanChange, n = 20),  # most negative = aggressive
       aes(x = reorder(Runner1B, meanChange), y = meanChange)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    x = "Runner",
    y = "Mean Lead Change (ft)",
    title = "Runners Taking Shorter Leads Than Optimal (Conservative)"
  ) +
  theme_minimal(base_size = 14)

ggplot(leadRunners %>% slice_max(meanChange, n = 20),  # most positive = conservative
       aes(x = reorder(Runner1B, meanChange), y = meanChange)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    x = "Runner",
    y = "Mean Lead Change (ft)",
    title = "Runners Taking Bigger Leads Than Optimal (Aggressive)"
  ) +
  theme_minimal(base_size = 14)


fast_runners <- leadsnew1b %>%
  select(Runner1B, sprint_speed, leadChangeNew) %>%
  group_by(Runner1B) %>%
  summarise(
    sprint_speed = mean(sprint_speed, na.rm = TRUE),
    meanChange = mean(leadChangeNew, na.rm = TRUE)
  ) %>%
  arrange(desc(sprint_speed))

fast_runners <- fast_runners %>%
  mutate(absChange = abs(meanChange))


ggplot(fast_runners, aes(x = sprint_speed, y = absChange)) +
  geom_point(color = "#40b8c4", size = 3) +
  geom_smooth(method = "lm", color = "gray40", linetype = "dotted") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Sprint Speed (ft/sec)",
    y = "Distance from Optimal Lead (|ft|)",
    title = "Are Faster Runners Closer to Optimal Leads?"
  )

fast_runners
