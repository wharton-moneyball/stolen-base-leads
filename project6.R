view(leadsnew1b)

doPlot(4.03206908, 1.92, 28.7)


get_optimal_lead(4.03206908, 1.92, 28.7)
get_optimal_lead(4.03206908, 1.92, 28.7)$PrimaryLead1B
get_prob(get_optimal_lead(4.03206908, 1.92, 28.7)$PrimaryLead1B, 4.03206908, 1.92, 28.7)
get_prob(, 4.03206908, 1.92, 28.7)


# 1. Put the xRuns, optimality into lead distance df
# 2. Sample 100, find optimal leads
# 3. look if they should go/not and plot xRuns above average

get_optimal_lead(Threat, poptime, sprint_speed)
get_probs(PrimaryLead1B, Threat, poptime, sprint_speed)

doPlot(1.57, 1.87, 28.4)
get_optimal_lead(1.57, 1.87, 28.4)


safe_xRuns <- possibly(
  function(lead, thr, pop, spd) {
    res <- get_prob(lead, thr, pop, spd)
    if(all(c("xsb","xcs","xpk") %in% names(res))) {
      0.20 * res$xsb - 0.45 * res$xcs - 0.45 * res$xpk
    } else NA_real_
  },
  otherwise = NA_real_
)

safe_optimal <- possibly(
  function(thr, pop, spd) {
    get_optimal_lead(thr, pop, spd)
  },
  otherwise = tibble::tibble(PrimaryLead1B = NA_real_, xRuns = NA_real_)
)

# leadsnewer1b: everything
# leads1bpkthrow: pickoff throw
# leads1bpk: pickoffs
# leads1bsample: Sample of 100

leads1bpk <- leadsnewer1b %>% filter(PK1 == 1)

leads1bpk <- leads1bpk %>%
  mutate(
    actualxRuns = 
  ) %>%
  select(-optimal, -SB3, -CS3, -PK3, -NBP)

view(leads1bpk)

leadsnewer1b <- leadsnewer1b %>%
  mutate(
    actualxRuns = pmap_dbl(list(PrimaryLead1B, Threat, poptime, sprint_speed), safe_xRuns),
    optimal = pmap(list(Threat, poptime, sprint_speed), safe_optimal),
    optimalLead1B = map_dbl(optimal, ~ .x$PrimaryLead1B),
    optimalxRuns  = map_dbl(optimal, ~ .x$xRuns),
    recommendation = if_else(optimalxRuns > 0, "Steal", "Stay"),
    leadChange = optimalLead1B - PrimaryLead1B,
  ) %>%
  select(-optimal, -SB3, -CS3, -PK3, -NBP)

view(leadsnewer1b)

leads1bsample <- leadsnew1b[sample(nrow(leads1bpk), 100),]

view(leads1bsample)


mean(leads1bsample$optimalxRuns) - mean(leads1bsample$actualxRuns)

leads1bsample <- leads1bsample %>%
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
         leadChange = optimalLead1B - PrimaryLead1B)
  select(-optimal)


  ggplot(leads1bsample, aes(x = actualxRuns, y = optimalxRuns, color = recommendation)) +
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

# histogram / line actual vs optimal
  
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
           leadChange = optimalLead1B - PrimaryLead1B)
  select(-optimal)

  df_long <- leadsnew1b %>%
    filter(SB1 + CS1 + PK1 >= 1) %>% 
    pivot_longer(
      cols = c(PrimaryLead1B, optimalLead1B),
      names_to = "type", values_to = "lead"
    ) %>%
    mutate(type = recode(type,
                         PrimaryLead1B = "Actual Lead (ft)",
                         optimalLead1B = "Optimal Lead (ft)"
    ))
  
  N <- nrow(leadsnew1b)
  
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
  
  ggplot(df_long, aes(x = lead, color = type, fill = type)) +
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

  mean_leadchange <- mean((leadsnew1b %>% filter(SB1 == 1))$leadChange, na.rm = TRUE) 
mean_leadchange
  
  ggplot(leadsnew1b %>% filter(SB1 == 1), aes(x = leadChange)) +
    geom_histogram(
      fill = "#40b8c4", 
      color = "black",      
      bins = 30               
    ) +
    geom_vline(
      xintercept = mean((leadsnew1b %>% filter(SB1 == 1))$leadChange, na.rm = TRUE),
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
