leadsPreds <- leads1b %>%
  mutate(
    # pmap_dfr is vectorized over columns you specify
    tmp = pmap_dfr(
      list(
        PrimaryLead1B = PrimaryLead1B,
        Threat = Threat,
        poptime = poptime,
        sprint_speed = sprint_speed
      ),
      get_prob
    )
  ) %>%
  # Unpack the new columns
  unnest_wider(tmp)

# Save prediction results
write.csv(leadsPreds, "data/processed/leads_with_predictions.csv", row.names = FALSE)

view(leadsPreds)

