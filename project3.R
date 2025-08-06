view(leads1b)
sum(leads1b$SB1)
sum(leads1b$CS1)


doPlot(1.803, 1.87, 28.1)


xsbModelnew <- glm(SB1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
    family = binomial(link = "logit"), data = leadsnew1b)

xcsModelnew <- glm(CS1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
                family = binomial(link = "logit"), data = leadsnew1b)

#old ineffective model
#xpkModelnew <- glm(PK1 ~ PrimaryLead1B + Threat + poptime + sprint_speed, 
#               family = binomial(link = "logit"), data = leadsnew1b)

xThrowOverModel <- glm(pickoffthrow ~ PrimaryLead1B + Threat, 
                       family = binomial(link = "logit"), data = leadsnew1b)

xpksuccessModel <- glm(PK1 ~ PrimaryLead1B, 
                       family = binomial(link = "logit"), data = leads1bpkthrow)

doPlot(4.744, 2.06, 28.6)

doPlot(4.03, 2.09, 30)
get_optimal_lead(4.03, 2.09, 30)

get_optimal_lead(4.744, 2.06, 28.6)

get_prob(20, 10, 2.01, 27)

xsbModel$coefficients
xsbModelnew$coefficients

xcsModel$coefficients
xcsModelnew$coefficients

xpkModel$coefficients
xpkModelnew$coefficients

testparameters <- data.frame(
  PrimaryLead1B = 0,
  Threat = 0.5,
  poptime = 2.01,
  sprint_speed = 29)

#10.2 0 2.01 28.5


predict(xsbModel, newdata = testparameters, type = "response")
predict(xcsModel, newdata = testparameters, type = "response")
predict(xpkModel, newdata = testparameters, type = "response")



get_probs <- function(PrimaryLead1B, Threat, poptime, sprint_speed) {
  newdata1 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed
  )
  newdata2 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed
  )
  newdata3 <- data.frame(
    PrimaryLead1B = PrimaryLead1B
  )
  
  data.frame(
    xsb = predict(xsbModelnew, newdata1, type = "response"),
    xcs = predict(xcsModelnew, newdata1, type = "response"),
    xpk = predict(xThrowOverModel, newdata2, type = "response") *
      predict(xpksuccessModel, newdata3, type = "response")
  )
}

#10.2 0 2.01 28.5
#lead threat pop speed
get_probs(30, 10, 2.01, 27)

get_prob <- function(PrimaryLead1B, Threat, poptime, sprint_speed) {
  newdata1 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat,
    poptime = poptime,
    sprint_speed = sprint_speed
  )
  newdata2 <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat = Threat
  )
  newdata3 <- data.frame(
    PrimaryLead1B = PrimaryLead1B
  )
  

    xsb = predict(xsbModelnew, newdata1, type = "response")
    xcs = predict(xcsModelnew, newdata1, type = "response")
    xpk = predict(xThrowOverModel, newdata2, type = "response") *
      predict(xpksuccessModel, newdata3, type = "response")
  # Prevent divide by zero
  steal_attempt_prob <- xsb + xcs
  xsb_pct <- ifelse(steal_attempt_prob == 0, 0, xsb / steal_attempt_prob)
  xcs_pct <- ifelse(steal_attempt_prob == 0, 0, xcs / steal_attempt_prob)
  
  P_PK <- xpk
  P_SB <- (1 - xpk) * xsb_pct
  P_CS <- (1 - xpk) * xcs_pct
  xRuns <- 0.20 * P_SB - 0.45 * P_CS - 0.45 * P_PK
  
  
  data.frame(
    P_PK = P_PK,
    P_SB = P_SB,
    P_CS = P_CS,
    xRuns = xRuns
  )
}

get_prob(40, 0.1011746, 1.960534, 27.32819)
get_prob(30, 3.068454647, 2.01, 29.5)
get_prob(30, -12.068454647, 1.91, 28.5)$xRuns
get_optimal_lead(3.068454647, 2.01, 29.5)
get_optimal_lead(3.068454647, 2.01, 29.5)$PrimaryLead1B
get_optimal_lead(3.068454647, 2.01, 29.5)$xRuns

# 10.2301
# 0.1011746
# 1.960534
# 27.32819

get_optimal_lead <- function(Threat, poptime, sprint_speed) {
  xRuns_func <- function(PrimaryLead1B) {
    get_prob(PrimaryLead1B, Threat, poptime, sprint_speed)$xRuns
  }
  opt <- optimize(xRuns_func, interval = c(0, 90), maximum = TRUE)
  tibble::tibble(
    PrimaryLead1B = round(opt$maximum, 2),
    xRuns = opt$objective
  )
}

lead_vec <- seq(0, 45, by = 0.05)


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



grid <- data.frame(PrimaryLead1B = seq(min(leads1bpkthrow$PrimaryLead1B, na.rm=TRUE), 
                                       max(leads1bpkthrow$PrimaryLead1B, na.rm=TRUE), 
                                       length.out = 500))
grid$pred <- predict(xpksuccessModel, newdata = grid, type = "response")
ggplot(leads1bpkthrow, aes(x = PrimaryLead1B, y = PK1)) +
  geom_jitter(height = 0.02, width = 0, alpha = 0.3, color = "gray40") +
  geom_line(data = grid, aes(x = PrimaryLead1B, y = pred), color = "blue", size = 1.2) +
  labs(
    x = "Primary Lead at 1B (ft)",
    y = "Pickoff Success Probability",
    title = "Logistic Regression Fit: Pickoff Success vs. Lead"
  ) +
  theme_minimal(base_size = 15)

fixed_Threat <- 0.1
fixed_sprint_speed <- 27.3
grid <- data.frame(
  PrimaryLead1B = seq(min(leadsnew1b$PrimaryLead1B, na.rm=TRUE),
                      max(leadsnew1b$PrimaryLead1B, na.rm=TRUE),
                      length.out = 500),
  Threat = fixed_Threat
  #sprint_speed = fixed_sprint_speed
)
#grid$lead_surge <- pmax(grid$PrimaryLead1B - 18, 0)
grid$pred <- predict(xThrowOverModel, newdata = grid, type = "response")
ggplot(leadsnewer1b, aes(x = PrimaryLead1B, y = pickoffthrow)) +
  geom_jitter(height = 0.02, width = 0, alpha = 0.3, color = "gray40") +
  geom_line(data = grid, aes(x = PrimaryLead1B, y = pred), color = "red", size = 1.2) +
  labs(
    x = "Primary Lead at 1B (ft)",
    y = "Probability of Pickoff Throw",
    title = "Pickoff Throw Probability vs. Lead (Threat, Sprint Speed Fixed)"
  ) +
  theme_minimal(base_size = 15)

