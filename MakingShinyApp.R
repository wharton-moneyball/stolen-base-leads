# app.R — 3 dropdowns + UPDATE button → Optimal lead + yellow visual
library(shiny)
library(dplyr)

# make folders
dir.create("~/steal-app/data/app", recursive = TRUE, showWarnings = FALSE)



# Runners: median sprint speed per runner
runners <- leadsnew1b %>%
  dplyr::filter(!is.na(Runner1B), !is.na(sprint_speed)) %>%
  dplyr::group_by(Runner1B) %>%
  dplyr::summarise(sprint_speed = median(sprint_speed, na.rm = TRUE), .groups="drop") %>%
  dplyr::arrange(Runner1B)
# Pitchers: median Threat per pitcher
pitchers <- leadsnew1b %>%
  dplyr::filter(!is.na(Pitcher), !is.na(Threat)) %>%
  dplyr::group_by(Pitcher) %>%
  dplyr::summarise(Threat = median(Threat, na.rm = TRUE), .groups="drop") %>%
  dplyr::arrange(Pitcher)

# Catchers: median poptime per catcher
catchers <- leadsnew1b %>%
  dplyr::filter(!is.na(Catcher), !is.na(poptime)) %>%
  dplyr::group_by(Catcher) %>%
  dplyr::summarise(poptime = median(poptime, na.rm = TRUE), .groups="drop") %>%
  dplyr::arrange(Catcher)

# save them where app.R expects them
saveRDS(runners,  "~/steal-app/data/app/runners.rds")
saveRDS(pitchers, "~/steal-app/data/app/pitchers.rds")
saveRDS(catchers, "~/steal-app/data/app/catchers.rds")

# ---------- Lookups ----------
runners  <- readRDS("data/app/runners.rds")   # Runner1B, sprint_speed
pitchers <- readRDS("data/app/pitchers.rds")  # Pitcher, Threat
catchers <- readRDS("data/app/catchers.rds")  # Catcher, poptime

dir.create("~/steal-app/data/models", recursive = TRUE, showWarnings = FALSE)

# after fitting your NEW models:
saveRDS(m_PO,               "~/steal-app/data/models/m_PO.rds")
saveRDS(m_PK_given_PO,      "~/steal-app/data/models/m_PK_given_PO.rds")
saveRDS(m_ATT_given_Pitch,  "~/steal-app/data/models/m_ATT_given_Pitch.rds")
saveRDS(m_SB_given_ATT,     "~/steal-app/data/models/m_SB_given_ATT.rds")

# ---------- NEW models ----------
m_PO              <- readRDS("data/models/m_PO.rds")
m_PK_given_PO     <- readRDS("data/models/m_PK_given_PO.rds")
m_ATT_given_Pitch <- readRDS("data/models/m_ATT_given_Pitch.rds")
m_SB_given_ATT    <- readRDS("data/models/m_SB_given_ATT.rds")

# ---------- Predict helpers ----------
pred_PO   <- function(nd) as.numeric(predict(m_PO,              nd, type="response"))
pred_PKPO <- function(nd) as.numeric(predict(m_PK_given_PO,     nd, type="response"))
pred_ATT  <- function(nd) as.numeric(predict(m_ATT_given_Pitch, nd, type="response"))
pred_SB_A <- function(nd) as.numeric(predict(m_SB_given_ATT,    nd, type="response"))

compose_probs_new <- function(PrimaryLead1B, Threat, poptime, sprint_speed){
  nd <- data.frame(
    PrimaryLead1B = PrimaryLead1B,
    Threat        = Threat,
    poptime       = poptime,
    sprint_speed  = sprint_speed
  )
  p_PO   <- pred_PO(nd)
  p_PKPO <- pred_PKPO(nd)
  p_ATT  <- pred_ATT(nd)
  p_SB_A <- pred_SB_A(nd)
  
  P_PK <- p_PO * p_PKPO
  P_SB <- (1 - p_PO) * p_ATT * p_SB_A
  P_CS <- (1 - p_PO) * p_ATT * (1 - p_SB_A)
  
  c(P_PK = P_PK, P_SB = P_SB, P_CS = P_CS)
}

xRuns_from_probs <- function(p) 0.20*p["P_SB"] - 0.45*p["P_CS"] - 0.45*p["P_PK"]

optimize_lead_new <- function(Threat, poptime, sprint_speed){
  leads <- seq(6, 30, by = 0.1)
  xr <- vapply(leads, function(L){
    xRuns_from_probs(compose_probs_new(L, Threat, poptime, sprint_speed))
  }, numeric(1))
  i <- which.max(xr)
  list(lead = round(leads[i], 2),
       xrun = round(xr[i], 4),
       probs = compose_probs_new(leads[i], Threat, poptime, sprint_speed))
}

# ---------- UI ----------
ui <- fluidPage(
  titlePanel("Optimal Lead (Nested Model)"),
  fluidRow(
    column(3, selectInput("runner",  "Runner",  choices = runners$Runner1B)),
    column(3, selectInput("pitcher", "Pitcher", choices = pitchers$Pitcher)),
    column(3, selectInput("catcher", "Catcher", choices = catchers$Catcher)),
    column(3, br(), actionButton("go", "Update", class = "btn-primary"))
  ),
  hr(),
  h2(textOutput("opt_text")),
  fluidRow(
    column(7, plotOutput("diamond_plot", height = 420)),
    column(5, tableOutput("support"))
  )
)

# ---------- SERVER ----------
server <- function(input, output, session){
  attrs <- reactive({
    r <- runners  %>% dplyr::filter(Runner1B == input$runner)  %>% dplyr::slice(1)
    p <- pitchers %>% dplyr::filter(Pitcher  == input$pitcher) %>% dplyr::slice(1)
    c <- catchers %>% dplyr::filter(Catcher  == input$catcher) %>% dplyr::slice(1)
    validate(
      need(nrow(r)==1, "Runner not found"),
      need(nrow(p)==1, "Pitcher not found"),
      need(nrow(c)==1, "Catcher not found")
    )
    list(Threat = p$Threat, poptime = c$poptime, sprint_speed = r$sprint_speed)
  })
  
  result <- eventReactive(input$go, {
    a <- attrs()
    optimize_lead_new(a$Threat, a$poptime, a$sprint_speed)
  }, ignoreInit = TRUE)
  
  output$opt_text <- renderText({
    req(result())
    paste0("Optimal Lead: ", result()$lead, " ft   |   xRuns: ", result()$xrun)
  })
  
  output$support <- renderTable({
    req(result())
    p <- result()$probs
    data.frame(
      `P(Pickoff)` = sprintf("%.2f%%", p["P_PK"] * 100),
      `P(SB)`      = sprintf("%.2f%%", p["P_SB"] * 100),
      `P(CS)`      = sprintf("%.2f%%", p["P_CS"] * 100)
    )
  })
  
  
  output$diamond_plot <- renderPlot({
    req(result())
    L <- as.numeric(result()$lead)
    plot(NA, xlim = c(-10, 45), ylim = c(-10, 45),
         xlab = "", ylab = "", axes = FALSE, asp = 1,
         main = "Lead from First Toward Second")
    rect(-10, -10, 45, 45, col = "#2e8b57", border = NA)  # grass
    segments(0, 0, 90, 90, lwd = 4, col = "gray40")       # baseline
    draw_base <- function(x, y, size = 8, col = "white", border = "black"){
      s <- size / sqrt(2); xs <- c(x, x+s, x, x-s); ys <- c(y+s, y, y-s, y)
      polygon(xs, ys, col = col, border = border, lwd = 2)
    }
    draw_base(0, 0, size = 8); draw_base(90, 90, size = 8)
    k <- 90 / sqrt(90^2 + 90^2); segments(0, 0, k*L, k*L, lwd = 12, col = "yellow")
    points(k*L, k*L, pch = 21, bg = "orange", cex = 2.4, lwd = 1.5)
    text(0, -4, "1B", cex = 1.3, font = 2); text(90, 96, "2B", cex = 1.1)
    text(k*L + 1.8, k*L + 3.2, paste0("Lead = ", sprintf("%.2f ft", L)), cex = 1.3, font = 2, col = "white")
    box(lwd = 2)
  })
}

shinyApp(ui, server)




