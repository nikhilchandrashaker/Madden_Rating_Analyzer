library(shiny)
library(DT)

bundle <- readRDS("madden_model.rds")
df <- bundle$data
models <- bundle$models

pos_order <- c("Quarterback","Halfback","Fullback","Wide Receiver","Tight End",
               "Left Tackle","Left Guard","Center","Right Guard","Right Tackle",
               "Left Edge","Right Edge","Defensive Tackle",
               "Sam Backer","Mike Backer","Weak Backer",
               "Cornerback","Free Safety","Strong Safety",
               "Kicker","Punter","Long Snapper")
positions <- pos_order[pos_order %in% names(models)]

ovr_range_for <- function(pos) range(df$OVERALL[df$Position == pos])

# ---------------------------------------------------------
# FORWARD function
# ---------------------------------------------------------
forward_requirements <- function(position, target_ovr, window = 2, min_n = 8) {
  pos_data <- df[df$Position == position, ]
  m <- models[[position]]
  preds <- m$predictors

  w <- window
  repeat {
    band <- pos_data[abs(pos_data$OVERALL - target_ovr) <= w, ]
    if (nrow(band) >= min_n || w > 20) break
    w <- w + 1
  }

  attr_stats <- data.frame(
    Attribute = preds,
    Importance = round(m$correlations[preds], 2),
    P25 = sapply(preds, function(a) round(quantile(band[[a]], .25, na.rm = TRUE))),
    Typical = sapply(preds, function(a) round(median(band[[a]], na.rm = TRUE))),
    P75 = sapply(preds, function(a) round(quantile(band[[a]], .75, na.rm = TRUE))),
    row.names = NULL
  )
  attr_stats <- attr_stats[order(-abs(attr_stats$Importance)), ]

  band$Diff <- abs(band$OVERALL - target_ovr)
  comps <- band[order(band$Diff), c("First.Name","Last.Name","Team","OVERALL")]
  comps <- head(comps, 6)
  names(comps) <- c("First","Last","Team","OVR")

  list(attribute_requirements = attr_stats, comparable_players = comps,
       n_comparable = nrow(band), window_used = w, r2 = round(m$r_squared, 3))
}

# ---------------------------------------------------------
# REVERSE function
# ---------------------------------------------------------
reverse_predict <- function(position, attr_values, target_ovr = NULL) {
  m <- models[[position]]
  preds <- m$predictors
  pos_data <- df[df$Position == position, ]

  full_input <- sapply(preds, function(a) {
    v <- attr_values[[a]]
    if (!is.null(v) && !is.na(v)) as.numeric(v) else median(pos_data[[a]], na.rm = TRUE)
  })
  newdata <- as.data.frame(t(full_input)); names(newdata) <- preds
  pred_ovr <- predict(m$model, newdata = newdata)
  pred_ovr <- max(40, min(99, round(pred_ovr)))

  out <- list(predicted_ovr = pred_ovr, r2 = round(m$r_squared, 3))

  if (!is.null(target_ovr)) {
    gap <- target_ovr - pred_ovr
    out$gap <- gap
    coefs <- coef(m$model)[preds]
    coefs <- coefs[!is.na(coefs) & coefs > 0]
    if (gap > 0 && length(coefs) > 0) {
      top_attrs <- names(sort(coefs, decreasing = TRUE))
      n_top <- min(5, length(top_attrs))
      top_attrs <- top_attrs[1:n_top]
      per_attr_gap <- gap / n_top
      increases <- sapply(top_attrs, function(a) ceiling(per_attr_gap / coefs[a]))
      increases <- pmin(increases, as.numeric(99 - full_input[top_attrs]))
      increases <- pmax(increases, 0)
      new_vals <- round(as.numeric(full_input[top_attrs]) + increases)
      out$plan <- data.frame(Attribute = top_attrs, Current = round(full_input[top_attrs]),
                              Increase = increases, NewValue = new_vals, row.names = NULL)
    } else if (gap <= 0) {
      out$message <- "You already meet or exceed this target with these attributes!"
    }
  }
  out
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  titlePanel("Madden 26 Rating Requirements Analyzer"),
  tabsetPanel(
    # ---------------- FORWARD TAB ----------------
    tabPanel("Target OVR \u2192 Attributes",
      sidebarLayout(
        sidebarPanel(
          selectInput("fwd_pos", "Position", choices = positions, selected = "Quarterback"),
          uiOutput("fwd_ovr_slider"),
          helpText("Shows the typical attribute build for real players near your target overall rating, based on the actual Madden 26 roster.")
        ),
        mainPanel(
          h4(textOutput("fwd_header")),
          DTOutput("fwd_table"),
          br(),
          h4("Comparable real players"),
          DTOutput("fwd_comps")
        )
      )
    ),
    # ---------------- REVERSE TAB ----------------
    tabPanel("My Attributes \u2192 Predicted OVR",
      sidebarLayout(
        sidebarPanel(
          selectInput("rev_pos", "Position", choices = positions, selected = "Quarterback"),
          uiOutput("rev_sliders"),
          numericInput("rev_target", "Target OVR (optional)", value = NA, min = 40, max = 99),
          helpText("Sliders default to the position's median. Adjust to match your build.")
        ),
        mainPanel(
          h3(textOutput("rev_predicted")),
          textOutput("rev_r2"),
          br(),
          conditionalPanel(
            condition = "output.rev_has_plan == true",
            h4("To reach your target:"),
            DTOutput("rev_plan")
          ),
          textOutput("rev_message")
        )
      )
    ),
    # ---------------- MODEL INSIGHTS TAB ----------------
    tabPanel("Model Insights",
      selectInput("ins_pos", "Position", choices = positions, selected = "Quarterback"),
      p("Correlation of each attribute with OVERALL for this position (data-driven, not hand-authored)."),
      DTOutput("ins_table"),
      br(),
      textOutput("ins_r2")
    )
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {

  # ---------- FORWARD ----------
  output$fwd_ovr_slider <- renderUI({
    rng <- ovr_range_for(input$fwd_pos)
    sliderInput("fwd_ovr", "Target Overall Rating", min = rng[1], max = rng[2],
                value = round(mean(rng)), step = 1)
  })

  fwd_result <- reactive({
    req(input$fwd_ovr)
    forward_requirements(input$fwd_pos, input$fwd_ovr)
  })

  output$fwd_header <- renderText({
    r <- fwd_result()
    sprintf("Typical %s OVR %s (model fit R\u00b2 = %.2f, based on %d comparable players)",
            input$fwd_ovr, input$fwd_pos, r$r2, r$n_comparable)
  })

  output$fwd_table <- renderDT({
    datatable(fwd_result()$attribute_requirements, rownames = FALSE,
              options = list(dom = 't', pageLength = 15))
  })

  output$fwd_comps <- renderDT({
    datatable(fwd_result()$comparable_players, rownames = FALSE,
              options = list(dom = 't', pageLength = 6))
  })

  # ---------- REVERSE ----------
  output$rev_sliders <- renderUI({
    m <- models[[input$rev_pos]]
    pos_data <- df[df$Position == input$rev_pos, ]
    lapply(m$predictors, function(a) {
      sliderInput(paste0("rev_attr_", a), a,
                  min = 0, max = 99,
                  value = round(median(pos_data[[a]], na.rm = TRUE)))
    })
  })

  rev_result <- reactive({
    m <- models[[input$rev_pos]]
    req(all(sapply(m$predictors, function(a) !is.null(input[[paste0("rev_attr_", a)]]))))
    attr_values <- setNames(
      lapply(m$predictors, function(a) input[[paste0("rev_attr_", a)]]),
      m$predictors
    )
    tgt <- if (!is.na(input$rev_target)) input$rev_target else NULL
    reverse_predict(input$rev_pos, attr_values, tgt)
  })

  output$rev_predicted <- renderText({
    sprintf("Predicted Overall: %d", rev_result()$predicted_ovr)
  })
  output$rev_r2 <- renderText({
    sprintf("Model fit for %s: R\u00b2 = %.2f", input$rev_pos, rev_result()$r2)
  })
  output$rev_has_plan <- reactive({ !is.null(rev_result()$plan) })
  outputOptions(output, "rev_has_plan", suspendWhenHidden = FALSE)

  output$rev_plan <- renderDT({
    req(rev_result()$plan)
    datatable(rev_result()$plan, rownames = FALSE, options = list(dom = 't'))
  })
  output$rev_message <- renderText({
    if (!is.null(rev_result()$message)) rev_result()$message
    else if (!is.null(rev_result()$gap) && rev_result()$gap <= 0) "" else ""
  })

  # ---------- MODEL INSIGHTS ----------
  output$ins_table <- renderDT({
    m <- models[[input$ins_pos]]
    tbl <- data.frame(Attribute = names(m$correlations),
                       Correlation_with_OVERALL = round(m$correlations, 3))
    tbl <- tbl[order(-abs(tbl$Correlation_with_OVERALL)), ]
    datatable(tbl, rownames = FALSE, options = list(dom = 't', pageLength = 15))
  })
  output$ins_r2 <- renderText({
    m <- models[[input$ins_pos]]
    sprintf("Overall model fit for %s: R\u00b2 = %.3f (n = %d players)", input$ins_pos, m$r_squared, m$n)
  })
}

shinyApp(ui, server)
