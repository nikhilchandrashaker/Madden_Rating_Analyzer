# =========================================================
# Madden Ratings Requirements Analyzer — modeling pipeline
# =========================================================
# Goal: for each position, build a data-driven (not hand-authored)
# model of OVERALL ~ attributes, then use it for:
#   (1) FORWARD: target OVR -> typical/percentile attribute requirements
#   (2) REVERSE: my attributes -> predicted OVR + what to improve

set.seed(42)

df <- read.csv("/home/claude/madden/Madden_Ratings.csv", stringsAsFactors = FALSE)

# ---- Identify attribute columns (the ~55 numeric rating columns) ----
meta_cols <- c("ID","First.Name","Last.Name","Position","Position.ID","Team",
               "Team.ID","Overall","Age","College","Height","Weight",
               "Jersey.Number","Years.Pro","Archetype","Player.Image",
               "Team.Logo","RUNNINGSTYLE","OVERALL")
# note: R read.csv converts "First Name" -> "First.Name" etc.

attr_cols <- setdiff(names(df), meta_cols)
attr_cols <- attr_cols[sapply(df[attr_cols], is.numeric)]
cat("Number of candidate attribute columns:", length(attr_cols), "\n")
print(attr_cols)

positions <- unique(df$Position)
cat("\nPositions:", length(positions), "\n")

# ---- Feature selection + model fitting per position ----
# Data-driven: correlate each attribute with OVERALL within that position's
# players, keep the strongest ones (capped so we don't overfit small groups),
# then fit a linear model on the selected attributes.

fit_position_model <- function(pos_data, pos_name) {
  n <- nrow(pos_data)
  # correlation of each attribute with OVERALL (guard against zero-variance cols)
  cors <- sapply(attr_cols, function(a) {
    x <- pos_data[[a]]
    if (sd(x, na.rm = TRUE) == 0) return(0)
    suppressWarnings(cor(x, pos_data$OVERALL, use = "complete.obs"))
  })
  cors <- sort(cors[!is.na(cors)], decreasing = TRUE)

  # cap number of predictors relative to sample size (avoid overfitting)
  max_preds <- max(3, min(10, floor(n / 8)))
  keep <- names(cors)[abs(cors) > 0.25][1:min(max_preds, sum(abs(cors) > 0.25))]
  keep <- keep[!is.na(keep)]
  if (length(keep) < 2) {
    keep <- names(sort(abs(cors), decreasing = TRUE))[1:min(3, length(cors))]
  }

  form <- as.formula(paste("OVERALL ~", paste(keep, collapse = " + ")))
  model <- lm(form, data = pos_data)

  list(
    position = pos_name,
    n = n,
    predictors = keep,
    correlations = cors[keep],
    model = model,
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared
  )
}

position_models <- list()
for (p in positions) {
  pd <- df[df$Position == p, ]
  position_models[[p]] <- fit_position_model(pd, p)
}

# ---- Report model quality ----
summary_tbl <- do.call(rbind, lapply(position_models, function(m) {
  data.frame(Position = m$position, N = m$n, NumPredictors = length(m$predictors),
             R2 = round(m$r_squared, 3), AdjR2 = round(m$adj_r_squared, 3))
}))
summary_tbl <- summary_tbl[order(-summary_tbl$R2), ]
cat("\n=== Model fit quality by position ===\n")
print(summary_tbl, row.names = FALSE)

cat("\n=== Selected predictors per position ===\n")
for (p in positions) {
  m <- position_models[[p]]
  cat(sprintf("\n%s (n=%d, R2=%.3f):\n", p, m$n, m$r_squared))
  print(round(m$correlations, 2))
}

saveRDS(list(data = df, attr_cols = attr_cols, models = position_models),
        "/home/claude/madden/madden_model.rds")
cat("\nSaved model bundle to madden_model.rds\n")
