# Madden 26 Rating Requirements Analyzer

## What this is
A data-driven tool built from your actual Madden 26 ratings file (2,035 players,
22 positions). For each position, it fits a linear regression of OVERALL on that
position's ratings, selecting predictor attributes by correlation strength
(not hand-authored) — model fit is R² = 0.85–0.99 for nearly every position,
since Madden's OVERALL is close to a deterministic formula per position.

## Files
- `Madden_Ratings.csv` — the source data
- `prep_and_model.R` — builds the per-position models; run this first if you
  ever refresh the data (produces `madden_model.rds`)
- `madden_model.rds` — the saved model bundle the app loads
- `app.R` — the Shiny app (three tabs, see below)

## Running it
```r
setwd("madden_ovr_analyzer")
shiny::runApp("app.R")
```
Requires the `shiny` and `DT` packages:
`install.packages(c("shiny","DT"))`, or on Debian/Ubuntu:
`sudo apt install r-cran-shiny r-cran-dt`.

## The three tabs
1. **Target OVR → Attributes** — pick a position and target overall rating;
   get the typical (median) build plus 25th/75th percentile ranges, drawn
   from real players near that rating, plus a table of the closest real
   comparable players.
2. **My Attributes → Predicted OVR** — sliders for that position's key
   attributes (defaulted to the position median); predicts your overall
   rating, and if you set a target OVR, suggests which attributes to raise
   (and by how much) to close the gap, weighted by which attributes matter
   most for that position.
3. **Model Insights** — shows which attributes actually drive OVERALL for
   each position (correlation strength) and the model's R².

## Notes / next steps if you want to extend it
- The "suggested increases" logic in the reverse tab is intentionally simple
  (splits the OVR gap across the top 5 positively-weighted attributes,
  proportional to each one's regression coefficient) — swap in a proper
  optimizer or an Elastic Net / regularized model if you want tighter
  suggestions.
- Long Snapper (R²=0.67) and Sam Backer/Fullback (small sample sizes) are
  the weakest-fit positions — worth flagging in the UI or merging with a
  similar position group if this becomes a real deliverable.
