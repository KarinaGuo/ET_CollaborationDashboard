library(dplyr)

# Working directory is handled automatically by Shiny
# setwd("~/RBGSyd_Technical Officer/MyrtleRust/Shiny app/")

IR_Multiplier <- data.frame(
  ImmuneResponse = c("HR", "R", "MRMS", "MS", "S", "HS"),
  Multiplier = c(0, 0.25, 0.5, 0.75, 0.875, 1)
)

individuals_scoring_COI <- read.csv("Data/3_Scoring_results.csv") %>%
  left_join(IR_Multiplier, by = "ImmuneResponse") %>%
  mutate(summary_score = Coverage * Multiplier) |> 
  dplyr::select(-Multiplier)

write.csv(individuals_scoring_COI, file = "Data/3a_Scoring_results_COI.csv")
