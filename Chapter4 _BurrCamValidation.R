library(dplyr)
library(lubridate)

camera_val <- read_csv("Camera_validation_thesis.csv")

dat <- camera_val |>
  mutate(datetime=as.POSIXct(paste(date,time), format = "%Y-%m-%d %H:%M:%S"))

# Keep only rows where dragon was seen on camera
present_only <- dat |>
  filter(camera_pres == 1)

# Count true positives and false negatives
TP <- sum(present_only$pit_pres == 1)  # PIT tag detected when dragon was seen
FN <- sum(present_only$pit_pres == 0)  # PIT tag NOT detected when dragon was seen

# Calculate metrics
sensitivity <- TP / (TP + FN) # TP = TRUE POSITIVE, FN = FALSE NEG
false_negative_rate <- FN / (TP + FN)

# Output
validation_results <- data.frame(
  True_Positive = TP,
  False_Negative = FN,
  Sensitivity = round(sensitivity, 2),
  False_Negative_Rate = round(false_negative_rate, 2)
)

validation_results
