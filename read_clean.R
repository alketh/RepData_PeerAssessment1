# The variables included in this dataset are:
#  - steps: Number of steps taking in a 5-minute interval (missing values are coded as `NA`)
# - date: The date on which the measurement was taken in YYYY-MM-DD format
# - interval: Identifier for the 5-minute interval in which measurement was taken

read_clean <- function(data = "activity.csv") {
  act <- read.csv("activity.csv")
  act$date <- lubridate::ymd(act$date)
  
  return(act)
}
