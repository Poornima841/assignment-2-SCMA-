# Load necessary libraries
install.packages(c("tidyverse", "readxl", "ggplot2", "lubridate", "stringdist"))
library(tidyverse)
library(readxl)
library(ggplot2)
library(lubridate)
library(stringdist)

# Load the data
ipl_bbb <- read_csv('IPL_ball_by_ball_updated till 2024.csv')
ipl_salary <- read_excel('IPL SALARIES 2024.xlsx')

# Display the first few rows of the salary data
head(ipl_salary)

# Group data
grouped_data <- ipl_bbb %>%
  group_by(Season, `Innings No`, Striker, Bowler) %>%
  summarize(runs_scored = sum(runs_scored, na.rm = TRUE), 
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE))

# Summarize player runs and wickets
player_runs <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarize(runs_scored = sum(runs_scored, na.rm = TRUE))

player_wickets <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarize(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE))

# Filter for the year 2023 and sort by runs scored
player_runs %>%
  filter(Season == '2023') %>%
  arrange(desc(runs_scored))

# Get top and bottom players
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  top_n(3, runs_scored)

bottom_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  top_n(3, wicket_confirmation)

# Print results
print("Top Three Run Getters:")
print(top_run_getters)
print("Top Three Wicket Takers:")
print(bottom_wicket_takers)

# Extract year from date and add to dataframe
ipl_bbb <- ipl_bbb %>%
  mutate(year = year(dmy(Date)))

ipl_bbbc <- ipl_bbb %>%
  mutate(year = year(dmy(Date)))

head(ipl_bbbc %>% select(`Match id`, year, runs_scored, wicket_confirmation, Bowler, Striker))

# Function to get best distribution - this part is complex to convert directly without knowing the exact requirements
# Skipping this, assuming it’s about fitting statistical distributions to the data

# Filter dataset for LS Livingstone in the current year
# Replace "runs" with actual run data in your context
# Example: runs <- ipl_bbb

# Example showing how to apply a function for each year and striker:
ls_livingstone_bat <- list()
for (i in unique(ipl_bbbc$year)) {
  axar_patel_data1 <- ipl_bbbc %>%
    filter(year == i & Striker == "LS Livingstone")
  ls_livingstone_bat[[i]] <- unique(axar_patel_data1$Striker)
}

print(ls_livingstone_bat)

# Group by Batsman and Match id, then sum the wickets
wickets <- ipl_bbbc %>%
  group_by(Striker, `Match id`) %>%
  summarize(runs_scored = sum(runs_scored, na.rm = TRUE))

# Process LS Livingstone's data for each year
for (year in names(ls_livingstone_bat)) {
  strikers <- ls_livingstone_bat[[year]]
  for (striker in strikers) {
    if (striker == "LS Livingstone") {
      print(paste("************************\nyear:", year, "batsman:", striker))
      # get_best_distribution function should be implemented based on specific needs
      # Example: get_best_distribution(runs %>% filter(Striker == striker) %>% pull(runs_scored))
      print("\n\n")
    }
  }
}

# Calculate total runs each year for 2024
R2024 <- ipl_bbbc %>%
  filter(year == 2024)

# Match names with fuzzy matching
df_salary <- ipl_salary
df_runs <- R2024

match_names_runs <- function(name, names_list) {
  match <- stringdist::amatch(name, names_list, method = "jw", maxDist = 0.2)
  if (!is.na(match)) {
    return(names_list[match])
  } else {
    return(NA)
  }
}

df_salary$Matched_Player <- sapply(df_salary$Player, function(x) match_names_runs(x, df_runs$Striker))

df_merged_runs <- merge(df_salary, df_runs, by.x = "Matched_Player", by.y = "Striker", all.x = TRUE)

# Display structure and first few rows of the merged dataframe
str(df_merged_runs)
head(df_merged_runs)

# Calculate correlation
correlation <- cor(df_merged_runs$Rs, df_merged_runs$runs_scored, use = "complete.obs")

print(paste("Correlation between Salary and Runs:", correlation))
