# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)
library(stats)
library(car)

# Load the datasets
results <- read_csv("results.csv")
shootouts <- read_csv("shootouts.csv")
goalscorers <- read_csv("goalscorers.csv")

# Data Cleaning and Transformation
# Convert date columns to Date type
results$date <- as.Date(results$date)
shootouts$date <- as.Date(shootouts$date)
goalscorers$date <- as.Date(goalscorers$date)

# Add goal difference column to results
goal_difference <- results %>% 
  mutate(goal_difference = home_score - away_score)

# Aggregate data for visualizations
matches_per_year <- results %>% 
  group_by(year = format(date, "%Y")) %>% 
  summarise(matches = n())

# Top scorers
top_scorers <- goalscorers %>% 
  group_by(scorer) %>% 
  summarise(goals = n()) %>% 
  arrange(desc(goals)) %>% 
  head(10)

# Exploratory Data Analysis (EDA)
print(head(results))
print(head(shootouts))
print(head(goalscorers))

# Visualization 1: Matches per Year
ggplot(matches_per_year, aes(x = as.numeric(year), y = matches)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Number of Matches per Year",
       x = "Year", y = "Number of Matches")

# Visualization 2: Distribution of Goal Differences
ggplot(goal_difference, aes(x = goal_difference)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Goal Differences",
       x = "Goal Difference", y = "Frequency")

# Visualization 3: Top Scorers
ggplot(top_scorers, aes(x = reorder(scorer, goals), y = goals)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top 10 Scorers",
       x = "Scorer", y = "Goals")

# Hypothesis Testing
# Does home advantage significantly impact the scores?
home_away_comparison <- results %>% 
  dplyr::select(home_score, away_score) %>% 
  pivot_longer(cols = everything(), names_to = "location", values_to = "score")

home_away_ttest <- t.test(score ~ location, data = home_away_comparison)
print(home_away_ttest)

# Ensure goal_difference is numeric
results <- results %>%
  mutate(goal_difference = as.numeric(home_score - away_score))

# ANOVA: Goal Differences across Tournament Types
tournament_anova <- aov(goal_difference ~ tournament, data = results)
summary(tournament_anova)


# Post-Hoc Analysis
if (anova(lm(goal_difference ~ tournament, data = results))$`Pr(>F)`[1] < 0.05) {
  tournament_posthoc <- TukeyHSD(tournament_anova)
  print(tournament_posthoc)
}

# Visualization 4: Average Goal Difference by Tournament
goal_difference_by_tournament <- results %>% 
  group_by(tournament) %>% 
  summarise(avg_goal_diff = mean(goal_difference, na.rm = TRUE))

ggplot(goal_difference_by_tournament, aes(x = reorder(tournament, avg_goal_diff), y = avg_goal_diff)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Average Goal Difference by Tournament",
       x = "Tournament", y = "Average Goal Difference")

# Export to GitHub-ready folder structure
# Create directory structure for GitHub
if(!dir.exists("output")) {
  dir.create("output")
}
write.csv(results, "output/cleaned_results.csv")
write.csv(shootouts, "output/cleaned_shootouts.csv")
write.csv(goalscorers, "output/cleaned_goalscorers.csv")

# Save hypothesis test results to Excel
write.csv(broom::tidy(home_away_ttest), "output/home_away_ttest_results.csv")
write.csv(broom::tidy(tournament_anova), "output/tournament_anova_results.csv")
# Save Post-Hoc Results to CSV if TukeyHSD exists
if (exists("tournament_posthoc")) {
  posthoc_results <- as.data.frame(tournament_posthoc$tournament) # Extract the results
  write.csv(posthoc_results, "output/tournament_posthoc_results.csv")
}

