# Load the sleep dataset
sleep_data <- read.csv("sleep.csv")

# Create a boxplot of REM sleep vs snoring rate
boxplot(rem ~ snor_rate, sleep_data = sleep_data, main = "Boxplot of REM Sleep vs Snoring Rate")

# Perform an ANOVA to test for significant differences in REM sleep across snoring rate groups
rem_anova <- aov(rem ~ snor_rate, sleep_data = sleep_data)
summary(rem_anova)
