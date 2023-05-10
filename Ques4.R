# Load the sleep dataset
sleep_data <- read.csv("sleep.csv")

# Remove any missing values
sleep_data <- na.omit(sleep)

# Create two groups based on stress level
low_stress <- sleep_data$heart_rate[sleep_data$stress_level == 0]
high_stress <- sleep_data$heart_rate[sleep_data$stress_level == 4]

# Conduct a t-test
t_test <- t.test(low_stress, high_stress)

# Print the test result
t_test

# Create a boxplot of heart rate by stress level
boxplot(sleep_data$heart_rate ~ sleep_data$stress_level, xlab="Stress Level", ylab="Heart Rate", main="Boxplot of Heart Rate by Stress Level")

# Conduct an ANOVA  
anova_test <- aov(sleep_data$heart_rate ~ sleep_data$stress_level)

# Print the test result
summary(anova_test)

