
####Ques 2 Is there any relation between heart rate and respiration 
#rate during sleep?###

sleep_data <- read.csv("sleep.csv")

# Check the normality of the heart_rate variable
qqnorm(sleep_data$heart_rate)
qqline(sleep_data$heart_rate)

# Check the normality of the resp_rate variable
qqnorm(sleep_data$resp_rate)
qqline(sleep_data$resp_rate)

#fitting a linear regression model
model <- lm(heart_rate ~ resp_rate, data = sleep_data)

# Get the R-squared value to measure the strength of the relationship
r_squared <- summary(model)$r.squared

# Perform a hypothesis test using a significance level of 0.05
alpha <- 0.05
p_value <- summary(model)$coefficients[2,4]

# Check if the p-value is less than the significance level
if (p_value < alpha) {
  cat("There is a significant relation (p =", p_value, ") between heart rate and respiration rate during sleep (R-squared =", r_squared, ").\n")
} else {
  cat("There is no significant relation (p =", p_value, ") between heart rate and respiration rate during sleep (R-squared =", r_squared, ").\n")
}



# Calculate the correlation between heart rate and respiration rate
correlation <- cor(sleep_data$heart_rate, sleep_data$resp_rate)
correlation
# Perform a hypothesis test
t_value <- (correlation * sqrt(nrow(sleep_data) - 2)) / sqrt(1 - correlation^2)
p_value <- 2 * pt(abs(t_value), df = nrow(sleep) - 2, lower.tail = FALSE)

# Print the results
cat("Pearson correlation coefficient:", correlation, "\n")
cat("t-value:", t_value, "\n")
cat("p-value:", p_value, "\n")

# Check for significance at a 5% level
if (p_value < 0.05) {
  cat("There is a significant correlation between heart rate and respiration rate during sleep.\n")
} else {
  cat("There is no significant correlation between heart rate and respiration rate during sleep.\n")
}

