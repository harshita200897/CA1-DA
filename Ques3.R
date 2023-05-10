# Load the dataset
sleep_data <- read.csv("sleep.csv")
# Define the variables of interest
blood_oxy <- sleep_data$blood_oxy
limb_mov <- sleep_data$limb_mov

# Plot the relationship between the variables
plot(limb_mov, blood_oxy, xlab="Limb Movement", ylab="Blood Oxygen",
     main="Scatterplot of Blood Oxygen and Limb Movement")

# Check for outliers
boxplot(blood_oxy, limb_mov, main="Boxplot of Blood Oxygen and Limb Movement")

# Check for normality
par(mfrow=c(1,2))
hist(blood_oxy, main="Histogram of Blood Oxygen", xlab="Blood Oxygen")
qqnorm(blood_oxy, main="Normal QQ plot of Blood Oxygen")
qqline(blood_oxy, col="red")

hist(limb_mov, main="Histogram of Limb Movement", xlab="Limb Movement")
qqnorm(limb_mov, main="Normal QQ plot of Limb Movement")
qqline(limb_mov, col="red")

# Calculate the correlation coefficient and p-value
correlation <- cor(blood_oxy, limb_mov)
p_value <- cor.test(blood_oxy, limb_mov)$p.value
print(paste0("Correlation coefficient: ", correlation))
print(paste0("p-value: ", p_value))



# Add a trendline to the plot
abline(lm(blood_oxygen ~ limb_movement), col="red")
