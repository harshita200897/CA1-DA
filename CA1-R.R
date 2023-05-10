sleep_data <- read.csv("sleep.csv")

###Ques1 We need to determine the effect of Sleep on body temperature###

# H0: Body temperate is not affected by Sleep
# h1: Body temperature is affected by Sleep

#We already have body temp in numerical value, need to convert our sleeping 
#hours to 'factor' to signify if person slept or not

sleep_data$sleep_hours[sleep_data$sleep_hours != 0] <- 1
sleep_data$sleep_hours <- factor(sleep_data$sleep_hours, labels = c("no", "yes"))
str(sleep_data)

pairs(sleep_data, labels = colnames(sleep_data), main = "Sleep dataset correlation plot")

install.packages("psych")
library(psych)

pairs.panels(sleep_data,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


attach(sleep_data)
tapply(body_temp, sleep_hours, summary)

plot(sleep_data$sleep_hours,body_temp, pch = 19, col = "blue")

library("lattice")
attach(sleep_data)
histogram(~body_temp | sleep_hours, 
          data = sleep_data, 
          main = "Distribution of sleep data", 
          xlab = "Body_temp (Fahreneit)", 
          ylab = "Sleep_hours %")
detach(sleep_data)

attach(sleep_data)
qqnorm(body_temp)
# this line represents normal distribution
qqline(body_temp, col = "red")

opar <- par(no.readonly = TRUE)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))

with(sleep_data, {
  qqnorm(body_temp[sleep_hours == "yes"], 
         main = "Sleep data")
  qqline(body_temp[sleep_hours == "yes"])
})

with(sleep_data, {
  qqnorm(body_temp[sleep_hours == "no"], 
         main = "No Sleep data")
  qqline(body_temp[sleep_hours == "no"])
})

par(opar)


normality_test <- shapiro.test(sleep_data$body_temp)
normality_test$p.value

# We can check the normality in each variable
# using the tapply() function instead
with(sleep_data, tapply(body_temp, sleep_hours, shapiro.test))


wilcox.test(body_temp~sleep_hours)

