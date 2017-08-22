###Chapter 2 Introduction to Linear Regression

###Impact Experiment
# The dataset `impact` is already loaded
# impact is in Data Camp's workspace

# Look at the dataset. Note that the variables we are interested in are on the 9th to 14th columns
impact

# Create a correlation matrix for the dataset
correlations <- cor(impact[9:14])

# Create the scatterplot matrix for the dataset
corrplot(correlations)

###Manual Computation of a Simple Linear Regression
# The dataset `impact` is already loaded.

# Calculate the required means, standard deviations and correlation coefficient
mean_sym2 <- mean(impact$sym2)
mean_ic2 <- mean(impact$ic2)
sd_sym2 <- sd(impact$sym2)
sd_ic2 <- sd(impact$ic2)
r <- cor(impact$ic2, impact$sym2)

# Calculate the slope
B_1 <- r * (sd_sym2)/(sd_ic2)

# Calculate the intercept
B_0 <- mean_sym2 - B_1 * mean_ic2

# Plot of ic2 against sym2
plot(impact$ic2, impact$sym2, main = "Scatterplot", ylab = "Symptoms", xlab = "Impulse Control")

# Add the regression line
abline(B_0, B_1, col = "red")

###Executing a Simple Linear Regression using R
# The dataset impact is still loaded

# Construct the regression model
model_1 <- lm(impact$sym2 ~ impact$ic2)

# Look at the results of the regression by using the summary function
summary(model_1)

# Create a scatter plot of Impulse Control against Symptom Score and add a regression line
plot(impact$sym2 ~ impact$ic2, main = "Scatterplot", ylab = "Symptoms", xlab = "Impulse Control")

# Add a regression line
abline(model_1, col = "red")

###Executing a Multiple Regression in R
# The impact dataset is already loaded 

# Multiple Regression
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Examine the results of the regression
summary(model_2)

# Extract the predicted values
predicted <- fitted(model_2)

# Plotting predicted scores against observed scores
plot(predicted ~ impact$sym2, main = "Scatterplot", ylab = "Predicted Scores", xlab = "Observed Scores")
abline(lm(predicted ~ impact$sym2), col = "green")

