###Chapter 3 Linear Regression Models continued

###Calculating the Sum of Squared Residuals
# The `impact` dataset is already loaded 
# impact dataset in Data Camp's workspace

# Create a linear regression with `ic2` and `vismem2` as regressors
model_1 <- lm(impact$sym2 ~ impact$ic2 + impact$vismem2)

# Extract the predicted values
predicted_1 <- fitted(model_1)

# Calculate the squared deviation of the predicted values from the observed values 
deviation_1 <- (impact$sym2 - predicted_1)^2

# Sum the squared deviations
SSR_1 <- sum(deviation_1)


# Create a linear regression with `ic2` and `vermem2` as regressors
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Extract the predicted values
predicted_2 <- fitted(model_2)

# Calculate the squared deviation of the predicted values from the observed values 
deviation_2 <- (impact$sym2 - predicted_2)^2

# Sum the squared deviations
SSR_2 <- sum(deviation_2)

#Compare the sum of squared residuals of these two models
SSR_1
SSR_2

###Standardized Linear Regression
# The dataset `impact` is already loaded

# Create a standardized simple linear regression
model_1_z <- lm(scale(impact$sym2) ~ scale(impact$ic2))

# Look at the output of this regression model
summary(model_1_z)

# Extract the R-Squared value for this regression
r_square_1 <- summary(model_1_z)$r.squared

# Calculate the correlation coefficient
corr_coef_1 <- sqrt(r_square_1)


# Create a standardized multiple linear regression
model_2_z <- lm(scale(impact$sym2) ~ scale(impact$ic2)+scale(impact$vismem2))

# Look at the output of this regression model
summary(model_2_z)

# Extract the R-Squared value for this regression
r_square_2 <- summary(model_2_z)$r.squared

# Calculate the correlation coefficient
corr_coef_2 <- sqrt(r_square_2)

###Plotting Residuals
# Extract the residuals from the model
residual <- resid(model_2)

# Draw a histogram of the residuals
hist(residual)

# Extract the predicted symptom scores from the model
predicted <- fitted(model_2)

# Plot the predicted symptom scores against the residuals
plot(residual ~ predicted, main = "Scatterplot", xlab = "Model 2 Predicted Scores", ylab = "Model 2 Residuals")
abline(lm(residual ~ predicted), col="red")



