# Simulating a linear regression
# 1.  Deciding on the true underlying relationship: For simplicity, let's assume a
#     simple linear relationship <- Y=\beta_0+\beta_1X+\epsilon, where
#     \epsilon is the error term.
#
# 2.  Simulate the predictor variable(s): Let's simulate X from a normal distribution.
# 3.  Generate the error term: Typically, the error term \epsilon is simulated from
#     a normal distribution with mean 0.
# 4. Compute the response variably Y: Using the true underlying relationship, generate
#                                     Y.
# 5.  Fit the linear regression model:  Using the "lm" function in R.
#--------------------------------------------------------------------

# Set the seed for reproducibility
set.seed(123)

n <- 100
X <- rnorm(n, mean=50, sd=10)
beta_0 <- 5
beta_1 <- 2
epsilon <- rnorm(n, mean=0, sd=5)

Y <- beta_0 + beta_1 * X + epsilon
model <- lm(Y ~ X)

summary(model)
y_pred <- predict(model, newdata = data.frame(X = X))

# Plot the data in a minimal style
plot(X, Y, pch=19, col="darkgray", xlab="X", ylab="Y", axes=TRUE, frame.plot=FALSE)
abline(h=0, v=0, col="black", lwd=1) # Add x and y axes
abline(model, col="lightblue", lwd=2)

# Plot interpretation
# 1.  Data distribution:  The data points are distributed in an upward trend, suggesting a positive correlation between
#                         X and Y. As the values of X increase, the values of Y tend to increase as well.
#
# 2.  Regression Line:  The light blue line represents the linear regression model fitted to the data. It aims to provide
#                       the best linear description of the relationship between X and Y.
#
# 3.  Scatter around the Line:  There's some scatter around the regression line, which indicates the variability in the data.
#                               This variability is due to the epsilon "epsilon" term you added in your regression model,
#                               which represents random error.
#
# 4.  Slope:  The slope of the regression line appears to be positive, which confirms the positive relationship between X and Y.
#             Given the code, the slope (or coefficient for X) is set to beta_1=2, which means for every one unit in
#             increase in X, Y is expected to increase by 2 units on average, holding other factors constant.
#
# 5.  Intercept:  The point where the regression line crosses the y-axis is the y-intercept. In the code, the beta_0=5,
#                 which represents the expected value of Y when X is 0.
#
# In summary, the plot visualizes a positive linear relationship between X and Y as captured by the linear regression model.
# The regression line represents the best linear fit to the data, taking into account the random term introduced by the
# "epsilon" term.
#----------------------------------------------------------------------------------------------------------------------

# model summary Interpretation
summary(model)

# 1.  Model Formula:  lm(formula = Y ~ X)
#                     This indicates that the linear model is predicting the dependent variable "Y"
#                     based on the independent variable "X".
#
# 2.  Residuals:  Residuals are the difference between the observed values of the dependent variable (Y)
#                 and the values predicted by the model.
#                   ->  "Min", "1Q", "Median", "3Q", "Max" are the summary statistics for the residuals, giving
#                       you an idea of their distribution. For instance, the "Median" value being close to 0
#                       suggests that, on average, the model's predictions are fairly accurate.
#
# 3.  Coefficients: These provide the details about the relationship between the independent variable(s) and the
#                   dependent variable.
#       * "(Intercept)":  is the y-intercept. Its estimate is "5.79778", which means when X is 0, the predicted value of Y is
#                         approximately 5.798.
#       * "X":  is the slope of the regression line. Its estimate "1.97376", which means for every one unit increase in X,
#               Y increases by approximately 1.974 units.
#       * "Std. Error": Indicates the standard error of the coefficients, a measure of the variability of the coefficient.
#       * "t value":  is the test statistic for the hypothesis that the coefficient is equal to zero (nopeffect). A large
#                     t-value suggests that the coefficient is statistically different from 0.
#       * "Pr(>|t|)": is the p-value for the t-test. A small p-value (<0.05) typically indicates that you can reject the null
#                     hypothesis, suggesting the coefficient is significant. Here, both the intercept and the slope for
#                     X are significant.
#
# 4.  Signif. codes:  This is a key explaining the significance stars. The more stars, the higher the significance.
#                     For example, "***" indicates a p-values less than 0.001, which is highly significant.
#
# 5.  Residual Standard Error:  This is the average amount that the response will deviate from the true regression line.
#                               It's "4.854" on "98 degrees of freedom". The degrees of freedom is n-2 for simple linear
#                               regression, where n is the number of observations.
#
# 6.  Multiple R-squared: This is the proportion of the variance in the dependent variable that is predictable from the independent
#                         variable(s). A value of "0.933" suggests that 93.3% of the variability in "Y" can be explained by "X".
#
# 7. Adjusted R-squared:  This is just the adjusted proportion of the variance explained, taking into account the number of predictors in the
#                         model. For simple linear regression, this value is very close to the Multiple R-squared. It's more relevant in
#                         multiple regression.
#
# 8.  F-statistic:  This is a measure of how significant the fit is. The value of 1364 on 1 and 98 DF is very high, suggesting a relationship
#                   between X and Y.
#
# 9.  P-value for the F-statistic:  The p-value associated with the F-statistic tests the hypothesis that the model with no
#                                   independent variables fits the data as well as your model. A very small  p-value (<0.05) indicates
#                                   your model provides a better fit to the data than a model with no independent variables.
#--------------------------------------------------------------------------------------------------------------------------------------------
#

# Creating the Mean Squared Error Function (MSE)
mse_function <- function(y_true, y_pred){
  return(mean((y_true - y_pred)^2))
}

mse_value <- mse_function(Y, y_pred)
print(mse_value)

# MSE interpretation
# The mean Squared Error (MSE) is a measured of the average of the squares of the errors or deviations
# between the actual and estimated values. It is one of the most commonly used metrics to evaluate
# the performance of regression models.
# INTERPRETATION: 
#   * Magnitude:  The MSE is always non-negative. A value of 0 indicates that the model's predictions are perfect
#                 (which rarely happens in real-life scenarios). The larger the MSE, the larger the errors the
#                 the model is making in its predictions.
#
#   * Units:  The MSE has the squared units of whatever variable you're predicting. For instance, if you're predicting house prices
#             in thousands of dollars, than an MSE of 23.08603 would be units of (thousands of dollars)^2.
#
# MSE VALUE OF 23.08603
# ---------------------
#   If your MSE is 23.08603, it means, on average, the squared difference between your predicted values and the actual values is
#   23.086603. In practice, to get a more  intuitive sense of the error magnitude, you might take the square root of the MSE,
#   known as the Root Mean Squared Error (RMSE). The RMSE would be in the same units as your target variable.
#
#   For the given MSE value of 23.08603, the RMSE would be:
#   RMSE = \sqrt{MSE} = \sqrt{23.08603} \approx 4.8
#
#   So, in the context of our example, if we're predicting house prices in thousands of dollars,
#   then on average, our prediction are off about by 4.8 thousand dollars.

# Plotting the MSE

# 1.  Extract predicted values from the model
predicted <- predict(model)
# 2.  Compute residuals
residuals <- Y - predicted
# 3.  Calculate the MSE
mse <- mean(residuals^2)

# 4.  Plot the actual vs. predicted values and visualze the residuals
plot(X, Y, main=paste("MSE", round(mse, 3)), xlab="X", ylab="Y", col="blue", pch=19, frame.plot=FALSE)
points(X, predicted, col="red", pch=19)
abline(model, col="green", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), pch=19, bg="white")
#   Optional: Add lines to represent residuals (errors)
segments(X, Y, X, predicted, col="grey", lty=2)














