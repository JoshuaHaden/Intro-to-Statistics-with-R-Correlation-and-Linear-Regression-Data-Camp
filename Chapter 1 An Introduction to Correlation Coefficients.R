###Chapter 1 An Introduction to Correlation Coefficients

###Manual Computation of Correlation Coefficients (1)
# A and B are in Data Camp's workspace
#  The vectors `A` and `B` have already been loaded

# Take a quick peek at both vectors
A
B

# Save the differences of each vector element with the mean in a new variable
diff_A <- A - mean(A)
diff_B <- B - mean(B)

# Do the summation of the elements of the vectors and divide by N-1 in order to acquire the covariance between the two vectors
cov <- sum(diff_A*diff_B)/(3-1)

###Manual Computation of Correlation Coefficients (2)
# Your workspace still contains the results of the previous exercise

# Square the differences that were found in the previous step
sq_diff_A <- diff_A^2
sq_diff_B <- diff_B^2

# Take the sum of the elements, divide them by N-1 and consequently take the square root to acquire the sample standard deviations
sd_A <- sqrt(sum(sq_diff_A)/(3-1))
sd_B <- sqrt(sum(sq_diff_B)/(3-1))

###Manual Computation of Correlation Coefficients (3)
# Your workspace still contains the results of the previous exercise

# Combine all the pieces of the puzzle
correlation <- cov/(sd_A*sd_B)
correlation

# Check the validity of your result with the cor() command
cor(A,B)

###Creating Scatterplots
# Read data from a URL into a dataframe called PE (physical endurance)
PE <- read.table("http://assets.datacamp.com/course/Conway/Lab_Data/Stats1.13.Lab.04.txt", header = TRUE)

# Summary statistics
summary(PE)

# Scatter plots
plot(PE$age ~ PE$activeyears)
plot(PE$endurance ~ PE$activeyears)
plot(PE$endurance ~ PE$age)

###Correlation Matrix
# PE is already loaded in

# Correlation Analysis
round(cor(PE[2:4]), 2)   

# Do some correlation tests. If the null hypothesis of no correlation can be rejected on a significance level of 5%, then the relationship between variables is significantly different from zero.
cor.test(PE$age, PE$activeyears) 
cor.test(PE$endurance, PE$activeyears)
cor.test(PE$endurance, PE$age)

###Non-Representative Data Samples
# The impact dataset is already loaded in
# impact dataset in Data Camp's workspace
# Summary statistics entire dataset
summary(impact)

# Calculate correlation coefficient 
entirecorr <- round(cor(impact$vismem2,impact$vermem2),2)

# Summary statistics subsets
describeBy(impact,impact$condition)

# Create 2 subsets: control and concussed
control <- subset(impact,impact[,2] == "control")
concussed <- subset(impact,impact[,2] == "concussed")

# Calculate correlation coefficients for each subset
controlcorr <- round(cor(control$vismem2,control$vermem2),2)
concussedcorr <- round(cor(concussed$vismem2,concussed$vermem2),2)

# Display all values at the same time
correlations <- cbind(entirecorr,controlcorr,concussedcorr)
correlations

