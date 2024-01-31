results <- read.csv("C:/Users/79FV/Downloads/results.csv", header = T)

attach(results)
names(results)

# We can quickly visualize the distribution of values in this dataset by creating a histogram:
par(mfrow=c(2,2))

hist(arch1, col ='steelblue', main="Architecture 1")
hist(prog1, col ='lightgreen', main="Programming 1") # Light green complements steel blue
hist(arch2, col ='salmon', main="Architecture 2") # Salmon is also a nice complement
hist(prog2, col ='gold', main="Programming 2") # Gold can work well with steel blue

# To begin, install the 'moments' package if it's not already installed
install.packages("moments")

# Secondly, through this specific library, we can automatically calculate the skewness

library(moments) # Enabling the skewness() and kurtosis() function

# In Horgan (2020), the equation 2.1 highlights that skewness of the data can be recalculated as:
# skewness = 3 * (mean - median)/standard deviation but only as approximation
auto_skew <- sapply(results[,2:5], skewness, na.rm = TRUE)
names(auto_skew) <- colnames(results)[2:5]
print(auto_skew)
# Determine the following results made by mean(), median(), and sd() functions.
# 
# # Store the means in a named vector
# mean_values <- colMeans(results[, 2:5], na.rm = TRUE)
# 
# # If you want to name the vector elements with the column names
# names(mean_values) <- colnames(results)[2:5]
# 
# # Output the vector to see the means and their corresponding column names
# print(mean_values)
# 
# # Calculate the medians for each of the data columns, excluding the first column for gender
# data_medians <- sapply(results[, 2:5], median, na.rm = TRUE)
# 
# # Name the elements of the vector with the column names
# names(data_medians) <- colnames(results)[2:5]
# 
# # Output the vector to see the medians and their corresponding column names
# print(data_medians)
# 
# # Calculate the standard deviations for each of the data columns, excluding the first column for gender
# data_sds <- sapply(results[, 2:5], sd, na.rm = TRUE)
# 
# # Name the elements of the vector with the column names
# names(data_sds) <- colnames(results)[2:5]
# 
# # Output the vector to see the standard deviations and their corresponding column names
# print(data_sds)


# Apply the skewness calculation as an anonymous function within sapply
data_skewness <- sapply(results[, 2:5], function(x) {
  # To emphasize once more, skewness = 3 * (mean - median)/standard deviation according to equation 2.1 (Horgan, 2020)
  3 * (mean(x, na.rm = TRUE) - median(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
})
# Name the elements of the vector with the column names
names(data_skewness) <- colnames(results)[2:5]
# Print the skewness for each column
print(data_skewness)

par(mfrow=c(1,1)) # Position Reset
# Set up the layout for the plots
par(mfrow=c(2,2))

# Density plot for Architecture 1
plot(density(arch1, na.rm = TRUE), col ='steelblue', main="Architecture 1", xlab="Value", ylab="Density")

# Density plot for Programming 1
plot(density(prog1, na.rm = TRUE), col ='lightgreen', main="Programming 1", xlab="Value", ylab="Density")

# Density plot for Architecture 2
plot(density(arch2, na.rm = TRUE), col ='salmon', main="Architecture 2", xlab="Value", ylab="Density")

# Density plot for Programming 2
plot(density(prog2, na.rm = TRUE), col ='gold', main="Programming 2", xlab="Value", ylab="Density")
