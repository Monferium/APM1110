# In a class of 50 students of computing, 23 are female and 27 are male. 
# The results of their first-year Java programming examination are given as follows

Females <- c(57, 59, 78, 79, 60, 65, 68, 71, 75, 48, 51, 55, 56, 41, 43,
             44, 75, 78, 80, 81, 83, 83, 85)
Males <- c( 48, 49, 49, 30, 30, 31, 32, 35, 37, 41, 86, 42, 51, 53, 56,
            42, 44, 50, 51, 65, 67, 51, 56, 58, 64, 64, 75)

Dataset <- c(Females, Males)
# If it was discovered that the mark for the 34th student was entered incorrectly 
# Should have been 46 instead of 86, use an appropriate editing procedure to change this.

Dataset[34] <- 46
Males[11] <- 46

stem(Females); stem(Males)
# Form the stem-and-leaf display for each gender, and discuss the advantages of this representation compared to the traditional histogram; 
par(mfrow=c(2,1))

hist(Females, col = 'salmon', main = "First Year Female Students", xlab = "Java Programming Examination Results", ylab = "Traditional Histogram Distribution")
hist(Males, col = 'gold', main = "First Year Male Students", xlab = "Java Programming Examination Results", ylab = "Traditional Histogram Distribution")


# Construct a box-plot for each gender and discuss the findings. 
par(mfrow=c(1,2))
boxplot(Females, col = 'lightgreen', main = "First Year Female Students", xlab = "Java Programming Examination Results", ylab = "Traditional Histogram Distribution")
boxplot(Males, col = 'steelblue', main = "First Year Male Students", xlab = "Java Programming Examination Results", ylab = "Traditional Histogram Distribution")
