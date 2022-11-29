library(readxl)
data <- read_excel("C:/Users/erico/Desktop/ACADEMICS/MY UofT COURSES/YEAR ONE SEM TWO/HAD 5772H INTERMEDIATE STATISTICS FOR HSR/DAY 1/QUIZ 1/Case studies 01_data.xlsx")
View(data)

library(psych)
library(DescTools)
library(Rmisc)
library(FSA)
library(plyr)
library(boot)
library(lattice)
library(ggplot2)
library(vcd)
library(rcompanion)


#1. Which variables are the participant variables? (They act as independent variables in this study.)
#gender and sport are the participant variables



#2. What are the dependent variables?
#Improvement in anger is the dependent variable. It is measured as expression using an index of general anger expression calculated from control-out, control-in, anger-out, and anger-in.



# 3. Is Anger-Out a continuous or a categorical variable?
#Anger-out is a continuous variable (specifically, an interval or scale as it is non-zero). It would be ordinal (or categorical variable) if the values were less than seven (7).


# 4. Does Anger-Out have a positive skew, a negative skew, or no skew?
#since it is continuous variable, we would use histogram.
#Anger-Out has a positive skew as the tail of its distribution is tilted to the right and the mean (16.08) is greater than the median (16).

hist(data$anger_out, col="gray",
     main="", xlab="Skewness of anger-out")

describe (data$anger_out)



# 5. What are the mean and standard deviation of the Anger-Out scores?
# mean of anger-out is 16.0769 and the standard deviation of anger-out is 4.2174
mean(data$anger_out)




sd(data$anger_out)



# 6. Is there a difference in how much males and females use aggressive behavior to improve an
#angry mood? To answer this question, create a parallel box plot for the males and females for
#Anger-Out scores.
#Yes. There is a difference in how much males and females use aggressive behavior to improve an angry mood.
#Remember 1 = males, 2 = females
#The median anger-out scores for males is higher (16) than that of females (15) and 50% of each group is above the median according to the box plot.
#range for male is 18 and female is 17

boxplot(anger_out ~ gender,
        data=data, xlab="Gender", ylab="Anger-Out scores", main="Difference in Anger improvement by Gender")


#median
describeBy(data$anger_out,group = data$gender, digits= 4)


#7. What is the range of the Anger-In scores?
#The range of anger-in scores is 21
describe (data$anger_in)




# 8. Create parallel box plots for the Anger-In scores by sports participation. Is there a difference in
#the distributions for athletes and non-athletes?
#Remember, 1 = athletes, 2 = non-athletes
#The median anger-in score for athletes is 17 (lower) and that of non-athletes is 20 (higher) and 50% of each group is above the median.
#range is 14 for athletes and 21 for non-athletes
#Yes. There is difference in the distributions of the Anger-In scores for athletes and non-athletes.


boxplot(anger_in ~ sports,
        data=data, xlab="sports", ylab="Anger-in scores", main="Difference in Anger improvement by sports participation")

describeBy(data$anger_in,group = data$sports, digits= 4)


# 9. Plot a histogram of the distribution of the Control-Out scores. What is the shape of it?
# The shape of Control-Out scores is symmetrical as it is not tilted to the left or right and also the mean (24 approximately) is the same as the median (24).

hist(data$control_out, col="gray",
     main="", xlab="Skewness of control-out")

describe (data$control_out)


# 10. What is the mean Control-Out score for the entire sample?
#The mean Control-Out score for the entire sample is 23.69
describe (data$control_out)




# 11. What is the mean Control-Out score for the athletes?
#Remember: 1 = athletes, 2 = non-athletes
# mean Control-Out score for the athletes is 24.68

describeBy(data$control_out,group = data$sports, digits= 4)




# 12. What is the mean Control-Out score for the non-athletes?
#Remember: 1 = athletes, 2 = non-athletes
# mean Control-Out score for the non-athletes is 23.23

describeBy(data$control_out,group = data$sports, digits= 4)



# 13. What is the standard deviation of the Control-In scores for the athletes?
#Remember: 1 = athletes, 2 = non-athletes
# standard deviation of the Control-In scores for the athletes is 4.53 

describeBy(data$control_in,group = data$sports, digits= 4)



# 14. What is the standard deviation of the Control-In scores for the non-athletes?
#Remember: 1 = athletes, 2 = non-athletes
# standard deviation of the Control-In scores for the non-athletes is 4.78

describeBy(data$control_in,group = data$sports, digits= 4)


# 15. Plot parallel box plots of the Anger Expression Index by sports participation. Does it look like
#there are any outliers?
#There is an outlier in the Anger Expression Index among the non-athletes
boxplot(anger_expression ~ sports,
        data=data, xlab="sports", ylab="anger_expression", main="Difference in Anger expression by sports participation")




# 16. Plot parallel box plots of the Anger Expression Index by gender. Does it look like there are any
#outliers?
#There is an outlier in the Anger Expression Index among the females

boxplot(anger_expression ~ gender,
        data=data, xlab="gender", ylab="anger_expression", main="Difference in Anger expression by gender")




#17. What is the correlation between the Control-In and Control-Out scores?
#Both control-in and control-out are not skewed based on their histogram and also their means are approximately equal to their median so we use Pearson's r
#There is a strong (r=0.72) positive relationship between the Control-In and Control-Out scores.

hist(data$control_in, col="gray",
     main="", xlab="Skewness of Control-In")

hist(data$control_out, col="gray",
     main="", xlab="Skewness of Control-out")


describe (data$control_out)
describe (data$control_in)




x<-cbind(data$control_in, data$control_out)
cor(x, y = NULL, use = "everything", method = "pearson")


ggplot(data, aes(x=control_in, y=control_out)) + geom_point()+
  labs(x="control_in, $", y = "control_in, $")+
  theme_classic()


# 18. Would you expect the correlation between the Anger-Out and Control-Out scores to be positive
#or negative? Compute this correlation.
#Based on the scatter plot, I expect the correlation between Anger-Out and Control-Out scores to be negative.
#The correlation between Anger-Out and Control-Out scores is -0.58 when I use Pearson's r, because Anger-out is skewed.
#However, since control-out is not skewed, when I use Spearman's Rho, I realize that the correlation between Anger-Out and Control-Out scores is -0.55.

hist(data$anger_out, col="gray",
     main="", xlab="Skewness of anger_out")

hist(data$control_out, col="gray",
     main="", xlab="Skewness of Control-out")

describe (data$control_out)
describe (data$anger_out)


ggplot(data, aes(x=anger_out, y=control_out)) + geom_point()+
  labs(x="anger_out, $", y = "control_out, $")+
  theme_classic()


x<-cbind(data$anger_out, data$control_out)
cor(x, y = NULL, use = "everything", method = "pearson")


x<-cbind(data$anger_out, data$control_out)
cor(x, y = NULL, use = "everything", method = "spearman")


#19. How many female athletes are in the sample?
#remember 1 = males, 2 = females
#Remember: 1 = athletes, 2 = non-athletes

#There are 14 female athletes in the sample






# 20. How many females are in the sample?
#remember 1 = males, 2 = females
# females in the sample are 48 in total
describeBy(data$gender,group = data$gender, digits= 4)
