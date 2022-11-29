if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(rstatix)){install.packages("rstatix")}
if(!require(broom)){install.packages("broom")}
if(!require(datarium)){install.packages("datarium")}
if(!require(readxl)){install.packages("readxl")}
if(!require(psych)){install.packages("psych")}
if(!require(lsr)){install.packages("lsr")}
if(!require(data.table)){install.packages("data.table")}
if(!require(ez)){install.packages("ez")}
if(!require(car)){install.packages("car")}
if(!require(emmeans)){install.packages("emmeans")}


#Load the required packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)
library(readxl)
library(psych)
library(lsr)
library(data.table)
library(ez)
library(car)
library(emmeans)



library(readxl)
Regression <- read_excel("C:/Users/erico/Desktop/ACADEMICS/MY UofT COURSES/YEAR ONE SEM TWO/HAD 5772H INTERMEDIATE STATISTICS FOR HSR/DAY 4/QUIZ 4/Case studies 04_data1.xlsx")
View(Regression)

#I created an ID for the students in the dataset and named it as "snum".

#MULTIPLE REGRESSION WITH CONTINUOUS VARIABLES
#Multiple regression model of DV univ_GPA and IVs math_SAT, high_GPA, verb_SAT, and comp_GPA



# Install packages for Regression Diagnostics
install.packages("car")
install.packages("faraway")
#Load packages into working environment
library(car)
library(faraway)



#Scatter plot matrix from package car
scatterplotMatrix(~ univ_GPA + high_GPA + comp_GPA + math_SAT + verb_SAT, data =Regression)

#Run Pearson correlations to examine the relationship between the predictor variables and the outcome
install.packages("corrplot")
library(corrplot)
rquery.cormat(Regression[ , 1:5])
M <- cor(Regression[ ,1:5])
M

#Visualize the pattern of correlations
corrplot.mixed(M, lower.col = "black", number.cex = .7)



#1. Draw a scatterplot comparing the students' high school GPAs to their overall university GPAs. What does the relationship appear to be?
#Scatter plot matrix from package car
#The relationship between univ_GPA and high_GPA shows a curvilinear trend. This might indicate the violation of linearity assumption and needs to be further investigated.

scatterplotMatrix(~ univ_GPA + high_GPA, data =Regression)

#2. What is the correlation between high school GPA and overall university GPA?

#Pearson's r
#0.7795631
x<-cbind(Regression$univ_GPA, Regression$high_GPA)
cor(x, y = NULL, use = "everything",
    method = "pearson")


#Spearman's rho
#0.8347626
x<-cbind(Regression$univ_GPA, Regression$high_GPA)
cor(x, y = NULL, use = "everything",
    method = "spearman")

 

#3. Find the regression line for predicting the overall university GPA from the high school GPA.
#m1 <- lm(univ_GPA ~ high_GPA + verb_GPA+comp_GPA, data = Regression)
m1 <- lm(univ_GPA ~ high_GPA, data = Regression)

#Print coefficients of the model
print(m1)

#Coefficients:
 # (Intercept)     high_GPA  
# 1.0968       0.6748 


#4. If someone had a 2.2 GPA in high school, what is the best estimate of his or her college GPA?
#Y=1.0968 +0.6748(2.2)=2.58136

#5. If someone had a 4.0 GPA in high school, what is the best estimate of his or her college GPA?
#Y=1.0968 +0.6748(4.0)=3.796

#6. What is the mean of the math SAT score in this sample?
#623.08
describe(Regression$math_SAT)

#7. What is the mean of the verbal SAT score in this sample?
#598.6
describe(Regression$verb_SAT)

#8. What is the correlation between math and verbal SAT scores?

#Pearson's r
#0.8352272
x<-cbind(Regression$math_SAT, Regression$verb_SAT)
cor(x, y = NULL, use = "everything",
    method = "pearson")



#Spearman's rho
#0.8271632 
x<-cbind(Regression$math_SAT, Regression$verb_SAT)
cor(x, y = NULL, use = "everything",
    method = "spearman")



#9. What is the correlation between the students' overall university GPAs and their computer science GPAs?

#Pearson's r
#0.9390459
x<-cbind(Regression$univ_GPA, Regression$comp_GPA)
cor(x, y = NULL, use = "everything",
    method = "pearson")



#Spearman's rho
# 0.9209647
x<-cbind(Regression$univ_GPA, Regression$comp_GPA)
cor(x, y = NULL, use = "everything",
    method = "spearman")





# 10. Did the students on average have higher overall GPAs or higher GPAs in their computer science classes?
#The mean for the overall GPAs was higher than GPAs in their computer science

#Use a different data by creating IDs for the students


#Load the required packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)
library(readxl)
library(psych)
library(lsr)
library(data.table)
library(ez)
library(car)
library(emmeans)

install.packages("data.table")
library(data.table)

Data4<-Regression



#First, we need to restructure the data, to have a grouping variable and all discrepancies results in one column.
 
group<-rep(1:2, each=15)

univ_GPA<-Data4$univ_GPA
comp_GPA<-Data4$comp_GPA
discrepancy<-c(univ_GPA, comp_GPA)


discrepancy=univ_GPA-comp_GPA


#Run descriptive statistics for difference scores and for each variable
describe(discrepancy)
describe(univ_GPA)
describe(comp_GPA)





#11. Conduct a paired-samples t test to see if this difference is statistically significant.
# Perform paired-samples t-test
#As can be seen from the output above, the results of the t-test are significant, indicating that there is a
#significant difference in the average level of discrepancy with univ_GPA and comp_GPA in the population of students

res <- t.test(univ_GPA, comp_GPA, paired = TRUE)
res



#cohen's D
cohensD(Regression$univ_GPA, Regression$comp_GPA, method = "paired")


#12. Find the regression line for predicting the overall university GPA from both the math SAT score and the verbal SAT score.
m1 <- lm(univ_GPA ~ math_SAT+verb_SAT, data = Regression)
#Print coefficients of the model
print(m1)
summary(m1)



#13. What is the R square of the model?
#Adjusted R-squared:  0.4598, Multiple R-squared:  0.4702

# 14. How do you interpret the value of R square?
#The independent variables explain 47% of the model
#The regression model explains 47% of the variability in the the overall university GPA
#The model with two predictor variables explains 47% of the variance in the overall university GPA

# 15. What is the p value for the math SAT score in the regression model?
#0.00321 **

# 16. What is the standardized regression coefficient for verbal SAT score?
#3.199 e-01
#0.3199

#Standardized regression model
m2.sd <- lm(scale(univ_GPA) ~ scale(math_SAT) + scale(verb_SAT), data = Regression)
#Display model results (coefficients are standardized)
summary(m2.sd)

# 17. What would you predict someone's overall university GPA to be if she got a 540 on the verbal and 600 on the math portion of the SAT?
#Y= -0.0000000000000001349 + 0.3956 (600) + 0.3199(540)
#Y=410.106


#Y=-0.238+0.003Math+ 0.002Verb
#Y=-0.238+0.003Math+ 0.002Verb=2.642

m1 <- lm(univ_GPA ~ math_SAT+verb_SAT, data = Regression)
#Print coefficients of the model
print(m1)
summary(m1)


#18. Are there any residual outliers in this dataset?
#There are NO residual outliers in this dataset


# Install packages for Regression Diagnostics
install.packages("car")
install.packages("faraway")
#Load packages into working environment
library(car)
library(faraway)




#Save standardized residuals
#
er.std <- rstandard(m2.sd)

#er.std <- rstandard(mlr2)
#Plot of studentized residuals
plot(er.std, ylab="Standardized Residual", ylim=c(-3.5,3.5))

#Adding horizontal lines for cut of value of outliers
abline(h =c(-3,0,3), lty = 2)
#Determine which row is outlier(outside of cut of values of -3 and 3)
index <- which(er.std > 3 | er.std < -3)
#Label Student ID to points that are out of bounds

#text(index-20, er.std[index] , labels = Regression$snum[index])



#Print row number of values that are out of bounds
index


#Print school names that are out of bounds
Regression$snum[index]


#19. What is the value of VIF for the math SAT?
#VIF for the math SAT is 3.306927

#Compute variance inflation factor
car::vif(m2.sd)



#20. Were the assumptions of normality of residuals, linearity and homoscedasticity met for this model?
#THERE IS A FUNNEL SHAPE
#homoscedasticity is not met

#Residual vs. fitted value plot for Homoscedasticity
plot(m2.sd$resid ~ m2.sd$fitted.values)
#Add horizontal line from 0
abline(h = 0, lty = 2)

#normality of residuals IS MET BECAUSE OBSERVED RESIDUAL ARE CLOSE TO THE DIAGONAL LINE
#Normal Quantile to Quantile plot

qqnorm(m2.sd$resid)
qqline(m2.sd$resid)

#linearity
#Scatter plot matrix from package car
scatterplotMatrix(~ univ_GPA + comp_GPA + high_GPA+ verb_SAT + math_SAT, data =Regression)

#As can be seen from the matrix of scatter plots, 
#the relationship between univ_GPA and comp_GPA is linear
#univ_GPA and the rest are curvilinear

#verb_GPA and math_GPA are linear as well

