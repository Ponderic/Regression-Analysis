library(readxl)
DataLR <- read_excel("C:/Users/erico/Desktop/ACADEMICS/MY UofT COURSES/YEAR ONE SEM TWO/HAD 5772H INTERMEDIATE STATISTICS FOR HSR/DAY 5/QUIZ 5/Case studies 05_data1.xlsx")
View(DataLR)

#packages
#Start by installing these packages (if they are not installed yet)
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(aod)){install.packages("aod")}
if(!require(caret)){install.packages("caret")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(psych)){install.packages("psych")}
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}
if(!require(coin)){install.packages("coin")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(rcompanion)){install.packages("rcompanion")}
#Then you need to load the these packages into for your work session:
#Load the required packages
library(tidyverse)
library(caret)
library(aod)
library(ggplot2)
library(psych)
library(RVAideMemoire)
library(coin)
library(reshape2)
library(rcompanion)


#..............................................
#LOGISTIC REGRESSION


#1. What percentage of applicants in the sample were admitted to a graduate school?
xtabs(~admit+rank, data=DataLR)

#(33+ 54 +28 +12)/ [(33+ 54 +28 +12)+(28 97 93 55)]
#=0.3175

# 2. What is the range of GRE scores in the sample?
#580
describe(DataLR$gre)

# 3. What percentage of the undergraduate programs have the highest prestige rank score?
xtabs(~admit+rank, data=DataLR)
#=0.165

#  4. Is there a statistically significant relationship between the admission decision and the prestige rank score? 
#What statistical test did you use to find this out?
#yes @ 1 and 5%
#CONDUCT T-TEST
res <- t.test(DataLR$admit, DataLR$rank, paired = TRUE)
res


#Paired samples t-test

#t = -37.692, df = 399, p-value < 2.2e-16

# 5. How should you treat the rank variable in logistic regression?
#As a categorical variable
#factor


# 6. Run binary logistic regression with all available variables. 
#Which variables have significant relationship to the graduate school admissions?

#All the variables had significant effects

#relevel rank var


#Convert categorical predictor variables into factors
DataLR$rank<-factor(DataLR$rank)

#levels(DataLR$rank)



#Run logistic regression analysis 
model1<-glm(admit ~ rank + gre + gpa, data=DataLR, family="binomial")
summary(model1)


#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -3.989979   1.139951  -3.500 0.000465 ***
 # rank2       -0.675443   0.316490  -2.134 0.032829 *  
#  rank3       -1.340204   0.345306  -3.881 0.000104 ***
 # rank4       -1.551464   0.417832  -3.713 0.000205 ***
  #gre          0.002264   0.001094   2.070 0.038465 *  
  #gpa          0.804038   0.331819   2.423 0.015388 *  
  
  #Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# 7. Are students from more prestigious undergraduate programs more likely or less likely to be admitted to a graduate program?
#They are more likely to be admitted


# 8. What is the exp(B) for the effect of GPA? How would you interpret it?
#2.2345448
#one Additional unit of GPA increases the odds of admission by 2.23 times or by 23%.

#Compute exponentiated regression coefficients
exp(coef(model1))

exp(cbind(OR=coef(model1), confint(model1)))

# 9. What is the confidence interval for the exp(B) of the gpa variable?
#Obtain confidence intervals for regression coefficients
#0.1602959439  1.464142727
confint(model1)

#  10. What percentage of cases in the model is predicted accurately?
  
#Make predictions based on the model
probabilities<-model1%>% predict(DataLR, type="response")
predicted.classes<-ifelse(probabilities>0.5, "admitted", "not_admitted")
  

#Compute accuracy of model prediction
DataLR$admitF<-factor(DataLR$admit)
DataLR<-cbind(DataLR, probabilities, predicted.classes)
xtabs(~admitF + predicted.classes, data=DataLR)

#(30+254)/(19+254+30+97)=0.71
#71% of cases are predicted correctly

  



#.......................................................................
#Exploratory factor analysis

library(readxl)
dataEFA1 <- read_excel("C:/Users/erico/Desktop/ACADEMICS/MY UofT COURSES/YEAR ONE SEM TWO/HAD 5772H INTERMEDIATE STATISTICS FOR HSR/DAY 5/QUIZ 5/Case studies 05_data2.xlsx")
View(dataEFA1)


# 1. Do the students on average tend to agree or disagree with the statements in the course evaluation form?
#Run basic descriptive statistics for the data set
#On average they agree

library(psych)
describe(dataEFA1)

#   2. What is the shape for the distribution of responses for most items?
# Install packages for Regression Diagnostics
#Normaal distribution
library(psych)
describe(dataEFA1)


# 3. Request a matrix of Pearson correlations among the items. What can you say about the pattern of correlations?
lowerCor(dataEFA1)
cor.plot(dataEFA1,numbers=FALSE,main="9 cognitive variables")
pairs.panels(dataEFA1,pch='.')




# 4. Run the Bartlett's test. How would you interpret the results of it?
#The Bartlett's test is significant (p=0), 
#it means that the variables are sufficiently strongly correlated with each other 
#to consider running EFA
#Run Bartlett's test
cortest.bartlett(dataEFA1)

#Visualize the matrix of correlations
pairs.panels(dataEFA1,pch='.')



#   5. Run the analysis investigating how many factors should be extracted in EFA. What is your decision based on this analysis?
#there are two eigenvalues that are larger than "ONE" in the output. Therefore, I should run EFA with 2 factors.

#Perform an exploratory run to investigate how many factors should be extracted.
#Request the numebr of factors equal to the number of variables.
efa_test <- fa(dataEFA1,9,n.obs=301, fm="pa", SMC=FALSE)

efa_test

#Request eigenvalues
efa_test$values




# 6. Run EFA with the number of factors you have decided upon. What item has the weakest relationship to the factor solution?
 # item08  has communlity value of 0.31 hence the weaker this item is related to the factor solution
#weakest

efa2 <- fa(dataEFA1,nfactors=2,n.obs=301, fm="pa", SMC=FALSE, rotate="oblimin")
efa2

#  7. What is the proportion of variance explained by the factor solution?
 #0.53


#  8. Do all items have sufficiently high factor loadings on one of the factors?
#YES. 

#  9. What item has the highest relationship with one of the factors?
 #item01

#  10. Are the factors correlated with each other?
#YES, 0.63

#Request a factor structure diagram
fa.diagram(efa2)
