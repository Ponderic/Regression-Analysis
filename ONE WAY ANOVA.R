library(readxl)
Data2 <- read_excel("R DATA/Case studies 02_data01.xlsx")
View(Data2)


library(psych)

#1. What is/are the independent variable(s) in this study?
#weight and relate 

#  2. What is/are the dependent variable(s)?
#Qualified

# 3. What is the mean rating of job qualification?
#mean is 6.18
describe(Data2$Qualified)

# 4. What is the standard deviation of job qualification?
#standard deviation is 1.33
describe(Data2$Qualified)


#  5. Does "Qualified" appear to be normally distributed overall?
#Yes. Qualified is normally distributed
#or difficult to tell

describe(Data2$Qualified)



hist(Data2$Qualified, col="grey",
     main="", xlab="Skewness of qualified")

boxplot(Data2$Qualified, ylab=" Qualified, $", xlab="")




#6. Does this variable appear to be normally distributed in the two Weight groups?
#No. It is normally distributed on obese 
#but skewed for average weight 



boxplot(Qualified ~ Weight,
        data=Data2, xlab="Weight", ylab="Qualified", main="Qualified distribution")


with(Data2, shapiro.test(Qualified[Weight == 1]))

with(Data2, shapiro.test(Qualified[Weight == 2]))

#Notice that the results of Shapiro-Wilk test are significant for Obese and average weight group. However, since the
#skewness (0.18) and kurtosis (1.98) values for this group do not exceed 2 in absolute value, we can conduct the
#analysis as planned.

describe(Data2$Weight)

#  7. What type of ANOVA would you use to address the research questions in this study?
#2-way ANOVA because there are 2 factors


# 8. How many factors are there, and how many levels does each factor have?
#There are two factors
#Weight has 2 levels
#Relate has two levels


# 9. Compute the mean of the dependent variable in each group of this design. What is the mean
#Qualified score in the obese girlfriend group?

# the mean qualified score is 5.65  

describeBy(Data2$Qualified, group = list(Data2$Weight, Data2$Relate), digits= 3)



# 10. What is the average Qualified score in the normal weight group?
#6.42
describeBy(Data2$Qualified,group = Data2$Weight, digits= 4)



#11. What is the standard deviation in the girlfriend group?
#1.31

describeBy(Data2$Qualified,group = Data2$Relate, digits= 4)


# 12. Check the assumption of equal variances before running a 2-way ANOVA. Is this assumption
#met?

#Yes. it is met

Weight_f<-as.factor(Data2$Weight)
Relate_f<-as.factor(Data2$Relate)
Data2<-cbind(Data2, Weight_f, Relate_f)
library(car)
leveneTest(Data2$Qualified ~ Data2$Weight_f, data = Data2)

leveneTest(Data2$Qualified ~ Data2$Relate_f, data = Data2)

leveneTest(Data2$Qualified ~ interaction(Data2$Weight_f,Data2$Relate_f), data = Data2)

#Since none of the Levene's tests are significant, we can assume that the assumption of equal variances is met and proceed with ANOVA analysis. 


# 13. Run a 2-way ANOVA. What is the the sum of squares for "weight"?

res_anova2 <- aov(Data2$Qualified ~ Data2$Weight_f*Data2$Relate_f, data = Data2)
# Summary of the analysis
summary(res_anova2)

#the interaction effect is not significant. 
#The main effects of weight and relate are both significant. 
#Since They have only two levels each, we do not need to conduct pairwise comparisons to understand the direction of this effect.
#We can simply look at the descriptive statistics in the sample and see which group has a higher mean.


#14. Is the main effect of "weight" significant?
#Yes. The main effect of weight is significant

#15. Look at the main effect of "relate". What is the sum of squares for "relate".
#The sum of squares of RELATE is 8.72

#16. Is the main effect of "relate" significant?
#Yes. The main effect of Relate is significant

# 17. Look at the interaction between "weight" and "relate". How many degrees of freedom does the
#interaction have?
#There is one degree of freedom


#18. Is the interaction between "weight" and "relate" significant?
#No...the interaction between "weight" and "relate" is not significant

# 19. Interpret the main effect of "weight".
#Weight matters in terms of rating of job qualifications
#or Job qualification ratings differ by weight
#Average weight has higher qualification than an obese person
describeBy(Data2$Qualified,group = Data2$Weight, digits= 4)


library("ggpubr")
ggline(Data2, "Weight_f", "Qualified",
       linetype = "Relate_f", shape = "Relate_f", add = "mean_se")

#20. Interpret the main effect of "relate".
#Relationship with an obese women matters in terms of rating of job qualifications
#or Job qualification ratings differ by relationship with an obese woman
#job applicants sitting with girlfriends have lower professional qualification ratings comparing to acquaintances

describeBy(Data2$Qualified,group = Data2$Relate, digits= 4)
