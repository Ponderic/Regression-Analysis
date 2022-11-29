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



#.............................................................................................................
#REPEATED MEASURES ANOVA
#I had transformed the original data such that I had .....id, placebo, low dosage, medium and high dosage...........
#I did not know how to do that in R so i cleaned it manually in my excel file by transposing the values for each id...........

library(readxl)
Data5 <- read_excel("QUIZ 3/Case studies 03_data1.xlsx")
View(Data5)

#Drug dosage (1=placebo, 2=low=0.15mg/kg, 3=medium=0.30mg/kg, 4=high=0.60mg/kg)



# Gather columns placebo, low, medium and high into long format
# Convert id and dosage into factor variables

install.packages("data.table")
library(data.table)
Data5long <- melt(setDT(Data5), id.vars = "id", variable.name = "dosage")

boxplot(I(Data5long$value ~ Data5long$dosage), data = Data5long,
        ylab = "Number of correct responses to the DOG task", varwidth = TRUE,
        names = c("place", "low", "medium", "high"))


#1. What is the independent variable of this experiment? How many levels does it have?
#Drug dosage and it has 4 levels


#  2. What is the dependent variable? On what scale (nominal, ordinal, interval, ratio) was it measured?
#Number of correct responses to the DOG task #interval or scale because it is more than 7, has no zero and a personality measure 
#and there has to be a response so no zero



#  3. What is the mean number of correct responses of the participants after taking the placebo (0mg/kg)?
#describe(Data5$dose)
#39.75

describeBy(Data5long,group = Data5long$dosage, digits= 4)

View(Data5long)

#  4. What is the mean number of correct responses of the participants after taking 0.60mg/kg dose of the drug?
#44.71
describeBy(Data5long,group = Data5long$dosage, digits= 4)




# 5. What is the correlation between the participants correct number of responses after taking the placebo and their correct number of responses after taking 0.30mg/kg dosage of medication?

#Pearson's r
#0.7484875
x<-cbind(Data5$placebo, Data5$medium)
cor(x, y = NULL, use = "everything",
    method = "pearson")


#Spearman's rho
#0.4928945
x<-cbind(Data5$placebo, Data5$medium)
cor(x, y = NULL, use = "everything",
    method = "spearman")


# Shapiro-Wilk normality test for placebo
with(Data5long, shapiro.test(value[dosage == "placebo"]))


# Shapiro-Wilk normality test for low
with(Data5long, shapiro.test(value[dosage == "low"]))




# Shapiro-Wilk normality test for medium
with(Data5long, shapiro.test(value[dosage == "medium"]))


# Shapiro-Wilk normality test for high
with(Data5long, shapiro.test(value[dosage == "high"]))


#Some violate the normality assumption and some do not violate it 
#so I will use both Spearman's rho over pearson's r







#6. What is the correlation between the participants' number of responses after taking 0.15mg/kg and 0.30mg/kg dosage of medication?

#Pearson's r
#0.7716869
x<-cbind(Data5$low, Data5$medium)
cor(x, y = NULL, use = "everything",
    method = "pearson")


#Spearman's rho
#0.744419
x<-cbind(Data5$low, Data5$medium)
cor(x, y = NULL, use = "everything",
    method = "spearman")




#7. Perform a repeated measures ANOVA. Is the assumption of sphericity met?
#Run repeated-measures ANOVA

#Sphericity assumption is met ....p value of Maulchy's test is >0.05...

#Hence, we read the results on the TOP.
install.packages("ez")
res.aov <- ezANOVA(data = Data5long, dv = value, wid = id, within = dosage)
res.aov

#8. What is the F-ratio value for repeated-measures ANOVA?
#5.178451


#9. What is the significance level of the repeated-measures ANOVA?
#5% or 5 per cent


#10. Which group means are significantly different from each other?
pairwise.t.test(Data5long$value, Data5long$dosage, paired=TRUE, p.adjust.method="bonferroni")

#placebo is significantly different from high dose
#medium dose is significantly different from low dose






#...............................................................................................................
#ANCOVA
library(readxl)
Data7 <- read_excel("QUIZ 3/Case studies 03_data2.xlsx")
View(Data7)



#1. What is the independent variable of this experiment? How many levels does it have?
#GENDER #2 LEVELS


# 2. What is the dependent variable? On what scale (nominal, ordinal, interval, ratio) was it measured?
#AVERAGE WEIGHT  #RATIO  OR SCALE


#3. What are the covariates in this study?
#AGE AND HEIGHT


#  4. Conduct correlation analysis between the covariates and the dependent variable. How strong is the relationship between these variables?

#correlation between Age and Weight is very strong

#Pearson's r
#0.6346364
x<-cbind(Data7$Age, Data7$Weight)
cor(x, y = NULL, use = "everything",
    method = "pearson")


#Spearman's rho
#0.6046454
x<-cbind(Data7$Age, Data7$Weight)
cor(x, y = NULL, use = "everything",
    method = "spearman")



#correlation between Height and Weight is very strong

#Pearson's r
#0.7748761
x<-cbind(Data7$Height, Data7$Weight)
cor(x, y = NULL, use = "everything",
    method = "pearson")


#Spearman's rho
#0.779636
x<-cbind(Data7$Height, Data7$Weight)
cor(x, y = NULL, use = "everything",
    method = "spearman")


#  5. Conduct an independent-samples t-tests to examine the difference in the average values of age covariate between the levels of the independent variable. What is the significance level of this test?
#THIS IS NOT SIGNIFICANT HENCE QUALIFIES As covariate
t.test(Data7$Age ~ Data7$Gender, alternative = "two.sided", var.equal=TRUE)

# 6. Conduct an independent-samples t-tests to examine the difference in the average values of height covariate between the levels of the independent variable. What is the significance level of this test?
#THIS IS SIGNIFICANT AT 5% LEVEL OF SIGNIFICANCE
t.test(Data7$Height ~ Data7$Gender, alternative = "two.sided", var.equal=TRUE)


# 7. Based on the analyses in steps 6 and 7 conclude which of the potential two covariates should be included in the model.
#Height



#8. Test the assumption of equal slopes before running ANCOVA. What is your conclusion?
#JUMP this for now as it creates a duplicate of Gender_f
#Check the assumption of equal slopes
#The interaction effect is significant, hence the assumption of equal slopes is not met...
#so we can go ahead and do the ANCOVA

Gender_f<-as.factor(Data7$Gender)
Data7<-cbind(Data7, Gender_f)
Data7 %>% anova_test(Weight ~ Gender_f*Height)

#  9. Perform an ANCOVA analysis. State your answer to the research question based on the results of this analysis.
#As can be seen from the output, the effect of Gender adjusted for Height is significant with a small
#effect size (GES=0.023). However, we need to conduct the follow-up analysis comparing pairs of Gender to
#understand which Gender means are different and also the adjusted means of weight for boys and girls.


Data7<-cbind(Data7, Gender_f)
res.aov <- Data7 %>% anova_test(Weight ~ Height + Gender_f)
get_anova_table(res.aov)



# 10. What are the adjusted means of weight for boys and girls?
#From the marginal means adjusted for Age we can see that MALES have a significantly higher mean
#comparing to FEMALES. We can use the mean plot to visualize these results.

# Pairwise comparisons
pwc <- Data7 %>%
  emmeans_test(
    Weight ~ Gender, covariate = Height,
    p.adjust.method = "bonferroni"
  )
pwc


# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)




# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "Gender", fun = "mean_se")
ggline(get_emmeans(pwc), x = "Gender", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs (
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc))





















