# Load in Libraries and data(employee satisfaction)
library("caret")      # used to test assumptions
library("magrittr")   # used for data wrangling
library("dplyr")      # used for data wrangling
library("tidyr")      # used for data wrangling
library("lmtest")     # used to test assumptions
library("popbio")     #used to graph your logistic regression model.
library("e1071")      #used to graph your logistic regression model.

View(Employee.Satisfaction.Index)
# predictor (IV) is the salary and age
# response variable (DV) is whether you are satisfied

# Subset dataframe/ Data Wrangling
esRating <-Employee.Satisfaction.Index%>%select ('rating', 'satisfied')
# Remove any na's
esRating <-drop_na(esRating)
esRating$ratingR <- ifelse(esRating$rating < 4, "0","1")

# Test Assumptions- rating to satisfied
# 1st test Sample Size
# The first thing, create the logistic regression model.
ratinglogit <- glm(satisfied ~ ratingR, data=esRating, family="binomial")
# With that model created, you can make predictions about satisfaction
ratingprobabilities <- predict(ratinglogit, type = "response")
# creating a probabilities column to a positive and negative prediction by having anything above .5 (half) be positive, and anything below .5 be negative.
ratingprobabilities
esRating$PredictedProb <- ifelse(ratingprobabilities > 0.4839858, "pos", "neg")
# then recode the Predicted variable/column just like above in data wrangling
esRating$PredictedProbR <- NA
esRating$PredictedProbR[esRating$PredictedProb=='pos'] <- 0
esRating$PredictedProbR[esRating$PredictedProb=='neg'] <- 1
# Convert Variables to Factors- the 2 new columns you created are numeric and you have to change to factors for the confusion Matrix
esRating$PredictedProbR <- as.factor(esRating$PredictedProbR)
esRating$satisfiedR <- as.factor(esRating$satisfiedR)
esRating$satisfiedR <- NA
esRating$satisfiedR[esRating$satisfied == '0'] <- 0
esRating$satisfiedR[esRating$satisfied == '1'] <- 1

#Create a Confusion Matrix 
ratingconf_mat <- caret::confusionMatrix(esRating$PredictedProbR, esRating$satisfiedR)
ratingconf_mat
# base on the 2x2 chart: It is 46% accuracy that the prediction is correct 
# 92  predicted that they where not satisfied, and they were correct in they're prediction
# 127 predicted that they where not satisfied, but they're prediction was wrong
# 145 predicted that they where satisfied, but they're prediction was wrong 
# 136 predicted that they where satisfied, and they were correct in they're prediction
# Sample Size-PASSED

# 2nd test Logit Linearity- numeric columns only
# here we are creating a new dataFrame with just the numeric variable/columns that are in the dataframe
esRating$PredictedProbR[es$Predicted=='2'] <- 1
esRating$PredictedProbR[es$Predicted=='1'] <- 0
esRating$satisfied <- as.numeric(esRating$satisfied)
esRating$PredictedProbR <- as.numeric(esRating$PredictedProbR)
esRating2 <- esRating %>% 
  dplyr::select_if(is.numeric)
predictors <- colnames(esRating2) #(values environment)
# create the logit- The logit is calculated as the log of the probabilities divided by one minus the probabilities.
esRating3 <- esRating2 %>%
  mutate(logit=log(ratingprobabilities/(1-ratingprobabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)
# now your datframe1 have changed
# With this logit in hand, you can graph to assess for linearity
ggplot(esRating3, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
# looking more at your IV( rating)line look pretty straight
# Logit Linearity- PASSED

# 3rd test Multicollinearity- only when you have more than 1 IV

# 4th test Independent Errors- Graphing the Errors
plot(ratinglogit$residuals)
# looking at the line 
# if you are not sure use the Durbin-Watson Test
dwtest(ratinglogit, alternative="two.sided")
# p value is not significant (greater > .05),passed and you have independent errors.
# DW is between 1 and 3 so we have passed the the assumption of independent errors
# Independent Errors- PASSED

# 5th test Screening for Outliers
ratinginfl <- influence.measures(ratinglogit)
summary(ratinginfl)
# if dfb.1_ or dffit values are greater than 1 you probably have an outlier than should be examined and possibly removed.
# if hat is greater than .3 or so, you probably have an outlier than should be examined and possibly removed.
#  Screening for Outliers- PASSED

# Running Logistic Regression and Interpreting the Output
summary(ratinglogit)
# the p value is significant at p < .05, which is great news
# the estimate of 39% tells you how much the rating variable influences the satisfied variable. 

# Graphing the Logistic Model
logi.hist.plot(esRating$rating,esRating$satisfied, boxp=FALSE, type="hist", col="gray")
# logi.hist.plot(originaldataframe$IV,originaldataframe$DV Rcolumn, boxp=FALSE, type="hist", col="gray")



# Test Assumptions- age to satisfied
# data wrangling
View(Employee.Satisfaction.Index)
esAge <-Employee.Satisfaction.Index%>%select ('age', 'salary', 'satisfied')
esAge <-drop_na(esAge)
# Recode: Age is between 23-54 GenZ11-26, Millennials27-42, GenX43-58
esAge$ageR <- NA
esAge$ageR <- ifelse(esAge$age < 27, "GenZ11-26", ifelse(esAge$age < 43, "Millennials27-42", "GenX43-58")) 
esAge$ageRR <- ifelse(esAge$age < 40, "0", ifelse(esAge$age < 59, "1")) 
# 1st test Sample Size- passed
Agelogit<- glm(satisfied ~ ageRR, data=esAge, family="binomial")
Ageprobabilities <- predict(Agelogit, type = "response")
esAge$PredictedProb <- ifelse(Ageprobabilities < 0.5475285, "neg", "pos")
esAge$PredictedProbR <- NA
esAge$PredictedProbR[esAge$PredictedProb=='pos'] <- 1
esAge$PredictedProbR[esAge$PredictedProb=='neg'] <- 0
esAge$PredictedProbR <- as.factor(esAge$PredictedProbR)
esAge$ageRR <- as.factor(esAge$ageRR)
esAge$satisfied <- as.factor(esAge$satisfied)
Ageconf_mat <- caret::confusionMatrix(esAge$PredictedProbR, esAge$satisfied)
Ageconf_mat
# 2nd test Logit Linearity- passed
esAge$satisfied[esAge$satisfied =='1'] <- 0
esAge$satisfied[esAge$satisfied =='2'] <- 1
esAge$PredictedProbR[esAge$PredictedProbR=='1'] <- 0
esAge$PredictedProbR[esAge$PredictedProbR=='2'] <- 1
esAge$ageRR[esAge$ageRR =='1'] <- 0
esAge$ageRR[esAge$ageRR =='2'] <- 1
esAge$satisfied <- as.numeric(esAge$satisfied)
esAge$PredictedProbR <- as.numeric(esAge$PredictedProbR)

esAge2 <- esAge %>% 
  dplyr::select_if(is.numeric)
predictors <- colnames(esAge2) #(values environment)
esAge3 <- esAge2 %>%
  mutate(logit=log(Ageprobabilities/(1-Ageprobabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)
ggplot(esAge3, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
# 4th test Independent Errors- passed
plot(Agelogit$residuals)
dwtest(Agelogit, alternative="two.sided") # p value is not significant (greater > .05),DW is between 1 and 3 so we have passed
# 5th test Screening for Outliers- passed
Ageinfl <- influence.measures(Agelogit)
summary(Ageinfl)
# Running Logistic Regression and Interpreting the Output
summary(Agelogit)
# the p value is not significant at p > .05,
# This means that age is not a significant predictor if you are satisfied.
# Graphing the Logistic Model
logi.hist.plot(esAge$age,esAge$satisfied, boxp=FALSE, type="hist", col="orange")











count      500.000000
mean     50416.056000
std      23671.392661
min      24076.000000
25%      29805.000000
50%      42419.000000
75%      65715.000000
max      86750.000000


install.packages("Rserve")
library(Rserve)
Rserve()
save(SWR_es, file="EmployeeSatisfation2")