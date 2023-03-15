# Modeling with Stepwise Regression
library("caret")      # used to test assumptions
library("magrittr")   # used for data wrangling
library("dplyr")      # used for data wrangling
library("tidyr")      # used for data wrangling
library("lmtest")     # used to test assumptions
library("popbio")     #used to graph your logistic regression model.
library("e1071")      #used to graph your logistic regression model.

# Load in Data
head(Employee.Satisfaction.Index)
# Test Assumptions: 

# Question:Create a model that will use saisfied as the response(DV) variable, and the other 11 columns of data as potential predictor(IV) variables. It is assumed that all 11 predictors don't really belong in the model.

# Data Wrangling: subset dataframe to the only columns that can predict the response
SWR_es <- Employee.Satisfaction.Index%>%select ('age', 'Dept','location', 'education', 'recruitment_type', 'job_level', 'rating', 'onsite','awards','certifications', 'salary', 'satisfied')
# Get a Baseline:
#FitAll = lm( y,DV,response variable ~. means "all IV,potential predictor variables", data + dataframe) or = lm( mpg ~ IV1 + IV2 + IV3, data = dataframe)
# summary(FitAll)
SWR_esFitAll = lm(satisfied ~ ., data = SWR_es)
summary(SWR_esFitAll)
# you can see that the p value is not significant, the Pr(>|t|) values are not

# Backward Elimination
step(SWR_esFitAll, direction = 'backward')
satis = 0.42577 + (.032630)
esfitsome = lm(satisfied ~ rating, data = SWR_es )

# Forward Selection
fitstart = lm(satisfied ~ 1, data = SWR_es)
summary(fitstart)
step(fitstart, direction = 'forward', scope = (~ age, Dept, location, education, recruitment_type, job_level, rating, onsite, awards, certifications, salary, satisfied))
step(fitstart, direction = 'forward', scope =(formula(SWR_esFitAll)))

# Hybrid Stepwise 
step(fitstart, direction="both", scope=formula(SWR_esFitAll))


