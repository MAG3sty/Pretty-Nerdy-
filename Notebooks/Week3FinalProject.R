library("rcompanion")
library("fastR2")
library("car")
library("dplyr")
library("ggplot2")
ESI = Employee_Satisfaction_Index
head(ESI)
## Histogram of Age
age <- c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)
a <- ggplot(ESI, aes(x = age))
a + geom_histogram()
## Adding Bins
a + geom_histogram(binwidth = 20)
## Adding a Title and labels
a + geom_histogram(binwidth = 20) +
  ggtitle("Histogram of Age") +
  xlab("age (in yrs)")
## Relative Frequencies
a + geom_histogram(binwidth = 10, aes(y = ..count../sum(..count..))) +
  ggtitle("Histogram of Age") + xlab("Age (in yrs)") +
  ylab("Relative frequency")
a + geom_histogram(binwidth = 10, fill = "goldenrod", color = "deepskyblue4") +
  ggtitle("Histogram of Age") + xlab("Age (in yrs)")
## Box Plots
a <- ggplot(ESI, aes(x = "", y = age))
a + geom_boxplot() + xlab("")
summary(ESI$age)
## Normal Probabliity Chart
ggplot(ESI, aes(sample = age)) + geom_qq()
## Filtering
## I have tried this multiple ways and the result is still the same running with
## all zero's, it runs but I don't believe it is right.
emsain = ESI[,!names(ESI) %in% c("emp_id", "recruitment_type", "job_level", "awards", "certifications", "salary" )]
filter(ESI, 'satisfied' == "0")
filter(ESI, "satisfied" == "1")
filter(ESI, a == "1")
filter(ESI, a == "0")
## %in%
## Does not run
filter(ESI, satisfied %in% c("0", "1"))
## I still feel this isn not right, but it does run
filter(ESI, 'satisfied' %in% c("0", "1"))
filter(ESI, "satisfied" %in% c("0", "1"))
## Ordering Data
arrange(emsain, age)
arrange(emsain, rating)
arrange(emsain, satisfied)
arrange(emsain, Dept)
arrange(emsain, location)
arrange(emsain, education)
arrange(emsain, onsite)
## Selecting and mutating Data
select(emsain, age, satisfied)
select(emsain, onsite, satisfied)
select(emsain, location, satisfied)
## Mutation
mutate(emsain, age_yrs = age >= 28, age <= 47)
## Adding a Column with the Same Value
mutate(emsain, Species = "Human Being")
mutate(emsain, Species = "May or May not be Human")
mutate(emsain, Status = "Anarchy")
## I am having fun if you can't tell
## Summarizing
summarize(emsain, ave_age = mean(age))
summarize(emsain, ave_rating = mean(rating))
summarize(emsain, ave_satisfied = mean(satisfied))
summarize(emsain, ave_onsite = mean(onsite))
summarize(ESI, ave_job_level = mean(job_level))
summarize(ESI, ave_awards = mean(awards))
summarize(ESI, ave_certifications = mean(certifications))
summarize(ESI, ave_salary = mean(salary))
## Graphing Data Grouped by Factors
ggplot(ESI, aes(x = age, y = onsite)) + geom_boxplot(aes(group=age, fill = "pink", color = "purple"))
emsain %>% group_by(age) %>% summarize(emsain = mean(onsite))
emsain %>% group_by(age) %>% summarize(emsain = mean(satisfied))
emsain %>% group_by(age) %>% summarize(emsain = mean(rating))
ESI %>% group_by(age) %>% summarize(ESI = mean(job_level))
## T-test for 1 Sample
emsain <- t.test(emsain$age, mu = 37)
print(emsain)
d <- ggplot(emsain, aes(x = age))
d + geom_histogram(binwidth = 15) +
  geom_vline(xintercept = emsain$conf.int[1], color = "red") +
  geom_vline(xintercept = emsain$conf.int[2], color = "red") +
  geom_vline(xintercept = emsain$null.value, color = "green")
ggplot(ESI, aes(sample = age)) + geom_qq()
## Independent T-test
t_ind <- t.test(ESI$age, ESI$satisfied, alternative="two.sided", var.equal=FALSE)
print(t_ind)
## Graphing Data for an Independent t Test
## It will run but it won't let me run it off of my adjusted table so it does 
## not look pretty in any way. Just a heads up
library(reshape2)
emsain1 <- melt(ESI, id="onsite")
print(emsain1)
ggplot(emsain) + geom_boxplot(aes(x = variable, y = value)) +
  xlab("Home Based") + ylab("Age")
## Dependent T-test
## Paired t-Test for Two Samples
head(emsain)
t_dep <- t.test(ESI$age, ESI$onsite, paired = TRUE)
t_dep
## Graphing Data for an Dependent t Test
library(reshape2)
ss <- melt(ESI, measure.vars = c("age", "onsite"))
ss
ggplot(ss) + geom_boxplot(aes(x = variable, y = value)) +
  xlab("onsite") + ylab("age")
## Graphing the Difference Scores
dd <- ESI$onsite - ESI$age
df <- data.frame(dd)
ggplot(df, aes(x = dd)) + geom_histogram(binwidth = 1) +
  xlab("Difference between Age and Who Prefer Home Based Position")
## Scatter Plots
d <- ggplot(ESI, aes(x = rating, y = age))
d + geom_point()
d + geom_point() + geom_smooth(method=lm)
d + geom_point() + geom_smooth(method=lm, se=FALSE)
d <- ggplot(ESI, aes(x = satisfied, y = onsite))
d + geom_point()
d + geom_point() + geom_smooth(method=lm)
d + geom_point() + geom_smooth(method=lm, se=FALSE)
## Correlation Basics
d <- ggplot(ESI, aes(x = rating, y = age))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
d <- ggplot(ESI, aes(x = rating, y = age))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
## Calculating Correlation
cor.test(ESI$age, ESI$rating, method="pearson", use = "complete.obs")
## Detailed Correlation Matrices
library("PerformanceAnalytics")
ESI_quant <- ESI[, c(3,9,10,14)]
ESI_quant
## I can not get this to run
chart.Correlation(ESI_quant, histogram=FALSE, method="pearson")
## Getting a Correlation Matrix Table using cor()
## I cannot get this to run
library(ggcorrplot)
library(corrplot)
corr_matrix <- cor(ESI_quant)
corr_matrix
corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")
## Computing Linear Regression
lin_reg <- lm(age ~ rating, ESI)
print(lin_reg)
lin_reg <- lm(age ~ satisfied, ESI)
print(lin_reg)
lin_reg <- lm(age ~ onsite, ESI)
print(lin_reg)
## Linear Regression Model Summary
summary(lin_reg)
## Scatter Plot with a Best Fit Line
d <- ggplot(ESI, aes(x = age, y = rating))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
## Data Exploration
## Graphing the age to onsite
ggplot(ESI, aes(x = factor(age), y = onsite)) + geom_boxplot()
ggplot(ESI, aes(x = factor(age), y = rating)) + geom_boxplot() +
  scale_y_log10()
## Filtering and Arranging by Population
ESI.big <- ESI %>%
  filter(age == 35) %>%
  arrange(desc(rating))
ESI.big
summary(ESI.big)
head(ESI.big, n = 10)
tail(gm.big, n = 10)
## Determining Outliers in Age
ggplot(ESI, aes(x = factor(age), y = rating)) + geom_boxplot()
filter(ESI, salary > 13988)
## Graphing Satisfaction
ggplot(ESI, aes(x = factor(age), y = satisfied)) + geom_boxplot()
## Graphing Onsite
ggplot(ESI, aes(x = factor(age), y = onsite)) + geom_boxplot()
filter(ESI, age < 37)   
## Data Exploration Part II
install.packages("gridExtra")
library("gridExtra")
age <- ggplot(ESI) + geom_line(aes(x = age, y = rating, color = rating)) 
factor <- ggplot(ESI) + geom_line(aes(x = age, y = satisfied, color = rating)) +
  ylab("Age")
grid.arrange(age, ESI, ncol = 1)
## A Statistical Summary
gm_medians <- ESI %>%
  filter(rating == 5) %>%
  group_by(age) %>%
  summarise(rating_med = median(rating), age_med = median(age))
gm_medians
## Transposing Data in R
esi <- t(ESI)
class(esi)
esi1 <- as.data.frame(esi)
class(esi1)
names(esi1) <- gsub("V", "age", names(esi1))
## Aggregating Data in R
## This is not working
esi2 <- aggregate(Rating~age, age, mean)
## Recoding into a New Variable in R
ESI$Gender <- NA
ESI$Gender[ESI$Gender=='Male'] <- 0
ESI$Gender[ESI1$Gender=='Female'] <- 1
print(ESI)
## Bar Charts with ggplot
ggplot(ESI, aes(rating))+ 
  geom_bar() +
  ggtitle("Ages and their Rating") +
  xlab("rating") +
  ylab("age")
## Bar Charts with Lattice
library("lattice")
barchart(ESI$rating)
## Stacked Bar Charts in R
ggplot(data=ESI) +
  geom_bar(mapping = aes(x = age, fill=location)) + 
  ggtitle("Ages that are Satisfied Based off Location") +
  xlab("Age") +
  ylab("Satisfied")
## Making Bar Heights the Same
ggplot(data=ESI) +
  geom_bar(mapping = aes(x = age, fill=location), position = "fill") + 
  ggtitle("Ages that are Satisfied Based off Location") +
  xlab("Age") +
  ylab("Satisfied")
## Multiple Categorical Variables
ggplot(data=ESI) +
  geom_bar(mapping = aes(x = age, fill=location), position = "dodge") + 
  ggtitle("Ages by Location") +
  xlab("Age") +
  ylab("Location")  
## Line Charts in R
str(ESI)
ESI$emp_id <- as.numeric(ESI$emp_id)
ESI$Dept <- as.numeric(ESI$Dept)
ESI$location <- as.numeric(ESI$location)
## Heat Maps in R
ESI1 <- ESI[,3:12]
ESI1
## I cannot get this to work 
heatmap(ESI)
