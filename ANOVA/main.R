
#######################
### Exercise 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, 
# Cambridgeshire County Council considered 14 pairs of locations. The locations were 
# paired to account for factors such as traffic, volume and type of road. One site in 
# each pair had a sign erected warning of the dangers of speeding and asking drivers 
# to slow down. No action was taken at the second site. Three sets of measurements 
# were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the 
# erection of the sign, shortly after the erection of the sign, and again after 
# the sign had been in place for some time.

# a) For the further reference please use ?amis. 
# It may take some time to understand the dataset. 

?amis

# b) Load the dataset, store it into a variable called "data", and briefly inspect it. 
# Feel free to make some plots and calculate some statistics in order to understand 
# the data.

data = boot::amis
str(data)

# c) All our columns have numeric type. Convert the categorial columns to factors.

data$period = as.factor(data$period)
data$warning = as.factor(data$warning)
data$pair = as.factor(data$pair)

# d) Plot boxplots for the distribution of `speed` for each of the `period` values 

ggplot(data = data) +
  geom_boxplot(aes(x = warning, y = speed, fill = period))

# (before, immediately after and after some time). Build 2 plots (each containing 3 
# boxplots) side by side depending on the `warning` variable.
# (For all plots here and below please use ggplot)


# e) What can you conclude looking at the plots? What can you say about people's 
# behaviour in different periods: before, immediately after and after some time?

# The warning signs do help but only a little. Before the sign was errected and shortly after the errection of sign people tend to slow down a little
# however after people get used to the sign(after a certain time period has passed) people stop paying attention to the signs.
# we would have a better estimate if the data was collected at a place where the general
# traffic was used to speeding unlike this data where only a few people are driving fast.

# f) What are your ideas about why the data with warning==2 (sites where no sign was 
# erected) was collected?

# The warning==2 acts as a null hypothesis here which is used in testing(comparing) our alternate hypothesis(warning signs help slow cars down).

#######################
### Exercise 2: 1-way ANOVA
#######################

# a) First let's create a new data frame which will be used for all exercise 2.
# For the 1-way ANOVA we will be working with a subset of `amis` using only the 
# data for sites where warning signs were erected, which corresponds to warning==1. 
# Therefore first subset your data to filter out warning==2 and then apply cast() 
# to average "speed" over each "pair" and "period". 
# Assign this new data frame to the variable casted_data.

casted_data = data[data$warning == 1, ]
casted_data = cast(casted_data,pair + period ~ ., mean, value = 'speed')
colnames(casted_data)[3] = 'speed'

# b) Build boxplots of the average speed depending on "period".

ggplot(casted_data) + 
  geom_boxplot(aes(x = period, y = speed))

# c) Looking at the boxplots, is there a difference between the periods?

# the period where the sign is recently errected is where the difference is most visible (they drive slower than before the sign was errected)
# some time after sign errection people get used to the sign and go back to their old habits only a little worse than before

# Now, let's check the ANOVA assumptions and whether they are violated or not 
# and why.

# d) Independence assumption
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

# since each observation is unique(have a unique pair/period combination because we casted our data) and we dont have any spatial and 
# time variables to cause(and check) dependencies we can assume that the observations are independent of each other
# otherwise the best way to check for independence is during data collection

# e) Normality of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

library("car")
resids = predict (lm(speed ~ pair + period, data  = casted_data))
resids = casted_data$speed - resids
qqPlot(resids, distribution = 'norm')


# I used a qqplot which plotted the distribution of residuals against the theoretical normal distribution and as you can see from the reference line
# the data is normally distributed (only a few outliers do not follow the distribution)


# f) Homogeneity of variance of residuals
# (Figure out the best way to check this assumption and give a detailed justified 
# answer to whether it is violated or not.)

#It can be seen from the above QQPlot that the residuals have a small homogeneous variance.
#We can also perform leveneTest for this purpose

leveneTest(speed ~ period, data = casted_data, center = mean)

# as p>0.6 for getting residuals using period we can safely conclude the homogenity of variance in residuals

# g) Now we are ready to perform 1-way ANOVA: please use the function aov() on the 
# speed depending on the period, report p-value and interpret the result in details.

summary(aov(speed ~ period, data = casted_data))
# p>0.3 so we can conclude that the variance in the data is more within the groups than between the groups. In simple words the variance
# between different pairs (before sign is errected, when sign is newly errected etc) is statistically insignificant and thus we cannot say that
# there is a huge difference  in speed between different pairs

# h) what were the degrees of freedom from the result in part g)

# degrees of freedom are 2 for period (3 - 1) and 39 for residuals

# i) Calcuate the effect size and interpret the results. 

sd = sd(casted_data$speed)
mean1 = mean(casted_data$speed[casted_data$period==1])
mean2 = mean(casted_data$speed[casted_data$period==2])
mean3 = mean(casted_data$speed[casted_data$period==3])

abs(mean1 - mean2)/sd
abs(mean2 - mean3)/sd
abs(mean1 - mean3)/sd


#the effect between group 1 and other groups is small (the mean of group is similiar to the mean of other groups).
#the effect between group 2 and group is is substantial (about 60% of group 2 is smaller than group 3)

# j) Please do pairwise t-tests of the same variables as in g) using pairwise.t.test().

pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method = "none")

# k) Report the pairwise p-values and interpret the result in detail.

# group 1-2 p-value = 0.59
# group 1-2 p-value = 0.4
# group 1-2 p-value = 0.17
# the 3 groups dont differ from each other as they are all statistically insignificant

# l) Try to use no adjustment for pairwise testing and then the Bonferroni correction.
# Does the result change? 

pairwise.t.test(casted_data$speed, casted_data$period, p.adjust.method = "bonferroni")
#yes the p value has increased

# m) If the results change why do they? What does Bonferroni correction do?

#the results change because in Bonferroni correction we are multiplying p-value with the number of tests(3 in this case) 
# and its 1.0 because 1 is the max value.

# n) If the assumption of Normality does not hold, which test would you be using in this scenario.

#I would use a non-parametric test like Kruskal-Wallis test in this scenario (if i had 2 groups i could have used wilcoxon test).

#######################
### Exercise 3: 2-way ANOVA
#######################
# a) Now we want to analyze the influence of 2 categorial variables 
# (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1).
# First, we need to average the speed over each `pair`, `warning` and `period
# Cast your data again and assign the resuts to casted_data2.

casted_data2 = cast(data, pair + period + warning ~ ., mean, value = "speed" , na.rm = T)
colnames(casted_data2)[4] = 'speed'

# b) Calculate the mean for each of the 6 possible pairs of `period` and `warning`.

cast(casted_data2, warning + period ~ ., mean, value = 'speed', na.rm = T)[3]

# c) Do you think there is a significant difference between some of the groups?

# There does not seem to be a significant different just by looking at their means

# d) State the main difference between the applicabilty of 1-way and 2-way ANOVA.

# 1. Sometimes its wrong to anaylse the variance of one variable independently as that variable depends on a different variable and reacts
# differently on different values of the other varialbe (e.g a single group of drugs(a single value of drugs) behaves differently under therapy
# and without therapy) and thus we should measure the variance of such a variable with this other variable the codepend on each other.

# 2. Two way anova should be used when we have 2 different variables whose variance we want to measure, specially when the 2 different variables
# are suspected to be related to each other, as one way anova cannot be used when the 2 variables are related to each other.

# e) Now apply the 2-way ANOVA: please use the function aov() on the speed depending 
# on the period and warning.
# Report the p-value and interpret the result in detail. Properly formulate the findings!

summary(aov(speed ~ warning + period, data = casted_data2))
#p value <0.05 for warning hence we can conclude that warning signs do effect speed (F(1, 80) = 8.5133, p<0.05). 
# However p is much greater than 0.05 for period hence (F(2, 80) = 1.127, p = 0.329)


# f) What do you conclude about the behaviour of drivers based on the 2-way ANOVA?

# With a warning sign drivers do slow down the cars, and despite what was previously assumed, it doesnt matter whether the sign was just put there
# or it has been there for a very long time drivers follow the sign similiarly and irrespective of the sign's errection duration.
