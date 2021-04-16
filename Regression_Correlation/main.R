
library(reshape)
library(languageR)
library(ggplot2)
library(ggpubr)

#######################
### Exercise 1: Correlation
#######################

# a) Get some data - access the ratings data set in languageR and name it "data".
# The data set contains subjective frequency ratings and their length averaged over 
# subjects, for 81 concrete English nouns.

data = languageR::ratings

# b) Take a look at the data frame.

str(data)
summary(data)

# c) Let's say you're interested in whether there is a linear relationship between 
# the word frequency of the 81 nouns and their length.
# Take a look at the relationship between the frequency and word length data by 
# means of a scatterplot (use the ggplot library for this).

library(ggplot2)
ggplot(data, aes(x = Length, y = Frequency)) +
  geom_point()

# d) Judging from the graphs, do you think that word frequency and word length are 
# in any way correlated with one another?

# yes it seems like frequency and word length are negatively correlated with frequency decreasing as length increases

# e) Compute the Pearson correlation coefficient for the two variables by means 
# of cor().
# Tell R to only include complete pairs of observations.
# As a reminder: Pearson coefficient denotes the covariance of the two variables 
# divided by the product of their respective variance. 
# It is scaled between 1 (for a perfect positive correlation) to -1 (for a perfect 
# negative correlation).

cor(data$Length, data$Frequency, use = "pairwise.complete.obs")

# f) Does the correlation coefficient suggest a small, medium or large effect?
# What about the direction of the effect?

# the correlation suggests a medium correlation between the variables and the direction is negative this implies that as one variable 
# increases the other decreases.

# g) Note that we have a large number of tied ranks in word length data 
# (since there are multiple words with the length of e.g. 5).
# Thus, we might draw more accurate conclusions by setting the method to 
# Kendall's tau instead of the Pearson correlation coefficient (which is the default).
# How do you interpret the difference between these 2 correlation coefficients?

cor(data$Length, data$Frequency, method = 'kendall', use = "pairwise.complete.obs")
#Kendall's tau is lower than the Pearson correlation. This is probably because Kendall's tau does not take into consideration pairs that
#are completely identical. I believe kendall's tau is the more accurate measure here as length is a factor and can be considered ordinal
# because there are exponentially less  words of length 15 then there are of length 5 

# h) What about significance? Use the more user-friendly cor.test()!
# Take a look at the output and describe what's in there.
# What do you conclude?

cor.test(data$Length, data$Frequency)
#pvalue < 0.05 hence our alternative hypothesis(that there exits a relationship between length and frequency) holds true.

# i) Finally, also calculate Spearman's rank correlation for the same data.

cor(data$Length, data$Frequency, method = "spearman",use = "pairwise.complete.obs")
# more similar to pearson's correlation which was expected as they are both parametric tests.

#######################
### Exercise 2: Regression
#######################

# a) Fit a linear regression model to the data frame "data" from exercise 1 
# for the variables Frequency (outcome variable) and Length (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"

linear_regression = lm(Frequency ~ Length, data = data)
linear_regression

# b) How do you interpret the output? Is the relationship between the two variables 
# positive or negative?
# Plot the data points and the regression line.

#the slope is -0.2943  this means that 1 point of increase in length corrosponds to a 0.2943 decrease in Frequency so the relation is negative
# the intercept is 6.5015 this means that the frequency starts from 6.5015 or that a word of length 0 would on average have a frequency of ~6-7. 

ggplot(data, aes(x = Length, y = Frequency)) +
  geom_point() + 
  geom_smooth(method = 'lm')

# c) Run the plotting command again and have R display the actual words that belong 
# to each point. 
# (Don't worry about readability of overlapping words.)

ggplot(data, aes(x = Length, y = Frequency)) +
  geom_point() + 
  geom_label(aes(label = Word))


#######################
### Exercise 3: Regression
#######################


# a) Try this again for another example:
# Let's go back to our digsym data set.
# Set your wd and load the data frame digsym_clean.csv and store it in a variable. 

dat = read.csv('digsym_clean.csv')
str(dat)

# b) Suppose you want to predict reaction times in the digit symbol task by 
# people's age.
# Fit a linear regression model to the data frame for the variables 
# correct_RT_2.5sd (outcome variable) and Age (predictor variable).
# General form: 
# "modelname <- lm(outcome ~ predictor, data = dataFrame, na.action = an action)"
  
mod = lm(correct_RT_2.5sd ~ Age, data = dat)
mod  

# But first we need to cast the data to compute an RT mean (use correct_RT_2.5sd) 
# for each subject, so that we have only one Age observation per Subject.
# Store the result in a new dataframe called "cast".
# In case you're wondering why we still have to do this - like the t-test, 
# linear regression assumes independence of observations.
# In other words, one row should correspond to one subject or item only.

cast = cast(dat, Subject + Age ~ ., mean, value = "correct_RT_2.5sd", na.rm = TRUE)
colnames(cast)[3] = 'correct_RT_2.5sd'

# c) Now fit the regression model.

mod = lm(correct_RT_2.5sd ~ Age, data = cast)
mod  
predict(mod)
# d) Let's go over the output - what's in there?
# How do you interpret the output?
  
# an increase of 1 points of our predictor(age) increases the response(RT) by 21.22 point. furthermore the reaction times start at 637.93 or that
# even a newborn baby should have a reaction time of 639.47

# e) Plot the data points and the regression line.

plot1 = ggplot(cast, aes(x = Age, y = correct_RT_2.5sd)) +
  geom_point() +
  geom_line(aes(y = predict(mod)))

plot1

# f) Plot a histogram and qq-plot of the residuals. 
# Does their distribution look like a normal distribution?

resid = residuals(mod)
hist(resid)
ggplot() + 
  geom_density(aes(resid))
# No the distribution looks like positively skewed

# g) Plot Cook's distance for the regression model from c) which estimates the 
# residuals (i.e. distance between the actual values and the  predicted value on 
# the regression line) for individual data points in the model.

plot(cooks.distance(mod))

# h) Judging from the plot in g) it actually looks like we have 1 influential 
# observation in there that has potential to distort (and pull up) our regression 
# line.
# The last observation (row 37) in cast has a very high Cook's distance 
# (greater than 0.6).
# In other words, the entire regression function would change by more than 
# 0.6 when this particular case would be deleted.
# What is the problem with observation 37?
# Run the plotting command again and have R display the subjects that belong to 
# each point.

# The problem is not with observation 37 but rather with our data. The data belongs to people upto age 32, and only one observation of age 45.
# Thus our model believes the person with age 45 will have a really high reaction time (because of high slope), that is however not the case here.
# Thus we need more data from older age groups to correct our model and predictions.

ggplot(cast, aes(x=Age, y=correct_RT_2.5sd)) +
  geom_point() +
  geom_line(aes(y = predict(mod))) +
  geom_label(aes(label=Subject))


# i) Make a subset of "cast" by excluding the influential subject and name it cast2.

cast2 = cast[c(1:nrow(cast) - 1),c(1:3)]

# j) Fit the model from c) again, using cast2, and take a good look at the output.

mod2 = lm(correct_RT_2.5sd ~ Age, data = cast2)
mod2  
predict(mod2)

# k) What's different about the output?
# How does that change your interpretation of whether age is predictive of RTs?

#we have a much smaller slope, so an increase in Age by 1 increases the reaction time half as much as previously thought  

# l) Plot the regression line again - notice the difference in slope in 
# comparison to our earlier model fit?

plot2 = ggplot(cast2, aes(x = Age, y = correct_RT_2.5sd)) +
  geom_point() +
  geom_line(aes(y = predict(mod2)))
plot2

# m) Display the two plots side by side to better see what's going on.


install.packages("gridExtra")
library(gridExtra)
gridExtra::grid.arrange(plot1, plot2)

# n) Compute the proportion of variance in RT that can be accounted for by Age.
# In other words: Compute R Squared.
# Take a look at the Navarro book (Chapter on regression) if you have trouble 
# doing this.

rsquared = 1 - sum((cast2$correct_RT_2.5sd - predict(mod2))^2) / sum((cast2$correct_RT_2.5sd - mean(cast2$correct_RT_2.5sd))^2)
rsquared

# o) How do you interpret R Squared?

#We have only explained 3.5% of the variance in correct_RT_2.5sd by the variance in Age
