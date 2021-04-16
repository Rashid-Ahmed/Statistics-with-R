

# 2. Read in the data into a variable called "dat".

dat = read.csv('digsym.csv')

# 3. Load the libraries languageR, stringr, dplyr and tidyr.

library(languageR)
install.packages("stringr")
library(stringr)
library(dplyr)
install.packages("tidyr")
library(tidyr)

# 4. How many rows, how many columns does that data have?

print (paste("rows:", nrow(dat)))
print (paste('cols:', ncol(dat)))

# 5. Take a look at the structure of the data frame using "glimpse".

glimpse(dat)

# 6. View the first 20 rows, view the last 20 rows.

head(dat, n = 20)
tail(dat, n = 20)

# 7. Is there any missing data in any of the columns?

colSums(is.na(dat))
# the column stimulDS1.RT has 370 missing values

# 8. Get rid of the row number column.

dat = subset(dat, select = -c(X))

# 9. Put the Sub_Age column second.

dat = dat[, c(1, 10, 2:9)]

# 10. Replace the values of the "ExperimentName" column with something shorter, more legible.

dat['ExperimentName'] = "DigSym-kop"

# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.

data2 = dat %>% filter(List == 'Trial:2')
dat = data2
rm(data2)

# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate".

dat = separate(dat, Sub_Age, c('subject', 'Age'))

# 13. Make subject a factor.

dat$subject = as.factor(dat$subject)

# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".

str_remove_all(dat$File, '[123456789_]')

# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 at the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc).


dat$File = str_pad(dat$File, 8, 'right', pad = '0')

# 16. Remove the column "List".

dat = dat[c(1:5, 7:11)]

# 17. Change the data type of "Age" to integer.

dat$Age = as.integer(dat$Age)
glimpse(dat)

# 18. Missing values, outliers:
# Do we have any NAs in the data, and if so, how many and where are they?

colSums(is.na(dat))
#we dont have any missing values

# 19. Create an "accuracy" column using ifelse-statement.
# If actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0.

dat['accuracy'] = if_else(dat$StimulDS1.RESP == dat$StimulDS1.CRESP, 1, 0)

# 20. How many wrong answers do we have in total?

sum(dat$accuracy == 0)
#Total of 185 wrong answers

# 21. What's the percentage of wrong responses?

sum(dat$accuracy ==0)/length(dat$accuracy)
#0.05555556

# 22. Create a subset "correctResponses" that only contains those data points where subjects 
# responded correctly. 

correctResponses = dat %>% filter(StimulDS1.RESP == StimulDS1.CRESP)

# 23. Create a boxplot of StimulDS1.RT - any outliers?

boxplot(dat$StimulDS1.RT)
# Yes there are quite a few outliers all above the upper limit above approximately the 3000 range the highest outlier at around 14000

# 24. Create a histogram of StimulDS1.RT with bins set to 50.

hist(dat$StimulDS1.RT, breaks = 50)

# 25. Describe the two plots - any tails? any suspiciously large values?

# In the histogram the data is positively skewed as the tail is on the right side similiarly in box plot tail is on the upper side 
# Most of the values lie below the 3500 margin after that there are a few suspicious outliers  on the 3500-4000 range a few outliers on the
# 6000 point margin and one  on 8000 and 14000 each

# 26. View summary of correct_RT.

summary(dat$StimulDS1.CRESP)

# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named 
# "cleaned".

cleaned = dat %>% filter(StimulDS1.RT <10000)

###############
### Exercise 2: Deriving sampling distributions
###############
## In this exercise, we're going to derive sampling distributions with 
## different sizes.

## a) Load the package languageR. We're going to work with the dataset 'dative'. 
## Look at the help and summary for this dataset.

library(languageR)
help(dative)
summary(dative)

## The term dative alternation is used to refer to the alternation between 
## a prepositional indirect-object construction
## (The girl gave milk (NP) to the cat (PP)) and a double-object construction 
## (The girl gave the cat (NP) milk (NP)).
## The variable 'LenghtOfTheme' codes the number of words comprising the theme.

## b) Create a contingency table of 'LenghtOfTheme' using table(). 
##    What does this table show you?

table(dative$LengthOfTheme)
# It shows me the count of each value of LengthOfTheme. so e.g in only one observation length of theme had the value 46 and in 710 observations
# the LengthOfTheme. had value 1

## c) Look at the distribution of 'LenghtOfTheme' by plotting a histogram and a boxplot. 
##    Do there appear to be outliers? Is the data skewed?

hist(dative$LengthOfTheme)
boxplot(dative$LengthOfTheme)

#Yes there are quite a few outiers that range from >10 and above, and yes the data is positively skewed

## d) Now we're going to derive sampling distributions of means for different 
##    sample sizes. 
##    What's the difference between a distribution and a sampling distribution?

# A distribution is basically a function that describes the relation between observations while the sampling distribution gives us a relationship
# between a specific parameter of all the samples of a particular size of that distribution. 
# E.g if we collect the age of everyone enrolled in saarland university, and then take a sample size of 10 people, and take mean as our parameter
# our sampling distribution will countain the means of ages of 10 random people for all possible combinations plotted against 
# the frequency of these means
# on the other hand our distribution will simply be the relationship of all the people studying in saarland e.g our distribution is taken from a
# university so we can assume the average mean of the distribution will be between 20 and 30 etc.

## e) We are going to need a random sample of the variable 'LengthOfTheme'. 
##    First create a random sample of 5 numbers using sample(). 
##    Assign the outcome to 'randomsampleoflengths'

randomsampleoflengths = sample(dative$LengthOfTheme, size = 5)

## f) Do this again, but assign the outcome to 'randomsampleoflengths2'. 

randomsampleoflengths2 = sample(dative$LengthOfTheme, size = 5)

## g) Now calculate the mean of both vectors, and combine these means 
##    into another vector called 'means5'.

means5 = c(mean(randomsampleoflengths), mean(randomsampleoflengths2))
## h) In order to draw a distribution of such a sample, we want means of 
##    1000 samples. However, we don't want to repeat question e and f 
##    1000 times. We can do this in an easier way: 
##    by using a for-loop. See dataCamp or the course books for 
##    how to write loops in R.

for (i in 1:1000){
  means5[i] = mean(sample(dative$LengthOfTheme, size = 5))
}


## i) Repeat the for-loop in question h, but use a sample size of 50. 
##    Assign this to 'means50' instead of 'means5'.

means50 = 0
for (i in 1:1000){
  means50[i] = mean(sample(dative$LengthOfTheme, size = 50))
}

## j) Explain in your own words what 'means5' and 'means50' now contain. 
##    How do they differ?

# means5 and means50 are both  sampling distributions for the mean parameter of our variance LengthOfTheme, the difference is in the sample size
# The values in the vector means5 are the mean of 5 random observations of LengthOfTheme, while the values in means50 are 
# the mean of 50 random observations.   

## k) Look at the histograms for means5 and means50. Set the number of breaks to 15.
##    Does means5 have a positive or negative skew?

hist(means50, breaks = 15)
hist(means5, breaks = 15)
# means5 has a positive skew


## l) What causes this skew? In other words, why does means5 have bigger 
##    maximum numbers than means50?

# because in means5 we are taking means of only 5 random values, and thus have a much larger likelihood of getting large
# means(having outliers which do not represent the population) while in means50 we are taking mean of 50 random values which gives us 
# a better average and even though this is still not a robust estimator, taking the mean of 50 values would minimize the effect of outliers


###############
### Exercise 3: Confidence interval
###############

## A confidence interval is a range of values that is likely to contain an 
## unknown population parameter.
## The population parameter is what we're trying to find out. 
## Navarro discusses this in more depth in chapter 10.

## a) What does a confidence interval mean from the perspective of experiment replication?

# It is the interval(range) of values a parameter(mean, variance etc) of the actual population will lie under.
# if we can replicate the experiment we will get different values, but it is very likely that our replication mean will 
# fall within the confidence interval



## b) Let's calculate the confidence interval for our means from the previous 
##    exercise.
##    First, install and load the packages 'lsr' and 'sciplot'

install.packages("lsr")
install.packages("sciplot")

## c) Look at the description of the function ciMean to see which arguments it takes.

??ciMean

## d) Use ciMean to calculate the confidence interval of the dataset dative from
##    the previous exercise.
##    Also calculate the mean for the variable LengthOfTheme.

ci = lsr::ciMean(dative)
me = mean(dative$LengthOfTheme)

## e) Does the mean of the sample fall within the obtained interval? 
##    What does this mean?


# Yes the mean falls within the obtained interval, this means that our confidence interval is accurate but the interval is about 0.3 in length
# which covers about 14% of the total data (excluding the outliers) and this is thus not a good interval (its too big)

## f) As the description of dative mentions, the dataset describes the 
##    realization of the dative as NP or PP in two corpora.
##    The dative case is a grammatical case used in some languages 
##    (like German) to indicate the noun to which something is given.
##    This dataset shows us, among other things, how often the theme is 
##    animate (AnimacyOfTheme) and how long the theme is (LengthOfTheme).
##    Plot this using the function bargraph.CI(). Look at the help for this function. 
##    Use the arguments 'x.factor' and 'response'.

sciplot::bargraph.CI(x.factor = dative$AnimacyOfTheme , response = dative$LengthOfTheme)

## g) Expand the plot from question f with the ci.fun argument 
##    (this argument takes 'ciMean'). 
##    Why does the ci differ in this new plot compared to the previous plot?

sciplot::bargraph.CI(x.factor = dative$AnimacyOfTheme , response = dative$LengthOfTheme, ci.fun = lsr::ciMean)

# Because the confidence interval calculated by our ciMean function is bigger than the default confidence interval of bargraph which happens because
# the ciMean takes as a default 95% confidence which is too high.
