
###############
### Cleaning Data
###############
library(lsr)
library(tidyr)
library(effsize)
library(dplyr)
library(ggplot2)

# 1. Download the data file "digsym_clean.csv" from the moodle and save it in your 
# working directory. 


# 2. Read in the data into a variable called "data".

data = read.csv("digsym_clean.csv")

# 3. Get rid of the column "X"

data = subset(data, select = -c(X))

# Say you're interested in whether people respond with different accuracy to 
# right vs. wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate 
  #             standard deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


# 4. Apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function above for the arguments description).

datac = summarySE(data = data, measurevar = 'accuracy', groupvars = c('condition'))

head(datac)
# 5. Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?

library(ggplot2)
ggplot(datac, aes(x=condition, y=accuracy, fill=condition)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), width=.5)

# 6. Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: Which assumption is violated?

#The assumption about normal distribution of our data is violated because accuracy follows a binomial distribution here 

# 7. We need to reshape the data to only one observation (average accuracy) per subject 
# and right/wrong condition. Here we will use cast() which we discussed in the tutorial
# for sheet 2. 
# Collapse the data, 
# using cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T).
# Store the result in a new variable called "cdata". 
## Check ?cast or https://www.statmethods.net/management/reshape.html for more infos on 
## cast(). 

library(reshape)
cdata = cast(data, Subject + condition ~ ., mean, value = "accuracy" , na.rm = T)
colnames(cdata)[3] = 'accuracy'
head(cdata)
# 8. Create histograms of the accuracy data depending on the right and wrong 
# condition and display them side by side.

ggplot(cdata, aes(x = accuracy, fill = condition)) +
  geom_histogram(position = position_dodge()) 

# 9. Display the same data in density plots. 


ggplot(cdata, aes(x = accuracy, fill = condition)) +
  geom_density(position = position_dodge())

# 10. Based on the histograms and the density plots - are these data normally 
# distibuted?

#No the data is negatively skewed

# 11. Create boxplots of the accuracy data.

ggplot(cdata, aes(x = condition, y = accuracy, fill = condition)) +
  geom_boxplot()

# 12. Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?

t.test(cdata$accuracy~cdata$condition, paired = FALSE)
# We need unpaired t-test as the condition wrong and right are independent of each other and do not belong to the same group 

# 13. What does the output tell you? What conclusions do you draw?

#the output gives us  t-score of 3.34 with 71.876 degrees of freedom giving us a p-value of 0.001

#this means that the likelihood of calculating the mean difference by chance is about .1%, which means 
#this difference in means is most likely not due to chance and is statistically significant
# and hence people do respond differently to right vs wrong prediction.

# 14. Compute the effect size using CohensD.

cohensD(cdata$accuracy[cdata$condition == 'right'] , cdata$accuracy[cdata$condition == 'wrong'])

# 15. Which effect size do we get? How do you interpret this result?

# We get a effect size of 0.7765. This is the standardized difference between the means of two independent samples
# Generally a difference of 0.8 or more is considered a large effect and a difference of 0.5-0.8 is considered medium so in our case we
# have a almost large difference and thus people do respond differently to right vs wrong prediction.

# 16. In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format (this is the format we have been using in 
# class examples.)
# Let's do a transformation of our data set (cdata) to see what it would look like in a wide 
# format.
# Use spread() from the tidyr package.

cdata.spread = spread(cdata, condition, accuracy)

# 17. Compute the t-test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.

t.test(cdata.spread$right,cdata.spread$wrong, paired = FALSE)

# 18. Compare the t-test results from the wide-format and the long-format data. 
# What do you notice?

# The results are the same, but the wide format uses a single observation per subject
# while the long format uses 2 observations per subject (1 for each condition)

# 19. Compute CohensD on the wide format data. What do you notice?

cohensD(cdata.spread$right, cdata.spread$wrong)
#The effect is the same for long and wide format, but it is computationally faster and easier to compute cohensD in this case

# 20. Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the original data, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T).
# Store the result in a new variable called "cdat"
cdat = cast(data, StimulDS1.RESP + Gender ~ .,mean, value = 'StimulDS1.RT', na.rm = T)
colnames(cdat)[3] = 'ReactionTime'

# 21. Take a look at cdat using head().

head(cdat)

# 22. Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?

t.test(cdat$ReactionTime~cdat$Gender, paired = FALSE)
# We need independent t-test as the Reaction times of males do not depend on the Reaction times of females and vice versa

#The result gives us a t-score of -2.7182 and 1.5928 degrees of freedom corresponding to a p-value of 0.1433
#Because the p-value is not statistically significant we cannot assume that the difference in means didnt
#happen by chance. We cannot reject the null hypothesis, so we have to say that there is no difference
#in reaction time between females and males

###############
### T-Test
###############
#In this exercise we will try to explore the independent samples t-test 
#and its affect on different samples. 
#We will take the same example discussed in the lecture. A class has two tutors, and we want to find out which tutor is better by
#comparing the performance of the students in the final exam by tutor group. 

#1. Generate 10 samples from a normal distribution with mean 0 and sd 10 and save it a variable names "first_tutor_grades"

first_tutor_grades = rnorm(n = 10 , mean = 0, sd = 10)

#2. Create a vector named "first_tutor" having same 10 values -> "tutor1"

first_tutor = rep('tutor1', rep = 10)

#3. Create a data frame named "data_frame" having 2 columns "first_tutor", "first_tutor_grades" created above. 

data_frame = data.frame('first_tutor' = first_tutor, 'first_tutor_grades' = first_tutor_grades)

#4. Change the column names of the data frame to "tutor" and "score"

colnames(data_frame)[c(1,2)] = c('tutor', 'score')

#5. repeat the steps 1-4 with the following changes:
  #i) generate another 10 samples with mean 10 and sd 25. save it in a variable: second_tutor_grades
  #ii)Create a vector named "second_tutor" having 10 same values -> "tutor2"
  #iii) Create a data frame named "data_frame2" having 2 columns "second_tutor", "second_tutor_grades" created above.
  #iv) Change the column names of the data frame to "tutor" and "score"
 
second_tutor_grades = rnorm(n = 10 , mean = 10, sd = 25)
second_tutor = rep('tutor2', rep = 10)
data_frame2 = data.frame('second_tutor' = second_tutor, 'second_tutor_grades' = second_tutor_grades)
colnames(data_frame2)[c(1,2)] = c('tutor', 'score')

#6. combine both data frames into a new one and name it "final_df"
# final_df should have 2 columns (tutor, score) having 20 rows. e.g.
#   tutor      score
#1  tutor1     9.09
#2  tutor1     4.66
#3  tutor1     3.56
#4  tutor2     1.56
#5  tutor2     545

final_df = rbind(data_frame, data_frame2)

#7. run the independent samples TTest (independentSamplesTTest) and formulate the findings as discussed in the lecture. 
#	What do you observe? 
#	independentSamplesTTest also provides the effect size (Cohen's d). How do you interpret the effect size?

independentSamplesTTest(score ~ tutor, data=final_df)
# The mean of tutor1 and 2 are 2.449 and 8.109 respectively and sd are 23.015 and 20.482 respectively this difference from the actual mean and sd
# especially in the case of tutor 2(mean diff = 23 - 10 = 13) is because our sample size is too small and if we want our sample to replicate the 
# population we need a bigger sample size. p<0.05 so the test is statistically significant and the population mean is different in both group
# cohen's d is also 1.32 which is > 0.8 hence there is a big difference between the two tutors


#
#8. Time to play around!
#	repeat the whole experiment you performed above with different sample size, mean and standard deviation  
#	repeat it 3 times changing all the values (sample size, mean, sd) and formulate the findings.  
#	what do you observe when we keep the means and sd same?



tTestExp <- function(samples, mean1, sd1, mean2, sd2) {
  first_tutor_grades = rnorm(samples, mean=mean1, sd=sd1)
  first_tutor = rep("tutor1", rep=samples)
  data_frame = data.frame("tutor"=first_tutor, "score"=first_tutor_grades)
  
  second_tutor_grades = rnorm(samples , mean = mean2, sd = sd1)
  second_tutor = rep('tutor2', rep = 10)
  data_frame2 = data.frame('tutor' = second_tutor, 'score' = second_tutor_grades)
  
  final_df = rbind(data_frame, data_frame2)
  
  vareq = F
  if(sd1 == sd2) {
    vareq = T
  } 
  
  independentSamplesTTest(score ~ tutor, data=final_df)
}


tTestExp(1000, 0, 10, 10, 50)
#The mean score in tutor1's class was 0.47 (sd=10.46), while the mean score in tutor2's class was 11.19 (sd=50.12)
#A Student's independent samples t-test showed that this difference was significant (t(1086)=-6.62, p<0.05, CI= [-13.9, -7.544], d=.3)
#suggesting that a real difference in scores had occurred between the students of the different tutors

tTestExp(5, 0, 1, 2, 1)
#The mean score in tutor1's class was 0.6 (sd=1.09), while the mean score in tutor2's class was 1.19 (sd=0.88)
#A Student's independent samples t-test showed that this difference was not significant (t(8)=-0.943, p>0.05, CI= [-2,03, 0.85], d=.6)
#suggesting that there was no difference in score between the tutors


tTestExp(15, -10, 100, 10, 100)
#The mean score in tutor1's class was -11.21 (sd=81.5), while the mean score in tutor2's class was -8.869 (sd=73.54)
#A Student's independent samples t-test showed that this difference was not significant (t(28)=-0.083, p>0.05, CI= [-60.4,55.72], d=.03)
#suggesting that there was no difference in score between the tutors


tTestExp(10, 10, 25, 10, 25) #same as in 7
#we observe that the values change in every time we perform the experiment. Most of the time the Students t-test shows no 
#significant difference between the scores of the tutors, and p>0.05 most of the time even with a small sample size and for large sample
# p is always >0.05 with same mean and sd


