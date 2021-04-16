
###############
### Exercise 1: Plotting graphs using ggplot.
###############
# There are many ways of making graphs in R, and each has their own advantages 
# and disadvantages. One popular package for making plots is ggplot2. 
# The graphs produced with ggplot2 look professional and the code is quite easy 
# to manipulate.
# In this exercise, we'll plot a few graphs with ggplot2 to show its functionalities.
# You'll find all the information you'll need about plotting with ggplot2 here: 
# http://www.cookbook-r.com/Graphs/


# Also, you have been assigned the ggplot2 course in DataCamp. Please work through 
# this course first to easily solve the assignment below

## a) First install and load the ggplot2 package. Look at the help for ggplot.

install.packages("ggplot2")
library(ggplot2)

## b) We're going to be plotting data from the dataframe 'ratings' 
##    (included in languageR). 
##    Look at the description of the dataset and the summary.
library(languageR)
str(languageR::ratings)
summary(languageR::ratings)

## For each word, we have three ratings (averaged over subjects), one for the 
## weight of the word's referent, one for its size, and one for the words' 
## subjective familiarity. Class is a factor specifying whether the word's 
## referent is an animal or a plant. 
## Furthermore, we have variables specifying various linguistic properties, 
## such as word's frequency, its length in letters, the number of synsets 
## (synonym sets) in which it is listed in WordNet [Miller, 1990], its 
## morphological family size (the number of complex words in which 
## the word occurs as a constituent), and its derivational entropy (an 
## information theoretic variant of the family size measure). 
## Don't worry, you don't have to know what all this means yet in order to 
## be able to plot it in this exercise!

## c) Let's look at the relationship between the class of words and the length. 
##    In order to plot this, we need a dataframe with the means.
##    Below you'll find the code to create a new dataframe based on the existing 
##    dataset ratings.
##    Plot a barplot of ratings.2 using ggplot. Map the two classes to two 
##    different colours. 
##    Remove the legend.
ratings = languageR::ratings
head(ratings)
summary(ratings)
condition <- c("animal", "plant")
frequency <- c(mean(subset(ratings, Class == "animal")$Frequency), mean(subset(ratings, Class == "plant")$Frequency))
length <- c(mean(subset(ratings, Class == "animal")$Length), mean(subset(ratings, Class == "plant")$Length))
ratings.2 <- data.frame(condition, frequency, length)
ratings.2

ggplot(data=ratings.2, aes(x=condition, y=length, fill=condition)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "none")


## d) Let's assume that we have additional data on the ratings of words. 
##    This data divides the conditions up into exotic and common animals 
##    and plants.
##    Below you'll find the code to update the dataframe with this additional data.
##    Draw a line graph with multiple lines to show the relationship between 
##    the frequency of the animals and plants and their occurrence.
##    Map occurrence to different point shapes and increase the size 
##    of these point shapes.
condition <- c("animal", "plant")
frequency <- c(7.4328978, 3.5864538)
length <- c(5.15678625, 7.81536584)
ratings.add <- data.frame(condition, frequency, length)
ratings.3 <- rbind(ratings.2, ratings.add)
occurrence <- c("common", "common", "exotic", "exotic")
ratings.3 <- cbind(ratings.3, occurrence)
ratings.3

ggplot(ratings.3, aes(x=condition, y=frequency, color=condition)) + 
  geom_point(aes(shape=occurrence, size=occurrence)) +
  geom_line()

## e) Based on the graph you produced in question d, 
##    what can you conclude about how frequently 
##    people talk about plants versus animals, 
##    with regards to how common they are?

#We can see that people more frequently talk about exotic animals than common animals
#but more frequently talk about about common plants than exotic plants.
#Also they more frequently talk about animals than plants in general

##########
##Exercise 2. Binomial distribution
##########
## Suppose there are 12 multiple choice questions in a quiz. 
## Each question has 4 possible answers, and only one of them is correct. 

## a) Please calculate the probability of getting exactly 5 answers right 
##    if you answer by chance. Calculate this using the dbinom() function.


dbinom(5,size = 12, prob = 0.25)
verified = round((.25^5)*(.75^7)*792, 8) == round(dbinom(5,size = 12, prob = 0.25), 8)
print (verified)
## b) Next please calculate the probability of answering 4 or less questions 
##    correctly by chance. 

sum(dbinom(x=0:4,12,0.25))

##########
##Exercise 3. Chi-square test
##########
## a) Consider the dataset dutchSpeakersDistMeta from sheet1. 
##    Load the package (languageR) and look at the summary of the variables, 
##    as well as their classes. Which variables are factors?

library(languageR)
summary(languageR::dutchSpeakersDistMeta)
str(languageR::dutchSpeakersDistMeta)

#Speaker, Sex, AgeGroup, ConversationType and EduLevel are factors

## b) We want to find out whether there is a difference between males and females 
##    with respect to the age groups they are in.
##	  First use the function 'table()' to get the counts and create 
##    a contingency table of AgeGroup by Sex.

table(languageR::dutchSpeakersDistMeta$Sex, languageR::dutchSpeakersDistMeta$AgeGroup)

##    Visualize your data with a single bar plot (use ggplot) that represents the counts with 
##    respect to each age group and each sex.

ggplot(languageR::dutchSpeakersDistMeta, aes(x = AgeGroup, fill = Sex)) +
  geom_bar(position = position_dodge(), na.rm = TRUE) +
  scale_x_discrete(na.translate = FALSE)

## c) Inspect the table you created in b). Does it look like there could be a significant 
##    difference between the sexes?

# I believe the difference between sexes is not very significant but it really depends on the significance level we are considering. 

## d) We are going to calculate whether there's a difference between males and females 
##    regarding their age group using the function chisq.test. 
##    Look at the help of this function. 
##    Then use the  function to calculate whether there's a difference in our table from b). 
##    Is there a significant difference in age group?

chisq.test(languageR::dutchSpeakersDistMeta$AgeGroup, languageR::dutchSpeakersDistMeta$Sex)
# There isnt a significant difference between the age groups, we get a significance value of even >0.5, usually significance values of >0.05 and 0.1
# are considered so we can comfortably say that there is no difference between the groups.

## e) What are the degrees of freedom for our data? How are they derived?

# The degrees of freedom in our data = 4 which is calculated as n - 1 (where n is the number of age groups). The n is the different number of 
# distribution we have. In our case we are calculating different type of age groups, similiarly an example of n would be the number of
# days of the weak where each day could have some amount of sales

##########
##Exercise 4. Binomial versus chi-square
########## 
##    In this exercise, we will consider a made up example of there doctors can predict  
##    if a patient has temperature or not just by holding their hand  
##    Several doctors were blindfolded and were asked to tell if the experimenter  
##    has temperature/fever or not.  
##    There were a total of 200  trials, of which the doctors 
##    correctly indicated that a patient had fever 83 times.

## a) What is the null hypothesis, i.e. how often would we expect the doctor to 
##    be correct by chance (in raw number and in percentage)?

# Our null hypothesis would be (mean = 100/p = 50%) or that on average 100 of 200 doctors 
# would give us the right result due to chance

## b) Using a chisquare test, what do you conclude about whether this idea
##    of checking fever/temperature works? 

expected = 100
observed = 83

(((observed-expected)^2) + (((200-observed)-expected)^2))/expected

# The value we get is 5.78
# The value of our probability lies between 1 - 2.5% which is less than the threshold set for significance level(5%) hence we can  reject
# our alternative hypothesis, and safely conclude that the doctors cannot check fever by just holding the patients hands

## c) Now calculate significance using the binomial test as we used it in exercise 2.

pbinom(83, size = 200, prob = 0.5)
#0.96%

## d) The results from these two tests are slightly different. Which test do you think 
##    is better for our data, and why?

# The binomial test is better because we only have two categories(has fever/ doesnt have fever). With two categories our degree of freedom is 1
# in chi squared test and that would gives us an inaccurate result.

##########
##Exercise 5.
##########
## Describe a situation where you would choose McNemar's test over the ChiSquare test. 
## What would be the problem of using the normal ChiSquare test in a case where 
## McNemar's test would be more appropriate?

# A situation where the observations are not independent of one another for example the amount of people taking the stats with R course before
# and after watching a video on the course structure. In this case the observations depend on watching the video so we should not apply
# chisquare test and should instead apply MCNemar's test. Furthermore MCNemar's test can only be applied in a 2x2 scenario such as this (YES/NO) for
# (Before/After watching video).
# If we want to know whether what we exposed the sample to helped us (so in the upper case if we want to know whether the video helped change   
# people's perspective of the course and persuaded them to take the course, it would be a bad idea to use ChiSquare test as it will not measure 
# this change that the video created.
   

