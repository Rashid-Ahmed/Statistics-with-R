
###############
### Exercise 1: Getting started
###############
## a) Look at your current working directory.
getwd()

## b) Get help with this function.
?help

## c) Change your working directory to another directory.
setwd("/Users/Laurent/StatsR/sheet1")


###############
### Exercise 2: Participants' age & boxplots
###############
## In this exercise, we will deal with data from a package.

## a) Install the package "languageR" and load it.
install.packages('languageR')
library(languageR)

## b) Specifically, we will deal with the dataset 'dutchSpeakersDistMeta'. 
##    This dataset should be available to you once you've loaded languageR.
##    The dataset contains information on the speakers included in the Spoken 
##    Dutch Corpus. Inspect 'dutchSpeakersDistMeta'. Look at the head, tail, 
##    and summary. What do head and tail show you?

head(dutchSpeakersDistMeta)
tail(dutchSpeakersDistMeta)
summary(dutchSpeakersDistMeta)
#head() shows the first 6 entries and tail() shows the last 6 entries.

## c) Each line in this file provides information on a single speaker. How many 
##    speakers are included in this dataset? In other words, use a function to 
##    retrieve the number of rows for this dataset.
nrow(dutchSpeakersDistMeta)

## d) Let's say we're interested in the age of the speakers included in the 
##    corpus, to see whether males and females are distributed equally. 
##    Create a boxplot for Sex and AgeYear.
boxplot(dutchSpeakersDistMeta$AgeYear~dutchSpeakersDistMeta$Sex)

## e) Does it seem as if either of the two groups has more variability in age?
#It seems as if the female group has more variability in age. The interquartile range is much larger.

## f) Do you see any outliers in either of the two groups?
#the male group has 2 data points outside 1.5*IQR  

## g) Now calculate the mean and standard deviation of the AgeYear per group. 
##    Do this by creating a subset for each group.
##    Do the groups seem to differ much in age?

males <- subset(dutchSpeakersDistMeta, Sex == "male")
females <- subset(dutchSpeakersDistMeta, Sex == "female")

## h) What do the whiskers of a boxplot mean?
#The Whiskers include all data points up to 1.5*IQR

## i) What is the inter-quartile range in the boxplot?
#It is the area between the whiskers. It shows the range between the bottom 25% and top 25% of data points

## j) Is the plot positively or negatively skewed?
#It is negatively skewed


###############
### Exercise 3: Children's stories & dataframes
###############
# A researcher is interested in the way children tell stories. More specifically,
# she wants to know how often children use 'and then'. She asks 25 children to
# tell her a story, and counts the number of times they use 'and then'.
# The data follow:

# 18 15 22 19 18 17 18 20 17 12 16 16 17 21 25 18 20 21 20 20 15 18 17 19 20 


## a) What measurement scale is this data? Is it discrete or continuous? Explain
##    in one sentence why? (remember, comment out written answers)
#It is ratio scale and discrete. 
#It can be ordered, has meaningful distance between numbers and has a meaningful zero.

## b) In the next questions (c-e), you will create a dataframe of this data, 
##    which will also include participant IDs.
##    Why is a dataframe better suited to store this data than a matrix?
#It is one dimensional and we can name the columns in a data frame

## c) First create a vector with participant IDs. Your vector should be named 
##    'pps', and your participants should be labeled from 1 to 25
pps <- c(1:25)
pps


## d) Next, create a vector containing all the observations. Name this vector 'obs'.
obs <- c(18, 15, 22, 19, 18, 17, 18, 20, 17, 12, 16, 16, 17, 21, 25, 18, 20, 21, 20, 20, 15, 18, 17, 19, 20)

## e) Create a dataframe for this data. Assign this to 'stories'. 
stories <- data.frame(pps=pps, obs=obs)
## f) Take a look at the summary of your dataframe, and at the classes of your 
##    columns. What class is the variable 'pps'?
summary(stories)
lapply(stories,class)
#it is class "integer"

## g) Change the class of 'pps' to factor. Why is factor a better class for this
##    variable?
stories$pps <- factor(stories$pps)
#Because it is a categorical variable that can only take on a limited number of values


## h) Plot a histogram (using hist()) for these data. Set the number of breaks 
##    to 8.
hist(stories$obs, breaks=8)



## i) Create a kernel density plot using density().
plot(density(stories$obs))

## j) What is the difference between a histogram and a kernel density plot?
#Histogram counts the number of values that fall into a bucket while kernel density plot calculates a normal distribution for every data point and adds those up

## This is a difficult one, remember you just need to provide a serious attempt at solving each 
## exercise in order to pass. 
## k) Overlay the histogram with the kernel density plot 
##    (hint: the area under the curve should be equal for overlaying the graphs 
##    correctly.)
h <- hist(stories$obs, breaks=8)
m <- (h$counts / h$density)[1]
d <- density(stories$obs)
d$y <- d$y*m

lines(d)


###############
### Exercise 4: Normal distributions
###############
## In this exercise, we will plot normal distributions.

## a) First, use seq() (?seq) to select the x-values to plot the range for
##    (will become the x-axis in the plot).
##    Get R to generate the range from -5 to 5, by 0.1. Assign this to the 
##    variable x.
x <- seq(-5, 5, by=0.1)

## b) Now we need to obtain the y-values of the plot (the density). We do this 
##    using the density function for the normal distribution. 
##    Use "help(dnorm)" to find out about the standard functions for the normal 
##    distribution.
y <- dnorm(x)

## c) Now use plot() to plot the normal distribution for z values of "x". 
plot(x,y)

## d) The plot now has a relatively short y-range, and it contains circles 
##    instead of a line. 
##    Using plot(), specify the y axis to range from 0 to 0.8, and plot a line 
##    instead of the circles.
plot(x,y,type="l", ylim=c(0,0.8))


## e) We want to have a vertical line to represent the mean of our distribution.
##    'abline()' can do this for us. Look up help for abline(). 
##    Use abline() to create the vertical line. Specify the median of x using
##    the argument 'v'.
##    In order to get a dashed line, set the argument 'lty' to 2.
abline(v=mean(x), lty=2)

## f) Take a look at the beaver1 dataset. (You can see it by typing "beaver1".) 
##    Then select only the temperature part and store it in a variable "b1temp".
b1temp <- beaver1$temp

## g) Calculate the mean and standard deviation of this dataset and plot a normal
##    distribution with these parameters.
b1tmean <- mean(b1temp)
b1tsd <- sd(b1temp)
y<-dnorm(b1temp, mean = b1tmean, sd = b1tsd)
plot(b1temp, y)

## h) We observe two temparatures (36.91 and 38.13). What's the likelihood that
##    these temperatures (or more extreme ones) respectively come 
##    from the normal distribution from g)?
pnorm(36.91, b1tmean, b1tsd, lower.tail = FALSE)
pnorm(38.13, b1tmean, b1tsd, lower.tail = FALSE)


## i) Use the random sampling function in R to generate 20 random samples from
##    the normal distribution from g), and draw a histogram based on this sample.
##    Repeat 5 times. What do you observe?
#?sample
#values <- list()
#for(i in 1:5)
#{
#  values[[i]] <- hist(sample(y, 20), breaks=8)
#}
#lapply(values, plot, ylim=c(0,50),add=TRUE)


hist(sample(y, 20), breaks=8)
#The values change slightly, but the distribution always looks roughly the same. There are always many more values around the mean of 2.


