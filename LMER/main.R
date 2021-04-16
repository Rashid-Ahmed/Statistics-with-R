
###########################################################################################
###########################################################################################
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(car)
library(reshape)


# a)There is (gender.Rdata) datasets on moodle.
#   Read in the data file (gender.Rdata) 
#   and assign it to a variable called "dat". 
#   See a description of the items in the datasets below.

# The files contain data from an experiment where people were reading sentences, 
# and pressed the space bar to see the next word. The duration for which a word was 
# viewed before pressing the space bar again is the reading time of the word, and is 
# stored in the file as "WORD_TIME". The experiment had 24 items (given as "ITEM_ID") 
# and 24 subjects (given as "PARTICIPANT"). The order in which the different sentences 
# were presented in the experiment is given in the variable "itemOrder". 

dat = read.table('gender.Rdata.txt')
head(dat)


# b) Inspect "dat" and provide 2 plots. 
#    The first plot should provide insights about the relationship between WORD_TIME 
#    and ITEM_TYPE. 
#    For the second plot you should first subset the data using only RELWDINDEX == 0 and
#    then plot the WORD_TIME for the different conditions (ITEM_TYPE).

str(dat)

ggplot(data = dat, aes(x = ITEM_TYPE, y = WORD_TIME, color = ITEM_TYPE)) +
  geom_boxplot() 

dat_subset = dat[dat$RELWDINDEX == 0,]
ggplot(data = dat_subset, aes(x = ITEM_TYPE, y = WORD_TIME)) +
  geom_point() 

# c) Decide whether you want to exclude any data points (provide not only the code,
#    but also a detailed (!) explanation). 
#    Note that we are evaluating WORD_TIME as our reponse variable. 
#    What time intervals make sense for such an experiment?

ggplot(data = dat, aes(x = WORD_TIME)) +
  geom_density()
plot(cooks.distance(lm(WORD_TIME ~ ITEM_TYPE + itemOrder, data = dat)))


# This looks like a normal distribution with outliers mostly on the right, thus it wouldn't be a bad idea to cut the data at ~95+% density 
# i.e remove points that are statistically significant (that are very likely to be here due to some error). And thus we are only including time
# intervals that cover 97.5% of the total points(2.5% from the right and 1.25% from the left).
# (the 1.25% on the left covers reaction time <210 and the 2.5% on the right covers reaction time > 1418.95 which both seem unrealistic under
# normal conditions)
#finally look at cook's distance here, this will also get much better as we implement the above.

upper_limit = quantile(dat$WORD_TIME, 0.975)
lower_limit = quantile(dat$WORD_TIME, 0.0125)

dat = dat[dat$WORD_TIME>=lower_limit & dat$WORD_TIME<=upper_limit, ]

ggplot(data = dat, aes(x = WORD_TIME)) +
  geom_density()
plot(cooks.distance(lm(WORD_TIME ~ ITEM_TYPE + itemOrder, data = dat)))

# d) Make a scatter plot where for each index word as the sentence progresses (RELWDINDEX),
#    the average reading time is shown for each of the two conditions (ITEM_TYPE).
#    Please use two different colours for the different conditions.


casted_dat = cast(dat,RELWDINDEX + ITEM_TYPE ~ ., mean, value = 'WORD_TIME')
colnames(casted_dat)[3] = 'WORD_TIME'

ggplot(data = casted_dat, aes(x = RELWDINDEX, y = WORD_TIME, color = ITEM_TYPE)) +
  geom_point() +
  scale_x_continuous('INDEX', breaks = casted_dat$RELWDINDEX * 2) +
  ylab('Average Word Time')

# e) You do not need to use ggplot here, just follow the example below.
#    The code is a plot for the dataset 'sleepstudy' from the package 'lme4'.
#    The figure shows relationships between days without sleeping and reaction 
#    time for each participant (subject) separately.

summary(sleepstudy)
print(xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
             layout = c(9,2), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "Days of sleep deprivation",
             ylab = "Average reaction time (ms)"))

#    Your task is to figure out how to adapt this plot for our data. What do you 
#    conclude regarding the reading sentences experiment?



#These are a bunch of 2d plots so i assume we can use any single predictor variable with a group variable, hence i used RELWDINDEX as predictor
# and PARTICIPANT as my subject variable 

casted_dat = cast(dat,PARTICIPANT + RELWDINDEX ~ ., mean, value = 'WORD_TIME')
colnames(casted_dat)[3] = 'WORD_TIME'
casted_dat
# We got the average reaction time for each participant seperately

print(xyplot(WORD_TIME ~ RELWDINDEX | PARTICIPANT, casted_dat, aspect = "xy",
             layout = c(6,4), type = c("g", "p", "r"),
             index.cond = function(x,y) coef(lm(y ~ x))[1],
             xlab = "INDEX",
             ylab = "Average reaction time (ms)"))


# f)  Explain the main need for switching to Linear mixed effect model for the study.
#And, report what could be the fixed and random effect stucture.

# We have participants(people) and items repeated in observations, because we have a single person in multiple observations we have dependency
# in observations, and so one reason we include LME models is because our assumption of independence of observations(which is fundamental in both
# parametric and non parametric models) is violated if we use regular linear regression or other tests.
# Another reason is that we can clearly see that the person giving a test has an effect on the reaction time, however because this effect is
# random (and thus normal) among other things, we cannot use  PARTICIPANT as a explanatory variable, thus Linear mixed effect models is a good
# approach here as it helps us distinguish between explanatory variables (fixed effect variables) and random variables.

# The random effect/group variables here are PARTICIPANT and ITEM_ID all other variables are Fixed effect/model variables.

# g) Experiment with calculating a linear mixed effects model for this study, 
#    and draw the appropriate conclusions 


# The one i dont choose
mod = lmer(WORD_TIME ~ EXPWORD*itemOrder*ITEM_TYPE + (1|PARTICIPANT)  ,data = dat)
mean((predict(mod) - dat$WORD_TIME)^2)


#The one i choose
mod = lmer(WORD_TIME ~ EXPWORD + itemOrder + (1|PARTICIPANT)  ,data = dat)
mean((predict(mod) - dat$WORD_TIME)^2)




mod = lmer(WORD_TIME ~(PARTICIPANT|ITEM_TYPE)  ,data = dat)
logLik(mod)
summary(mod)$coefficients

# The above model seems very simple but the problem here is that we have too many factors and groups and too little data, so if i use any variable
# as a slope for the subject or if i include interaction(EXPWORD * itemOrder) i will have too many estimators, which is terrible for a linear model
# as when in a linear model estimators > datapoints we have infinite solutions and when estimators are too many but < datapoints we have a 
# highly overfit model both of which we want to avoid, hence i dont include all the model level and group level variables which contribute little

# some observations

# I dont use the subject TYPE_ID because it does not provide us with a substantial reduction in mse, hence ITEMS dont effect reaction time
# substantially
#It would be a bad idea to have a  EXPWORD for each participant even though it would give us a good training accuracy because that is a total
# of 181*24 = 4344 linear estimators add a few more estimators for other variables and we have for >5.5k examples which is infinitely many solutions
# and which forces lmer to drop some estimators.

# h} Describe how would you report and write up the analysis giving a detailed explanation for each model 

# Provide the data i trained on and using a random seed so the readers can reproduce the results. Also  giving due credit for the creators of lmer 
# and citing the version of R

#Also I would first test mse on multiple models(with/without interaction and grouping of different variables), train it onto the
# test data and then report the results of all significant models so that the reader can get a bigger picture of how i got these results.
# finally i would compare it with other models using anova.

# i) Let's get back to the dataset 'sleepstudy'. The following plot shows 
#    subject-specific intercepts and slopes. Adapt this plot for our study 
#    and draw conclusions.

model = lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["Subject"]])


# I am using itemOrder as a predictor as i assume i can use any one of the predictors

model = lmer(WORD_TIME ~ itemOrder + (itemOrder|PARTICIPANT), dat)
print(dotplot(ranef(model,condVar=TRUE),  scales = list(x = list(relation = 'free')))
      [["PARTICIPANT"]])

# Different participants have different base reaction times(before accounting for predictors) and different PARTICIPANTS also differ a little
# in the slope of itemOrder they hence it would not be the worst idea to use itemOrder as a slope for PARTICIPANT, but we need to further 
# argue  whether the number of increased estimators due to this slope of itemOrder can be justified with this little difference in 
# the value of itemOrder for different participants(it would be justified had this difference between participants been huge)

