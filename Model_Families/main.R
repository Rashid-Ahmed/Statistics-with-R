
library(ggplot2)
library(lme4)

####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person iâ€™s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.

data = read.csv('Speed Dating Data.csv')
#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.

mod_binomial = glm(dec ~ attr + sinc + intel + fun, data = data, family = binomial())
summary(mod_binomial)

# Atrractiveness and fun are both  important while the other person's intelligence and sincerity are not imporant.


ggplot(data = data) + 
  geom_bar(aes(x  = attr, fill = factor(dec)), position = 'dodge') 

# attractiveness clearly has effect on match as the propertions of match/no-match are much higher as we go higher on the attractive scale.

ggplot(data = data) + 
  geom_bar(aes(x  = sinc, fill = factor(dec)), position = 'dodge') 

# There is some difference in propertion of match/no-match here as we go high on sincerity

ggplot(data = data) + 
  geom_bar(aes(x  = intel, fill = factor(dec)), position = 'dodge') 

#similiar  to sincerity

ggplot(data = data) + 
  geom_bar(aes(x  = fun, fill = factor(dec)), position = 'dodge') 

# high corelation between fun and dec, the proportion shoots up as we go high on fun. 




#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model. 


mod1_binomial = glmer(dec ~ attr + sinc + intel + fun + (1|iid), data = data, family = binomial())

summary(mod1_binomial)
sum(as.integer(as.integer(predict(mod_binomial, type = 'response') > 0.5) == data$dec))/nrow(data)
sum(as.integer(as.integer(predict(mod1_binomial, type = 'response') > 0.5) == data$dec))/nrow(data)

# Now we are accounting for the individual biases of the person making the evaluation. It helps a little in making accurate predictions but very
# little and the warning and this bad prediction is because predict removes some narows while im using all rows in the actual output.

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.Interpret the model outcome and explain what the varying Intercepts are.

mod2_binomial = glmer(dec ~ attr + sinc + intel + fun + (1|iid) + (1|pid), data = data, family = binomial())
summary(mod2_binomial)

# The varying intercepts (iid AND pid) are a intercept value for the person evaluating and a intercept for a person being evaluated.
# But this model does not converge

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

new_data = data[c('iid','pid', 'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'dec')]
new_data =na.omit(new_data)

mod3_binomial = glmer(dec ~attr + sinc + intel + fun + shar + amb + (1|iid), data = new_data, family = binomial())
#model fails to converge if i even use a single predictor slope  for the evaluator

#only shar, fun and attr are significant so my next model will only include these three and i have removed data the predictor attributes with na's

mod4_binomial = glmer(dec ~ attr + fun + shar  + (1|iid), data = new_data, family = binomial())
sum(as.integer(as.integer(predict(mod4_binomial, type = 'response') > 0.5) == new_data$dec))/nrow(new_data)

# this is a simple enough model and yet provides me with 87.5% accuracy (after excluding na's from data)


#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?


mod5_binomial = glm(dec ~ attr + fun + shar, data = new_data, family = binomial())
sum(as.integer(as.integer(predict(mod4_binomial, type = 'response') > 0.5) == new_data$dec))/nrow(new_data)

#no if i use these 3 attributes then it doesnt matter if i use random intercept/slopes for evaluator or evaluatee (as the accuracy stays the same)
# these 3 attributes explain enough variance

####
#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  ï..id <- factor(ï..id)
  
})
colnames(p)[1] = 'id'
summary(p)
head(p)


#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(data = p, aes(x = math, y = num_awards, color = prog)) +
  geom_point() + 
  geom_smooth(method = 'glm', method.args =  list(family = 'poisson'))

# the more score in math the more awards the students are likely to have, and people in academic program tend to have the highest number of awards

#(7) what model family is used and explain the reason for using it.
#Run a generalized linear model to test for significance of effects.

# I have used the poisson family as our outcome is a count(discrete) variable with a minimum value of 0, we cannot use binomial as we have multiple
# unique counts and we cannot assume gaussian as it is discrete.
mod = glm(num_awards ~ math + prog, data = p, family = poisson(link = 'log'))
summary(mod)

#(8) Do model comparisons to find out whether the predictors significantly improve model fit.

coef(summary(mod))

#math is definitely significant (it helps alot in improving model fit), vocational program on a whole isnt (only 1 category helps in our predictions)
# because the only program category that helps us is academic, and that too a little, and the other two categories are not helpful at all.

# also the mse does get a little better when using both the predictors, instead of just math but the difference is very little

mod = glm(num_awards ~ math, data = p, family = poisson(link = 'log'))
mean((as.integer(predict(mod, type = 'response') > 0.5) - p$num_awards)^2)

mod = glm(num_awards ~ math + prog, data = p, family = poisson(link = 'log'))
mean((as.integer(predict(mod, type = 'response') > 0.5) - p$num_awards)^2)


#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.

mod_lm = lm(num_awards ~ math + prog, data = p)
summary(mod_lm)

mse_lm = mean((as.integer(predict(mod_lm) > 0.5) - p$num_awards)^2)
mse_glm = mean((as.integer(predict(mod, type = 'response') > 0.5) - p$num_awards)^2)
mse_glm
mse_lm
# Even though mse is not the best loss function, we can clearly see the poisson distribution has a lower mse then the model with gaussian 
# distribution 


##Task 3

## Please explain within and between subject experimental design.
##How does the design affect the random effect structure during analysis.

#In between subject experimental design either each subject is repeated once or you only have 1 subject thus the difference in response variable
# is only measured due to predictor variables and the algorithm, and the subjects are unrelated with each other and we dont take them into 
# consideration

#In within subject experimental design sbujects are repeated so a single subject has multiple observations (usually 1 for each item) , in this case
# we have to take the subject into consideration as well(depending on the algorithm we might average over the response of the  same subject or
# consider subject as a random effect variable etc).

# If we have a between subject experimental design, all the variables are fixed. If we have a within subject experimental design only then do we 
# consider random effects and our subjects in that case are treated as random effect variables (because these effects are basically biases
# and biases are random(normal) we consider subject as a random effect). Thus in within subject we have a intercept term for each subject
# we also could have the  predictor variable's slope for each subject (in case of multiple predictors we could have multiple slope for each subject). 

