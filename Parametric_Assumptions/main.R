
# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1 = high school education, 0 = no high school degree.

library(ggplot2)
data = read.table('kidiq.txt')
summary(data)
data$mom_hs = as.factor(data$mom_hs)
data$mom_work = as.factor(data$mom_work)

# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.


ggplot(data = data, aes(x = mom_iq, y = kid_score)) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(title = 'Kid\'s score vs mum\'s iq ' ) +
  xlab('Mother\'s IQ') +
  ylab('Kid\'s Score')

# c) State the main difference between correlation and regression .Calculate a simple regression model 
#for kid_score with mom_hs as a predictor and interpret the results.

# Correlation tells us if there exists a linear relation between two variables(cor = 1 means a perfect positive relation and vice versa)
# if this correlation is between a predictor and respose variable, it tells us whether a single predictor has a linear relation with its response
# variable. It does not explain exactly how much the predictor changes the response. Regression on the other hand tells us how much exactly
# does the predictor effect the response (it isnt restricted between -1 and 1 like cor is ), also regression also tells us this information if
# the relation is non linear (if we use polynomial regression).

mod1 = lm(kid_score ~ mom_hs, data = data)
mod1
# Kid's whose mother attended high school had on average 11.77 iq higher than kids whose mothers did not attend high school
# Kid's IQ before taking into considering their mom's education history is 77.55, if their mom did attend high school this iq increases by 11.77

# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model 
#    Then compare this regression model to the previous model and state which has a better model fit.

mod2 = lm(kid_score ~ mom_hs + mom_iq, data = data)
mod2
# The one with two predictors is better because it depends mostly on the variables (intercept is much smaller than in 1 predictor model). 
# furthermore because the intercept is much smaller this also means that the second variable is also very relevant in explaining the response.


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without degree in another color. Then also fit two separate regression lines 
#    such that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, 
#    kid_score_pred=fitted(your_model))

ggplot(data = data, aes(x = mom_iq, y = kid_score, color = mom_hs)) +
  geom_point() + 
  geom_line(aes(y = predict(mod2))) +
  labs(title = "Kids iq fitted against Mom's iq and Education Level") +
  xlab("'Mom's iq") +
  ylab("Kid's iq")

# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Fit the model and interpret your results.

mod3 = lm(kid_score ~ mom_hs * mom_iq, data = data)
# This model tries to explain kid's IQ with the assumption that the mother's IQ and education level are related to each other ( more flexible slope).
# And thus the model puts alot more emphasis on whether the mother has highschool degree (10x more than previous model), but this is only because
# it has given the interaction a negative relation (this is purely because no highschool interaction = 0).

# g) Next, let's plot the results of this model.

ggplot(data = data, aes(x = mom_iq, y = kid_score, color = mom_hs)) +
  geom_point() + 
  geom_line(aes(y = predict(mod3))) +
  labs(title = "Kids iq fitted against Mom's iq and Education Level") +
  xlab("'Mom's iq") +
  ylab("Kid's iq")


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the corresponding
#    child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

z = data.frame("mom_hs" = as.factor(1), "mom_iq" = 100)
predict(mod3, newdata = z, interval = "confidence", level = 0.95)

# i) Meaning of confidence intervals for regression line.
#    Let's go back to exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of this confidence interval?

ggplot(data = data, aes(x = mom_iq, y = kid_score)) +
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(title = 'Kid\'s score vs mum\'s iq ' ) +
  xlab('Mother\'s IQ') +
  ylab('Kid\'s Score')

##The confidence interval tells us  the range in which the true value lies (with a certain probability).
##If we repeat the experiment we might get different values for our statistic, but with a certain probability those
##new results will fall within the confidence interval

# j) Finally, do model checking on your model from f), i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

plot(mod3)

# The first plot checks the homogeneity of residuals. It looks like the residuals have  more or less homogeneous variance.

# The second plot checks the fitted vs theoretical distribution(normal in this case). Our residuals line up nicely and are thus normally distributed.

# The third plot is like the first plot the  difference being the residuals are standardized.

# The fourth plot looks for outliers and tells us if we have influential points that are changing our model. In this case the points 
# on the far right(with high leverage) are points of high influence as they are far away from other points and are also influencing our model more
# than other points.