# MODEL EVALUATION ####

library(dplyr)

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sd parameter of the random.walk.model or the criterion
# parameter of the accumulator model.

random.walk.result <- random.walk.model(5000, drift=0.012, criterion = 4.85)

sum(random.walk.result$correct) / length(random.walk.result$correct) 

random.walk.result %>% group_by(correct) %>% summarize(rt =mean(rt))

accumulator.result <- accumulator.model(1000, rate.1 = 0.012, rate.2 = 0.011)

sum(accumulator.result$correct) / length(accumulator.result$correct) 

accumulator.result %>% group_by(correct) %>% summarize(rt =mean(rt))


# Can both models do a reasonable job of accounting for the mean RT and accuracy? 
# Report the results of your efforts:

# Yes, both models can do a reasonable job of accounting for the mean RT and accuracy.

# It's likely that further optimization of the parameters could improve the fit even 
# more, but there's no evidence to suggest that one model is doing a substantially 
# better job than the other at fitting the summary statistics.

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. (You can copy the visualization code from the models.)
# Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.


layout(matrix(1:4, nrow=2, byrow=T))
hist((random.walk.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,100), main="RW Model, correct", xlab="RT")
hist((random.walk.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,100), main="RW Model, incorrect", xlab="RT")
hist((accumulator.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,10), main="ACC Model, correct", xlab="RT")
hist((accumulator.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,10), main="ACC Model, incorrect", xlab="RT")

# From these histograms, two substantial differences in the model predictions jump out at me
# 1. The RW model predicts a very skewed distribution of RTs. The ACC model predicts 
#    a relatively symmmetrical distribution.
# 2. The variability is WAY higher in the RW model.

# The histograms are so different that it would probably be fine to simply visually compare
# the histogram of real RT data to these histograms and select the model that better fits the
# data. 

# More formally, we could look at the variance or standard deviation of RTs predicted by the 
# model and compare this to the variance or standard deviation in the real data:
random.walk.result %>% group_by(correct) %>% summarize(sd.rt = sd(rt))
accumulator.result %>% group_by(correct) %>% summarize(sd.rt = sd(rt))

# The RW model predicts SDs around 200. The ACC model predicts SDs around 13. That's a big enough
# difference to make a qualitative judgment in model fit.
