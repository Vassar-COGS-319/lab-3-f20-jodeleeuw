# implement the model by filling in the function below

# see README for model description

# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 0.025)
# rate.1 is the evidence accumulation rate for the incorrect response (default value is 0.025)
# criterion is the threshold for a response (default value is 3)

accumulator.model <- function(samples, rate.1=0.025, rate.2=0.025, criterion=3){
  
  # create placeholder arrays for the data
  accuracy.array <- rep(F, samples)
  rt.array <- rep(0, samples)
  
  # fill in code here!
  for(i in 1:samples){
    accumulator.1 <- 0
    accumulator.2 <- 0
    steps <- 0
    while(accumulator.1 < criterion && accumulator.2 < criterion){
      accumulator.1 <- accumulator.1 + rexp(1, 1/rate.1)
      accumulator.2 <- accumulator.2 + rexp(1, 1/rate.2)
      steps <- steps + 1
    }
    accuracy.array[i] <- accumulator.1 > accumulator.2
    rt.array[i] <- steps
  }
    

  output <- data.frame(
    correct = accuracy.array,
    rt = rt.array
  )
  
  return(output)
}

# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

library(ggplot2)

ggplot(initial.test, aes(x=rt))+
  geom_histogram()+
  facet_grid(correct~.)+
  theme_bw()+
  theme(panel.grid = element_blank())
