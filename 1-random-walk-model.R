# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# drift is the drift rate (default value is 0)
# sd is the variability in the drift rate (default value is 0.3)
# criterion is the threshold for a response (default value is 3)

random.walk.model <- function(samples, drift=0, sd=0.3, criterion=3){
  
  # create placeholder arrays for the data
  accuracy.array <- rep(F, samples)
  rt.array <- rep(0, samples)
  
  # for every sample that we want to draw
  for(i in 1:samples){
    # create the accumulator at the starting point 
    accumulator <- 0
    steps <- 0
    while(abs(accumulator) < criterion){
      # sample some evidence with mean = drift rate, and standard deviation = sd
      evidence <- rnorm(1, mean=drift, sd=sd)
      accumulator <- accumulator + evidence
      steps <- steps + 1
    }
    
    # we've hit the boundary!
    rt.array[i] <- steps
    accuracy.array[i] <- accumulator > 0
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

initial.test <- random.walk.model(1000, drift=0.03)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

library(ggplot2)

ggplot(initial.test, aes(x=rt))+
  geom_histogram()+
  facet_grid(correct~.)+
  theme_bw()+
  theme(panel.grid = element_blank())
