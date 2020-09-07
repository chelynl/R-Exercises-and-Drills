###------------------------------------### 
#
# Programming Fundamentals Exercise:
#
###------------------------------------###   

#1 Set the seed to 1.
set.seed(1)

#2 Create a variable "pop_truth" that is 100,000 samples from an exponential distribution with rate 2. 
#What function is used to draw a random sample from an exponential distribution?
pop_truth <- rexp(100000,rate=2)

#3 What is the mean of "pop_truth"? You can use the function hist() to plot a histogram of the population data.
mean(pop_truth) # 0.5013488
hist(pop_truth)

#4 Create a variable "sample_pop" that is a random sample of 5,000 values from "pop_truth". 
#Set the seed to 1 again, what is the mean of "sample_pop"?
#Note, the seed needs to be set each time you call a function with a random component
set.seed(1)
sample_pop <- sample(pop_truth, 5000)
mean(sample_pop) # 0.5065573

#5 Create a function named "sample_stats" that calculates and returns the mean and standard deviation from a provided vector. 
sample_stats <- function(v) {
  mean <- mean(v)
  std <- sd(v)
  stats <- list(mean=mean, std=std)
  return(stats)
}

#6 Use the function to determine the mean and standard deviation from "sample pop". What is the returned standard deviation?
sample_stats(sample_pop) # 0.5146478

#7 Create a for loop from 1 to 10000 (note, use a smaller value when developing) that does the following:
# 7.1 - If the first iteration, create a new data frame named "df_results" with three columns - "mean", "stdev", and "iteration"
# 7.2 - Draws a new 500 observation sample from "pop_truth" to "sample_pop"
# 7.3 - Uses the "sample_stats" function created previously to calculate and return the mean and standard deviation of "sample_pop" to a new variable named "metrics"
# 7.4 - Adds a third element to "metrics" that is the current value of the iterator
# 7.5 - Appends "metrics" to the data frame "df_results"
# 7.6 - Prints the message, "Currently on iteration" and the current value of the iterator
for (i in 1:10000){
  if (i==1){
    df_results <- data.frame(mean=0, stdev=0, iteration=0)
    colnames(df_results) <- c('mean', 'stdev', 'iteration')
  }
  sample_pop <- sample(pop_truth, 500, replace=TRUE)
  metrics <- c(mean = sample_stats(sample_pop)$mean, stdev = sample_stats(sample_pop)$std)
  metrics$iteration <- i
  df_results[i,] <- metrics
  df_results <- rbind(df_results, c(metrics$mean, metrics$std, metrics$iteration))
  print(paste("Currently on iteration ", i))
}



#8 What is the average of "df_results$mean"?
mean(df_results$mean) #0.5014333

#9 Compute the 5th and 95th percentiles of "df_results$mean". What is the 5th percentile?
quantile(df_results$mean, c(.05, .95)) 

#10 Use the hist() function to create a histogram of "df_results$mean" and "pop_truth". 
#What type of distribution does df_results$mean appear to be? Normal
hist(df_results$mean)
hist(pop_truth)

#11 What have we demonstrated with this simulation?
CLT