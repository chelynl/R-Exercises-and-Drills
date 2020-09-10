## Visualization Exercise

# Import libraries

#1 Prepare by:
#1.1 Import the required libraries:

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

#1.2 Set your working directory and upload the "Auto_MPG.csv" data. Ensure stringAsFactors=FALSE.
#1.3 Create a column names "Car_Make" from the first element of the "Car_Name" variable
#1.4 Update "horsepower" to be type numeric
#1.4 How many unique values of "Car_Make" are there? 37
df_auto <- fread('Auto_MPG.csv', stringsAsFactors = F) # data.table method
df_auto$Horsepower <- as.numeric(df_auto$Horsepower)

df_auto <- df_auto %>%
  rowwise() %>%
  mutate(Car_Make = str_split(Car_Name, " ")[[1]][1])

length(unique(df_auto$Car_Make))

#We are going to create a function that provides am univariate analysis on a provided variable vector

#2 Change the type of "Cylinders", "Model_Year", and "Origin" from numeric to character.
# How many variables in "df_auto" are of type character? 5
df_auto$Cylinders <- as.character(df_auto$Cylinders)
df_auto$Model_Year <- as.character(df_auto$Model_Year)
df_auto$Orgin <- as.character(df_auto$Orgin)

str(df_auto)

#3 Create a function "univariate_analysis" that:
#3.1 Calculates the number of observations, mean, standard deviation, and quartiles of a provided vector. 
#3.1 Ensure the na.rm argument is TRUE
#3.2 Have the function return the results in a list with named elements "obs", "mean", "stdev", and "quartiles"
#Using the function created, what is the 2nd quartile of "MPG"? 23.0
univariate_analysis <- function(x){
  obs <- length(x)
  mean <- mean(x, na.rm = T)
  stdev <- sd(x, na.rm = T)
  quartiles <- quantile(x, na.rm = T)
  results <- list(obs=obs, mean=mean, stdev=stdev, quartiles=quartiles)
  return(results)
}

univariate_analysis(df_auto$MPG)

#4 To the "univariate_analysis" function, create a histogram plot named "histogram". Add the plot to the return list with the element named "histogram". 
#Note, you will be providing your function a vector, but ggplot only accepts data frames. You will need to convert your vector to a data frame with one column.
#What is the default number of bins in the histogram for "MPG"? 30
univariate_analysis <- function(x){
  obs <- length(x)
  mean <- mean(x, na.rm = T)
  stdev <- sd(x, na.rm = T)
  quartiles <- quantile(x, na.rm = T)
  histogram <- ggplot(as.data.frame(x)) + aes(x) + geom_histogram()
  results <- list(obs=obs, mean=mean, stdev=stdev, quartiles=quartiles, histogram=histogram)
  return(results)
}

univariate_analysis(df_auto$MPG)

#5 Update the histogram in the "univariate_analysis" to:
# 5.1 Change the number of bins to 20
# 5.2 Add vertical lines at the quartiles of the data. 
# 5.3 Change the colors of the quartile vertical lines so they are red, blue, and green respectively
# 5.4 Using geom_text, add a label next to each vertical line that corresponds to the value of of the quartile:
# 5.4.1 Set the vertical starting position to 5
# 5.4.2 Offset the label's x position by 1.2
# 5.4.3 Change the text color to match the line color 
# 5.4.4 Change the angle to 90 
# 5.4.5 Change the text size to 12
# 5.5 Change the theme to minimal
#What x-axis tick label does the green line fall nearest for "MPG"? 30
univariate_analysis <- function(x){
  obs <- length(x)
  mean <- mean(x, na.rm = T)
  stdev <- sd(x, na.rm = T)
  quartiles <- quantile(x, na.rm = T)
  histogram <- ggplot(as.data.frame(x)) + aes(x) + 
    geom_histogram(bins = 20) + 
    geom_vline(xintercept =  quartiles[2:4], color = c('red', 'blue', 'green')) +
    geom_text(aes(x=quartiles[2], y=5, label=quartiles[2]), angle = 90, size = 12, nudge_x = 1.2) +
    geom_text(aes(x=quartiles[3], y=5, label=quartiles[3]), angle = 90, size = 12, nudge_x = 1.2) +
    geom_text(aes(x=quartiles[4], y=5, label=quartiles[4]), angle = 90, size = 12, nudge_x = 1.2) +
    theme_minimal()
  
  results <- list(obs=obs, mean=mean, stdev=stdev, quartiles=quartiles, histogram=histogram)
  return(results)
}

univariate_analysis(df_auto$MPG)


# 6 Thus far, we've assumed the variable provided will be numeric. However, we should account for categorical variables. 
# Update the "univariate_analysis" function to:
#6.1 If the provided vector is numeric, execute everything created in the function thus far
#6.2 If the provided vector is character:
#6.2.1 Store the number of elements in the vector in a variable named "obs"
#6.2.2 Store the unique values of the vector in a variable named "unique_levels" 
#6.2.1 Store a bar chart in a variable "bar_chart" that plots the number of observations by the unique levels of the variable 
#6.3 Returns the previous variables in a list with the elements named "obs", "unique_levels", and "bar_chart")
#Using the function created, what "Model_Year" has the greatest number of observations?
univariate_analysis <- function(x){
  if (is.numeric(x)) {
    obs <- length(x)
    mean <- mean(x, na.rm = T)
    stdev <- sd(x, na.rm = T)
    quartiles <- quantile(x, na.rm = T)
    histogram <- ggplot(as.data.frame(x)) + aes(x) + 
      geom_histogram(bins = 20) + 
      geom_vline(xintercept =  quartiles[2:4], color = c('red', 'blue', 'green')) +
      geom_text(aes(x=quartiles[2], y=5, label=quartiles[2]), angle = 90, size = 12, nudge_x = 1.2) +
      geom_text(aes(x=quartiles[3], y=5, label=quartiles[3]), angle = 90, size = 12, nudge_x = 1.2) +
      geom_text(aes(x=quartiles[4], y=5, label=quartiles[4]), angle = 90, size = 12, nudge_x = 1.2) +
      theme_minimal()
    
    results <- list(obs=obs, mean=mean, stdev=stdev, quartiles=quartiles, histogram=histogram)
    return(results)
  }
  else if (is.character(x)) {
    obs <- length(x)
    unique_levels <- unique(x)
    bar_chart <- ggplot(as.data.frame(x)) + aes(x) + geom_bar()
    results <- list(obs=obs, unique_levels=unique_levels, bar_chart=bar_chart)
    return(results)
  }
}

univariate_analysis(df_auto$Model_Year)


#7 Using the "univariate_analysis" function, what is the standard deviation of "Horsepower"? 38.49116
univariate_analysis(df_auto$Horsepower)

#8 Using the "univariate_analysis" function, what is the median of "Displacement"? 148.50
univariate_analysis(df_auto$Displacement)

#9 Using the "univariate_analysis" function, what is the most common number of cylinders for cars in the data set? 4
univariate_analysis(df_auto$Cylinders)

#10 Using the "univariate_analysis" function, what is the mean of Weight? 2970.425
univariate_analysis(df_auto$Weight)
