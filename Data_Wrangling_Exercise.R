###------------------------------------### 
#
# Data Wrangling Exercise:
#
###------------------------------------### 

# Import libraries
library(data.table)
library(dplyr)
library(stringr)


#1 Load the Auto_MPG.csv into a data frame named "df_auto". How many columns are in "df_auto"?
df_auto <- fread('Auto_MPG.csv', stringsAsFactors = F)
str(df_auto) # 9 columns

#2 Convert "Horsepower" to numeric. How many NAs are in "df_auto" after converting "Horsepower"?
df_auto$Horsepower <-  as.numeric(df_auto$Horsepower)
sum(is.na(df_auto$Horsepower)) # 6 na values

#3 Using dplyr, add to "df_auto":
# 3.1 Variable named "Car_Make" that is the first word from "Car_Name"
# 3.2 Variable named "Car_Model" that is the remaining words (not first) from "Car_Name" 
# How many observations of "Car_Make" "ford" are there?
df_auto <- df_auto %>%
  rowwise() %>% # allows you to compute operations for each row
  mutate(Car_Make = str_split(Car_Name, " ")[[1]][1]) %>%
  mutate(Car_Model = paste(str_split(Car_Name, " ")[[1]][-1], collapse = " "))

df_auto %>%
  filter(Car_Make=="ford") #51 obs


#4 Using dplyr, how many cars are there for model year 73?
df_auto %>%
  filter(Model_Year==73) #40

#5 Using dplyr, what "Car_Make" "ford" car ("Car_Name") has the highest "Weight_per_Horsepower" (Weight/Horsepower) ratio? Ignore NAs. 
df_auto %>%
  filter(Car_Make=="ford") %>%
  select(Car_Name, Weight, Horsepower) %>%
  mutate(Weight_per_Horsepower = Weight/Horsepower, na.rm=T) %>%
  arrange(desc(Weight_per_Horsepower))
# ford granada ghia

#6 Using dplyr, create a ranking variable, "MPG_Rank", that ranks, using the row_number() function, "MPG" by "Model_Year" in descending order. 
# What is the third highest "MPG" "Car_Name" for "Model_Year"? toyota corola

test <- df_auto %>%
  arrange(desc(Model_Year)) %>%
  arrange(desc(MPG)) %>%
  mutate(MPG_Rank = row_number()) %>%
  filter(Model_Year==72)

#7 Using dplyr and stringr, update "df_auto" so all values of "Car_Make" that contain "chev" say "chevrolet".
# How many observations have "Car_Make" "chevrolet"? 47
df_auto <- df_auto %>%
  rowwise() %>%
  mutate(Car_Make = if_else(str_detect(Car_Make, "chev"), "chevrolet", Car_Make))

df_auto %>%
  filter(Car_Make=='chevrolet')


#8 What is the average MPG for "chevrolet" cars? 20.2
df_auto %>%
  group_by(Car_Make) %>%
  summarise(Average_MPG = mean(MPG))

#Below is a data frame, "df_auto_start", that contains the first production year for some of the auto makes.

Car_Make <- c("amc", "audi", "bmw", "buick", "chevrolet", "datsun", "dodge", "ford")
First_Year <- c(1954, 1910, 1916, 1903, 1911, 1931, 1900, 1903)
df_auto_start <- data.frame(Car_Make=Car_Make, First_Year=First_Year, stringsAsFactors = FALSE)

# Use the data to answer the following questions:

#9 Using dplyr joins, what is the average "MPG" for car makes where "First_Year" exists? 21.55528
merge_df <- merge(df_auto_start, df_auto, by = "Car_Make", all = T)
merge_df <- merge_df %>%
  filter(!is.na(First_Year))
mean(merge_df$MPG)

df_auto_inner <- df_auto_start %>%
  inner_join(df_auto, by='Car_Make')

#10 Using dplyr joins, how many cars exist in the data set where "First_Year" is null? 195
df_auto_left <- df_auto %>%
  left_join(df_auto_start, by="Car_Make")

sum(is.na(df_auto_left$First_Year))
