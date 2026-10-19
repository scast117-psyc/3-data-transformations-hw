#PSYC 259 Homework 3 - Data Transformation
#For full credit, provide answers for at least 7/11

#List names of students collaborating with: 
#Dylan and Amberley
### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")

### Question 1 ---------- 

#Use a function to check the class of all of the variables in 'ds' 
#Convert "Year" to a numeric, saving it back to 'ds'
#Use a function to check that your conversion succeeded

#ANSWER
glimpse(ds)
is.numeric(ds$Year)
#False obviously 
ds$Year <- as.numeric(ds$Year)
glimpse(ds)
is.numeric(ds$Year)
#True now

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- ds %>% rename_with(tolower)
print(ds)
### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor and make sure year is numeric (from Question 1)

#ANSWER
?floor
ds <- ds %>% mutate(ds, year = floor(year/10))
print(ds)
ds <- ds %>% mutate(ds, year = floor(year*10))
print(ds)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top
#ANSWER
ds <- arrange(ds,(rank))
print(ds)

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
top10 <- ds %>%
  filter(rank < 11) %>% 
  select(artist, song)
print(top10)
#BOOM 

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# and save the results to a new tibble called "ds_sum"
# Hint, the function drop_na() might help. Read ?drop_na

#ANSWER

ds_sum <- ds %>% summarize (earliest_year = min(year, na.rm =T),
                most_recent_year = max(year, na.rm = T), 
                avg_year = mean(year, na.rm = T))
print(ds_sum)

### Question 7 ----------

# Use filter on the original dataset 'ds' to find all of the artists/song titles from 
# the min/max/mean years that you identified in 'ds_sum'
# Use one filter command only, and sort the responses by year
# Bonus: find a way to do it without hard coding the years (i.e. pull them from ds_sum)
# (this might take some searching to find functions that will help)

#ANSWER
ds_filtered <- ds %>% filter(year = 1870, 2020, 1976)

### Question 8 ---------- 

# There's an error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error in 'ds'
# then recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
ds <- ds %>% mutate(year = ifelse(year == "1870", "1979", year))
print(ds)
#Worked! 
class(ds$year)
#got an error and saw that "year" isn't numeric for some reason again
is.numeric(ds$year)
#False
ds$year <- as.numeric(ds$year)
glimpse(ds)
is.numeric(ds$year)
#True now
ds <- ds %>% mutate(ds, year = floor(year/10))
ds <- ds %>% mutate(ds, year = floor(year*10))
print(ds)
#had to remove "Year" column again...don't know how I made it reappear 
ds$Year <- NULL
print(ds)
#okay finally year is fixed for song that was messed up! 
#Responses 6
ds_sum <- ds %>% summarize (earliest_year = min(year, na.rm =T),
                            most_recent_year = max(year, na.rm = T), 
                            avg_year = mean(year, na.rm = T))
print(ds_sum)
#earliest is 1930, most recent = 2020, avg = 1976 
#Response #7 

ds_filtered <- ds %>% filter(year == 1930 | 2020 | 1976) %>% 
  arrange(ds_filtered,(year))
print(ds_filtered)
# | means OR not commas 
#Not sure if this is right...

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% filter(!is.na(year)) %>% 
  group_by(year) %>% 
  summarize(mean(rank, na.rm=T))

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together
# Check against your answers from Question 9 to make sure it matches

#ANSWER
?count
ds %>% count(song, year) %>% slice_max(year)
### Question 11 ------
# Use RStudio's styler to style all the code in your homework
install.packages("styler")
library(styler)
