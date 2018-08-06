#Chapter 14: relational data
options(repos="http://cran.rstudio.com/")
library(tidyverse)
library(nycflights13)
library(maps)

#13.4 mutating joins
flights2 <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2

#imagine we want to add the full airline name to the flights2 data
#this will be done by COMBINING the airlines and flights2 data frames with left_join()
#modify the existing tbl
flights2 %>%
  #drop origin and destination columns
  select(-origin, -dest) %>%
  #join to the airlines table, using the CARRIER column
  left_join(airlines, by = "carrier")

#the result shows the new NAME column from airlines has now been added to the flights2 table

#13.4.1 UNDERSTANDING JOINS
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)
#key columns are used to match the rows between the tables; value columns are "carried along" for the ride. <_ REMEMBER THIS ITS REALLY USEFUL

#13.4.2 Inner Join
#simplest type of join is the inner join. This matches pairs of observations whenever their keys are equal
#pipe operator shortcut is ctrl shift m

x %>%
  inner_join(y, by = "key")

#the most important property of an inner join is that unmatched rows are not included in the result. this means that inner joins usually not appropriate
#as its easy to lose observations.

#13.4.3 OUTER JOINES
#outer joins keep observations that appear in at least one of the tables.There are 3 types of outer join:
#LEFT JOIN RIGHT JOIN AND FULL JOIN - left joins will use the keys in the left table, forcing a NA if the right table doesnt have that key
#Left join should be your default join-use it unless you have a strong reason to prefer the others


#13.4.4 DUPLICATE KEYS
#there are two possibilities - the first being that one table has duplicate keys - this is cool and good 

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")

#the second is that both tables have duplicate keys - this is usually an error because in neither table do the keys uniquely identify observations

#13.4.5 Defining the key columns
#the default, by = "Null" uses all variables that appear in both tables, the so called NATURAL JOIN

#exercise1 -compute the average delay by destination, then join on the airports dataframe so you can show the spatial
#distribution of delays

avg_dest_delays <-
  flights %>% 
  #group by destination column in flights tibble because the question states by dest
  group_by(dest) %>%
  #look up the summarise function but basically it gets the mean value - make a new col called delay
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))
  
  
#REVISION - GO BACK OVER THE GROUPBY AND SUMMARISE FUNCTIONS IN R - see chapter 5.6

#group_by changes the unit of analysis from the complete dataset to individual groups.
#Then when you use the dplyr verbs on grouped dataframe they are automatically applied by group
#

#note ctrl shift f10 plus ctrl shift s

avg_dest_delays %>% 
  ggplot(aes(lon, lat, colour = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#13.4.6.ex2 - add the location of the origin and destination (ie the lat and lon) to flights
#ok got to join airports

fligts2 %>% 
  left_join(airports, )

#13.4.6.ex3 -is there a relationship between the age of a plane and its delays?

#1creates new df just showing the age of each plane.mutate creates a new column and select grabs only that and the id
plane_ages <-
  planes %>% 
  mutate(age = 2013 - year) %>%
  select(tailnum, age)

#2 note how now join the ages into the main data and groupby age and summarise over departure delay
flights %>% 
  inner_join(plane_ages, by = "tailnum") %>% 
  group_by(age) %>% 
  filter(!is.na(dep_delay)) %>% 
  summarise(delay = mean(dep_delay)) %>% 
  ggplot(aes(x = age, y = delay)) + 
  geom_point() +
  geom_line()

#13.4.6.ex4 What weather conditions make it more likely to see a delay?

#step1 break question down into required columns - here we are looking at precipitation and delay

#create new df with all required variables
flight_weather <- 
  flights %>% 
  inner_join(weather)

flight_weather %>% 
  group_by(precip) %>% 
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + geom_point()



#13.5 Filtering Joins 

#Filtering joins match observations in the same way as mutating joins, but affect the observations, not the variables

#semi join(x, y) and anti_join(x, y) keep and drop all observations in x and y, respectively

#semijoins are useful for matching filtered summary tables back to the original rows, ie say you've found the top ten
#most popular destinations

top_dest <- flights %>% 
  count(dest, sort = TRUE) %>% 
  head(10)

flights %>% 
  semi_join(top_dest)

#anti_joins are useful for diagnosing join mismatches - for example, when connecting flights and planes you
#can see that there are many flights that don't have a match in planes.
flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(tailnum, sort = TRUE)

#13.5.1 Exercise 1 - what does it mean for a flight to have a missing tailnum? 
flights %>% 
  anti_join(planes, by = "tailnum") %>% 
  count(carrier, sort = TRUE)

#13.5.2 2 filter flights to only show flights with planes that have flown at least 100 flights
#first get panes that have flown at least 100 flights
planes_100 <- 
  filter(flights) %>% 
  group_by(tailnum) %>% 
  count() %>% 
  filter(n > 100)

#then join the rest of the data - here we use a semijoin because we want to match based on observations not Keys
#another example of using semiJOIN as a filter instead of filter.
flights %>%
  semi_join(planes_100, by = "tailnum")
  
#Combine fueleconomy::vehicles and fueleconomy::common to find only the records for the most common models

#here we are simply doing a semi join on vehicles and common

glimpse(fueleconomy::vehicles)

#this does the semijoin - note that you have to specify which keys to use the observations in
fueleconomy::vehicles %>% 
  semi_join(fueleconomy::common, by = c("make", "model"))


#13.5.1.4  - Find the 48 hours over the course of the whole year that have the worst delays. Cross reference it with the 
#weather data

#first bit entails grabbing 48 hour periods and putting them together - i had no idea how to get the 48 hour period
#i looked up an answer that used the lag function

flights %>% 
  group_by(year, month, day) %>% 
  summarise(total_24 = sum(dep_delay, na.rm = TRUE) + sum(arr_delay, na.rm = TRUE)) %>% 
  mutate(total_48 = total_24 + lag(total_24)) %>%
  arrange(desc(total_48))
           
  
#13.5.1.5  -what does antijoin(flights, airports, by = c("dest" = "faa")) tell you?
# What does anti_join(airports, flights, by = c("faa", "dest")) tell you?

#1 says get me all the flight records for which there is no corresponding airport (in the FAA list of destinations)

#2 says get me all the airport records for which there were no corresponding flights (correct)


#13.5.1. - is there an implicit relationship between plane and airline??

#here the question is asking to test the hypothesis - is each plane flown by a single airline?


#come back to this exercise

#the rest is tips

#13.6 JOIN PROBLEMS
#How to make sure your joins go smoothly
#Start by identifying variables that form the primary key in each table. 
#2check that ne of the variables in the primary key are missing, if a value is missing then it cant identify an observation
#3. chec that you foreign keys match pks in another table - best way to do this is with an anti-join
#4. 




