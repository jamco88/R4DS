#Chapter 14: relational data
options(repos="http://cran.rstudio.com/")
library(tidyverse)
library(nycflights13)
install.packages("nycflights13")

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



