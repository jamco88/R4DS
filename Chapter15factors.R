#creating factors - a factor is a type categorical variable that can only take a 1. SPECIFIC  set of 2. ORDERED values
#you use them when you don't want to be exposed to typos and want to coerce a specific order. rather than a string
library(readr)
library(forcats)
library(tidyverse)
#ie the following shows how when you use strings you can 1. make typos and 2. not sort very cleverly.
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)
#make it a factor!
y1 <- factor(x1, levels = month_levels)
#behold - now you get the months in the correct order (together with the levels!)
sort(y1)

#any values not in the set will be silently converted to NA:
y2 <-  factor(x2, levels = month_levels)
y2
#if you want a warning you can use parse_factor: (from readr)
y2 <- parse_factor(x2, levels = month_levels)

#General Social Survey results
gss_cat

#you can use the count() function to see factors <-  KEY POINT
gss_cat %>%
  count(race)

#or bar chart
ggplot(gss_cat, aes(race)) +
  geom_bar()

#can force display of levels that don't have any values with 
ggplot(gss_cat, aes(race)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

#the two most important operations with factors are changing the orders of the levels and changing the values of the levels
#These operations are described after the quiz.

#15.3.1 - 1 explore dist of rincome. What makes the default bar cart hard to understand and how to improve?
ggplot(gss_cat, aes(rincome)) +
  geom_bar()
#the massive overlap of level variable names makes it hard to read the x axis.improve it by altering the names of the levels
#so that they are smaller and/or change the orientation if possible

#15.3.1 - 2 What is the most common relig in this survey? Whats the most common partyid?
ggplot(gss_cat, aes(relig)) +
  geom_bar()
ggplot(gss_cat, aes(partyid)) +
  geom_bar()
#protestant, independent

#15.3.1 -3 Which relig does denom apply to? how can you find out with a table? How can you find out with a visualisation?

#with a table you could just look at it and for each observation with a specific religion, check if there is a specific
#denomination value. In this case a glance at the table reveals that only Protestant has non-NA Levels for denom.

#With a visualisation you could count over both religion AND denomination variables
#this wouldn't work with a bar graph naively. however you could use a SCATTER PLOT
#which uses size of dot as proxy for count of observations
gss_cat %>%
  count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))

#15.4 Modifying Factor Order
#it can be useful to change the order of the factor levels in a visualisation. IE imagine you wanted to explore
#the average number of hours spent watching tv per day across regions:
relig_summary <- gss_cat %>%
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

#this plots difficult to interpret because theres no overall pattern - we improve it by reordering the levels of relig
#using fct_reorder() <-  KEY POINT
#this takes 3 arguments - f - fator to modify, x- numeric vector you want to use, FUN - a function used if multiple
#values of x for each value of f (default is median)

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()
#so here relig acts as the factor whose levels I want to modify and tvhours is the numeric vector to be used to reorder.
#this output is much easier to read

#RECCOMMENDATION - MOVE TRANSFORMATIONS OUT OF AES() and into a separate MUTATE() step
#ie
relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
    geom_point()
#similar plot looking at how average age varies across reported income level
rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) + geom_point()
#in this case using fct_reorder is the wrong approach because rincome already has a meaningful order.
#however it does make sense to pull NA to the front using fct_relevel,which takes factor f then any number of levels
#that you want ot move to the front (ie bottom) of the graph

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

#when colouring lines on a plot fct_reorder2() reorders the factor by the y values associated with the largest x values
#This makes the plot easier to read because the line colurs line up ith the legend.

#translation - the order of the variables in the legend will correspond to their ranking AT THE RIGHTMOST POINT IN THE
#GRAPH

#proportion married by age
by_age <- gss_cat %>%
  filter(!is.na(age)) %>% 
  count(age, marital) %>%
  group_by(age) %>% 
  mutate(prop = n / sum(n))
#DEFAULT CONFUSING OUTPUT
ggplot(by_age, aes(age, prop, colour = marital)) +
  geom_line(na.rm = TRUE)
#COOL AND GOOD CLEAR OUTPUT WITH NEATLY ALIGNED VARIABLES
ggplot(by_age, aes(age, prop, colour = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(colour = "marital")

#finally you can use fct_infreq() to order levels by increasing frequency and can be combined with fct_rev()
#(fct_rev reverses the order of a factoral variable)
gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
    geom_bar()

#There are some high numbers in TV hours - IS THE MEAN A GOOD SUMMARY?
#No, we should use median as our measure of central tendency instead - it's less sensitive to outliers.

#FOR EACH FACTOR IN GSS_CATIDENTIFY WHETHER THE ORDER OF THE LEVELS IS ARBITRARY OR PRINCIPLED
#get a reminder of the names - remember you can use count() to see factors but only if you go col by col.
gss_cat %>%
  count(year)
#year is principled
gss_cat %>% 
  count(marital)
#marital is arbitrary
gss_cat %>%
  count(race)
#race order is arbitrary too - and so on for each variable

#WHY DID MOVING NA to the front of the levels move it to the bottom of the plot?
#presumably becausTe the function that constructs the plot takes an input vector or list - NA has been moved to the front
#or top of that list and therefore is the first value encountered by the function, and gets put at the first position
#in the graph

#15.5 MODIFYING FACTOR LEVELS

#recode letyou chnge the value of each level. you can also collapse and restructure these. 1200 WED 8th

