#Exercises Chapter 1


#1.1 ggplot(data=mpg) , what does it look like?
ggplot(data=mpg)
# Empty plot

#1.2 how many rows and columns in mtcars?
sprintf( "rows: %d , columns: %d", nrow(mtcars) , ncol(mtcars) )
# 32 rows and 11 columns

#1.3 what does de drv variable describe?
?mpg
# the type of drive train, where f= front-wheel drivem r= rear wheel drive,
#4 = 4wd

#1.4 Make a scatter plot of hwy(highway miles per galon) vs 
#cyl(number of cylinders)
ggplot(data=mpg)+
  geom_point(mapping = aes(x = hwy, y = cyl)) +
  ggtitle("Highway miles per galon VS number of cylinders" )+
  xlab("Highway miles per galon)") + 
  ylab("Number of cylinders")

#1.5 What happens if you make a scatterplot of class (type of car) 
#versus drv(type of drive train)?
ggplot(data=mpg)+
  geom_point(mapping = aes(x = class, y = drv))
# It is not correlated in any way, therefore no usefull information. 

#Excercises chapter 1.1 Aesthetics

#1.6 Whats wrong with this code?
# ggplot(data=mpg)+
#   geom_point(mapping=aes (x=displ,y=hwy,color="blue"))

# 'color="blue"' should be outside 'aes()'

#1.7 Which variable in mpg are categorical? Which are continous
?mpg
numericalData = names(Filter(is.numeric,mpg))
categoricalData = names(mpg[,!sapply(mpg, is.numeric)])
sprintf("numerical data: %s",toString(numericalData));
sprintf("categorical data:%s",toString(categoricalData))
#"numerical data: displ, year, cyl, cty, hwy  "
#"categorical data:manufacturer, model, trans, drv, fl, class"

#1.8 Map a continuous variable to color, size and shape. 
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, size = cty ))
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, color = cty ))
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, shape = year ))
#some give errors (namely shape), size works not so bad since it is not 
#cumulative. And color works great!

#1.9 What happens if you map the same variable to multiple aesthetics?

ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, color = cty, size=cty ))

#It looks nice, it is easy to read and it combines nicely


#1.10 What does stroke aesthetic do?
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy),stroke = 2)
?geom_point
# it sets the points radius. 

#1.11 What happens if you map an aesthetic to something other than a 
#variable name 
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, color = displ < 5))
#it segmentates the mapping according to the Boolean formula

#Excercises Facets

#1.12 What happens if you facet on a continuous variable?
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_wrap(~ cty)
# It  creates too many plots

#1.13 What do the empty cells in a plot with facet_grid(drv~cyl) mean?
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv ~ cyl)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=drv,y=hwy, color=cyl))
#it means the is no 4 wheel drive that has 5 cilynders for example. You can see
#it in the the other plot using aesthetics.

#1.14 Explain the plots

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv ~ .)
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(. ~ cyl)
# the . in a "anything" variable substitute, it helps making plot in which you
#diferentiate in a specific axis. 

#1.15 advantages of faceting over coloring aesthetics
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(~ class)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy, color = class))
# faceted plots let you analyze more clearly individual classes, while the color
#aesthetic migh be really good for clustering purposes. When you have larger 
# datasets too many data will be hard to differentiate clearly on colors, 
#however too many facets might make it impossible to plot.

#1.16 What does ncol, and nrow do in facet wrap?
?facet_wrap
# they determine the number of columns and rows there are. Facet grid doesn't
# have this option since it is determined by the data itself.

#1.17 why do you put variable with more unique levels in the columns 
# in  the facet_grid method?

#Because images tend to be wider rather than taller, so you have more space 
#width-wise than in the othe direction

#1.18 what geom use to draw a line chart

#a geom_line or geom_smooth, depending on how smooth I want it. 

#1.19 Predict the output
#prediction: It will make 6 lines and the 6 colored dots. 
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy,color=drv)
)+
  geom_point()+
  geom_smooth(se=FALSE)

# it only made 3 :p i read the wrong data. 

#W1.20 What does show.legend=FALSE do?

ggplot(data=mpg,mapping=aes(x=displ,y=hwy,color=drv))+
  geom_point(show.legend=FALSE)

#It removes the legend like the dots color names. 

#1.21 What does se do in geom_smooth?
# it allows or not to show the confidence interval. 


#1.22 Plots are the same?
#yes

#1.23 Re create the plots:
ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
)+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
)+
  geom_point()+
  geom_smooth(data=filter(mpg, drv== 4),se=FALSE)+
  geom_smooth(data=filter(mpg, drv== "r"),se=FALSE)+
  geom_smooth(data=filter(mpg, drv== "f"),se=FALSE)

ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy, color = drv)
)+
  geom_point()+
  geom_smooth(se=FALSE)

ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
)+
  geom_point(mapping = aes(color = drv))+
  geom_smooth(se=FALSE)

ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
)+
  geom_point(mapping = aes(color=drv))+
  geom_smooth(mapping= aes(linetype = drv),se=FALSE)

ggplot(
  data=mpg,
  mapping=aes(x=displ,y=hwy)
)+
  geom_point(color="white", size=10)+
  geom_point(mapping= aes(color=drv),size=5)
  
#3.7.1 Excercises
#1. What is the default geom associated with stat_summary()? 
# How could you rewrite the previous plot to use that geom function instead 
# of the stat function?
?stat_summary
#the associaten geom is "pointrange", to override just pass a geom parameter in 
# the geom parameter

#2 What does geom_col() do? How is it different to geom_bar()?

ggplot(
  data=mpg,
  mapping=aes(x=drv, y = year)
)+
  geom_col()

ggplot(
  data=mpg,
  mapping=aes(x=drv)
)+
  geom_bar()
# geom bar counts the occurences of a certain category, while geom_col counts
# in respect to a second variable. 

# 3. Most geoms and stats come in pairs that are almost always used in concert. 
# Read through the documentation and make a list of all the pairs. 
# What do they have in common?

geom <- c(
  'abline',
  'area',
  'bar',
  'bin_2d',
  'blank',
  'boxplot',
  'col',
  'contour',
  'count',
  'crossbar',
  'curve'
) 
statList <- c(
  'None',
  'identity',
  'count',
  'bin_2d',
  'identity',
  'boxplot',
  'count',
  'contour',
  'sum',
  'identity',
  'identity'
)
df<- data.frame(geom,statList)
df
# most use the same name for stat and geom or something very similar/identity

#4. What variables does stat_smooth() compute? What parameters control its 
# behaviour?
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()
# it computes a smoothed curve that fits the scatterplot with a certain degree
# of confidence. It has parameters, like position, method, formula,se(show
# confidence), and some others.

#5. In our proportion bar chart, we need to set group = 1. Why? 
# In other words what is the problem with these two graphs?
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop)))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group=color))
#Works filling
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut,  y = after_stat(prop), fill= clarity,group=clarity), position="fill")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut,  y = after_stat(prop), fill= clarity,group=clarity), position="stack")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut,  y = after_stat(prop), fill= color,group=color), position="dodge")


# I don't know, but it is the way of normalizing(?).  Internet answer:

#`..prop..` finds proportions of the groups in the data. 
#If we don't specify that we want all the data to be regarded as one group, 
# then `geom_barchart` we end up with each cut as a separate group, and if we 
# find the proprtion of "Premium" diamonds that are "Premium", 
# the answer is obviously 1.



#3.8.1
#1. What is the problem with this graph?
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(position= "jitter")+
  geom_smooth(se=FALSE)
# A little jitter and some smooth. 

#2. What parameters to geom_jitter() control the amount of jittering?

# width and height

#3. Compare and contrast geom_jitter() with geom_count().

#they are the same, is a shortcut according to documentation. 

#4. What’s the default position adjustment for geom_boxplot()? 
# Create a visualisation of the mpg dataset that demonstrates it.

ggplot(data = mpg, mapping = aes(x = drv, y = hwy,fill=class)) +
  geom_boxplot()

  
# the default position is dodge



#3.9.1
#1. Turn a stacked bar chart into a pie chart using coord_polar().

# a little weird, 

#2. What does labs() do? Read the documentation.
?labs
# it helps changing labels in axis and so on. 
  
#3. What’s the difference between coord_quickmap() and coord_map()?
?coord_quickmap
?coord_map
# coord map is more exact but more computationally intensive.
  
#4. What does the plot below tell you about the relationship between city 
# and highway mpg? Why is coord_fixed() important? What does geom_abline() do?

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed()
# geom abline adds an f(x)=x line, and coord fixed allows that the line makes 
# sense

#4.4 Exercises

#Why does this code not work?

my_variable <- 10
my_varıable
#> Error in eval(expr, envir, enclos): object 'my_varıable' not found
# Typo

#Tweak each of the following R commands so that they run correctly:
library(tidyverse)

ggplot(dota = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

fliter(mpg, cyl = 8)
filter(diamond, carat > 3)
#solution
library("tidyverse")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)
filter(diamonds, carat > 3)
  
#CHAPTER 5

#1. Find all flights that

#Had an arrival delay of two or more hours
filter(flights,arr_delay>120)

#Flew to Houston (IAH or HOU)
filter(flights, dest == "HOU" | dest == "IAH" )

#Were operated by United, American, or Delta
filter(flights, carrier == "UA" | carrier == "AA" | carrier == "DL" )
filter( flights, carrier %in% c ("UA","AA","DL"))
#Departed in summer (July, August, and September)
filter( flights, month %in% c (7,8,9))
#Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay>120, dep_delay <= 0)
#Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60, dep_delay - arr_delay >= 30 )

#Departed between midnight and 6am (inclusive
filter(flights, hour <= 5 | (hour == 6 & minute == 0))

#2. Another useful dplyr filtering helper is between(). 
# What does it do? Can you use it to simplify the code needed to answer 
# the previous challenges?
?between
#we can abreviate the months
filter(flights,between(month,7,9))

# 3.How many flights have a missing dep_time? What other variables are missing? 
# What might these rows represent?
View(filter(flights,is.na(dep_time)))
# a lot of data missing too, this are probably canceled flights

# 4. Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE 
# & NA not missing? Can you figure out the general rule? 
# (NA * 0 is a tricky counterexample!)

# it works with boolean when & and | are used as a FALSE. In arithmetic it 
# always gives NA except NA ^ 0

#5.3.1 Exercises

#1. How could you use arrange() to sort all missing values to the start? 
# (Hint: use is.na()).
(arrange(flights, desc(is.na(dep_time))))

#2. Sort flights to find the most delayed flights. 
# Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)
# a flight that left 43 minutes before!

#3. Sort flights to find the fastest (highest speed) flights.

View(arrange(flights, desc(distance/air_time)))
 # fastest one was going 700 miles per hour!!!

#4. Which flights travelled the farthest? Which travelled the shortest?
View(arrange(flights, desc(distance)))

# Furthest one is from JFK to Hawaii! Closest one is Newark to philadelphia!.

#5.4.1 Exercises
#1. Brainstorm as many ways as possible to select dep_time, dep_delay, 
# arr_time, and arr_delay from flights.
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, ends_with("delay"), starts_with("arr_"), "dep_time")

#2. What happens if you include the name of a variable multiple times 
# in a select() call?
select(flights,"dep_time", "dep_time")
# it does the smart thing of including it only once.

#3. What does the any_of() function do? Why might it be helpful in 
# conjunction with this vector?
?any_of
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars))
select(flights, all_of(vars))

#4. Does the result of running the following code surprise you? 
# How do the select helpers deal with case by default? 
# How can you change that default?
select(flights, contains("TIME"))

select(flights, contains("TIME", ignore.case = FALSE))
# case works ignoring by default

#5.5.2 Exercises

#1. Currently dep_time and sched_dep_time are convenient to look at, but hard 
# to compute with because they’re not really continuous numbers. Convert them 
# to a more convenient representation of number of minutes since midnight.
flights_foo <- flights
flights_foo <- mutate(flights_foo,
       dep_time_min = (dep_time %/% 100 )*60 + (dep_time %% 100)
       )
#2. Compare air_time with arr_time - dep_time. What do you expect to see? 
# What do you see? What do you need to do to fix it?
flights_foo <- flights
flights_foo <- mutate(flights_foo,
                      difference = air_time - (arr_time-dep_time)
)
# is not cero, but is the time spend in taxi

#3. Compare dep_time, sched_dep_time, and dep_delay. 
# How would you expect those three numbers to be related?
select(flights,dep_time, sched_dep_time, dep_delay )
# dep_time-sched_dep_time == dep_delay

#4.Find the 10 most delayed flights using a ranking function. 
# How do you want to handle ties? Carefully read the documentation for min_rank().
min_rank(select(flights,dep_delay))
#The min_rank() function is a function that returns the 
# same values as rank when the ties_method is set to "min", 
# that is, ties are assigned the minimum ranking possible. 

#5. What does 1:3 + 1:10 return? Why?
1:3 + 1:10
1:3
1:10
# it replicates 1:3 until it finishes 1:10

#6. What trigonometric functions does R provide?
# All of them?


#5.6.7 Exercises
#1. Brainstorm at least 5 different ways to assess the typical delay 
# characteristics of a group of flights. Consider the following scenarios:

#1.1 A flight is 15 minutes early 50% of the time, 
# and 15 minutes late 50% of the time.
#median

flights %>%
  filter(!is.na(dep_time)) %>% 
  group_by(dep_delay)  %>%
  summarise(
    count = n(),
  )%>%
  mutate(
    cum_count = cumsum(count),
    cum_dist = cum_count / max(cum_count)
  )%>%
  filter( count > 100) %>%
  ggplot(mapping = aes(x = dep_delay, y = cum_dist))+
  geom_line()
  

flights %>%
  filter(!is.na(dep_time)) %>% 
 ggplot(mapping = aes(x = dep_delay))+
  geom_step(stat = "ecdf")+
  coord_fixed(xlim = c(-15,150), ylim = c(0,1), ratio = 100)
# A flight departs earlier than ~15 minutes half of the time  but it 
# starts in the 15 minute late mark only ~30% of the time, the
# remaining 20% of the time is late by more than 15 minutes. 

# A flight is always 10 minutes late.

# 25% of the time is later than that

# A flight is 30 minutes early 50% of the time,
# and 30 minutes late 50% of the time.

#Closer to reality. Although missing 15% of more than 30 minutes late.







#2. Come up with another approach that will give you the same output as 
# not_cancelled %>% count(dest) and not_cancelled %>% 
# count(tailnum, wt = distance) (without using count()).
flights %>%
  filter(!is.na(dep_time))%>%
  group_by(dest) %>%
  summarise(
    destination = n()
  )

#3. Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) )
# is slightly suboptimal. Why? Which is the most important column?

# they contain the same information in regards to NA or !NA. Therefore you only 
# need one

#4. Look at the number of cancelled flights per day. Is there a pattern? 
# Is the proportion of cancelled flights related to the average delay?

flights %>%
  group_by(year,month,day) %>% 
  summarize(
    percentage_cancelled = (sum(is.na(dep_time))) / n() ,
    avg_delay = mean(dep_delay, na.rm = TRUE) 
  ) %>%
  ggplot(mapping = aes(x = avg_delay, y = percentage_cancelled)) +
    geom_jitter()+
    geom_smooth(se=FALSE)
  
# Cancelled flights tend to happen more when there are too many delays.

#5. Which carrier has the worst delays? Challenge: can you disentangle the 
# effects of bad airports vs. bad carriers? Why/why not? (Hint: think about 
# flights %>% group_by(carrier, dest) %>% summarise(n()))
# Easy
flights %>%
  group_by(carrier) %>%
  summarise(
    count = sum(!is.na(dep_time)),
    avg_delay_dep = mean(dep_delay, na.rm = TRUE)  
  ) %>%
  arrange(desc(avg_delay_dep))%>%
  View()

# Challenge
flights %>%
  group_by(carrier,dest) %>%
  summarise(
    count = sum(!is.na(dep_time)),
    avg_delay_dep = mean(dep_delay, na.rm = TRUE)  
  ) %>%
  filter(count > 100)%>%
  arrange(desc(avg_delay_dep)) %>%
  View()
# It cannot be disentangled since there is no way of comparing it.
# EV for example is the only one that goes to TYS that has a horrible 
# mean delay, same with CAE, TUL ... but eliminating all those flights 
# would affect how we view the problem. We would have to consider the exact 
# same dest and origin for it to be fair, and then hace some merging function
# among those flights which is beyond the scope. 


#6. What does the sort argument to count() do. When might you use it?

flights %>%
  group_by(dest) %>%
  count(sort = TRUE)

# it lets you sort after a group by

# 5.7.1 Exercises
# 2. Which plane (tailnum) has the worst on-time record?
flights %>%
  group_by(tailnum) %>%
  summarise(
    dep_time_delay_record = max(dep_delay)
  )%>%
  arrange(desc(dep_time_delay_record))
# N384HA has the record! with 1300 minutes delayed, that is 21 hours!

#3. What time of day should you fly if you want to avoid delays 
# as much as possible?
flights %>%
  filter(!is.na(dep_time))%>%
  mutate(
    sched_time_min = (sched_dep_time %/% 100 )*60 + (sched_dep_time %% 100)
  )%>%
  group_by(sched_time_min)%>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(mapping = aes(x = sched_time_min,y = delay))+
  geom_line()+
  geom_smooth()

#The earliest the better!

#4. For each destination, compute the total minutes of delay. For each flight,
# compute the proportion of the total delay for its destination.


flights %>%
  filter(!is.na(dep_time))%>%
  filter(arr_delay>0) %>%
  group_by(dest)%>%
  filter(n() > 100 )%>%
  mutate(
    total_delay_arr_min = sum(arr_delay,na.rm = TRUE)
  ) %>%
  ungroup()%>%
  mutate(
    contribution = arr_delay/total_delay_arr_min *100
  )%>%
  View()
#7.3.4 Exercises

#1. Explore the distribution of each of the x, y, and z variables in diamonds. 
# What do you learn? Think about a diamond and how you might decide which 
# dimension is the length, width, and depth.

diamonds %>%
  ggplot( )+
  geom_freqpoly(mapping = aes(x), color = "blue")+
  geom_freqpoly(mapping = aes(y), color = "yellow")+
  geom_freqpoly(mapping = aes(z), color = "purple")+
  coord_cartesian(xlim = c(0,12))

diamonds %>% 
  mutate (
    difference = abs(x-y)
  )%>%
  filter(difference < 0.25)%>%
  arrange(desc(difference))%>%
  ggplot(mapping = aes(x = difference))+
  geom_histogram(binwidth = 0.01)
  

# I would assume since the difference of X and Y tends to be less than 0.15 
# they define length and width, and then Z that tends to be smaller than the two 
# of then defines deepness. 

#2. Explore the distribution of price. Do you discover anything unusual or 
# surprising? (Hint: Carefully think about the binwidth and make sure you try a 
# wide range of values.)

diamonds %>%
  filter(between(price,1400,1600))%>%
  ggplot(mapping = aes(x=price))+
  geom_histogram(binwidth = 1)

diamonds %>%
  ggplot(mapping = aes(x=price))+
  geom_density()
# there is no diamond priced at 1500 +- 50 . I guess some sort of law(?)

#3. How many diamonds are 0.99 carat? How many are 1 carat? What do you think 
# is the cause of the difference?

diamonds %>%
  filter(between(carat, 0.90,1.10))%>%
  ggplot(mapping = aes(x=carat))+
  geom_histogram(binwidth = 0.01)
# There is little to no diamond on the 0.99 because people who sell diamonds
# would round up the value. 

#7.4.1 Exercises
#1. What happens to missing values in a histogram? What happens to 
# missing values in a bar chart? Why is there a difference?

# In a histogram is always removed the only difference is the warning. Same 
# goes for barchart

#2. What does na.rm = TRUE do in mean() and sum()?

# It allow to do sum and mean since NA absorbs the rest of the values. 


#7.5.1.1 Exercises

#1. Use what you’ve learned to improve the visualisation of the departure times
# of cancelled vs. non-cancelled flights.


nycflights13::flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(x = sched_dep_time, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

#2. What variable in the diamonds dataset is most important for predicting the 
# price of a diamond? How is that variable correlated with cut? Why does the 
# combination of those two relationships lead to lower quality diamonds being 
# more expensive?

ggplot(data = diamonds,mapping = aes(x= price, y = carat))+
  geom_jitter()+
  geom_smooth(se = FALSE)+
  geom_density2d()

ggplot(data = diamonds,mapping = aes(x= price, y = depth))+
  geom_jitter()+
  geom_smooth(se = FALSE)+
  geom_density2d()
  
ggplot(data = diamonds,mapping = aes(x= price, y = ..density..))+
  geom_freqpoly(mapping = aes(color = clarity))

diamonds%>%
  mutate(
    volume = x * y * z
  )%>%
  filter(volume < 600)%>%
  ggplot(mapping = aes(x= price, y = volume))+
  geom_jitter()+
  geom_smooth(se = FALSE)

diamonds %>%
  ggplot (mapping = aes(y = price, x = cut))+
  geom_boxplot()

#carat is quite important but the "volume" I created is amazing!

#7.5.2.1 Exercises
# 1. How could you rescale the count dataset above to more clearly show the 
# distribution of cut within colour, or colour within cut?
ggplot(data = diamonds) +
  geom_jitter(mapping = aes(x = cut, y = color, stroke = 0.001))

#2. Use geom_tile() together with dplyr to explore how average flight delays 
# vary by destination and month of year. What makes the plot difficult to read? 
# How could you improve it?

flights %>%
  ggplot(mapping = aes(y = arr_delay, x = month))+
  geom_tile(alpha = 0.05, na.rm = TRUE)+
  coord_cartesian(ylim = c(-100 ,500))

#7.5.3.1 Exercises

#2. Visualise the distribution of carat, partitioned by price.
ggplot(data = diamonds, mapping = aes(y = carat, x = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))+
  coord_cartesian(ylim = c(0 ,2.5))

ggplot(data = diamonds , mapping = aes(x=carat))+
  geom_freqpoly(
    mapping = aes(
      group=cut_number(price,20),
      colour = factor(cut_number(price,20)),
    ), 
    show.legend= TRUE
  )

diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()
