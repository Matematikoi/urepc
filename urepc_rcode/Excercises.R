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
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = 1, fill = color, width =color),show.legend = TRUE, width=1,position="dodge", ) 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)+
  coord_polar()
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
  
  






























