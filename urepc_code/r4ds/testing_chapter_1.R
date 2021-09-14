library("tidyverse")

# Plot engine size vs fuel efficiency in scatter plot
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy)) 

# Creates an empty plot
ggplot(data=mpg)

#add hue (can also add it with size)
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, color = class)) 

ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, shape = class)) 

ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy, alpha = class))

#Make it all one color
ggplot(data = ggplot2::mpg) + 
  geom_point(mapping =aes(x = displ, y = hwy), color = "blue")


#Using facets
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_wrap(~ class, nrow=2)

ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv ~ cyl)



# Geometric Objects
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy))

#different linetype

ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy, linetype= drv))

#multiple plots on top of each other
ggplot(data=mpg)+
  geom_smooth(mapping=aes(x=displ,y=hwy))+
  geom_point(mapping=aes(x=displ ,y=hwy))

# Without duplication
ggplot(data=mpg, mapping=aes(x=displ ,y=hwy))+
  geom_smooth()+
  geom_point(mapping=aes(color=class))

#with filtering
ggplot(data=mpg, mapping=aes(x=displ ,y=hwy))+
  geom_smooth(data=filter(mpg, class=="subcompact"),
              se = FALSE
              )+
  geom_point(mapping=aes(color=class))


# Barplots
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data=diamonds)+
  stat_count(mapping = aes(x = cut))
# Change the stat
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")


#probability

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group = 1))

#summary
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#position adjustments
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
# stack barplot
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
# random noise to plot 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")

#flip coordinates 
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
# Adjusting for maps
nz <- map('worldHires','Italy')

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
# kind of a pie chart but not really
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


#Libraries for chapter 5
library(nycflights13)
library(tidyverse)

#FILTER
filter(flights, month == 1, day == 1)


# get the density
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)













































