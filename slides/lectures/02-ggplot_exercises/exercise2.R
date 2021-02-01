## Exercise 2
##

## Read the spine data

library(tidyverse) # includes ggplot2
dat_spine <- read_csv('data/Dataset_spine.csv') %>%
  set_names(make.names(names(.)))  # Makes column names more tractable

## Take a look at this data


## Using this data, draw a scatter plot looking at the relationship between
## the sacral slope and the degree of spondylolisthesis

plt <- ggplot(
  data = dat_spine,
  aes(x = Sacral.slope,
      y = Degree.spondylolisthesis)
) +
  geom_point()

## TODO
## Does running the above code display a plot?
## We've actually saved the plot to a name, "plt". See what happens if you
## type "print(plt)" or "plot(plt)".

## TODO
## Add to the aesthetics to color points based on their class attribute (Normal/Abnormal)




## TODO
## Fix the ordering of the legend so that we have Normal, then Abnormal




