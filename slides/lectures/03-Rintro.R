## ----setup, include=FALSE-----------------------------------
knitr::opts_chunk$set(echo = TRUE, message=F, warning=F, comment = '#> ', cache=FALSE)
setwd(here::here('slides/lectures'))


## ----Rintro-1, eval = F-------------------------------------
## if (age < 18){
##    person <- 'Minor'
## } else if (age > 65) {
##   person <- 'Senior'
## } else {
##   person <- 'Adult'
## }


## ----Rintro-2, eval=F---------------------------------------
## my_mean <- function(x, na.rm = T){
##   if(na.rm){
##     x <- x[!is.na(x)]
##   }
##   s <- sum(x)
##   n <- length(x)
##   mn <- s / n # There is a built-in function mean, so I don't use that word
##   return(mn)
## }


## ----Rintro-3-----------------------------------------------
c(1,2,5,6,7,8)
c('apple','berry','melon','citrus')


## ----Rintro-4-----------------------------------------------
x <- c(1,2,4,5,6,7)
y <- 10:16  # Shortcut for c(10,11,12,13,14,15,16)


## ----Rintro-5, eval=T---------------------------------------
cbind(x, y) # Vectors as columns


## ----Rintro-6, eval=T---------------------------------------
rbind(x, y) # Vectors as rows


## ----b1, eval=F---------------------------------------------
## my_list <- list('a', c(2,3,5,6), head(ggplot2::mpg))
## my_list


## ----Rintro-7, echo=F, eval=T, ref.label='b1'---------------



## ----Rintro-8-----------------------------------------------
# use the import function from the package rio
beaches <- rio::import('data/sydneybeaches3.csv') 
class(beaches)
dim(beaches)
head(beaches)



## ----Rintro-9, message=F------------------------------------
library(tidyverse) # Activate the tidyverse package
beaches_t <- as_tibble(beaches)
class(beaches_t)
beaches_t


## ----Rintro-10, eval=F--------------------------------------
## install.packages("tidyverse")


## ----Rintro-11, eval = F------------------------------------
## install.packages("BiocManager") # do once
## BioManager::install('limma')


## ----Rintro-12, eval = F------------------------------------
## install.packages('remotes') # do once
## remotes::install_github("rstudio/rmarkdown") # usual format is username/packagename


## ----Rintro-13, include=F-----------------------------------
detach_pkg <- search()[str_detect(search(), 'gg')]
map(detach_pkg, function(x) {
  ind <- which(str_detect(search(), x))
  if(length(ind) > 0) detach(pos = ind)
})



## ----Rintro-14, error=TRUE----------------------------------
ggplot(beaches, aes(temperature, rainfall)) +
  geom_point()


## ----Rintro-15, message=F, warning = F, fig.height=4--------
library(ggplot2) # or library(tidyverse)
ggplot(beaches, aes(temperature, rainfall)) +
  geom_point()


## ----Rintro-16, echo = T------------------------------------
head(mtcars, 3)


## ----Rintro-17, error=T-------------------------------------
link <- 'https://dl.dropboxusercontent.com/s/pqavhcckshqxtjm/brca.csv'
brca_data <- rio::import(link)


## ----Rintro-18, warning=FALSE-------------------------------
# install.packages('tidyverse')
library(tidyverse)


## ----Rintro-19, echo = FALSE, message=F, warning=F----------
my_tbl <- tibble::tribble(
  ~Package, ~Description,
      'ggplot2',     'Data visualization',
      'tibble',     'data.frame on steroids',
      'tidyr',     'Data tidying (today)',
      'readr',     'Reading text files (CSV)',
      'purrr',     'Applying functions to data iteratively',
      'dplyr',     'Data cleaning and munging (today)',
      'stringr',     'String (character) manipulation',
      'forcats',     'Manipulating categorical variables'
  )

require(knitr)
kable(my_tbl, digits = 3, row.names = FALSE, align = "l",
              caption = NULL, format='html')


## ----Rintro-20, echo=F--------------------------------------
my_tbl <- tibble::tribble(
  ~Package, ~Description,
      'readxl',     'Read Excel files',
      'haven',     'Read SAS, SPSS, Stata files',
      'lubridate',  'Deal with dates and times'  ,
      'magrittr',     'Provides the pipe operator %>%',
      'glue', 'Makes pasting text and data easier')

require(knitr)
kable(my_tbl, digits = 3, row.names = FALSE, align = "l",
              caption = NULL, format='html')



## ----Rintro-21, echo=F--------------------------------------
my_tbl <- tibble::tribble(
  ~Package, ~Description,
  'broom', 'Turns the results of models or analysis into tidy datasets',
  'fs','Allows directory and file manipulation in OS-agnostic manner',
  'here', 'Allows robust specification of directory structure in a Project')

require(knitr)
kable(my_tbl, digits = 3, row.names = FALSE, align = "l",
              caption = NULL, format='html')


## ----a2, eval=T---------------------------------------------
mpg1 <- mpg %>% mutate(id=1:n()) %>% select(id, year, trans, cty, hwy)
mpg_metric <- mpg1 %>% 
  mutate_at(vars(cty, hwy), function(x) {x * 1.6/3.8})


## ----Rintro-22, echo=F--------------------------------------
knitr::kable(head(mpg1, 5), format='html')


## ----Rintro-23, echo = F------------------------------------
knitr::kable(head(mpg_metric, 5), format='html')


## ----Rintro-24, echo=F--------------------------------------
my_tbl <- tribble(
  ~"Verb", ~"Functionality",
  'mutate','Transform a column with some function',
  'select', 'Select some columns in the data',
  'arrange', 'Order the data frame by values of a column(s)',
  'filter','Keep only rows that meet some data criterion',
  'group_by', 'Group by levels of a variable',
  'gather', 'Transform a wide dataset to a long dataset',
  'spread','Transform a long dataset to a wide dataset',
  'separate', 'Separate one column into several columns',
  'unite', 'Concatenate several columns into 1 column')
knitr::kable(my_tbl, align = 'l', format='html')


## ----ex1, eval=F--------------------------------------------
## url <- "http://varianceexplained.org/files/Brauer2008_DataSet1.tds"
## raw_data <- read_delim(url, delim='\t')
## head(raw_data)

## ----Rintro-25, echo=F, eval=T, ref.label='ex1'-------------



## ----ex2, eval=F--------------------------------------------
## head(raw_data$NAME)

## ----Rintro-26, ref.label='ex2',echo=F, eval=T--------------



## ----ex3, message=FALSE, eval=F-----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"), #<<
##            sep = "\\|\\|")#<<
## head(cleaned_data)

## ----Rintro-27, eval=T, echo=F, ref.label='ex3'-------------



## ----ex4, eval=F, message=FALSE-----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"),
##            sep = "\\|\\|") %>%
##   mutate_at(vars(name:systematic_name), funs(stringr::str_trim)) #<<
## head(cleaned_data)

## ----Rintro-28, echo=F, eval=T, ref.label='ex4'-------------



## ----ex5, eval=F, message=FALSE-----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"),
##            sep = "\\|\\|") %>%
##   mutate_at(vars(name:systematic_name), funs(stringr::str_trim)) %>%
##   select(-number, -GID, -YORF, -GWEIGHT) #<<
## head(cleaned_data)

## ----Rintro-29, ref.label='ex5', eval=T, echo=F, message=F----



## ----ex6, eval=F,  message=FALSE----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"),
##            sep = "\\|\\|") %>%
##   mutate_at(vars(name:systematic_name), funs(stringr::str_trim)) %>%
##   select(-number, -GID, -YORF, -GWEIGHT) %>%
##   tidyr::gather(sample, expression, G0.05:U0.3) #<<
## head(cleaned_data)

## ----Rintro-30, ref.label='ex6', echo=F, eval=T, message=F----



## ----ex7, eval=F, message=FALSE-----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"),
##            sep = "\\|\\|") %>%
##   mutate_at(vars(name:systematic_name), funs(stringr::str_trim)) %>%
##   select(-number, -GID, -YORF, -GWEIGHT) %>%
##   tidyr::gather(sample, expression, G0.05:U0.3) %>%
##   separate(sample, c("nutrient", "rate"), sep=1, convert = TRUE) #<<
## head(cleaned_data)

## ----Rintro-31, ref.label='ex7', echo=F,eval=T, message=F----



## ----ex8, eval=F, message=FALSE-----------------------------
## cleaned_data <- raw_data %>%
##   separate(NAME, c("name", "BP", "MF", "systematic_name", "number"),
##            sep = "\\|\\|") %>%
##   mutate_at(vars(name:systematic_name), funs(stringr::str_trim)) %>%
##   select(-number, -GID, -YORF, -GWEIGHT) %>%
##   tidyr::gather(sample, expression, G0.05:U0.3) %>%
##   separate(sample, c("nutrient", "rate"), sep=1, convert = TRUE) %>%
##   filter(!is.na(expression), systematic_name != '') #<<
## head(cleaned_data)

## ----Rintro-32, ref.label='ex8', eval=T, echo=F, message=F----



## ----ex9, eval=T, fig.height=3, fig.width=6, fig.pos='center'----
cleaned_data %>%
  filter(BP == "leucine biosynthesis") %>%
  ggplot(aes(rate, expression, color = nutrient)) + #<<
  geom_point() +#<<
  geom_smooth(method = "lm", se = FALSE) + #<<
  facet_wrap(~name + systematic_name, nrow=1) + #<<
  theme(legend.position='right') #<<



## ----Rintro-34, echo=T--------------------------------------
library(survival)
myLinearModel <- lm(chol ~ bili, data = pbc)


## ----Rintro-35, echo=T--------------------------------------
myLinearModel


## ----Rintro-36, echo=T--------------------------------------
summary(myLinearModel)


## ----Rintro-37, echo=T--------------------------------------
broom::tidy(myLinearModel)


## ----Rintro-38, echo=T--------------------------------------
broom::glance(myLinearModel)


## ----Rintro-39, echo=T, eval=F------------------------------
## # install.packages('ggfortify')
## library(ggfortify)
## autoplot(myLinearModel)


## ----Rintro-40, echo=F, eval=T, fig.height=6----------------
# install.packages('ggfortify')
library(ggplot2)
library(ggfortify)
autoplot(myLinearModel)



## ----Rintro-41----------------------------------------------
myModel <- lm(log10(enterococci) ~ rainfall + temperature + season_name + factor(year), data = beaches)
broom::tidy(myModel)


## ----Rintro-42----------------------------------------------
plt_data <- broom::tidy(myModel)
plt_data <- plt_data %>% 
  filter(term != '(Intercept)') %>% #<<
  mutate(term = str_replace(term, 
                            'season_name','')) #<<
plt_data


## ----Rintro-43----------------------------------------------
plt_data <- broom::tidy(myModel)
plt_data <- plt_data %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = str_replace(term, 
                            'season_name','')) %>% 
  mutate(term = str_replace(term, 
                            'factor\\(year\\)','')) # Brackets are "escaped" using \\ #<< 
plt_data


## ----c1, eval=F---------------------------------------------
## plt_data <- broom::tidy(myModel)
## plt_data %>%
##   filter(term != '(Intercept)') %>%
##   mutate(term = str_replace(term,
##                     'season_name','')) %>%
##   mutate(term = str_replace(term,
##                     'factor\\(year\\)','')) %>% # Brackets need to be "escaped" using \\
##   ggplot(aes(x = term, y = estimate,
##              ymin = estimate - 2 * std.error,
##              ymax = estimate + 2 * std.error))+
##     geom_pointrange()
## 


## ----Rintro-44, ref.label='c1', echo=F, eval=T, fig.height=6----



## ----c2, eval=F---------------------------------------------
## plt_data <- broom::tidy(myModel)
## plt_data %>%
##   filter(term != '(Intercept)') %>%
##   mutate(term = str_replace(term,
##                   'season_name','')) %>%
##   mutate(term = str_replace(term,
##                   'factor\\(year\\)','')) %>% # Brackets need to be "escaped" using \\
##   ggplot(aes(x = term, y = estimate,
##              ymin = estimate - 2 * std.error,
##              ymax = estimate + 2 * std.error))+
##     geom_pointrange() +
##     geom_hline(yintercept = 0, linetype=2) +
##     theme_bw() +
##     coord_flip()
## 
## 


## ----Rintro-45, ref.label='c2', echo=F, eval=T, fig.height=6----



## ----Rintro-46, eval=F--------------------------------------
## plt_data <- broom::tidy(myModel)
## plt_data %>%
##   filter(term != '(Intercept)') %>%
##   mutate(term = str_replace(term,
##                   'season_name','')) %>%
##   mutate(term = str_replace(term,
##                   'factor\\(year\\)','')) %>% # Brackets need to be "escaped" using \\
##   ggplot(aes(x = term, y = estimate,
##              ymin = estimate - 2 * std.error,
##              ymax = estimate + 2 * std.error))+
##     geom_pointrange() +
##     geom_hline(yintercept = 0, linetype=2) +
##     theme_bw() +
##     coord_flip()
## 
## ggsave('results.png') # ggsave knows format from file
## 
## 


## ----Rintro-47, ref.label='c2', echo=F, eval=T, fig.height=6----



## ----d1, eval=F---------------------------------------------
## library(ggpubr)
## theme_viz <- function(){ #<<
##   theme_bw() + #<<
##     theme(axis.title = element_text(size=16),#<<
##           axis.text = element_text(size=14),#<<
##           text = element_text(size = 14)) #<<
## } #<<
## ggplot(
##   data=beaches,
##   mapping= aes(x = season_name,
##                y = log10(enterococci),
##                color = season_name)) +
##   geom_boxplot()+geom_jitter()+
##   labs(x = 'Season',
##        y = expression(paste('log'['10'],'(enterococci)')),
##        color='Season') +
##   theme_viz()


## ----Rintro-48, ref.label='d1', eval=T, echo=F--------------



## ----d2, eval=F---------------------------------------------
## library(ggpubr)
## plt <- ggplot(
##   data=beaches,
##   mapping= aes(x = season_name,
##                y = log10(enterococci),
##                color = season_name)) +
##   geom_boxplot() +
##   geom_jitter(width=0.1) +
##   labs(x = 'Season',
##        y = expression(paste('log'['10'],'(enterococci)')),
##        color='Season')+
##   theme_viz()
## my_comparisons <- list(c('Autumn','Spring'),
##                        c('Spring','Summer'),
##                        c('Summer','Winter'),
##                        c('Spring','Winter'))
## plt + stat_compare_means() #<<


## ----Rintro-49, ref.label='d2', eval=T, echo=F--------------



## ----d3, eval=F---------------------------------------------
## library(ggpubr)
## plt <- ggplot(
##   data=beaches,
##   mapping= aes(x = season_name,
##                y = log10(enterococci),
##                color = season_name)) +
##   geom_boxplot() +
##   geom_jitter(width=0.1)+
##     labs(x = 'Season',
##          y = expression(paste('log'['10'],'(enterococci)')),
##          color='Season') +
##   theme_viz()
## my_comparisons <- list(c('Autumn','Spring'),
##                        c('Spring','Summer'),
##                        c('Summer','Winter'),
##                        c('Spring','Winter'))
## plt + stat_compare_means() +
##   stat_compare_means(comparisons = my_comparisons) #<<


## ----Rintro-50, ref.label='d3', eval=T, echo=F--------------



## ----d5, eval=F---------------------------------------------
## library(ggpubr)
## plt <- ggplot(
##   data=beaches,
##   mapping= aes(x = season_name,
##                y = log10(enterococci),
##                color = season_name)) +
##   geom_boxplot() +
##   geom_jitter(width=0.1)+
##   labs(x = 'Season',
##        y = expression(paste('log'['10'],'(enterococci)')), color='Season')+
##   theme_viz()
## my_comparisons <- list(c('Autumn','Spring'),
##                        c('Spring','Summer'),
##                        c('Summer','Winter'),
##                        c('Spring','Winter'))
## plt + stat_compare_means(label.y = 6) +
##   stat_compare_means(comparisons = my_comparisons)


## ----Rintro-51, ref.label='d5', eval=T, echo=F--------------



## ----Rintro-52----------------------------------------------
dat_spine <- rio::import('data/Dataset_spine.csv', 
                         check.names=T)
head(dat_spine)


## ----Rintro-53----------------------------------------------
dat_spine %>% 
  tidyr::gather(variable, value, everything())


## ----Rintro-54, fig.height=3--------------------------------
dat_spine %>% 
  select(Pelvic.incidence:Sacral.slope) %>% 
  tidyr::gather(variable, value) %>% 
  ggplot(aes(x = value)) + 
  geom_density() +
  facet_wrap(~variable) +
  labs(x = '')


## ----e1, fig.height=4---------------------------------------
beaches %>% 
  ggplot(aes(x = season_name, y = temperature)) +
  geom_boxplot() +
  scale_y_continuous(labels = 
        scales::unit_format(unit = "\u00B0C")) +
  labs(x = 'Season', y = 'Temperature') +
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))


## ----e2, fig.height=4---------------------------------------
beaches %>% 
  mutate(season_name = 
      fct_relevel(season_name, 
        'Autumn','Winter','Spring','Summer')) %>% #<<
  ggplot(aes(x = season_name, y = temperature)) +
  geom_boxplot() +
  scale_y_continuous(labels = 
        scales::unit_format(unit = "\u00B0C")) +
  labs(x = 'Season', y = 'Temperature') +
  theme_bw() + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

