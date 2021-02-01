library(pacman)
library(flexdashboard)
p_load(char=c('tidyverse','broom','plotly','leaflet','rnaturalearth',
              'rnaturalearthdata', 'janitor'))

confirmed_data <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv') %>% clean_names()
death_data <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv') %>% clean_names()
recovered_data <- read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv') %>% clean_names()

confirmed_data_country <-
  confirmed_data %>% pivot_longer(starts_with('x'), names_to='date', values_to = 'number') %>%
  group_by(country_region, date) %>%
  summarize(number = sum(number)) %>%
  ungroup() %>%
  mutate(date = str_replace(str_replace_all(date, '_', '/'), 'x', '')) %>%
  mutate(date = as.Date(date, format = '%m/%d/%y'))
death_data_country <-
  death_data %>% pivot_longer(starts_with('x'), names_to = 'date',
                              values_to = 'number') %>%
  group_by(country_region, date) %>%
  summarize(number = sum(number)) %>%
  ungroup() %>%
  mutate(date = str_replace(str_replace_all(date, '_', '/'), 'x', '')) %>%
  mutate(date = as.Date(date, format = '%m/%d/%y'))


top_countries <- confirmed_data_country %>%
  filter(date == max(date)) %>%
  top_n(4, number)
ggplot(confirmed_data_country, aes(date, number, color = country_region))+
  geom_line(show.legend=F) +
  scale_y_log10(labels = scales::label_number()) +
  geom_label_repel(data = top_countries, mapping = aes(label = country_region),
                   show.legend = F)+
  theme_ipsum()



# Date of first confirmed case --------------------------------------------

first_case_country <- confirmed_data_country %>%
  group_by(country_region) %>%
  summarize(first_date = min(date[number >= 1])) %>%
  ungroup()

first_case_country %>% count(first_date) %>%
  mutate(cum_n = cumsum(n)) %>%
  ggplot(aes(x = first_date, y = cum_n))+
  geom_segment(aes(x = first_date, y=0, xend = first_date, yend = cum_n),
               color = 'lightgrey') +
  geom_point(color = 'orange') +
  theme_ipsum()


# Case fatality rate ------------------------------------------------------

new_data = confirmed_data_country %>%
  rename('cases' = 'number') %>%
  left_join(death_data_country) %>%
  mutate(cfr = ifelse(cases > 0, number/cases, NA)) %>%
  mutate(cfr_lcb = ifelse(cases > 0, cfr - 2 * sqrt(cfr * (1-cfr)/cases), NA),
         cfr_ucb = ifelse(cases > 0, cfr + 2 * sqrt(cfr * (1-cfr) / cases), NA))

top_ten = confirmed_data_country %>% filter(date==max(date)) %>% top_n( 10, number)
ggplot(data = new_data %>% filter(country_region %in% top_ten$country_region),
       aes(date, cfr))+geom_line() + facet_wrap(~country_region)+ylim(0,0.1)+
  theme_ipsum()

global_cfr = new_data %>% group_by(date) %>%
  summarize(cases=sum(cases, na.rm=T), number=sum(number, na.rm=T)) %>%
  ungroup() %>%
  mutate(cfr=number/cases, cfr_se = sqrt(cfr*(1-cfr)/cases))

ggplot(global_cfr, aes(date, cfr))+geom_line() + geom_ribbon(aes(ymin=cfr-2*cfr_se,
                                                                 ymax=cfr+2*cfr_se),
                                                             alpha=0.2)+
  scale_y_percent()


library(sf)

death_data %>% group_by(country_region) %>%
  summarize(lat = mean(lat), long = mean(long), x3_9_20 = sum(x3_9_20)) %>%
  ungroup()-> world_death
