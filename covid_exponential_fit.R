library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(broom)

covid_raw = read.csv(
  # file=url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"),
  file=url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"),
  header=T,
  stringsAsFactors=F,
  check.names = T
)

covid_world = 
  covid_raw %>%
  select(starts_with("X")) %>%
  summarize_all(sum) %>%
  set_rownames('World') %>%
  ### Transpose to make countries the columns
  t() %>%
  data.frame(stringsAsFactors = F) %>%
  rownames_to_column("Date") %>%
  mutate(Date =  str_remove(Date, "X") %>% as.Date(format="%m.%d.%y")) %>%
  mutate_at(vars(-Date), ~as.numeric(.))

covid =
  covid_raw %>%
  select(Country.Region, starts_with("X")) %>%
  group_by(Country.Region) %>%
  summarize_all(sum) %>%
  mutate(Country.Region = make.names(Country.Region)) %>%
  column_to_rownames("Country.Region") %>%
  ### Transpose to make countries the columns
  t() %>%
  data.frame(stringsAsFactors = F) %>%
  rownames_to_column("Date") %>%
  mutate(Date =  str_remove(Date, "X") %>% as.Date(format="%m.%d.%y")) %>%
  mutate_at(vars(-Date), ~as.numeric(.)) %>%
  ### Add In World
  inner_join(covid_world, by="Date") %>%
  mutate(days = 
           difftime(Date, min(Date), units="days") %>%
           as.numeric()
         )

last_14 = 
  covid %>%
  select(World, US, days) %>%
  tail(14) %>%
  mutate(days = days - min(days))

print(last_14)

lmfit = lm(log2(World) ~ days, last_14)
print(lmfit)

glmfit = glm(formula = World ~ days,  data=last_14, family=gaussian(link='log'))
print(glmfit)

nslfit = nls(World ~ exp(a + b*days), last_14, start=list(a=5, b=0.03))
print(nslfit)

# models = data.frame(
#   days = last_14$days,
#   lm = exp(2.59 + 0.188*last_14$days),
#   glm = exp(8.009 + 0.088*last_14$days),
#   nls = exp(8.01 + 0.0819*last_14$days)
# )
# 
# models %>%
#   ggplot() + 
#   geom_point(aes(x=days, y=lm, color="lm")) + 
#   geom_point(aes(x=days, y=glm, color="glm"))
