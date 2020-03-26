library(tidyverse)
library(magrittr)
library(ggplot2)
library(gridExtra)

covid_raw = read.csv(
  file=url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"),
  # file=url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"),
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
  mutate_at(vars(-Date), ~as.numeric(.)) %>%
  mutate(days=as.numeric(Date)-as.numeric(min(Date))) %>%
  head(-1)

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
  inner_join(covid_world, by="Date")

p1 = covid %>%
  filter(Date > as.Date('2020-03-01')) %>%
  select("Date", "US", "Italy", "Spain", "France", "Iran", "World") %>%
  # mutate_at(vars(-Date), ~./max(.)) %>%
  gather(key="Country", value="Infected", -Date) %>%
  ggplot(aes(x=Date, y=Infected, color=Country)) +
  # geom_line() + 
  geom_point(alpha=0.4, size=1.5) +
  scale_y_log10() +
  geom_smooth(method='lm', se=F)

p2 = covid %>%
  select("Date", "US", "Italy", "Spain", "France", "Iran", "World") %>%
  # mutate_at(vars(-Date), ~./max(.)) %>%
  gather(key="Country", value="Infected", -Date) %>%
  ggplot(aes(x=Date, y=Infected, color=Country)) +
  geom_line() +
  geom_point(alpha=0.4, size=1.5) +
  scale_y_log10()

grid.arrange(p1, p2,
  top = sprintf("Covid 19 Infection Rates -- %s", Sys.Date())
  )

# zeroed =
#   covid %>%
#   mutate(US = US[min(which(US!=0)):(51 + min(which(v!=0)))])


# v = c(0,0,1,2,3,4)
# first = min(which(v!=0))
# c(tail(v, 1-min(which(v!=0))), head(v,  min(which(v!=0)) -1))
#
# v[c(min(which(v!=0)):length(v), 1:min(which(v!=0)))]


