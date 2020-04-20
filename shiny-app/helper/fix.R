library(tidyverse)
climate = read_csv("raw-data/climate.csv")
web_fips = read_csv("raw-data/web_fips.csv")

climate <- climate %>% 
  mutate(County = replace(County, County=="Miami-Dade", "Dade"))

states <- c(climate$State)
climate$State = state.abb[match(states,state.name)] 

climate = climate %>% 
  left_join(web_fips, by = c("County" = "c", "State" = "s"))

climate = climate %>% 
  select(county = County, state = State, net = `Net Migration`, mig_rank = Rank, f) %>% 
  mutate(cb_net = cbrt(net))

climate = data.frame(climate)

