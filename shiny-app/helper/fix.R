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

counties_sf = read_counties()

ch <- counties_sf %>% 
  filter(state_name == "Florida")

climate_sf = counties %>% 
  left_join(climate, by = c("county_fips" = "f"))


counties %>% 
  filter()

# in counties_sf which is from urbanmapr , miami dade fips == 	12086

%>% 
  select(-fips_class, -state_abbv, -state_fips, -county, -state, -mig_rank)

return(climate_sf)

cle = readRDS("clean-data/climate.Rda")

cle %>% 
  filter(county =="Dade")


climate_sf %>% 
  filter(county_fips == 12025 )
