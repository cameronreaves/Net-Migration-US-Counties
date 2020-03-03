#libraries
library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(openintro) # state abbr
library(gt)
library(stringr)


#subset of counties that include only states along the coast of the United States

get_include = function(){
  include = c(
    "Washington",
    "California", 
    "Texas", 
    "Florida",
    "Georgia",
    "South Carolina", 
    "North Carolina",
    "Virginia",
    "Maryland",
    "Delaware",
    "New Jersey",
    "New York",
    "Connecticut",
    "Rhode Island",
    "Massachusetts",
    "New Hampshire", 
    "Louisiana", 
    "Mississippi", 
    "Alabama", 
    "Oregon", 
    "All")
  
  return(include)
}

#function used to cube root the net migration for a county, can't transform negative values with log

cbrt <- function(x) {
  sign(x) * abs(x)^(1/3)
}


#returns filtered sf dataframe of of US counties -- only coastal cities 

read_counties = function(){
  
  counties_sf <- get_urbn_map("counties", sf = TRUE)
  
  counties_sf = counties_sf %>% 
    filter(state_name %in% get_include())
  
}

#returns cleaned dataframe of net migratoin for US counties merged with county fips from scraped from the web

read_climate = function() {
  
  climate = read_csv("raw-data/climate.csv")
  web_fips = read_csv("raw-data/web_fips.csv")
  
  states <- c(climate$State)
  climate$State = state.abb[match(states,state.name)] 
  
  climate = climate %>% 
    left_join(web_fips, by = c("County" = "c", "State" = "s"))
  
  climate = climate %>% 
    select(county = County, state = State, net = `Net Migration`, mig_rank = Rank, f) %>% 
    mutate(cb_net = cbrt(net))
  
  climate = data.frame(climate)

  return(climate)
  
}

#merge income and population and climate data sets together, then clean them

make_stats = function(){
    stats = climate %>% 
    inner_join(income, by = c("f" = "GEOID")) %>% 
    select("name" = NAME, net, cb_net, f, "income" = estimate) %>% 
    inner_join(population, by = c("f" = "GEOID")) %>% 
    rename("population" = estimate) %>% 
    select(name, net, cb_net,income, population)
}

#returns sf dataframe that joins climate dataframe with counties sf dataframe

join_climate = function(climate, counties){
  
  climate_sf = counties %>% 
    left_join(climate, by = c("county_fips" = "f")) %>% 
    select(-fips_class, -state_abbv, -state_fips, -county, -state, -mig_rank)
  
  return(climate_sf)
  
}

save_as_dataframe = function(data, name){
  saveRDS(data,file=paste0(name, ".Rda", sep = ""))
}


if (FALSE) {
  save_as_dataframe(read_climate(), "clean-data/climate")
  save_as_dataframe(read_zillow(), "clean-data/zillow")
  save_as_dataframe(read_opp(), "clean-data/opp")
  saveRDS(stats, "clean-data/stats.Rds")
}
