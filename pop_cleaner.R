# Populations cleaner

library(readr)

# Using the population at the start of the year for my estimate
pop <- read_csv("pop_estimates.csv") |> 
  janitor::clean_names() |>
  mutate(ref_date = ymd(paste(ref_date,"-01", sep = ""))) |> 
  filter(ref_date %in% the_years & geo %in% c("British Columbia", 
                                              "Alberta", "Ontario")) |> 
  select(ref_date, geo, value) |>
  mutate(geo = str_replace_all(geo, c("British Columbia"= "BC",
                                      "Alberta" = "AB",
                                      "Ontario" ="ON")))
colnames(pop) <- c("the_year", "province", "population")

