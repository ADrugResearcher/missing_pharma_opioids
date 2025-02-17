`%notin%` <- Negate(`%in%`)
library(readxl)
library(tidyverse)
#Function for making pretty y-axis breaks
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#read file into R
#Data file
pharm <- read_excel("Drugs_Data.xlsx")

####-------------------------Cleaning Time---------------------------------####
# clean up column names (essentially to lower)
pharm <- janitor::clean_names(pharm)
# still too lengthy
new_colnames <- c("id", "province", "date_loss", "report_date",
                  "brandname", "generic", "quant", "unit", 
                  "substance_data", "subs_quant","facility",
                  "incident_type", "incident_subtype",
                  "incident_other")
colnames(pharm) <- new_colnames
#Fixing the date to standard y-m-d
class(pharm$date_loss)
pharm <- pharm |> 
  mutate(across(contains("date"), ymd)) |> 
  mutate(across(all_of(c("facility", "brandname", "generic")), str_to_title),
         across(starts_with("incident"), str_to_title))

# I want to an a drug class column (e.g., opioids, stimulants, etc.) to
# the data - this makes it easier to filter, as well as a good colour variable
# This will allow us to match HC Drug Tables - picked via a bit of trial and error
pharm_new <- pharm |> 
  #filtering here to 2018 will speed things up
  filter(date_loss >= ymd("2018-01-01")) |> 
  #This will allow us to match the HC Data
  mutate(generic= case_when(
    # HC includes heroin, but not diacetylmorphine - so we'll fix that there
    str_detect(generic, "[Bb]uprenorphine") ~ "Buprenorphine",
    str_detect(generic, "[Aa]mph") &
      !str_detect(generic, "Lisdex|Dextro") ~ "Amphetamine",
    str_detect(generic, "Lisdexamphetamine") ~ "Lisdexamfetamine",
    is.character(generic) ~ generic
  )) |> 
  mutate(generic_match = str_extract(generic, "^[0-9A-Za-z\\-]+"),
         month_loss = floor_date(date_loss, unit = "months"))

#For the precursor chemicals/drugs that are 2 words
two_word_generic <- c("Acetone", "Methyl", "Hypophosphorous", 
                      "White", "7-Keto", "Sodium", "potassium", "Acetic")
pharm_new <- pharm_new |> 
  mutate(generic_match = str_replace_all(generic_match, 
                                         c("Diamorphine" = "Diacetylmorphine",
                                           "Esketamine" = "Ketamine"
                                         ))) |> 
  mutate(generic_match = ifelse(generic_match %in% two_word_generic, 
                                generic, generic_match))
# HC Drug Categorization Table
source("drug_class.R")
# Joining the drug class to the data
pharm_new <- pharm_new |> 
  left_join(drug_class, by = c("generic_match" = "drug_name"))

pharm_new |> 
  filter(pharma_class == "Opioids" & unit != "ML") |> 
  select(generic_match, quant) |> 
  group_by(generic_match) |>
  summarise(total_quant = sum(quant, na.rm = T)) |> 
  arrange(-total_quant)
  


####--------------------Data organizing/recoding--------------------------####

pharm_new <- pharm_new |> 
  select(id, province, month_loss, brandname, generic_match,
         quant, unit, substance_data, subs_quant, facility,
         incident_type, incident_subtype, incident_other,
         pharma_class, specific_class) |>
  filter(pharma_class %in% c("Sedatives/ Hypnotics", "Stimulants",
                               "Opioids","Dissociatives") &
           # I've limited the cases here to remove either the 0mg entries
           # or where half a pill/ML etc. goes missing
           quant >= 1) |> 
  mutate(grouped_cat = case_when(
    quant > 0 & quant <=50 ~      "1-50",
    quant > 50 & quant <= 99 ~   "51-99",
    quant >= 100 & quant < 500 ~   "100-499",
    quant >= 500 & quant < 1000 ~ "500-999",
    quant >= 1000 & quant < 1500 ~ "1000-1499",
    quant >= 1500 & quant < 2000 ~ "1500-1999",
    quant >= 2000 ~ "2000+",
    .default = as.character(quant)
    # One limit here is that 2000+ actually downplays how big
    # Some of these losses/thefts are (they can be up to 15K pills)
    # But most of the data is actually small quantities (check the histogram)
  )) |> 
  mutate(the_year = floor_date(month_loss, unit = "years")) |> 
  # The data distinguishes between Hospital & non hospital LTC - but the n is small
  mutate(facility = ifelse(str_detect(facility, "Long Term"),
                           "All LTC", 
                           facility))
# Turning the grouped category above into an ordered factor
pharm_new$grouped_cat <- factor(pharm_new$grouped_cat, 
                           levels = c("1-50",
                                      "51-99",
                                      "100-499",
                                      "500-999",
                                      "1000-1499",
                                      "1500-1999",
                                      "2000+"),
                           ordered = T)
levels(pharm_new$grouped_cat)

# Some of the data hasn't been filled out properly, so this is a quick fix
pharm_new$substance_data[which(is.na(pharm_new$substance_data))] <- 
  str_extract(pharm_new$brandname[which(is.na(pharm_new$substance_data))], 
              "[0-9]+ Mg/Tab")
# To see which facilities are most represented
plyr::count(pharm_new$facility)
# I've limited it to facilities with large quantities
tosearch <- c("Pharmacy", "Precursor Institution", 
              "Hospital", "Licensed Dealer - Ocs",
              "Dealer Producer", "All LTC")
# This is just a hold over from the HC Data, we're less interested in these opioids
notdrug <- c("Diphenoxylate", "Pentazocine", "Meperidine")
pharm_new <- pharm_new |> 
  filter(facility %in% tosearch &
           generic_match %notin% notdrug &
           specific_class != "Barbiturates" &
           # I've set > 100 here as an arbitrary limit, assuming that losses 
           # are not likely to be diversion if they're for such small amounts
           # that makes it a more conservative estimate
           quant >= 100 & unit %in% c("TAB", "CAP", "ML")) |> 
  mutate(substance_data = str_to_title(substance_data),
         subs_quant = as.numeric(subs_quant))

# See how this is broken down by province
plyr::count(pharm_new$province) |> 
  arrange(-freq)

pharm_new |> 
  filter(pharma_class == "Opioids") |> 
  group_by(province, generic_match) |> 
  count(name = "total_drugs") |> 
  arrange(-total_drugs) |>
  print(n = 30)
# Note that Ontario is in the top list here - we're going to focus on
# just AB & BC for now

# Limiting to just oxycodone & hydromorphone + BC & AB for now
prov_pick <- c("BC", "AB")
oxyhydro <- c("Hydromorphone", "Oxycodone")

abc_oh <- pharm_new |> 
  filter(generic_match %in% oxyhydro & province %in% prov_pick &
           unit == "TAB") |> 
  select(province, the_year, generic_match, quant,
         unit, substance_data, subs_quant, 
         facility, incident_type, incident_subtype, grouped_cat)
# See summary statistics of the various columns
abc_oh |> 
  select(generic_match, quant, unit, facility, incident_type, grouped_cat) %>%
  mutate(across(where(is.character), factor)) |> 
  summary()

####----------------Data Visualization and Analysis------------------------####
# Missing/Lost Drugs (of any kind)
# Let's make some graphs!!!
# 
# First two of these are incidence or occurrence graphs
# These are not necessarily separate events, e.g., someone might steal oxycodone
# and steal hydromorphone in a single robbery

full_theme <- function(){
  theme(plot.title = element_text(hjust = 0.5, size = 20),
                      axis.text = element_text(size = 12),
                      axis.title = element_text(size = 14),
                      plot.caption = element_text(size = 12),
                      legend.title=element_text(size=14),
                      legend.text = element_text(size = 12),
                      strip.text = element_text(size = 15)
  )
}


the_years <- unique(abc_oh$the_year)
loss_g <- abc_oh |> 
  filter(generic_match %in% oxyhydro
                          & incident_type == "Loss") |> 
  group_by(province, generic_match, the_year, grouped_cat) |> 
  count(name = "the_count") |> 
  ungroup() |> 
  mutate(grouped_cat = fct_rev(grouped_cat))

loss_max <- loss_g |> 
  group_by(province, the_year, generic_match) |> 
  summarise(max_count = sum(the_count)) |> 
  pull(max_count) |> 
  max()
ggplot(loss_g, aes(x = the_year, y = the_count, fill = grouped_cat)) +
  geom_col() +
  scale_x_date(date_labels = "%Y", breaks = the_years) +
  scale_y_continuous(expand = c(0,0), limits = c(0, loss_max +5)) +
  labs(x = "Year", y = "Number of Incidents", fill = "Quantity Reported",
       title = "Hydromorphone, Oxy, Morphine & Methadone 'Lost' in AB, BC & ON by Year", 
       caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
  theme_bw() +
  facet_grid(generic_match ~ province) +
  full_theme()
ggsave("G1-loss_g.png", width = 15, height = 10, dpi = 300)


# Theft of Drugs (of any kind)
theft <- abc_oh |> 
  filter(incident_type == "Theft") |> 
  group_by(province, generic_match, the_year, grouped_cat) |> 
  count(name = "the_count") |> 
  ungroup() |> 
  mutate(grouped_cat = fct_rev(grouped_cat))

stack_max <- theft |> 
  group_by(province, the_year, generic_match) |> 
  summarise(max_count = sum(the_count)) |> 
  pull(max_count) |> 
  max()

ggplot(theft, aes(x = the_year, y = the_count, fill = grouped_cat)) +
  geom_col() +
  scale_x_date(date_labels = "%Y", breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, stack_max+10)) +
  labs(x = "Year", y = "Number of Incidents", fill = "Quantity Reported",
       title = "Hydromorphone, Oxy, Morphine & Methadone Stolen in AB, BC & ON by Year", 
       caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
  theme_bw() +
  facet_grid(generic_match ~ province) +
  full_theme()
ggsave("G2-theft_g.png", width = 15, height = 10, dpi = 300)

# Now let's look at the total mass e.g., how many mg of drugs were lost/stolen
# By each category
# So for example, if 1000 10mg oxycodone pills were lost in 2023 the y axis
# should be 10 000 mg

plyr::count(abc_oh$unit)
npills <-abc_oh |> 
  filter(unit == "TAB") |> 
  mutate(prov_drug = paste(province, generic_match, sep = "-")) |>
  group_by(prov_drug, the_year, incident_type, substance_data) |> 
  summarise(total = sum(quant)) |>
  ungroup() |> 
  mutate(unit_type = as.numeric(str_extract(substance_data,
                                            "[0-9]+\\.*[0-9]*"))) |>
  arrange(unit_type) |> 
  mutate(substance_data = factor(substance_data),
         # standardized factor levels as integers
         unit_type = ifelse(unit_type == 2.5, 3, unit_type))

npills$substance_data <- factor(npills$substance_data, 
                                   levels= unique(npills$substance_data[order(npills$unit_type)]), 
                                   ordered=TRUE)
npills$prov_drug <- factor(npills$prov_drug,
                              levels = op_lev$lev, ordered = T)

pillmax <- npills |> 
  filter(incident_type == "Theft") |>
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()
# Number of Tablets
npills |> 
  select(-unit_type) |>
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "Number of Tablets", fill = "Tablet Strength",
       title = "Number of Tablets Lost/Stolen from Pharmacies in BC & AB by MG of Tablet",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
               breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, pillmax+10000),
                     breaks = integer_breaks(n = 6)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G3-total_pills.png", width = 15, height = 10, dpi = 300)
# Note I've only limited this to Tabs (n = 1547), 
# and excluded Capsules (n = 104) & ML (n = 67)
totweight <-abc_oh |> 
  mutate(prov_drug = paste(province, generic_match, sep = "-")) |>
  group_by(prov_drug, the_year, incident_type, substance_data) |> 
  summarise(total = sum(subs_quant)) |>
  ungroup() |> 
  mutate(unit_type = as.numeric(str_extract(substance_data,
                                            "[0-9]+\\.*[0-9]*"))) |>
  arrange(unit_type) |> 
  mutate(substance_data = factor(substance_data),
         # standardized factor levels as integers
         unit_type = ifelse(unit_type == 2.5, 3, unit_type)) 


# FACTORS :(
totweight$substance_data <- factor(totweight$substance_data, 
                                   levels= unique(totweight$substance_data[order(totweight$unit_type)]), 
                                   ordered=TRUE)
totweight <- totweight[, -c(6)] # Removing the column used to make factors
# I need to fix the y-axes here

op_lev <- data.frame(Prov = rep(prov_pick, length(oxyhydro)),
                     Drug = rep(oxyhydro, length(prov_pick))) |> 
  unite("lev", Prov:Drug, sep = "-", remove = F) |> 
  arrange(Drug, Prov)
op_lev <- crossing(prov_pick, oxyhydro) |> 
  unite("lev", prov_pick:oxyhydro, sep = "-", remove = F) |> 
  arrange(oxyhydro, prov_pick)


totweight$prov_drug <- factor(totweight$prov_drug,
                              levels = op_lev$lev, ordered = T)
# Find Max for y-axis
weightmax <- totweight |> 
  filter(incident_type == "Theft") |>
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()
# Quantity of Pills
totweight |> 
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "Total Mass (mg)", fill = "Tablet Strength",
       title = "Total MG of Opioids Lost/Stolen from Pharmacies in BC & AB by Year",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
              breaks = the_years) +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0, weightmax +100000)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G4-total_mass.png", width = 15, height = 10, dpi = 300)



####--------------------- BC + ON + AB ------------------------------------####
# Now let's compare BC + AB & ON just with the last 2
prov_pick <- c("BC", "AB", "ON")
# Note that here there are substantively more capsules missing, but for
# comparison purposes I've left it as tablets


# I've simplied the code from above just for speed
abcon <-  pharm_new |> 
  filter(generic_match %in% oxyhydro & province %in% prov_pick &
           unit == "TAB") |> 
  select(province, the_year, generic_match, quant,
         unit, substance_data, subs_quant, 
         facility, incident_type, incident_subtype, grouped_cat) |> 
  mutate(prov_drug = paste(province, generic_match, sep = "-"), 
         unit_type = as.numeric(str_extract(substance_data,
                                            "[0-9]+\\.*[0-9]*"))) |>
  arrange(unit_type) |> 
  mutate(substance_data = factor(substance_data),
         # standardized factor levels as integers
         unit_type = ifelse(unit_type == 2.5, 3, unit_type))

abcon$substance_data <- factor(abcon$substance_data, 
                                   levels= unique(abcon$substance_data[order(abcon$unit_type)]), 
                                   ordered=TRUE)

op_lev <- data.frame(Prov = rep(prov_pick, length(oxyhydro)),
                     Drug = rep(oxyhydro, length(prov_pick))) |> 
  unite("lev", Prov:Drug, sep = "-", remove = F) |> 
  arrange(Drug, Prov)
op_lev <- crossing(prov_pick, oxyhydro) |> 
  unite("lev", prov_pick:oxyhydro, sep = "-", remove = F) |> 
  arrange(oxyhydro, prov_pick)


abcon$prov_drug <- factor(abcon$prov_drug,
                              levels = op_lev$lev, ordered = T)

totweight <- abcon |> 
  group_by(prov_drug, the_year, incident_type, substance_data, unit_type) |> 
  summarise(total = sum(subs_quant)) |>
  ungroup() 

weightmax <- totweight |> 
  filter(incident_type == "Theft") |>
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()

totweight |> 
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "Total Mass (mg)", fill = "Tablet Strength",
       title = "Total MG of Opioids Lost/Stolen from Pharmacies in BC, AB & ON by Year",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
               breaks = the_years) +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0, weightmax +100000)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G5-total_weightBCONAB.png", width = 20, height = 10, dpi = 300)


npills <- abcon |> 
  group_by(prov_drug, the_year, incident_type, substance_data, unit_type) |> 
  summarise(total = sum(quant)) |>
  ungroup() 
pillmax <- npills |> 
  filter(incident_type == "Theft") |>
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()
npills |> 
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "Number of Tablets", fill = "Tablet Strength",
       title = "Number of Tablets Lost/Stolen from Pharmacies in AB, BC & ON by MG/Tablet",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
               breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, pillmax+10000),
                     breaks = integer_breaks(n = 6)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G6-total_pillsBCONAB.png", width = 20, height = 10, dpi = 300)


#####----------------- BC + AB + ON by Population--------------------------#####
#
# Because this area is basially just the same code as above I haven't added
# as extensive comments
# The only major difference is that i take the population for each province
# And then get the rate per 100 000 people
#
source("pop_cleaner.R")
abcon <- abcon |> 
  left_join(pop)

totweight <- abcon |> 
  group_by(province, prov_drug, the_year, incident_type, 
           substance_data, unit_type) |> 
  summarise(total = sum(subs_quant)/unique(population)*100000) |>
  ungroup() 


weightmax <- totweight |> 
  filter(incident_type == "Theft") |>
  filter(province != "ON") |> 
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()

totweight |> 
  filter(province != "ON") |> 
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "MG per 100000 people", fill = "Tablet Strength",
       title = "Total MG of Opioids Lost/Stolen from Pharmacies in BC & AB by Year",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
               breaks = the_years) +
  scale_y_continuous(labels = scales::comma,
                     expand = c(0,0),
                     limits = c(0, weightmax+100)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G7-total_weightBCABpop.png", width = 20, height = 10, dpi = 300)

npills <- abcon |> 
  group_by(prov_drug, the_year, incident_type, substance_data, unit_type) |> 
  summarise(total = sum(quant)/unique(population)*100000) |>
  ungroup() 
pillmax <- npills |> 
  filter(incident_type == "Theft") |>
  group_by(the_year,prov_drug) |> 
  summarise(ymax = sum(total)) |> 
  pull(ymax) |> 
  max()
npills |> 
  mutate(grouped_cat = fct_rev(substance_data)) %>%
  ggplot(., aes(x = the_year, y = total, fill = substance_data)) +
  geom_col() +
  labs(x = "Year", y = "Number of Tablets per 100 000 people", 
       fill = "Tablet Strength",
       title = "Number of Tablets Lost/Stolen from Pharmacies in AB, BC & ON by Population",
       caption = "Limited to LTC, Licensed Dealers/Producers, Hospitals & Pharmacies
       Only includes report where 100+ Units went missing/stolen") +
  scale_x_date(date_labels = "%Y", expand = c(0,0),
               breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, pillmax+100),
                     breaks = integer_breaks(n = 6)) +
  theme_bw() +
  facet_grid(incident_type ~prov_drug) +
  full_theme()
ggsave("G8-total_pillsBCONABpop.png", width = 20, height = 10, dpi = 300)

abcon_lg <- pharm_new |> 
  filter(generic_match %in% oxyhydro & province %in% prov_pick &
           unit == "TAB") |> 
  select(province, the_year, generic_match, quant,
         unit, substance_data, subs_quant, 
         facility, incident_type, incident_subtype, grouped_cat) |> 
  left_join(pop)
loss_g <- abcon_lg |> 
  filter(generic_match %in% oxyhydro
         & incident_type == "Loss") |> 
  group_by(province, generic_match, the_year, grouped_cat) |> 
  summarise(the_count = n()/unique(population)*100000) |>
  ungroup() |> 
  mutate(grouped_cat = fct_rev(grouped_cat))

loss_max <- loss_g |> 
  group_by(province, the_year, generic_match) |> 
  summarise(max_count = sum(the_count)) |> 
  pull(max_count) |> 
  max()
ggplot(loss_g, aes(x = the_year, y = the_count, fill = grouped_cat)) +
  geom_col() +
  scale_x_date(date_labels = "%Y", breaks = the_years) +
  scale_y_continuous(expand = c(0,0), limits = c(0, loss_max)) +
  labs(x = "Year", y = "Number of Incidents per 100 000 people",
       fill = "Quantity Reported",
       title = "Hydromorphone & Oxycodone 'Lost' in AB, BC & ON by Year", 
       caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
  theme_bw() +
  facet_grid(generic_match ~ province) +
  full_theme()
ggsave("G9-loss_g per capita.png", width = 15, height = 10, dpi = 300)


# Theft of Drugs (of any kind)
theft <- abcon_lg|> 
  filter(incident_type == "Theft") |> 
  group_by(province, generic_match, the_year, grouped_cat) |> 
  summarise(the_count = n()/unique(population)*100000) |>
  ungroup() |> 
  mutate(grouped_cat = fct_rev(grouped_cat)) 

stack_max <- theft |> 
  group_by(province, the_year, generic_match) |> 
  summarise(max_count = sum(the_count)) |> 
  pull(max_count) |> 
  max()

ggplot(theft, aes(x = the_year, y = the_count, fill = grouped_cat)) +
  geom_col() +
  scale_x_date(date_labels = "%Y", breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, stack_max)) +
  labs(x = "Year", y = "Number of Incidents per 100 000 people", fill = "Quantity Reported",
       title = "Hydromorphone, Oxycodone Stolen in AB, BC & ON by Year", 
       caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
  theme_bw() +
  facet_grid(generic_match ~ province) +
  full_theme()
ggsave("G10-theft_g.png", width = 15, height = 10, dpi = 300)

ls_st <- abcon_lg|> 
  group_by(province, generic_match, the_year, grouped_cat) |> 
  summarise(the_count = n()/unique(population)*100000) |>
  ungroup() |> 
  mutate(grouped_cat = fct_rev(grouped_cat)) 

stack_max <- ls_st |> 
  group_by(province, the_year, generic_match) |> 
  summarise(max_count = sum(the_count)) |> 
  pull(max_count) |> 
  max()

ggplot(ls_st, aes(x = the_year, y = the_count, fill = grouped_cat)) +
  geom_col() +
  scale_x_date(date_labels = "%Y", breaks = the_years) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, stack_max+1)) +
  labs(x = "Year", y = "Number of Incidents per 100 000 people", fill = "Quantity Reported",
       title = "Hydromorphone, Oxycodone Lost/Stolen in AB, BC & ON by Year Per Capita", 
       caption = "Limited to (All) Long Term Care, Licensed Dealers/Producers, Hospitals & Pharmacies") +
  theme_bw() +
  facet_grid(generic_match ~ province) +
  full_theme()
ggsave("G11-ls_st_g.png", width = 15, height = 10, dpi = 300)

