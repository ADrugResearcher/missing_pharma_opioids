# Missing Pharma Opioids
An examination of the data of missing pharma opioids in BC, AB &amp; Ontario

Data exploration based on the work of CBC data journalist Tara Carman https://github.com/taracarman/druglosses

The R Code below produces multiple graphs that look at hydromorphone and oxycodone that has gone missing, either via theft or loss from pharmacies in British Columbia, Alberta and (to a lesser extent) Ontario from 2018-2023

I've tried to make the code as accessible as possible to follow along, although a basic understanding of R & tidyverse is probably required. I've made certain choices to group together variables (such as all long term care) together, and for the most part to restrict the analysis just to tablets. Running the code line-by-line should give you the information I've provided as well as certain summary statistics that shaped the analysis.

**Variables in the original dataset (renamed by me for ease):**
ID: Basically the incident report, so if a pharmacy was robbed and lost 10 hydromorphone pills, they probably also had 10 oxy's stolen etc.
province: self-explanatory
date_loss = date items lost
report_date = when was it filed
Brand name: Drug Brand name, sometimes includes the unit
Generic: what the generic name of the substance is
Quant: How many units were taken
Unit: what is it measured in, e.g., pouches, tablets, capsules, ML
Substance_data: Mg/unit
subs_quant: quant x substance_data
facility: location
incident_type: Loss/theft
incident_subtype: breakdown of the type of event
incident_other: in a few cases there is greater explanation

**Variables I've Added Using other datasets**
pharma_class: e.g., opioid, stimulant
specific_class: subclass
population: taken from Statistics Canada's population estimates. I've used the beginning of the year for calculating per capita values

**Secondary Data Sources:**
They're included here under fair use, but the original data is from:
Drug Classification: https://health-infobase.canada.ca/src/data/DAS/newUpdate/newLayout/20241016_Classification_List.csv

Statistics Canada Population Estimates quarterly: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901
