# Modifications to the Health Canada spreadsheet
# Probably easier to do this by hand, but just for accountability sake
drug_class <- read.csv("Health_Canada_Classification.csv")
head(drug_class)

# Note that the Health Canada list is in title form & doesn't distinguish
# between salts and freebase - for the purposes of classification
# we'll need to remove that data - for cleaning see "Drug_class.R"


drug_class <- drug_class |> 
  select(ends_with("EN"), Act:Schedule)


colnames(drug_class) <- c("drug_name", "specific_class", "pharma_class",
                          "act", "schedule")
# We don't need most of this, so to speed up matching we'll filter by
# what is in the dataset

p_unique <- c(unique(pharm_new$generic_match),
              unique(pharm_new$generic))

amph <- drug_class %>%
  filter(drug_name == "Amphetamine") |> 
  mutate(drug_name = str_replace(drug_name, 
                                 "Amphetamine",
                                 "Dextroamphetamine"))
# Fixing from the other side the table           
drug_class2 <- drug_class |> 
  mutate(drug_name = str_replace_all(drug_name, "Heroin",
                                     "Diacetylmorphine")) |>
  add_row(amph) |> 
  filter(drug_name %in% p_unique)

p_unique <- unique(pharm_new$generic_match[which(!is.na(pharm_new$generic_match))])

missing <- p_unique[p_unique %notin% drug_class2$drug_name]
# We can see that there's a few things missing here - we'll add them in manually

missing
generic_miss <- data.frame(
  drug_name = missing
) |> 
  mutate(specific_class = case_when(
    str_detect(drug_name, "Methadone") ~ "Opioids",
    str_detect(drug_name, "[Cc]anna") ~ "Cannabinoids & derivatives",
    str_detect(drug_name, "[Aa]ndro|osterone") ~ "Steroids",
    str_detect(drug_name, "pam$") ~ "Benzodiazepines",
    str_detect(drug_name, "phanol|morph") ~ "Opioids",
    str_detect(drug_name, "fent") ~ "Fentanyl & analogues",
    #Note this last one only works b/c of the limited drugs w/o a class
    str_detect(drug_name, "dine|phine") ~ "Other opioids",
    TRUE ~ NA
  ))
#to see what's left (I think it's sufficient here)
leftovers <- generic_miss$drug_name[which(is.na(generic_miss$specific_class))]

nrow(pharm_new[pharm_new$generic_match %in% leftovers,])/nrow(pharm_new)
nrow(pharm_new[pharm_new$generic_match %in% leftovers,])
#Only 1549/444045 rows is pretty good


#Ok, to put the whole thing together

drug_sched <- drug_class |> 
  distinct(specific_class, .keep_all = TRUE) |> 
  select(-drug_name)

# Bounced back and forth on whether it should be "other" or "U/K"
# Went with the latter because it doesn't confuse HC categories
generic_miss <- generic_miss |> 
  left_join(drug_sched, by = "specific_class") 

drug_class <- drug_class2 |>
  bind_rows(generic_miss) |> 
  mutate(across(contains("class"), ~ifelse(is.na(.x) |
                                             .x == "Other", 
                                           "Other/ U/K", .x)) ) |> 
  distinct(drug_name, .keep_all = TRUE)

rm(drug_class2, generic_miss, amph, 
   drug_sched, p_unique, missing, leftovers)

