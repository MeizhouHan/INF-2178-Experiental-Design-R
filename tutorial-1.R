#### Preamble ####
# Purpose: Provide a skeleton of code for the Week 2 tutorial based on Sharla 
# Gelfand's presentation: https://sharla.party/talk/2019-10-24-uoft-brown-bag/
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Last updated: 16 January 2020


#### Set up workspace ####
# Call libraries
library(tidyverse)
library(janitor)
library(tidyr)
library(stringr)


#### Read in data ####
# Read in 2015 data 
ridings_results_2015_raw <- read_csv("https://raw.githubusercontent.com/sharlagelfand/talks/master/2019-10-24-uoft-brown-bag/data/results_2015.csv")

# Basic cleaning
ridings_results_2015 <- ridings_results_2015_raw %>%
  select(
    `Electoral District Name/Nom de circonscription`,
    `Elected Candidate/Candidat élu`
  )

# Read in 2019 data 
ridings_results_2019_raw <- read_tsv("https://raw.githubusercontent.com/sharlagelfand/talks/master/2019-10-24-uoft-brown-bag/data/results_2019.txt", skip = 1)

# Basic cleaning
ridings_results_2019_raw <- slice(ridings_results_2019_raw, -n()) # Get rid of the last line

ridings_results_2019 <- ridings_results_2019_raw %>%
  select(
    `Electoral district`,
    `Political affiliation`
  )


#### Prepare to merge the data ####
# Make the row names easier to deal with
ridings_results_2015 <- ridings_results_2015 %>%
  clean_names()
ridings_results_2019 <- ridings_results_2019 %>%
  clean_names()

# Make ridings consistent
ridings_results_2015 <- ridings_results_2015 %>%
  separate(electoral_district_name_nom_de_circonscription,
           into = "riding",
           sep = "/") # Don't worry about the warning - we want that behaviour
#English riding name is everything before the first "/" (if there is one)

ridings_results_2015 <- ridings_results_2015 %>%
  arrange(riding)

#arrange 排序
ridings_results_2019 <- ridings_results_2019 %>%
  rename(riding = electoral_district) %>%
  arrange(riding)
all(ridings_results_2015[["riding"]] == ridings_results_2019[["riding"]])

#❌ Ensure the ridings names from the two data sets match
ridings_results_2015 %>%
  select(riding_2015 = riding) %>%
  bind_cols(ridings_results_2019 %>%
              select(riding_2019 = riding)) %>%
  filter(riding_2015 != riding_2019)

ridings_results_2015 <- ridings_results_2015 %>%
  mutate(riding = str_replace_all(riding, "--", "-")) %>%
  arrange(riding)

ridings_results_2019 <- ridings_results_2019 %>%
  mutate(riding = str_replace_all(riding, "--", "-")) %>%
  arrange(riding)

#✅ Ensure the ridings names from the two data sets match
all(ridings_results_2015[["riding"]] == ridings_results_2019[["riding"]])

# Make party consistent
ridings_results_2015 %>%
  select(elected_candidate_candidat_elu)

#Getting the party name in French would be easy - everything after the slash
ridings_results_2015 %>%
  separate(
    elected_candidate_candidat_elu,
    into = c("candidate_and_english_party", "french_party"),
    sep = "/"
  ) %>%
  count(french_party)

ridings_results_2015 <- ridings_results_2015 %>%
  mutate(party = case_when(
    str_detect(elected_candidate_candidat_elu, "Conservative") ~ "Conservative",
    str_detect(elected_candidate_candidat_elu, "NDP") ~ "NDP-New Democratic Party",
    str_detect(elected_candidate_candidat_elu, "Liberal") ~ "Liberal",
    str_detect(elected_candidate_candidat_elu, "Bloc Québécois") ~ "Bloc Québécois",
    str_detect(elected_candidate_candidat_elu, "Green Party") ~ "Green Party"
  )) %>%
  select(-elected_candidate_candidat_elu)
head(ridings_results_2015)

ridings_results_2019 <- ridings_results_2019 %>%
  rename(party = political_affiliation)

#### Merge the datasets ####
riding_results <- ridings_results_2015 %>%
  full_join(ridings_results_2019,
            by = "riding",
            suffix = c("_2015", "_2019")
  )
head(riding_results)

#How many ridings elected a different party in 2019 than they did in 2015?
different_parties <- riding_results %>%
  filter(party_2015 != party_2019)
different_parties

library(glue)
different_parties <- different_parties %>%
  mutate(party_change = glue("{party_2015} to {party_2019}"))
different_parties["party_change"]

different_parties %>%
  tabyl(party_change) %>% #count number
  adorn_pct_formatting() %>% #percentage
  arrange(-n)

#### Make a graph ####
# Change to long format so we can use ggplot
riding_results_long <- 
  riding_results %>% 
  pivot_longer(cols = party_2015:party_2019, names_to = "year")

riding_results_long <- 
  riding_results_long %>%
  mutate(year = case_when(
    str_detect(year, "party_2019") ~ "2019",
    str_detect(year, "party_2015") ~ "2015")
  )

riding_results_long %>% 
  ggplot(aes(x = year, y = riding, color = value)) +
  geom_point()
# Make a bar chart
#coord_flip()横纵坐标换位置
riding_results_long %>% 
  ggplot(aes(x = value, fill = year)) +
  geom_bar(position = "dodge") +#分开2015，2019， otherwise 就连在一起
  coord_flip() +
  labs(x = "Party", 
       y = "Number of seats",
       title = "Number of seats won by major Canadian parties at the 2015 and 2019 elections")

