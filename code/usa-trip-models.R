## Predictors:

##### Trip
# Trip purpose
    # Home-school
    # Home-other
    # School-other
    # Other-other
# Trip distance
# Time of day
    # Morning, Afternoon, Evening, Night
# Max Origin/Destination density

##### Person (child)
#x Age
#x Sex
#x Race
#x Birth order (only, oldest, youngest, middle)

##### Household
#x Income
#x Highest education
#x Number of household vehicles per driver
#x Number of licensed drivers
#x Number of children

## Outcome:
#x Trip is independent
#x Trip is active
#x Active trip is independent
#x Independent trip is active

## Load trip data

library(here)
library(tidyverse)
library(jtools)
library(treemapify)

trip_path <- here("NHTS-2017", "trippub.csv")
person_path <- here("NHTS-2017", "perpub.csv")
year <- 2017

people <- person_path %>%
  read_csv() 

hh_ed_kid_counts <- people %>%
  mutate(adult = R_AGE > 17,
         kid_age = ifelse(R_AGE < 18, R_AGE, -2),
         R_AGE = ifelse(R_AGE < 0, 999, R_AGE)) %>%
  group_by(HOUSEID) %>%
  mutate(highest_ed = max(as.numeric(EDUC)),
            n_adults = sum(adult),
            num_records = n()) %>%
  mutate(youngest_kid = ifelse(num_records > HHSIZE, 1, min(R_AGE)),
         oldest_kid = max(kid_age),
         n_children = HHSIZE - n_adults) %>%
  mutate(birth_order = case_when(R_AGE > 17 ~ "adult",
                                 n_children == 1 ~ "only",
                                 R_AGE == oldest_kid ~ "oldest",
                                 R_AGE == youngest_kid ~ "youngest",
                                 TRUE ~ "middle")) %>%
  mutate(has_young_sib = birth_order == "middle" | birth_order == "oldest",
         has_old_sib = birth_order == "middle" | birth_order == "youngest") %>%
  mutate(highest_ed = case_when(highest_ed == 1 ~ "Less than high school",
                                highest_ed == 2 ~ "High school",
                                highest_ed == 3 ~ "Some college",
                                highest_ed == 4 ~ "Bachelor's degree",
                                highest_ed == 5 ~ "Graduate degree",
                                TRUE ~ "missing")) %>%
  filter(n_children > 0) 

hh_ages <- people %>%
  select(HOUSEID, PERSONID, R_AGE) %>%
  pivot_wider(names_from = PERSONID,
              names_prefix = "age_",
              values_from = R_AGE)

trips <- trip_path %>%
  read_csv() %>%
  mutate(is_bike = TRPTRANS == "02",
         is_walk = TRPTRANS == "01") %>%
  mutate(is_active = is_bike | is_walk) %>%
  left_join(people) %>%
  left_join(hh_ed_kid_counts) %>%
  left_join(hh_ages) %>%
  mutate(alone = NUMONTRP == 1,
         with_hh_adult = 
           (as.numeric(ONTD_P1) == 1 & age_01 > 17) |
           (as.numeric(ONTD_P2) == 1 & age_02 > 17) |
           (as.numeric(ONTD_P3) == 1 & age_03 > 17) |
           (as.numeric(ONTD_P4) == 1 & age_04 > 17) |
           (as.numeric(ONTD_P5) == 1 & age_05 > 17) |
           (as.numeric(ONTD_P6) == 1 & age_06 > 17) |
           (as.numeric(ONTD_P7) == 1 & age_07 > 17) |
           (as.numeric(ONTD_P8) == 1 & age_08 > 17) |
           (as.numeric(ONTD_P9) == 1 & age_09 > 17) |
           (as.numeric(ONTD_P10) == 1 & age_10 > 17) |
           (as.numeric(ONTD_P11) == 1 & age_11 > 17) |
           (as.numeric(ONTD_P12) == 1 & age_12 > 17) |
           (as.numeric(ONTD_P13) == 1 & age_13 > 17)) %>%
  mutate(vehs_per_driver = ifelse(DRVRCNT > 0, HHVEHCNT/DRVRCNT, 0)) %>%
  mutate(home_based = WHYFROM == "01" |
           WHYFROM == "02" |
           WHYTO == "01" |
           WHYTO == "02",
         school_based = WHYFROM == "08" |
           WHYFROM == "09" |
           WHYTO == "08" |
           WHYTO == "09") %>%
  mutate(purpose = case_when(home_based & school_based ~ "home-school",
                             home_based ~ "home-other",
                             school_based ~ "school-other",
                             TRUE ~ "other-other")) %>%
  mutate(hour_of_day = as.numeric(substr(STRTTIME, 1, 2))) %>%
  mutate(time_of_day = case_when(hour_of_day < 7 ~ "Night",
                                 hour_of_day < 13 ~ "Morning",
                                 hour_of_day < 18 ~ "Afternoon",
                                 hour_of_day < 22 ~ "Evening",
                                 TRUE ~ "Night")) %>%
  mutate(max_od_dens = ifelse(DBPPOPDN > OBPPOPDN, DBPPOPDN, OBPPOPDN)) %>%
  mutate(veh_avail = case_when(vehs_per_driver == 0 ~ "no_car",
                               vehs_per_driver < 1 ~ "car_lite",
                               vehs_per_driver == 1 ~ "one_per_driver",
                               TRUE ~ "excess_cars")) %>%
  mutate(num_drivers = case_when(DRVRCNT == 0 ~ "None",
                                 DRVRCNT == 1 ~ "One",
                                 DRVRCNT == 2 ~ "Two",
                                 TRUE ~ "More than two")) %>%
  mutate(race = case_when(R_HISP == "01" ~ "Hispanic",
                          R_RACE == "01" ~ "Non-Hispanic white",
                          R_RACE == "02" ~ "Non-Hispanic Black",
                          R_RACE == "03" ~ "Non-Hispanic Asian",
                          TRUE ~ "Other")) %>%
  mutate(time_of_day = fct_infreq(time_of_day),
         purpose = fct_infreq(purpose),
         num_drivers = fct_infreq(num_drivers),
         veh_avail = fct_infreq(veh_avail),
         race = fct_infreq(race),
         highest_ed = fct_relevel(highest_ed, 
                                  "Bachelor's degree", after = 0)) %>%
  mutate(income_k = case_when(HHFAMINC == "01" ~ 5,
                              HHFAMINC == "02" ~ 12.5,
                              HHFAMINC == "03" ~ 20,
                              HHFAMINC == "04" ~ 30,
                              HHFAMINC == "05" ~ 42.5,
                              HHFAMINC == "06" ~ 62.5,
                              HHFAMINC == "07" ~ 87.5,
                              HHFAMINC == "08" ~ 112.5,
                              HHFAMINC == "09" ~ 137.5,
                              HHFAMINC == "10" ~ 175,
                              HHFAMINC == "11" ~ 250,
                              TRUE ~ -9)) %>%
    mutate(female = R_SEX_IMP == "02") %>%
  filter(income_k > 0,
         TRPMILES > 0,
         max_od_dens > 0,
         R_AGE < 15,
         R_AGE < 7) %>%
  select(alone,
         with_hh_adult,
         is_bike,
         is_walk,
         is_active,
         TRPMILES,
         TRPMILAD,
         purpose,
         time_of_day,
         max_od_dens,
         veh_avail,
         num_drivers, 
         income_k,
         highest_ed,
         n_children,
         R_AGE,
         female,
         race,
         birth_order,
         has_old_sib,
         has_young_sib,
         HOUSEID)

ggplot(trips) +
  geom_histogram(aes(x = TRPMILES)) +
  scale_x_continuous(trans = "log")

trips %>%
  group_by(purpose) %>%
  summarise(count = n()) %>%
  mutate(label = paste0(purpose, 
                        "\n(",
                        round(count*100/sum(count)),
                        "%)")) %>%
ggplot(aes(area = count, label = label)) +
  geom_treemap() +
  geom_treemap_text()

trips %>%
  group_by(time_of_day) %>%
  summarise(count = n()) %>%
  mutate(label = paste0(time_of_day, 
                        "\n(",
                        round(count*100/sum(count)),
                        "%)")) %>%
  ggplot(aes(area = count, label = label)) +
  geom_treemap() +
  geom_treemap_text()

ggplot(trips) +
  geom_histogram(aes(x = max_od_dens), bins = 8) +
  scale_x_continuous(trans = "log")


ggplot(trips) +
  geom_histogram(aes(x = income_k), bins = 15) +
  scale_x_continuous(trans = "log")

trips %>%
  group_by(veh_avail) %>%
  summarise(count = n()) %>%
  mutate(label = paste0(veh_avail, 
                        "\n(",
                        round(count*100/sum(count)),
                        "%)")) %>%
  ggplot(aes(area = count, label = label)) +
  geom_treemap() +
  geom_treemap_text()

# Will a trip be independent?

model_ind <- glm(!with_hh_adult ~ 
                   log(TRPMILES) +
                   purpose +
                   time_of_day +
                   log(max_od_dens) +
                   veh_avail +
                   num_drivers +
                   log(income_k) +
                   highest_ed +
                   n_children +
                   R_AGE +
                   female +
                   race +
                   has_young_sib +
                   has_old_sib,
                 data = trips,
                 family = "binomial")

model_active <- glm(is_active ~ 
                   log(TRPMILES) +
                   purpose +
                   time_of_day +
                   log(max_od_dens) +
                   veh_avail +
                   num_drivers +
                   log(income_k) +
                   highest_ed +
                   n_children +
                   R_AGE +
                   female +
                   race +
                     has_young_sib +
                     has_old_sib,
                 data = trips,
                 family = "binomial")

export_summs(model_ind, model_active,
             scale = TRUE,
             cluster = "HOUSEID", 
             robust = "HC1",
             error_pos = "right", 
             error_format = "p = {p.value}")
