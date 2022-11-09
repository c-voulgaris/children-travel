#########
# Assembling data from NHTS

library(tidyverse)
library(here)
library(survey)
library(srvyr)

indie_active_summary <-
  function(person_path, trip_path, year) {

    people <- person_path %>%
      read_csv() %>%
      select(HOUSEID, # Household identifier
             PERSONID, # Person identifier (within household)
             R_AGE,   # Age
             WTPERFIN) # Weight (person)

    hh_ages <- people %>%
      select(HOUSEID, PERSONID, R_AGE) %>%
      pivot_wider(names_from = PERSONID,
                  names_prefix = "age_",
                  values_from = R_AGE)

    people <- people %>%
      filter(R_AGE < 18,
             R_AGE > 0)

    if (year == 2017) {
      trips <- trip_path %>%
        read_csv() %>%
        mutate(is_bike = TRPTRANS == "02",
               is_walk = TRPTRANS == "01",
               is_school_bus = TRPTRANS == "10",
               is_transit = (TRPTRANS == "11" | # Public or commuter bus
                               TRPTRANS == "16"), # Rail
               is_car = (TRPTRANS == "03" | # Car
                         TRPTRANS == "04" | # SUV
                         TRPTRANS == "05" | # Van
                         TRPTRANS == "06"))  # Pickup truck
    }
    else if (year == 2001) {
      trips <- trip_path %>%
        read_csv() %>%
        mutate(is_bike = TRPTRANS == "25",
               is_walk = TRPTRANS == "26",
               is_school_bus = TRPTRANS == "12",
               is_transit = (TRPTRANS == "10" | # local bus
                               TRPTRANS == "11" | # commuter bus
                               TRPTRANS == "16" | # commuter Rail
                               TRPTRANS == "17" | # Subway
                               TRPTRANS == "18"), # streetcar/trolley
               is_car = (TRPTRANS == "01" | # Car
                           TRPTRANS == "02" | # Van
                           TRPTRANS == "03" | # SUV
                           TRPTRANS == "04"))  # Pickup truck      
    }
    else  { # 2009
      trips <- trip_path %>%
        read_csv() %>%
        mutate(is_bike = TRPTRANS == "22",
               is_walk = TRPTRANS == "23",
               is_school_bus = TRPTRANS == "11",
               is_transit = (TRPTRANS == "09" | # local bus
                               TRPTRANS == "10" | # commuter bus
                               TRPTRANS == "16" | # commuter Rail
                               TRPTRANS == "17" | # Subway
                               TRPTRANS == "18"), # streetcar/trolley
               is_car = (TRPTRANS == "01" | # Car
                           TRPTRANS == "02" | # Van
                           TRPTRANS == "03" | # SUV
                           TRPTRANS == "04"))  # Pickup truck      
    }
    
    trips %>%
      select(HOUSEID, # Household identifier 
             PERSONID, # Person identifier (within household)
             TRPTRANS, # Mode of transportation
             NUMONTRP, # Number of people on trip
             R_AGE, # Age
             ONTD_P1, # person 1 on this trip
             ONTD_P2,
             ONTD_P3,
             ONTD_P4,
             ONTD_P5,
             ONTD_P6,
             ONTD_P7,
             ONTD_P8,
             ONTD_P9,
             ONTD_P10,
             ONTD_P11,
             ONTD_P12,
             ONTD_P13,
             is_bike,
             is_walk,
             is_transit,
             is_car) %>%  
      mutate(is_trip = ifelse(is.na(TRPTRANS), FALSE, TRUE)) %>%
      filter(R_AGE < 18,
             R_AGE > 0) %>%
      full_join(people) %>%
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
      mutate(bike_alone = is_bike & alone,
             bike_indie = is_bike & !with_hh_adult,
             walk_alone = is_walk & alone,
             walk_indie = is_walk & !with_hh_adult,
             car_alone = is_car & alone,
             car_indie = is_car & !with_hh_adult,
             transit_alone = is_transit & alone,
             transit_indie = is_transit & !with_hh_adult)  %>%
      group_by(HOUSEID, PERSONID, R_AGE, WTPERFIN) %>%
      summarise(trips = sum(is_trip, na.rm = TRUE),
                walk_trips = sum(is_walk, na.rm = TRUE),
                bike_trips = sum(is_bike, na.rm = TRUE),
                car_trips = sum(is_car, na.rm = TRUE),
                alone_trips = sum(alone, na.rm = TRUE),
                indie_trips = sum(!with_hh_adult, na.rm = TRUE),
                transit_trips = sum(is_transit, na.rm = TRUE),
                walk_alone_trips = sum(walk_alone, na.rm = TRUE),
                bike_alone_trips = sum(bike_alone, na.rm = TRUE),
                car_alone_trips = sum(car_alone, na.rm = TRUE),
                transit_alone_trips = sum(transit_alone, na.rm = TRUE),
                walk_indie_trips = sum(walk_indie, na.rm = TRUE),
                bike_indie_trips = sum(bike_indie, na.rm = TRUE),
                car_indie_trips = sum(car_indie, na.rm = TRUE),
                transit_indie_trips = sum(transit_indie, na.rm = TRUE)) %>%
      mutate(any_trips = trips > 0,
             any_bike = bike_trips > 0,
             any_walk = walk_trips > 0, 
             any_car = car_trips > 0,
             any_transit = transit_trips > 0,
             any_alone = alone_trips > 0,
             any_indie = indie_trips > 0,
             any_alone_walk = walk_alone_trips > 0,
             any_alone_bike = bike_alone_trips > 0,
             any_alone_transit = transit_alone_trips > 0,
             any_alone_car = car_alone_trips > 0,
             any_indie_walk = walk_indie_trips > 0,
             any_indie_bike = bike_indie_trips > 0,
             any_indie_transit = transit_indie_trips > 0,
             any_indie_car = car_indie_trips > 0) %>%
      mutate(age_group = case_when(R_AGE < 10 ~ "age_05_09",
                                   R_AGE > 15 ~ "age_16_17",
                                   TRUE ~ "age_10_15")) %>%
      as_survey_design(weight = WTPERFIN) %>%
      group_by(age_group) %>%
      summarise(pct_any_trip = survey_mean(any_trips),
                avg_trips = survey_mean(trips),
                pct_alone = survey_mean(any_alone),
                pct_indie = survey_mean(any_indie),
                pct_walk = survey_mean(any_walk),
                pct_bike = survey_mean(any_bike),
                pct_transit = survey_mean(any_transit),
                pct_car = survey_mean(any_car),
                pct_alone_walk = survey_mean(any_alone_walk),
                pct_alone_bike = survey_mean(any_alone_bike),
                pct_alone_transit = survey_mean(any_alone_transit),
                pct_alone_car = survey_mean(any_alone_car),
                pct_indie_walk = survey_mean(any_indie_walk),
                pct_indie_bike = survey_mean(any_indie_bike),
                pct_indie_transit = survey_mean(any_indie_transit),
                pct_indie_car = survey_mean(any_indie_car)) %>%
      
      mutate(pct_of_walk_alone = pct_alone_walk / pct_walk) %>%
      mutate(pct_of_walk_alone_se = pct_of_walk_alone * 
               ((pct_alone_walk_se/pct_alone_walk)^2+
                  (pct_walk_se/pct_walk)^2)^0.5) %>%
      
      mutate(pct_of_bike_alone = pct_alone_bike / pct_bike) %>%
      mutate(pct_of_bike_alone_se = pct_of_bike_alone * 
               ((pct_alone_bike_se/pct_alone_bike)^2+
                  (pct_bike_se/pct_bike)^2)^0.5) %>%
      
      mutate(pct_of_transit_alone = pct_alone_transit / pct_transit) %>%
      mutate(pct_of_transit_alone_se = pct_of_transit_alone * 
               ((pct_alone_transit_se/pct_alone_transit)^2+
                  (pct_transit_se/pct_transit)^2)^0.5) %>%
      
      mutate(pct_of_car_alone = pct_alone_car / pct_car) %>%
      mutate(pct_of_car_alone_se = pct_of_car_alone * 
               ((pct_alone_car_se/pct_alone_car)^2+
                  (pct_car_se/pct_car)^2)^0.5) %>%
      
      mutate(pct_of_walk_indie = pct_indie_walk / pct_walk) %>%
      mutate(pct_of_walk_indie_se = pct_of_walk_indie * 
               ((pct_indie_walk_se/pct_indie_walk)^2+
                  (pct_walk_se/pct_walk)^2)^0.5) %>%
      
      mutate(pct_of_bike_indie = pct_indie_bike / pct_bike) %>%
      mutate(pct_of_bike_indie_se = pct_of_bike_indie * 
               ((pct_indie_bike_se/pct_indie_bike)^2+
                  (pct_bike_se/pct_bike)^2)^0.5) %>%
      
      mutate(pct_of_transit_indie = pct_indie_transit / pct_transit) %>%
      mutate(pct_of_transit_indie_se = pct_of_transit_indie * 
               ((pct_indie_transit_se/pct_indie_transit)^2+
                  (pct_transit_se/pct_transit)^2)^0.5) %>%
      
      mutate(pct_of_car_indie = pct_indie_car / pct_car) %>%
      mutate(pct_of_car_indie_se = pct_of_car_indie * 
               ((pct_indie_car_se/pct_indie_car)^2+
                  (pct_car_se/pct_car)^2)^0.5) %>%
      mutate(year = year)
}


summary_2017 <- indie_active_summary(
  here("NHTS-2017", "perpub.csv"), 
  here("NHTS-2017", "trippub.csv"), 
  2017) 

summary_2009 <- indie_active_summary(
  here("NHTS-2009", "PERV2PUB.CSV"),
  here("NHTS-2009", "DAYV2PUB.CSV"),
  2009
)

summary_2001 <- indie_active_summary(
  here("NHTS-2001", "PERPUB.CSV"),
  here("NHTS-2001", "DAYPUB.CSV"),
  2001
)

all_years <- rbind(summary_2017,
                   summary_2009,
                   summary_2001)

write_csv(all_years,
          here("results",
               "usa-nhts-summary-data.csv"))

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
               group = age_group,
               color = age_group,
               lty = age_group,
               y = pct_any_trip)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_any_trip - (1.96*pct_any_trip_se),
                    ymax = pct_any_trip + (1.96*pct_any_trip_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who made\na trip on the survey day",
                     limits = c(0.7,1),
                     breaks = breaks <- seq(0.7, 1, by = 0.05),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17"),
                       values = c("dotted",
                                  "solid",
                                  "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_any_trip.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")


ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = avg_trips)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = avg_trips - (1.96*avg_trips_se),
                    ymax = avg_trips + (1.96*avg_trips_se)),
                width = 0.1) +
  scale_y_continuous(name = "Average number of daily trips",
                     limits = c(0,4.5),
                     breaks = breaks <- seq(0, 4.5, by = 0.5)) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "trip_count.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = pct_alone)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_alone - (1.96*pct_alone_se),
                    ymax = pct_alone + (1.96*pct_alone_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who\nmade a trip alone",
                     limits = c(0,1),
                     breaks = breaks <- seq(0, 1, by = 0.1),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_alone.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = pct_indie)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_indie - (1.96*pct_indie_se),
                    ymax = pct_indie + (1.96*pct_indie_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who made\na trip without a household adult",
                     limits = c(0,1),
                     breaks = breaks <- seq(0, 1, by = 0.1),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_independent.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = pct_bike)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_bike - (1.96*pct_bike_se),
                    ymax = pct_bike + (1.96*pct_bike_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who\nmade a bike trip",
                     limits = c(0, 0.1),
                     breaks = breaks <- seq(0, 0.1, by = 0.01),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_bike.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = pct_walk)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_walk - (1.96*pct_walk_se),
                    ymax = pct_walk + (1.96*pct_walk_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who\nmade a walk trip",
                     limits = c(0, 0.4),
                     breaks = breaks <- seq(0, 0.4, by = 0.02),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_walk.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")

ggplot(all_years) +
  geom_line(aes(x = as_factor(year),
                group = age_group,
                color = age_group,
                lty = age_group,
                y = pct_transit)) +
  geom_errorbar(aes(x = as_factor(year),
                    group = age_group,
                    color = age_group,
                    lty = age_group,
                    ymin = pct_transit - (1.96*pct_transit_se),
                    ymax = pct_transit + (1.96*pct_transit_se)),
                width = 0.1) +
  scale_y_continuous(name = "Share of population who\nmade a transit trip",
                     limits = c(0, 0.07),
                     breaks = breaks <- seq(0, 0.07, by = 0.005),
                     labels = paste0(breaks*100, "%")) +
  scale_x_discrete(name = "Year") +
  scale_color_discrete(name = "Age group",
                       labels = c("5 to 9",
                                  "10 to 15",
                                  "16 to 17")) +
  scale_linetype_manual(name = "Age group",
                        labels = c("5 to 9",
                                   "10 to 15",
                                   "16 to 17"),
                        values = c("dotted",
                                   "solid",
                                   "dashed")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"))

here("results",
     "figures",
     "pct_transit.png") %>%
  ggsave(width = 5, height = 3, dpi = 300, units = "in")



