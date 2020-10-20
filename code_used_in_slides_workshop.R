library(tidytuesdayR)
himalaya <- tidytuesdayR::tt_load('2020-09-22')
members <- himalaya$members
members %>% 
  head()

library(dplyr) ## load the library
members %>% 
  filter(oxygen_used)


members %>% 
  filter(!oxygen_used,age > 75)


members %>% 
  filter(death_height_metres > injury_height_metres)


members1905 <- members %>% 
  filter(year == 1905)


members %>% 
  arrange(year)



members1905 %>% 
  arrange(member_id)


members %>% 
  arrange(desc(year),citizenship,hired,peak_name)


members %>% 
  filter(injured) %>% 
  arrange(highpoint_metres - injury_height_metres)

members %>% 
  select(year,sex,citizenship)

members %>% 
  select(age:success)

members %>% 
  select(-expedition_id,-member_id,-peak_id)


members %>% 
  select(4,6:10,highpoint_metres)

members_n <- members %>% 
  filter(died) %>% 
  select(-ends_with('id'),-ends_with('ed'))


members %>% 
  select(gender = sex,used_oxygen = oxygen_used,role = expedition_role) 


members %>% 
  rename(gender = sex,used_oxygen = oxygen_used,role = expedition_role) 


members_n %>% 
  mutate(Role_citizenship = paste(expedition_role,citizenship))


members_n %>% 
  mutate(Difference_mts_died = death_height_metres - highpoint_metres)

members1905 %>% # this has 9 rows!
  mutate(my_row = (1:9)^2 + log(15)*(9-row_number()))

members1905 %>% ## notice the difference between: ' and "
  mutate(Cheating = if_else(expedition_role == 'Leader',
                            'Cheated',
                            "Didn't cheat"))

members1905 %>% 
  mutate(anyone_died = any(died),
         max_height = max(highpoint_metres,
                          na.rm = TRUE),
         last_citizenship = last(citizenship),
         youngest = min(age),
         percent_dead = mean(died),
         diff_from_average_height_death = death_height_metres - 
                                          mean(death_height_metres,
                                               na.rm = TRUE) ) %>% 
  select(-(1:21)) # to see the new variables


members1905 %>% 
  summarise(anyone_died = any(died),
            max_height = max(highpoint_metres,na.rm = TRUE),
            last_citizenship = last(citizenship),
            youngest = min(age),
            percent_dead = mean(died))

members %>% 
  mutate(Avg_general = mean(highpoint_metres,
                            na.rm = TRUE)) %>%
  filter(sex=='F',
         citizenship=='France') %>%
  mutate(Mean_f_women =  mean(highpoint_metres,
                              na.rm = TRUE)) %>% 
  summarise(Mean_diff = mean(highpoint_metres - Avg_general,na.rm = TRUE))


members %>% 
  group_by(died) %>% 
  summarise(Percentage_injured = mean(injured))


members_n %>% select(-(4:14)) %>% 
  group_by(season) %>% 
  mutate(Obs_per_season = n())


members %>% 
  group_by(year,season) %>% 
  summarise(Average_height = mean(highpoint_metres,
                                  na.rm = TRUE))


members %>% 
  filter(expedition_role =='Climber') %>%
  group_by(season,year) %>% 
  summarize(Total = n()) %>% 
  summarise(Mean = mean(Total),
            Std = sd(Total))

(surviving_members <- members %>% 
    group_by(expedition_id) %>% 
    filter(n() > 10))




























