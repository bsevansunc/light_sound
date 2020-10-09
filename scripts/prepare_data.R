# setup -------------------------------------------------------------------

library(tidyverse)

# Read in data:

db <-
  read_rds('data/observations_db.rds')

light_sound <-
  read_csv('data/light_sound.csv')

impervious <-
  read_csv('data/impervious.csv')

# prepare data ------------------------------------------------------------

# Sites with impervious surface, light and sound:

sites <- 
  # Start with impervious file:
  impervious %>%
  # Select the relevant columns:
  select(-c(long, lat)) %>% 
  # Join the light and sound data:
  inner_join(
    light_sound %>% 
      select(site_id, SoundExisting:NightBrightness),
    by = 'site_id')

# Filter visits to those associated with sites:

visits <-
  db$visits %>%
  filter(site_id %in% sites$site_id)

# Bird frame filtered to known adults and sex:

birds <-
  db$birds %>%
  filter(
    sex %in% c('M', 'F'),
    band_age %in% c('AHY', 'ASY', 'ATY')) %>% 
  # Sex should be a factor variable:
  mutate(sex = factor(sex)) %>% 
  # Select relevant columns: 
  select(bird_id, site_id, spp, sex)
  
# Observations of marked birds:

observations <-
  db$observations %>% 
  # Filter to matching records in "birds" and "visits":
  filter(
    bird_id %in% birds$bird_id,
    visit_id %in% visits$visit_id) %>% 
  # Add year and remove unnecessary columns:
  transmute(
    bird_id,
    yr  = lubridate::year(date_time)) %>% 
  # Only one observation per bird, per year:
  distinct()

# format data for analysis ------------------------------------------------

encounters <-
  observations %>% 
  # Dummy values signify an encounter happened on a given year:
  mutate(enc = 1) %>% 
  # Wide frame, each column (except bird id) is a year:
  pivot_wider(
    names_from = yr,
    values_from = enc,
    values_fill = list(enc = 0)) %>% 
  # Concatenate columns to generate a capture history:
  unite(
    col = 'ch',
    -bird_id,
    sep = '') %>% 
  # Bring in bird info:
  inner_join(birds, by = 'bird_id') %>% 
  # Bring in site info:
  inner_join(sites, by = 'site_id') %>% 
  # Reduce to relevant fields:
  select(ch, spp, sex, imp:NightBrightness)

# Remove all files except for the encounter frame:

rm(
  list =
    c(
      'birds',
      'db',
      'impervious',
      'light_sound',
      'observations',
      'sites',
      'visits'
    ))


  

