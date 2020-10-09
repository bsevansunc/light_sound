# Setup -------------------------------------------------------------------

# Libraries used:

library(marked)
library(AICcmodavg)
library(tidyverse)

# Make use the select function ALWAYS refers to the function as defined
# in the package dplyr:

select <- dplyr::select

gc()

# Run data preparation script:

source('scripts/prepare_data.R')

# I made a functions file (that you don't need to understand, but do need
# to run):

source('scripts/functions.R')

# Define focal species for analysis:

focal_spp <-
  c('AMRO',
    'CACH',
    'CARW',
    'GRCA',
    'HOWR',
    'NOCA',
    'SOSP')

# process data (this will take a bit of time to run):

processed_data <-
  make_model_data(
    dataframe_in =
      encounters %>%
      mutate_if(
        is.numeric,
        ~ scale(.)[,1]),
    species_vector = focal_spp,
    factors_vector = 'sex')


# View correlation table of predictor variables ---------------------------

encounters %>% 
  select(imp:NightBrightness) %>% 
  distinct() %>% 
  cor()

# Model running -----------------------------------------------------------

# Define formulas (you'll have to add your own variables here!):

formulas <-
  crossing(
    # Define formulas for Phi (apparent survival):
    Phi = c(
      # One variable models:
      '~ 1',
      '~ sex',
      '~ imp',
      '~ imp + I(imp^2)',
      '~ NightBrightness',
      '~ SoundExisting',
      # Two variable additive models:
      '~ sex + imp',
      '~ sex + imp + I(imp^2)',
      '~ sex + NightBrightness',
      '~ sex + SoundExisting',
      '~ imp + NightBrightness',
      '~ imp + SoundExisting',
      '~ imp + I(imp^2) + NightBrightness',
      '~ imp + I(imp^2) + SoundExisting',
      # Three variable additive models:
      '~ sex + imp + NightBrightness',
      '~ sex + imp + SoundExisting',
      '~ sex + imp + I(imp^2) + NightBrightness',
      '~ sex + imp + I(imp^2) + SoundExisting',
      '~ imp + NightBrightness + SoundExisting',
      '~ imp + I(imp^2) + NightBrightness + SoundExisting',
      # Four variable additive models:
      '~ sex + imp + NightBrightness + SoundExisting',
      '~ sex + imp + I(imp^2) + NightBrightness + SoundExisting'#,
      # Two variable interaction of imp*Nightbrightness and imp*soundExisting:
      # '~ imp * NightBrightness',
      # '~ imp * SoundExisting',
      # '~ imp * NightBrightness + I(imp^2)',
      # '~ imp * SoundExisting + I(imp^2)',
      # Three variable interaction of imp*Nightbrightness and imp*soundExisting:
      # '~ sex + imp * NightBrightness',
      # '~ sex + imp * SoundExisting',
      # '~ sex + imp * NightBrightness + I(imp^2)',
      # '~ sex + imp * SoundExisting + I(imp^2)',
      # '~ imp * NightBrightness + SoundExisting',
      # '~ imp + NightBrightness + imp * SoundExisting',
      # '~ imp * NightBrightness + imp * SoundExisting',
      # '~ imp * NightBrightness + I(imp^2) + SoundExisting',
      # '~ imp * SoundExisting + I(imp^2) + NightBrightness',
      # '~ imp * SoundExisting + imp * NightBrightness + I(imp^2)',
      # Four variable interaction of imp*Nightbrightness and imp*soundExisting:
      # '~ sex + imp * NightBrightness + SoundExisting',
      # '~ sex + imp * SoundExisting + NightBrightness',
      # '~ sex + imp * SoundExisting + imp * NightBrightness',
      # '~ sex + imp * NightBrightness + SoundExisting + I(imp^2)',
      # '~ sex + imp * SoundExisting + NightBrightness + I(imp^2)',
      # '~ sex + imp * SoundExisting + imp * NightBrightness + I(imp^2)'
      ),
    # Define formulas for p (detectability):
    p = '~ sex')

# Run models (this will take a long time to run!):

models <-
  run_models(
    processed_data_list = processed_data,
    formula_dataframe = formulas,
    factors_vector = 'sex',
    compute_hessian = TRUE)

# Note: The above generates a nested list of models:
# - The top level of the list (mod_list[[x]]) is species
# - The bottom level of the list (mod_list[[x]][[i]]) is the model

# Model output ------------------------------------------------------------

# Model selection, AIC table (list, names = species):

model_selection_tables <-
  make_model_selection_tables(model_list = models)

# Beta estimates
# Note 2 level list, level 1 names = species; level 2 names = model):

model_betas <-
  get_model_betas(
    model_list = models,
    formula_dataframe = formulas)

# Real estimates for Phi (apparent survival)
# Note 2 level list, level 1 names = species; level 2 names = model):

model_reals_Phi <-
  get_model_reals(
    model_list = models,
    parameter = 'Phi')

# Real estimates for Phi (detectability)
# Note 2 level list, level 1 names = species; level 2 names = model):

model_reals_p <-
  get_model_reals(
    model_list = models,
    parameter = 'p')

# Generate model predictions ----------------------------------------------

# Make a data frame of all combinations of the data (list, names = species)
# Note: numeric variables are scaled to match the scaling of the models:


new_data_list <-
  make_new_data(
    encounter_dataframe = encounters,
    spp_vector = focal_spp,
    numeric_vars_vector = c('imp','SoundExisting','NightBrightness'), # You will have to change this!
    numeric_length = 50,
    factor_vars_vector = 'sex')


# Predict based on new data for each model (takes a long time to run)
# Note 2 level list, level 1 names = species; level 2 names = model):
#look at error

test <- predict_model(
  model_list = models,
  spp = 'AMRO',
  model_formula = '~ NightBrightness ~ sex',
  new_data = new_data_list,
  predictor_vars = c('sex', 'NightBrightness'),
  processed_data = processed_data,
  parameter = 'Phi')

model_predictions <-
  predict_models(
    model_list = models,
    new_data = new_data_list,
    processed_data = processed_data,
    parameter = 'Phi')


# Write to output files ---------------------------------------------------

# Because the above takes a long time to run, this allows to save the
# results so they only have to be run once.

# The models themselves (huge file, takes a long time to write!):

write_rds(
  models,
  'output/models.rds')

# Model selection tables:

write_rds(
  model_selection_tables,
  'output/model_selection.rds')

# Beta estimates:

write_rds(
  model_betas,
  'output/model_betas.rds')

# Real estimates for Phi and p:

write_rds(
  model_reals_Phi,
  'output/model_reals_Phi.rds')

write_rds(
  model_reals_p,
  'output/model_reals_p.rds')

# Model predictions:

write_rds(
  model_predictions,
  'output/model_reals_p.rds')

#Code to Modify for plotting the Output
# plot theme --------------------------------------------------------------

plot_theme <-
  function() {
    theme(
      plot.title = element_text(size = rel(1.5)),
      axis.title = element_text(size = rel(1.5)),
      axis.text = element_text(size = rel(1.1)),
      axis.title.x = element_text(
        margin = margin(t = .5, unit = 'cm')),
      axis.title.y = element_text(
        margin = margin(r = .5, unit = 'cm')),
      panel.spacing = unit(1, 'lines')
    )
  }


# plot beta ---------------------------------------------------------------

# Just a quick-and-dirty look, for now ...

# Impervious surface:

purrr::map_dfr(
  names(model_betas),
  function(spp) {
    pluck(model_betas[[spp]], '~ imp ~ sex') %>% 
      filter(parameter == 'Phi.imp') %>% 
      mutate(spp = spp)
  }) %>% 
  mutate(spp = as.factor(spp) %>%
           fct_rev()) %>%
  rename(beta = Estimate) %>% 
  ggplot(aes(x = spp, y = beta)) +
  geom_point(size = 4) +
  geom_segment(
    aes(
      x = spp,
      xend = spp,
      y = lcl,
      yend = ucl),
    size = 1) +
  geom_hline(
    yintercept = 0,
    linetype = 'dashed') +
  coord_flip() +
  labs(
    x = 'Species',
    y = 'Beta',
    title = 'Beta values for Phi ~ imp'
  ) +
  theme_bw() +
  plot_theme()

# Night brightness:

purrr::map_dfr(
  names(model_betas),
  function(spp) {
    pluck(model_betas[[spp]], '~ NightBrightness ~ sex') %>% 
      filter(parameter == 'Phi.NightBrightness') %>% 
      mutate(spp = spp)
  }) %>% 
  mutate(spp = as.factor(spp) %>%
           fct_rev()) %>%
  rename(beta = Estimate) %>% 
  ggplot(aes(x = spp, y = beta)) +
  geom_point(size = 4) +
  geom_segment(
    aes(
      x = spp,
      xend = spp,
      y = lcl,
      yend = ucl),
    size = 1) +
  geom_hline(
    yintercept = 0,
    linetype = 'dashed') +
  coord_flip() +
  labs(
    x = 'Species',
    y = 'Beta',
    title = 'Beta values for Phi ~ NightBrightness'
  ) +
  theme_bw() +
  plot_theme()


purrr::map_dfr(
  names(model_betas),
  function(spp) {
    pluck(model_betas[[spp]], '~ SoundExisting ~ sex') %>% 
      filter(parameter == 'Phi.SoundExisting') %>% 
      mutate(spp = spp)
  }) %>% 
  mutate(spp = as.factor(spp) %>%
           fct_rev()) %>%
  rename(beta = Estimate) %>% 
  ggplot(aes(x = spp, y = beta)) +
  geom_point(size = 4) +
  geom_segment(
    aes(
      x = spp,
      xend = spp,
      y = lcl,
      yend = ucl),
    size = 1) +
  geom_hline(
    yintercept = 0,
    linetype = 'dashed') +
  coord_flip() +
  labs(
    x = 'Species',
    y = 'Beta',
    title = 'Beta values for Phi ~ SoundExisting'
  ) +
  theme_bw() +
  plot_theme()



# model predictions -------------------------------------------------------

newdata_list <-
  purrr::map(
    focal_spp,
    function(x) {
      enc_subset <-
        enc_all %>%
        filter(spp == x)
      crossing(
        sex = unique(enc_subset$sex),
        region = unique(enc_subset$region),
        imp = seq(
          min(enc_subset$imp),
          max(enc_subset$imp),
          by = 1)) %>%
        mutate_at(
          vars(sex, region),
          as.factor
        ) %>%
        data.frame()
    }) %>%
  set_names(focal_spp)

# Predictions by region

predictions_region <-
  map_dfr(
    focal_spp,
    function(x) {
      predict(
        mod_list[[x]]$`~ region ~ region + sex`,
        newdata = newdata_list[[x]],
        parameter = 'Phi',
        ddl=proc_list[[x]]$enc_ddl,
        vcv=TRUE) %>%
        .$real %>%
        as_tibble() %>%
        mutate(species = x) %>%
        select(species, everything())
    }) %>%
  mutate(
    region =
      factor(region,
             levels = c(
               'gainesville',
               'atlanta',
               'raleigh',
               'dc',
               'pittsburgh',
               'springfield')))

# Predictions by region and sex:

predictions_region_sex <-
  map_dfr(
    focal_spp,
    function(x) {
      predict(
        mod_list[[x]]$`~ region * sex ~ region + sex`,
        newdata = newdata_list[[x]],
        parameter = 'Phi',
        ddl=proc_list[[x]]$enc_ddl,
        vcv=TRUE) %>%
        .$real %>%
        as_tibble() %>%
        mutate(species = x) %>%
        select(species, everything())
    }) %>%
  mutate(
    region =
      factor(region,
             levels = c(
               'gainesville',
               'atlanta',
               'raleigh',
               'dc',
               'pittsburgh',
               'springfield')))

# Predictions by region and impervious:

predictions_region_imp <-
  map_dfr(
    focal_spp,
    function(x) {
      predict(
        mod_list[[x]]$`~ region + imp + I(imp^2) ~ region + sex`,
        newdata = newdata_list[[x]],
        parameter = 'Phi',
        ddl=proc_list[[x]]$enc_ddl,
        vcv=TRUE) %>%
        .$real %>%
        as_tibble() %>%
        mutate(species = x) %>%
        select(species, everything())
    }) %>%
  mutate(
    region =
      factor(region,
             levels = c(
               'gainesville',
               'atlanta',
               'raleigh',
               'dc',
               'pittsburgh',
               'springfield')))

# Write files:

write_rds(
  newdata_list,
  'output/newdata_list.rds')

write_csv(
  predictions_region,
  'output/predictions_region.csv')

write_csv(
  predictions_region_sex,
  'output/predictions_region_sex.csv')

write_csv(
  predictions_region_imp,
  'output/predictions_region_imp.csv')

# plot predictions --------------------------------------------------------

# Plot predictions by region:

predictions_region %>%
  ggplot(
    aes(x = region, y = estimate)) +
  geom_point(size = 3) +
  geom_segment(
    aes(
      x = region,
      xend = region,
      y = lcl,
      yend = ucl),
    size = 1) +
  coord_flip() +
  facet_wrap(~ species, nrow = 2, ncol = 4) +
  labs(
    title = 'Estimates of apparent survival by region',
    x = 'Apparent survival',
    y = 'Region'
  ) +
  theme_bw() +
  plot_theme()

# Plot predictions by region and sex:

predictions_region_sex %>%
  ggplot(
    aes(x = region, y = estimate, color = sex)) +
  geom_point(
    size = 3,
    position = position_dodge(width = .75)) +
  geom_linerange(
    aes(
      ymin = lcl,
      ymax = ucl),
    size = 1,
    position = position_dodge(width = .75)) +
  coord_flip() +
  facet_wrap(~ species, nrow = 2, ncol = 4) +
  labs(
    title = 'Estimates of apparent survival by region and sex',
    x = 'Apparent survival',
    y = 'Region'
  ) +
  theme_bw() +
  plot_theme()

# Plot predictions by region and imp:

predictions_region_imp %>%
  ggplot(
    aes(x = imp, y = estimate)) +
  geom_smooth(
    aes(ymin = lcl, ymax = ucl),
    stat = 'identity',
    color = '#000000'
  ) +
  facet_grid(region ~ species) +
  labs(
    title = 'Estimates of apparent survival by region and impervious cover',
    x = 'Proportion impervious surface within 500 m (%)',
    y = 'Apparent survival') +
  theme_bw() +
  plot_theme()