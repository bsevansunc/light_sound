# Setup -------------------------------------------------------------------

# Libraries used:

library(marked)
library(AICcmodavg)
library(tidyverse)

# Make use the select function ALWAYS refers to the function as defined
# in the package dplyr:

select <- dplyr::select

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

# Model running -----------------------------------------------------------

# Define formulas (you'll have to add your own variables here!):
# 
# formulas <-
#   crossing(
#     # Define formulas for Phi (apparent survival):
#     Phi = c(
#       '~ 1',
#       '~ sex',
#       '~ imp',
#       '~ imp + I(imp^2)',
#       '~ sex + imp',
#       '~ sex + imp + I(imp^2)'),
#     # Define formulas for p (detectability):
#     p = '~ sex')

formulas <-
  crossing(
    # Define formulas for Phi (apparent survival):
    Phi = c(
      '~ 1',
      '~ NightBrightness',
      '~ SoundExisting',
      '~ sex + NightBrightness',
      '~ sex + SoundExisting',
      '~ NightBrightness + SoundExisting',
      '~ sex + NightBrightness + SoundExisting',
      '~ sex',
      '~ imp',
      '~ imp + I(imp^2)',
      '~ sex + imp',
      '~ sex + imp + I(imp^2)',
      '~ imp + NightBrightness',
      '~ imp + SoundExisting',
      '~ imp + NightBrightness + SoundExisting'),
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
    numeric_vars_vector = c('imp', 'SoundExisting', 'NightBrightness'), # You will have to change this!
    numeric_length = 100,
    factor_vars_vector = 'sex')

# Predict based on new data for each model (takes a long time to run)
# Note 2 level list, level 1 names = species; level 2 names = model):

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

# New data for predictions:

write_rds(new_data_list, 'output/new_data_list.rds')

# Model predictions:

write_rds(
  model_predictions,
  'output/model_reals_p.rds')

