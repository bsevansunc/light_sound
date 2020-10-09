
# Process data ------------------------------------------------------------

make_model_data <-
  function(
    dataframe_in = encounters,
    species_vector = focal_spp,
    factors_vector = 'sex') {
    purrr::map(
      species_vector,
      function(x) {
        # Make a processed data frame:
        enc_proc <-
          process.data(
            data = 
              dataframe_in %>%
              # Filter to a given species, x:
              filter(spp == x) %>%
              # Remove species field:
              select(-spp) %>%
              # Convert from a tibble to a data frame:
              data.frame(),
            # Define the type of model:
            model="cjs",
            # Define which variables are a grouping variable (factor):
            groups = factors_vector)
        # Output is a list with the processed data and design data:
        list(
          enc_proc = enc_proc,
          enc_ddl = make.design.data(enc_proc))
      }) %>% 
      # Set the names of the list to species:
      set_names(species_vector)
  }

# Run models --------------------------------------------------------------

run_models <-
  function(
    processed_data_list = proc_list,
    formula_dataframe = formulas,
    factors_vector = 'sex',
    compute_hessian = TRUE) {
    purrr::map(
      processed_data_list,
      function(x) {
        purrr::map(
          1:nrow(formula_dataframe),
          function(i){
            tryCatch({
              crm(
                x$enc_proc,
                x$enc_ddl,
                model.parameters = list(
                  Phi = list(formula = formula_dataframe[i,]$Phi),
                  p = list(formula = formula_dataframe[i,]$p)),
                groups = factors_vector,
                hessian = compute_hessian,
                accumulate = TRUE)},
              error = function(cond) NULL
            )
          }) %>% 
          set_names(paste(formula_dataframe$Phi, formula_dataframe$p))
      }) %>% 
      set_names(names(processed_data_list))
  }
  
# model output ------------------------------------------------------------

# Modified model table function:

my_model_table <-
  function (model.list = NULL) 
  {
    model.table = NULL
    for (i in 1:(length(model.list))) {
      if (!is.list(model.list[[i]])) {
        load(model.list[[i]])
        if (length(grep("\\\\", model.list[[i]])) > 
            0 | length(grep("/", model.list[[i]])) > 
            0) 
          model.list[[i]] = basename(model.list[[i]])
        eval(parse(text = paste("mymodel=", unlist(strsplit(model.list[[i]], 
                                                            ".rda")))))
        eval(parse(text = paste("rm(", unlist(strsplit(model.list[[i]], 
                                                       ".rda")), ")")))
        gc()
      }
      else mymodel = model.list[[i]]
      formulae = sapply(mymodel$model.parameters, function(x) {
        return(paste(x$formula, collapse = ""))
      })
      formulae = paste(paste(names(formulae), "(", formulae, 
                             ")", sep = ""), collapse = "")
      df = data.frame(model = formulae, npar = sum(sapply(mymodel$results$beta, 
                                                          length)), AIC = mymodel$results$AIC, neg2lnl = mymodel$results$neg2lnl, 
                      convergence = mymodel$results$convergence)
      model.table = rbind(model.table, df)
    }
    model.table$DeltaAIC = model.table$AIC - min(model.table$AIC)
    model.table$weight = exp(-0.5 * model.table$DeltaAIC)
    model.table$weight = model.table$weight/sum(model.table$weight)
    model.table = model.table[order(model.table$DeltaAIC), c("model", 
                                                             "npar", "AIC", "DeltaAIC", "weight", 
                                                             "neg2lnl", "convergence")]
    return(model.table)
  }

# AIC table:

make_model_selection_tables <-
  function(model_list = models) {
    map(
      names(model_list),
      function(x) {
        my_model_table(model_list[[x]]) %>% 
          as_tibble() %>% 
          mutate(
            model = model %>% 
              str_remove('p\\(.*') %>%
              str_remove('\\$') %>% 
              str_remove('Phi\\(') %>% 
              str_remove('\\)$')) %>% 
          select(-convergence) %>% 
          rename(phi_formula = model)
      }) %>% 
      set_names(names(models))
  }

# Model output, beta estimates:

get_model_betas <-
  function(model_list = models, 
           formula_dataframe = formulas) {
    map(
      names(model_list),
      function(x) {
        map(
          1:length(model_list[[x]]),
          function(i) {
            coef(model_list[[x]][[i]]) %>% 
              rownames_to_column(var = 'parameter') %>% 
              as_tibble()
          }) %>% 
          set_names(names(model_list[[x]]))
      }) %>% 
      set_names(names(model_list))
  }

# Get real estimates:

get_model_reals <-
  function(model_list = models, parameter = 'Phi') {
    map(
      names(model_list),
      function(x) {
        map(
          1:length(model_list[[x]]),
          function(i) {
            model_list[[x]][[i]]$results$reals[[parameter]] %>% 
              as_tibble() %>% 
              {if(parameter == 'p'){
                filter(., estimate < 1)
              } else .}
          }) %>% 
          set_names(names(model_list[[x]]))
      }) %>% 
      set_names(names(model_list))
  }


# predictions -------------------------------------------------------------

# Generate a new data frame of all combinations of variable for a given
# species:

make_new_data <-
  function(
    encounter_dataframe = encounters,
    spp_vector = focal_spp,
    numeric_vars_vector = 'imp',
    numeric_length = 100,
    factor_vars_vector = 'sex') {
    purrr::map(
      focal_spp,
      function(x) {
        map(
          c(numeric_vars_vector, factor_vars_vector),
          function(variable) {
            var_data <-
              encounter_dataframe %>% 
              mutate_if(
                is.numeric,
                ~ scale(.)[,1]) %>% 
              filter(spp == x) %>% 
              pull(variable)
            if(variable %in% numeric_vars_vector) {
              seq(
                min(var_data), 
                max(var_data),
                by = (max(var_data) - min(var_data))/numeric_length)
            } else {
              unique(var_data)
            }
          }) %>% 
          set_names(c(numeric_vars_vector, factor_vars_vector)) %>% 
          cross_df()
      }) %>% 
      set_names(focal_spp)
  }

# Generate predictions for a given model:

predict_model <-
  function(
    model_list = models,
    spp = 'AMRO',
    model_formula = '~ 1 ~ sex',
    new_data = new_data_list,
    predictor_vars = c('sex'),
    processed_data = processed_data,
    parameter = 'Phi') {
    
    predict(
      model_list[[spp]][[model_formula]],
      newdata = 
        new_data_list[[spp]] %>% 
        select(all_of(predictor_vars)) %>% 
        distinct(),
      parameter = 'Phi',
      ddl = processed_data[[spp]]$enc_ddl,
      vcv = TRUE) %>% 
      .$real %>% 
      as_tibble()
  }

predict_models <-
  function(
    model_list = models,
    new_data = new_data_list,
    processed_data = processed_data,
    parameter = 'Phi') {
    map(
      names(model_list), 
      function(x) {
        map(
          names(model_list[[x]]),
          function(y) {
            predict(
              model_list[[x]][[y]],
              newdata = new_data_list[[x]],
              parameter = 'Phi',
              ddl = processed_data[[x]]$enc_ddl,
              vcv = TRUE) %>% 
              .$real %>% 
              as_tibble()
          }) %>% 
          set_names(model_list[[x]])
      }) %>% 
      set_names(names(model_list))
  }
  

    
  



  