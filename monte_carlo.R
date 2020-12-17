library(tidyverse)
library(tidymodels)
theme_set(theme_classic())

directory <- "C://Users/Laboratorio DID/Documents/Proyectos Diabb Zegpi/sigle_sics/"
predictor <- readRDS(file = paste0(directory, "Models/final_rf.rds"))
simulador <- readRDS(file = paste0(directory, "Models/simulador.rds"))
horizonte <- 52
# Set beginning date for simulation
inicio <- Sys.Date()

# Defining function for looping over simulations
repeat_simulation <- function(repeats) {
  repetitions <- list()
  repetitions <- rerun(repeats, 
                       simulador(inicio = inicio, horizonte = horizonte))
  return(repetitions)
}

# Number of simulations
total_sims <- 10000
step_length <- 200
start_sim <- 200

# Parallel computation to speed-up simulations
library(furrr)
library(tictoc)
n_cores <- availableCores() 
plan(strategy = multisession, workers = n_cores)
  
tic("Simulación de escenarios")
# Perform simulations
simulations <- tibble(nsim = seq(from = start_sim, 
                                 to = total_sims, 
                                 by = step_length)) %>% 
  mutate(scenario = future_map(nsim, ~ repeat_simulation(repeats = .)))
toc()

simulations_unnest <- simulations %>% 
  unnest(scenario) %>% 
  mutate(id = 1:nrow(.)) 

write_rds(simulations, 
          file = "C://Users/Laboratorio DID/Desktop/simulations.rds")
write_rds(simulations_unnest,
          file = "C://Users/Laboratorio DID/Desktop/simulations_unnest.rds")



tic("Predicción de escenarios")
predictions <- simulations_unnest %>% 
  mutate(predictions = map(scenario, ~ predict(predictor, new_data = .))) %>% 
  select(-scenario)
toc()

write_rds(predictions, file = "C://Users/Laboratorio DID/Desktop/predictions.rds")