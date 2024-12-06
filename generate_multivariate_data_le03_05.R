generate_multivariate_data_le03_05 <- function(student_id){
  # Set seed for reproducibility
  set.seed(student_id)
  
  # Number of observations per species and variables
  n_obs <- 25
  n_species <- 3
  
  # Define species names
  species <- c("Species_A", "Species_B", "Species_C")
  
  # Define behavioral variables and realistic ranges for their means and standard deviations
  behavioral_data <- list(
    ActivityLevel = list(species_means = c(70, 50, 40), sd_range = c(5, 10)),         # Percent active time
    FeedingRate = list(species_means = c(30, 25, 20), sd_range = c(3, 7)),           # Feeding events/hour
    GroomingDuration = list(species_means = c(15, 20, 10), sd_range = c(2, 5)),      # Minutes/day
    InteractionCount = list(species_means = c(5, 10, 15), sd_range = c(1, 4)),       # Number of social interactions
    VocalizationRate = list(species_means = c(8, 12, 6), sd_range = c(1, 3)),        # Calls/hour
    RestingTime = list(species_means = c(200, 250, 300), sd_range = c(10, 20)),      # Minutes/day
    ExplorationTime = list(species_means = c(50, 40, 30), sd_range = c(5, 10)),      # Minutes/day
    AggressiveEncounters = list(species_means = c(3, 2, 1), sd_range = c(0.5, 1.5)), # Number of encounters
    TerritorialMarking = list(species_means = c(4, 5, 3), sd_range = c(1, 2)),       # Marks/hour
    EscapeAttempts = list(species_means = c(2, 3, 1), sd_range = c(0.5, 1.5))        # Attempts/day
  )
  
  # Initialize an empty data frame
  behavioral_data_df <- data.frame()
  
  # Generate random normally distributed data for each species and variable
  for (species_idx in seq_along(species)) {
    sp <- species[species_idx]
    sp_data <- data.frame(
      Species = rep(sp, n_obs)
    )
    
    for (var_name in names(behavioral_data)) {
      mean_val <- behavioral_data[[var_name]]$species_means[species_idx] # Unique mean for each species
      sd_val <- runif(1, behavioral_data[[var_name]]$sd_range[1], behavioral_data[[var_name]]$sd_range[2])
      generated_values <- rnorm(n_obs, mean = mean_val, sd = sd_val)
      sp_data[[var_name]] <- pmax(generated_values, 0) # Ensure no negative values
    }
    
    # Combine species data
    behavioral_data_df <- rbind(behavioral_data_df, sp_data)
  }
  
  # View the first few rows of the data
  head(behavioral_data_df)
  
  # Save the data as a CSV file (optional)
  write.csv(behavioral_data_df, "bird_behavioral_data.csv", row.names = FALSE)
  
  }
  

