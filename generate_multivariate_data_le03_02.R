generate_multivariate_data_le03_02 <- function(student_id){
  # Set seed for reproducibility
  set.seed(student_id)
  
  # Number of observations per species and variables
  n_obs <- 25
  n_species <- 3
  
  # Define species names
  species <- c("Species_A", "Species_B", "Species_C")
  
  # Define morphometric variables and realistic ranges for their means and standard deviations
  morphometrics <- list(
    Height = list(species_means = c(50, 70, 90), sd_range = c(5, 10)),         # cm
    LeafLength = list(species_means = c(15, 25, 35), sd_range = c(2, 5)),      # cm
    LeafWidth = list(species_means = c(5, 10, 15), sd_range = c(1, 3)),        # cm
    StemDiameter = list(species_means = c(2, 3, 4), sd_range = c(0.2, 0.5)),   # cm
    RootLength = list(species_means = c(20, 30, 40), sd_range = c(3, 7)),      # cm
    FlowerDiameter = list(species_means = c(5, 7, 10), sd_range = c(1, 2)),    # cm
    PetalLength = list(species_means = c(2, 3, 5), sd_range = c(0.5, 1)),      # cm
    PetalWidth = list(species_means = c(0.5, 1, 1.5), sd_range = c(0.1, 0.3)), # cm
    SeedWeight = list(species_means = c(0.1, 0.15, 0.2), sd_range = c(0.01, 0.05)), # g
    LeafArea = list(species_means = c(75, 150, 300), sd_range = c(10, 30))     # cm²
  )
  
  # Initialize an empty data frame
  plant_data <- data.frame()
  
  # Generate random normally distributed data for each species and variable
  for (species_idx in seq_along(species)) {
    sp <- species[species_idx]
    sp_data <- data.frame(
      Species = rep(sp, n_obs)
    )
    
    for (var_name in names(morphometrics)) {
      mean_val <- morphometrics[[var_name]]$species_means[species_idx] # Unique mean for each species
      sd_val <- runif(1, morphometrics[[var_name]]$sd_range[1], morphometrics[[var_name]]$sd_range[2])
      generated_values <- rnorm(n_obs, mean = mean_val, sd = sd_val)
      sp_data[[var_name]] <- pmax(generated_values, 0) # Ensure no negative values
    }
    
    # Combine species data
    plant_data <- rbind(plant_data, sp_data)
  }
  
  # View the first few rows of the data
  head(plant_data)
  
  # Save the data as a CSV file (optional)
  write.csv(plant_data, "plant_morphometrics_data.csv", row.names = FALSE)
  
  }
  

