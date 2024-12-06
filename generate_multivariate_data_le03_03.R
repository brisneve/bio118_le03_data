generate_multivariate_data_le03_03 <- function(student_id){
  # Set seed for reproducibility
  set.seed(student_id)
  
  # Number of observations per species and variables
  n_obs <- 25
  n_species <- 3
  
  # Define species names
  species <- c("Species_X", "Species_Y", "Species_Z")
  
  # Define morphometric variables and realistic ranges for their means and standard deviations
  morphometrics <- list(
    ShellLength = list(species_means = c(50, 60, 70), sd_range = c(5, 10)),     # mm
    ShellWidth = list(species_means = c(25, 30, 35), sd_range = c(3, 7)),       # mm
    ShellHeight = list(species_means = c(20, 25, 30), sd_range = c(2, 6)),      # mm
    Weight = list(species_means = c(15, 20, 25), sd_range = c(1, 3)),           # g
    MantleLength = list(species_means = c(30, 35, 40), sd_range = c(3, 5)),     # mm
    Girth = list(species_means = c(40, 50, 60), sd_range = c(4, 8)),            # mm
    HingeLength = list(species_means = c(10, 15, 20), sd_range = c(1, 3)),      # mm
    LigamentLength = list(species_means = c(5, 7, 10), sd_range = c(0.5, 2)),   # mm
    AdductorMuscleSize = list(species_means = c(8, 10, 12), sd_range = c(1, 2)),# mm²
    ShellThickness = list(species_means = c(1.5, 2, 2.5), sd_range = c(0.2, 0.5))# mm
  )
  
  # Initialize an empty data frame
  bivalve_data <- data.frame()
  
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
    bivalve_data <- rbind(bivalve_data, sp_data)
  }
  
  # View the first few rows of the data
  head(bivalve_data)
  
  # Save the data as a CSV file (optional)
  write.csv(bivalve_data, "bivalve_morphometrics_data.csv", row.names = FALSE)
  
  }
  

