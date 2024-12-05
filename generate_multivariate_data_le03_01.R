generate_multivariate_data_le03_01 <- function(student_id){
  # Set seed for reproducibility
  set.seed(student_id)
  
  # Number of observations per site and variables
  n_obs <- 25
  n_sites <- 3
  
  # Define variable names and realistic ranges for their means and standard deviations
  variables <- list(
    Temperature = list(site_means = c(25, 27, 29), sd_range = c(0.5, 1.5)),  # °C
    pH = list(site_means = c(7.2, 7.8, 8.2), sd_range = c(0.1, 0.3)),         # pH units
    DO = list(site_means = c(6, 8, 9), sd_range = c(0.5, 1.5)),              # Dissolved Oxygen (mg/L)
    Salinity = list(site_means = c(32, 34, 36), sd_range = c(0.5, 1.5)),     # PSU
    Nitrate = list(site_means = c(0.2, 0.4, 0.6), sd_range = c(0.05, 0.2)),  # mg/L
    Phosphate = list(site_means = c(0.02, 0.05, 0.1), sd_range = c(0.005, 0.05)), # mg/L
    Turbidity = list(site_means = c(2, 3, 4), sd_range = c(0.5, 1.5)),       # NTU
    Ammonia = list(site_means = c(0.02, 0.05, 0.08), sd_range = c(0.005, 0.02)), # mg/L
    Chlorophyll = list(site_means = c(1, 5, 8), sd_range = c(0.5, 2)),       # µg/L
    Conductivity = list(site_means = c(35000, 40000, 45000), sd_range = c(500, 1000)) # µS/cm
  )
  
  # Site names
  sites <- paste0("Site", 1:n_sites)
  
  # Initialize an empty data frame
  water_quality_data <- data.frame()
  
  # Generate random normally distributed data for each site and variable
  for (site_idx in seq_along(sites)) {
    site <- sites[site_idx]
    site_data <- data.frame(
      Site = rep(site, n_obs)
    )
    
    for (var_name in names(variables)) {
      mean_val <- variables[[var_name]]$site_means[site_idx] # Unique mean for each site
      sd_val <- runif(1, variables[[var_name]]$sd_range[1], variables[[var_name]]$sd_range[2])
      generated_values <- rnorm(n_obs, mean = mean_val, sd = sd_val)
      site_data[[var_name]] <- pmax(generated_values, 0) # Ensure no negative values
    }
    
    # Combine site data
    water_quality_data <- rbind(water_quality_data, site_data)
  }
  
  # View the first few rows of the data
  head(water_quality_data)
  
  # Save the data as a CSV file (optional)
  write.csv(water_quality_data, "water_quality_data.csv", row.names = FALSE)
} 


