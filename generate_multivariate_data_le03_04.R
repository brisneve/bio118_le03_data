generate_multivariate_data_le03_04 <- function(student_id){
  # Set seed for reproducibility
  set.seed(student_id)
  
  # Number of observations per month and variables
  n_obs <- 25
  n_months <- 3
  
  # Define month names
  months <- c("January", "May", "September")
  
  # Define physiological variables and realistic ranges for their means and standard deviations
  physiological_data <- list(
    RespirationRate = list(month_means = c(1.5, 2.0, 2.5), sd_range = c(0.2, 0.5)),  # µmol O2/hr
    GrowthYield = list(month_means = c(0.8, 1.0, 1.2), sd_range = c(0.1, 0.2)),      # g biomass/g substrate
    ATPContent = list(month_means = c(2.5, 3.0, 3.5), sd_range = c(0.2, 0.5)),       # µmol/mL
    ProteinContent = list(month_means = c(50, 60, 70), sd_range = c(5, 10)),         # µg/mL
    CarbohydrateContent = list(month_means = c(30, 40, 50), sd_range = c(5, 10)),    # µg/mL
    LipidContent = list(month_means = c(10, 15, 20), sd_range = c(2, 5)),            # µg/mL
    EnzymeActivity = list(month_means = c(100, 120, 140), sd_range = c(10, 20)),     # U/mL
    MembranePotential = list(month_means = c(70, 80, 90), sd_range = c(5, 10)),      # mV
    pH = list(month_means = c(6.8, 7.0, 7.2), sd_range = c(0.1, 0.2)),               # pH
    OsmoticPressure = list(month_means = c(300, 350, 400), sd_range = c(20, 50))     # mOsm/kg
  )
  
  # Initialize an empty data frame
  bacteria_data <- data.frame()
  
  # Generate random normally distributed data for each month and variable
  for (month_idx in seq_along(months)) {
    month <- months[month_idx]
    month_data <- data.frame(
      Month = rep(month, n_obs)
    )
    
    for (var_name in names(physiological_data)) {
      mean_val <- physiological_data[[var_name]]$month_means[month_idx] # Unique mean for each month
      sd_val <- runif(1, physiological_data[[var_name]]$sd_range[1], physiological_data[[var_name]]$sd_range[2])
      generated_values <- rnorm(n_obs, mean = mean_val, sd = sd_val)
      month_data[[var_name]] <- pmax(generated_values, 0) # Ensure no negative values
    }
    
    # Combine month data
    bacteria_data <- rbind(bacteria_data, month_data)
  }
  
  # View the first few rows of the data
  head(bacteria_data)
  
  # Save the data as a CSV file (optional)
  write.csv(bacteria_data, "bacteria_physiological_data.csv", row.names = FALSE)
  
  }
  

