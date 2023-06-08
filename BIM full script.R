
# To do:
# Create ToT function. Use ToT as an input in acquisition and admin costs.



calculateIncidentPopulation <- function(total_population_males, total_population_females,
                                        percent_diagnosed_males, percent_diagnosed_females,
                                        proportion_stage_IIIC_IV, percent_increase_per_year,
                                        num_years) {
  incident_population <- numeric(num_years)
  
  incident_population[1] <- (total_population_males * percent_diagnosed_males +
                               total_population_females * percent_diagnosed_females) *
    (1 + proportion_stage_IIIC_IV)
  
  for (i in 2:num_years) {
    incident_population[i] <- incident_population[i-1] * (1 + percent_increase_per_year)
  }
  
  return(incident_population)
}


# Function to calculate the number of patients treated each year for each drug
calculateNumPatientsTreated <- function(market_share_with_PRODUCT_Y, market_share_without_PRODUCT_Y,
                                        incident_population, num_treatments, num_years) {
  
  num_patients_treated_with_PRODUCT_Y <- matrix(0, nrow = num_years, ncol = num_treatments)
  num_patients_treated_without_PRODUCT_Y <- matrix(0, nrow = num_years, ncol = num_treatments)
  
  for (i in 1:num_years) {
    num_patients_treated_with_PRODUCT_Y[i, ] <- market_share_with_PRODUCT_Y[i, ] * incident_population[i]
    num_patients_treated_without_PRODUCT_Y[i, ] <- market_share_without_PRODUCT_Y[i, ] * incident_population[i]
  }
  
  colnames(num_patients_treated_with_PRODUCT_Y) <- colnames(market_share_with_PRODUCT_Y)
  colnames(num_patients_treated_without_PRODUCT_Y) <- colnames(market_share_with_PRODUCT_Y)
  rownames(num_patients_treated_with_PRODUCT_Y) <- rownames(market_share_with_PRODUCT_Y)
  rownames(num_patients_treated_without_PRODUCT_Y) <- rownames(market_share_with_PRODUCT_Y)
  
  return(list(
    with_PRODUCT_Y = num_patients_treated_with_PRODUCT_Y,
    without_PRODUCT_Y = num_patients_treated_without_PRODUCT_Y
  ))
}



# Function to calculate the acquisition costs
calculateAcquisitionCosts <- function(num_patients_treated, dose_per_day, unit_cost_per_drug, num_years, num_treatments) {
  # Calculate the cost per mg, cost per day, and cost per time on treatment for each drug
  cost_per_mg <- matrix(0, nrow = num_years, ncol = num_treatments)
  cost_per_day <- matrix(0, nrow = num_years, ncol = num_treatments)
  cost_per_time_on_treatment <- matrix(0, nrow = num_years, ncol = num_treatments)
  
  for (i in 1:num_years) {
    for (j in 1:num_treatments) {
      cost_per_mg[i, j] <- unit_cost_per_drug[j] / dose_per_day[i]
      cost_per_day[i, j] <- unit_cost_per_drug[j] * dose_per_day[i]
      cost_per_time_on_treatment[i, j] <- cost_per_day[i, j] * num_patients_treated[i, j]
    }
  }
  
  colnames(cost_per_mg) <- c("PRODUCT Y", "Nivolumab + PRODUCT Y", "Nivolumab + Ipilimumab",
                             "Nivolumab monotherapy", "Ipilimumab monotherapy",
                             "Pembrolizumab monotherapy", "Encorafenib plus binimetinib",
                             "Trametinib plus dabrafenib")
  colnames(cost_per_day) <- c("PRODUCT Y", "Nivolumab + PRODUCT Y", "Nivolumab + Ipilimumab",
                              "Nivolumab monotherapy", "Ipilimumab monotherapy",
                              "Pembrolizumab monotherapy", "Encorafenib plus binimetinib",
                              "Trametinib plus dabrafenib")
  colnames(cost_per_time_on_treatment) <- c("PRODUCT Y", "Nivolumab + PRODUCT Y", "Nivolumab + Ipilimumab",
                                            "Nivolumab monotherapy", "Ipilimumab monotherapy",
                                            "Pembrolizumab monotherapy", "Encorafenib plus binimetinib",
                                            "Trametinib plus dabrafenib")
  
  return(list(cost_per_mg = cost_per_mg,
              cost_per_day = cost_per_day,
              cost_per_time_on_treatment = cost_per_time_on_treatment))
}


# Function to calculate the administration costs
calculateAdministrationCosts <- function(num_patients_treated, num_administrations_per_day, admin_unit_cost) {
  # Calculate the number of administrations per time on treatment and total administration cost over time on treatment
  num_administrations_per_time_on_treatment <- matrix(0, nrow = 5, ncol = 8)
  total_admin_cost_over_time_on_treatment <- matrix(0, nrow = 5, ncol = 8)
  
  for (i in 1:5) {
    for (j in 1:8) {
      num_administrations_per_time_on_treatment[i, j] <- num_administrations_per_day[j] * num_patients_treated[i, j]
      total_admin_cost_over_time_on_treatment[i, j] <- num_administrations_per_time_on_treatment[i, j] * admin_unit_cost
    }
  }
  
  colnames(num_administrations_per_time_on_treatment) <- c("PRODUCT Y", "Nivolumab + PRODUCT Y", "Nivolumab + Ipilimumab",
                                                           "Nivolumab monotherapy", "Ipilimumab monotherapy",
                                                           "Pembrolizumab monotherapy", "Encorafenib plus binimetinib",
                                                           "Trametinib plus dabrafenib")
  colnames(total_admin_cost_over_time_on_treatment) <- c("PRODUCT Y", "Nivolumab + PRODUCT Y", "Nivolumab + Ipilimumab",
                                                         "Nivolumab monotherapy", "Ipilimumab monotherapy",
                                                         "Pembrolizumab monotherapy", "Encorafenib plus binimetinib",
                                                         "Trametinib plus dabrafenib")
  
  return(list(num_administrations_per_time_on_treatment = num_administrations_per_time_on_treatment,
              total_admin_cost_over_time_on_treatment = total_admin_cost_over_time_on_treatment))
}


# Function to estimate the mg dose needed per day
estimateDosePerDay <- function(dose_size, frequencies, forms, weight) {
  # Define a mapping of frequency options to their corresponding factors
  frequency_factors <- c("Twice a day" = 2, "Once a day" = 1, "Once weekly" = 1/7,
                         "Once every two weeks" = 1/14, "Once every three weeks" = 1/21)
  
  # Calculate the mg per day for each drug based on the dose size, frequency factor, and form
  mg_per_day <- numeric(length(dose_size))
  
  for (i in 1:length(dose_size)) {
    if (forms[i] == "mg") {
      mg_per_day[i] <- dose_size[i] * frequency_factors[frequencies[i]]
    } else if (forms[i] == "mg/kg") {
      mg_per_day[i] <- dose_size[i] * weight * frequency_factors[frequencies[i]]
    } else {
      mg_per_day[i] <- 0  # Invalid form, set dose to 0
    }
  }
  
  return(mg_per_day)
}

# Inputs

names_treatments <- c("PROD Y", "NIV + PROD Y", "NIV + IPILI",
                      "NIV", "IPILI",
                      "PEMB", "ENCOR + BINI",
                      "TRAM + DABRA")
num_treatments <- 8
num_years <- 5

# Define the market_share_with_PRODUCT_Y matrix with column and row names
market_share_with_PRODUCT_Y <- matrix(c(0.3, 0.2, 0.4, 0.1, 0.3,
                                        0.4, 0.3, 0.5, 0.2, 0.4,
                                        0.5, 0.4, 0.3, 0.2, 0.1,
                                        0.2, 0.4, 0.1, 0.3, 0.3,
                                        0.4, 0.2, 0.3, 0.2, 0.1,
                                        0.3, 0.1, 0.4, 0.2, 0.3,
                                        0.2, 0.1, 0.3, 0.4, 0.2,
                                        0.3, 0.2, 0.1, 0.4, 0.1), nrow = 5)
colnames(market_share_with_PRODUCT_Y) <- names_treatments
rownames(market_share_with_PRODUCT_Y) <- paste0("Year ", 1:5)

# Define the market_share_without_PRODUCT_Y matrix with column and row names
market_share_without_PRODUCT_Y <- matrix(c(0.5, 0.4, 0.6, 0.3, 0.4,
                                           0.6, 0.5, 0.4, 0.4, 0.2,
                                           0.4, 0.3, 0.2, 0.2, 0.3,
                                           0.3, 0.4, 0.2, 0.1, 0.2,
                                           0.2, 0.1, 0.3, 0.2, 0.3,
                                           0.3, 0.1, 0.4, 0.2, 0.3,
                                           0.2, 0.1, 0.3, 0.4, 0.2,
                                           0.3, 0.2, 0.1, 0.4, 0.1), nrow = 5)
colnames(market_share_without_PRODUCT_Y) <- names_treatments
rownames(market_share_without_PRODUCT_Y) <- paste0("Year ", 1:5)



dose_size <- c(10, 20, 15, 12, 8, 10, 20, 15)
frequencies <- c(
  "Twice a day", "Once a day", "Once weekly",
  "Once every two weeks", "Once every three weeks",
  "Twice a day", "Once a day", "Once weekly"
)
forms <- c("mg", "mg", "mg/kg", "mg", "mg", "mg", "mg", "mg")

weight <- 70

total_population_males <- 500000
total_population_females <- 600000
percent_diagnosed_males <- 0.02
percent_diagnosed_females <- 0.015
proportion_stage_IIIC_IV <- 0.3
percent_increase_per_year <- 0.05


incident_population <- calculateIncidentPopulation(total_population_males, total_population_females,
                                                   percent_diagnosed_males, percent_diagnosed_females,
                                                   proportion_stage_IIIC_IV, percent_increase_per_year,
                                                   num_years)

# Estimate the mg dose needed per day
dose_per_day <- estimateDosePerDay(dose_size, frequencies, forms, weight)

# Calculate the number of patients treated each year for each drug
num_patients_treated <- calculateNumPatientsTreated(market_share_with_PRODUCT_Y,
                                                    market_share_without_PRODUCT_Y,
                                                    incident_population,
                                                    num_treatments,
                                                    num_years)

# # Calculate the acquisition costs
unit_cost_per_drug <- c(100, 150, 200, 120, 180, 200, 120, 180)
acquisition_costs_wY <- calculateAcquisitionCosts(num_patients_treated$with_PRODUCT_Y,
                                               dose_per_day,
                                               unit_cost_per_drug,
                                               num_years,
                                               num_treatments)

acquisition_costs_woY <- calculateAcquisitionCosts(num_patients_treated$without_PRODUCT_Y,
                                               dose_per_day,
                                               unit_cost_per_drug,
                                               num_years,
                                               num_treatments)

# # Calculate the administration costs
num_administrations_per_day <- c(2, 1, 1, 2, 3,  1, 2, 3)
admin_unit_cost <- 50
administration_costs <- calculateAdministrationCosts(num_patients_treated$with_PRODUCT_Y,
                                                     num_administrations_per_day,
                                                     admin_unit_cost)

# Print the results

print("Incident Population:")
print(incident_population)

print("Number of patients treated each year for each drug:")
print(num_patients_treated$with_PRODUCT_Y)

print("\nAcquisition costs (world with PRODUCT Y):")
print(acquisition_costs_wY$cost_per_time_on_treatment)
print("\nAcquisition costs (world without PRODUCT Y):")
print(acquisition_costs_woY$cost_per_time_on_treatment)
# 
# print("\nAdministration costs:")
# print(administration_costs$total_admin_cost_over_time_on_treatment)
