#----------------------------------------------------------#
#
#
#               PalaeoR R-Ratepol workshop
#
#                       Example 02
#                 
#
#                       O. Mottl
#                         2021
#
#----------------------------------------------------------#

# Show other settings of R-Ratepol

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

# Install packages if needed
# source("R/00_Install_packages.R")

# Load confic file
source("R/00_Config_file.R")

# set number of randominsation
n_rand <- 10 # set to 10e3 for better result but take longer to calculate 

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

example_01_data <-
  readr::read_rds("Data/Output/Example_01_data.rds")


#----------------------------------------------------------#
# 3. Scenario 1  -----
#----------------------------------------------------------#

# 'Classic' approach with individual levels

scenario_1 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = FALSE,
    smooth_method = "none",
    DC = "chisq",
    Working_Units = "levels",
    standardise = FALSE) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_1,
  age_threshold= max(scenario_1$Age),
  Roc_threshold = max(scenario_1$ROC_up),
  Peaks = FALSE)+
  theme_custom()

#----------------------------------------------------------#
# 3. Scenario 2  -----
#----------------------------------------------------------#

# Addition of smoothing of pollen data

scenario_2 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = FALSE,
    smooth_method = "m.avg",
    smooth_N_points = 9,
    DC = "chisq",
    Working_Units = "levels",
    standardise = FALSE) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_2,
  age_threshold= max(scenario_2$Age),
  Roc_threshold = max(scenario_2$ROC_up),
  Peaks = FALSE)+
  theme_custom()


#----------------------------------------------------------#
# 3. Scenario 3  -----
#----------------------------------------------------------#

# Addition of standardization by random sub-sampling to 150 pollen grains
#    in each level

scenario_3 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = FALSE,
    smooth_method = "m.avg",
    smooth_N_points = 9,
    DC = "chisq",
    Working_Units = "levels",
    standardise = TRUE,
    N_individuals = 150,
    rand = n_rand) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_3,
  age_threshold= max(scenario_3$Age),
  Roc_threshold = max(scenario_3$ROC_up),
  Peaks = FALSE)+
  theme_custom()

#----------------------------------------------------------#
# 3. Scenario 4 -----
#----------------------------------------------------------#

# Addition of age uncertainties from age-depth model

scenario_4 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = example_01_data$age_uncertainty,
    smooth_method = "m.avg",
    smooth_N_points = 9,
    DC = "chisq",
    Working_Units = "levels",
    bin_size = 500,
    standardise = TRUE,
    N_individuals = 150,
    rand = n_rand) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_4,
  age_threshold= max(scenario_4$Age),
  Roc_threshold = max(scenario_4$ROC_up),
  Peaks = FALSE)+
  theme_custom()

#----------------------------------------------------------#
# 3. Scenario 5 -----
#----------------------------------------------------------#

# Switching to using 'bins' of 500 years instead of individuals levels

scenario_5 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = example_01_data$age_uncertainty,
    smooth_method = "m.avg",
    smooth_N_points = 9,
    DC = "chisq",
    Working_Units = "bins",
    bin_size = 500,
    standardise = TRUE,
    N_individuals = 150,
    rand = n_rand) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_5,
  age_threshold= max(scenario_5$Age),
  Roc_threshold = max(scenario_5$ROC_up),
  Peaks = FALSE)+
  theme_custom()

#----------------------------------------------------------#
# 3. Scenario 6 -----
#----------------------------------------------------------#

# Using 'binning with moving window' 

scenario_6 <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_data$pollen_data,
    data_source_age = example_01_data$sample_age,
    age_uncertainty = example_01_data$age_uncertainty,
    smooth_method = "m.avg",
    smooth_N_points = 9,
    DC = "chisq",
    Working_Units = "MW",
    bin_size = 500,
    Number_of_shifts = 3,
    standardise = TRUE,
    N_individuals = 150,
    rand = n_rand) 

RRatepol::fc_plot_RoC_sequence(
  data_source = scenario_6,
  age_threshold= max(scenario_6$Age),
  Roc_threshold = max(scenario_6$ROC_up),
  Peaks = FALSE)+
  theme_custom()




