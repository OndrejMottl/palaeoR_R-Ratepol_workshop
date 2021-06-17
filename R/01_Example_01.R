#----------------------------------------------------------#
#
#
#               PalaeoR R-Ratepol workshop
#
#                       Example 01
#                 
#
#                       O. Mottl
#                         2021
#
#----------------------------------------------------------#

# This workflow should show full strength of *RRatepol package* and serve as 
#   step by step guidance starting from downloading dataset from Neotoma,
#   building age-depth models, to estimating rate-of-change 
#   using age uncertainty.

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

# Install packages if needed
# source("R/00_Install_packages.R")

# Load confic file
source("R/00_Config_file.R")


#----------------------------------------------------------#
# 2. Download dataset 'Ageröds Mosse' -----
#----------------------------------------------------------#
datataset_id <- 12

example_01_dataset_download <- 
  neotoma::get_download(datataset_id)

chronology_id <- 
  example_01_dataset_download[[1]]$sample.meta$chronology.id %>% 
  unique()

example_01_chron_control_table_download <- 
  neotoma::get_chroncontrol(chronology_id)


#----------------------------------------------------------#
# 3. Prepare the pollen counts -----
#----------------------------------------------------------#
example_01_counts <- 
  example_01_dataset_download[[1]]$counts 

example_01_taxon_lits <- 
  example_01_dataset_download[[1]]$taxon.list

example_01_taxon_lits_selected <- 
  example_01_taxon_lits %>% 
  dplyr::filter(variable.element == "pollen")

example_01_counts_selected <-
  example_01_counts %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(., var = "sample_id") %>% 
  as_tibble() %>% 
  dplyr::select(
    sample_id,
    dplyr::any_of(example_01_taxon_lits_selected$taxon.name)) %>% 
  janitor::clean_names() 

# View the taxa 
example_01_counts_selected %>% 
  head()

.plot.taxa(
  data_source_taxa =  example_01_counts_selected,
  n_taxa = 15)

# Here, we strongly advocate that attention should be paid to the section 
#   of ecological ecological group, as well, as harmonisation of the 
#   pollen taxa. However, that is not subject of this workflow.


#----------------------------------------------------------#
# 4. Preparation of the levels -----
#----------------------------------------------------------#

#--------------------------------------------------#
# 4.1. Sample depth ----- 
#--------------------------------------------------#

# Extract depth for each level
example_01_level <-
  example_01_dataset_download[[1]]$sample.meta %>% 
  as_tibble() %>% 
  rename(sample_id = sample.id) %>% 
  dplyr::select(sample_id, depth) %>% 
  dplyr::mutate_if(is.integer, as.character) %>% 
  dplyr::arrange(depth)

# View the sample depth
example_01_level %>% 
  head()


#--------------------------------------------------#
# 4.2. Age depth modelling ----- 
#--------------------------------------------------#
# We will recalculate new age-depth model 'de novo' using *Bchron* package.
#  In this toy example we will use only iteration multiplier (*i_multiplier*) 
#  of 1 to reduce the computation time. However, we strongly recommend to 
#  increase it to 5 for any normal age-depth model construction.

# Prepare chron.control table and run Bchron
example_01_chron_control_table <-
  example_01_chron_control_table_download$chron.control %>% 
  # here we calculate the error as the avarage as the age.old and age age.young
  dplyr::mutate(
    error = round((age.old - age.young) / 2)) %>% 
  # as Bchron cannot accept error of 0, we need to replace the value with 1 
  dplyr::mutate(
    error = replace(error, error == 0, 1)) %>% 
  # as Bchron cannot NA thickness, we will replace with 1  
  dplyr::mutate(
    thickness = tidyr::replace_na(thickness, 1)) %>% 
  # we need to specify which calibration curve should be used for what point
  dplyr::mutate(
    curve = ifelse(control.type == "Radiocarbon", "intcal20", "normal")) %>% 
  dplyr::select(
    chron.control.id, age, error, depth, thickness, control.type, curve)

# View the table
example_01_chron_control_table %>% 
  head()

# Here we only present few of the important steps of preparation of 
#   chron.control table. There are many more potential issues issues but 
#   solving those is not the focus of this workflow.

i_multiplier <- 1 # increase to 5

n_iteration_default <- 10e3
n_burn_default <- 2e3
n_thin_default <- 8

n_iteration <- n_iteration_default * i_multiplier
n_burn <- n_burn_default * i_multiplier
n_thin <- n_thin_default * i_multiplier

example_01_bchron <- 
  Bchron::Bchronology(
    ages = example_01_chron_control_table$age,
    ageSds = example_01_chron_control_table$error,
    positions = example_01_chron_control_table$depth,
    calCurves = example_01_chron_control_table$curve,
    positionThicknesses = example_01_chron_control_table$thickness,
    iterations = n_iteration,
    burn = n_burn,
    thin = n_thin,
    jitterPositions = FALSE)

# You can also just load the pre-made Bchron output (i_multiplier = 5)
# example_01_bchron <-
#   readr::read_rds(here::here("Data/Output/Example_01_ad_model.rds"))

# View the Age-depth model
plot(example_01_bchron) + 
  theme_custom() +
  labs(
    x = "Age (cal yr BP)",
    y = "Depth (cm)")

# predict ages
age_position <- 
  Bchron:::predict.BchronologyRun(
    example_01_bchron,
    newPositions = example_01_level$depth)

age_uncertainties <- 
  age_position %>% 
  as.data.frame() %>% 
  dplyr::mutate_all(., as.integer) %>% 
  as.matrix()

colnames(age_uncertainties) <- 
  example_01_level$sample_id

# View age uncertainties
age_uncertainties %>% 
  head()

# Visualize them
.plot.uncertainties(
  data_source = age_uncertainties,
  n_uncertainties = 100)

# use median of age uncertainties as default value of levels
example_01_level_predicted <- 
  example_01_level %>% 
  dplyr::mutate(
    age = apply(
      age_uncertainties, 2,
      stats::quantile,
      probs = 0.5))

# View levels
example_01_level_predicted %>% 
  head()

#----------------------------------------------------------#
# 5. Estimation Rate-of-Change -----
#----------------------------------------------------------#
# Here we use the the prepared data to estimate the rate of vegetation change.
#   We will use the method of the *binning with the mowing window*, 
#   *Shepard's 5-term filter* as data smoothing *Chi-squared coefficient* as
#   dissimilarity coefficient. This is again a toy example for a quick 
#   computation and we would recommend  increasing the *randomisations* to 
#   10.000 for any real estimation. 

randomisations <- 10 # increase to 10e3

# estimate RoC
example_01_roc <-
  RRatepol::fc_estimate_RoC(
    data_source_community = example_01_counts_selected,
    data_source_age = example_01_level_predicted,
    age_uncertainty = age_uncertainties,
    smooth_method = "shep",
    DC = "chisq",
    Working_Units = "MW",
    bin_size = 500,
    time_standardisation = 500,
    standardise = TRUE,
    N_individuals = 150,
    rand = randomisations) 

# You can also just load the pre-made RoC output (randomisations = 10e3)
# example_01_roc <-
#   readr::read_rds(here::here("Data/Output/Example_01_roc.rds"))

# View the results
example_01_roc %>% 
  head()

# Visualize the result
RRatepol::fc_plot_RoC_sequence(
  data_source = example_01_roc,
  age_threshold= max(example_01_roc$Age),
  Roc_threshold = max(example_01_roc$ROC_up))+
  theme_custom()

# detect peak-points
example_01_roc_peaks <-
  RRatepol::fc_detect_peak_points(
    data_source = example_01_roc,
    method = "trend_non_linear",
    sd_threshold = 2)

# View the results
example_01_roc_peaks %>% 
  head()

# Visualize the result
(example_01_roc_plot <-
    RRatepol::fc_plot_RoC_sequence(
      data_source = example_01_roc_peaks,
      age_threshold= max(example_01_roc_peaks$Age),
      Roc_threshold = max(example_01_roc_peaks$ROC_up),
      Peaks = TRUE)+
    theme_custom())

# Compare with pollen 
example_01_taxa_plot <- 
    .plot.taxa(
      data_source_taxa =  example_01_counts_selected,
      data_source_levels = example_01_level_predicted,
      n_taxa = 15)

.plot.double(
  example_01_roc_plot,
  example_01_taxa_plot,
  relative_widths = c(0.3, 1))
  

