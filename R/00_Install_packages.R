#----------------------------------------------------------#
#
#
#               PalaeoR R-Ratepol workshop
#
#                   Install packages
#                 
#
#                       O. Mottl
#                         2021
#
#----------------------------------------------------------#

# Configuration script to install packages 


#----------------------------------------------------------#
# 1. Install packages -----
#----------------------------------------------------------#

# set list of packages
packages_list <- 
  c("tidyverse",
    "neotoma",
    "Bchron",
    "janitor",
    "here",
    "cowplot",
    "ggpubr")

# install them
install.packages(packages_list)

# install R-Ratepol from GitHub 
remotes::install_github("HOPE-UIB-BIO/R-Ratepol-package")
