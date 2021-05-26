#----------------------------------------------------------#
#
#
#               PalaeoR R-Ratepol workshop
#
#                     Config file
#                 
#
#                       O. Mottl
#                         2021
#
#----------------------------------------------------------#

# Configuration script for the repo 

#----------------------------------------------------------#
# 1. Attach packages and load functions -----
#----------------------------------------------------------#
library(tidyverse)
library(RRatepol)
library(neotoma)
library(Bchron)
library(janitor)
library(here)
library(cowplot)
library(ggpubr)


#----------------------------------------------------------#
# 2. Load functions -----
#----------------------------------------------------------#

# load custom functions
functions_list <-
  list.files("R/Functions/")

sapply(paste0("R/functions/", functions_list, sep =""), source)

#----------------------------------------------------------#
# 3. Graphical options -----
#----------------------------------------------------------#

text_size <- 15
line_size <- 0.1

theme_custom <- function(){
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(
        angle = 45),
      line = element_line(size = line_size),
      text = element_text(size = text_size))
}
  

