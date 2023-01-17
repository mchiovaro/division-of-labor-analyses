#### Load necessary packages ####
#
# Last updated: 2022_01_17

# list of required packages
required_packages = c(
  'dplyr',
  'ggplot2',
  'crqa',
  'tidyr',
  'tseriesChaos',
  'scales'
)

# load required packages
invisible(lapply(required_packages, require, character.only = TRUE))
