#### Install missing packages ####
#
# Last updated: 2022_01_17

# list of required packages as strings
required_packages = c(
  'dplyr',
  'ggplot2',
  'crqa',
  'tidyr',
  'tseriesChaos',
  'scales',
  'effectsize',
  'gtable',
  'gridExtra',
  'gtools',
  'lme4',
  'sjPlot',
  'emmeans',
  'viridis',
  'ggforce',
  'data.table',
  'scales',
  'tidyr'
)

# install missing packages (adapted from <http://stackoverflow.com/a/4090208> and a-paxton/perception-memory-coordination)
missing_packages = required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
