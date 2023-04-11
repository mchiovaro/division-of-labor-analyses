#### Load packages ####
#
# Last updated: 2022_04_10

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

# "pander_lme": simplify lme4 printouts (available on GitHub: https://github.com/a-paxton/stats-tools)
pander_lme = function(lme_model_name, stats.caption){
  
  # load in pander
  library(pander)
  
  # disable scientific notation
  options(scipen = 999)
  
  # convert the model summary to a dataframe
  neat_output = data.frame(summary(lme_model_name)$coefficient)
  
  # round p-values (using Psychological Science's recommendations)
  neat_output$p = 2*(1-pnorm(abs(neat_output$t.value)))
  neat_output$p[neat_output$p < .0005] = round(neat_output$p[neat_output$p < .0005],4)
  neat_output$p[neat_output$p >= .0005] = round(neat_output$p[neat_output$p >= .0005],3)
  neat_output$p[neat_output$p >= .25] = round(neat_output$p[neat_output$p >= .25],2)
  
  # create significance and trending markers
  neat_output$sig = ' '
  neat_output$sig[neat_output$p < .1] = '.'
  neat_output$sig[neat_output$p < .05] = '*'
  neat_output$sig[neat_output$p < .01] = '**'
  neat_output$sig[neat_output$p < .001] = '***'
  
  # set a caption that includes R-squared values
  if (stats.caption == TRUE){
    
    # use MuMIN to calculate R-squared
    library(MuMIn)
    model_marginal_r_squared = r.squaredGLMM(lme_model_name)[[1]]
    model_conditional_r_squared = r.squaredGLMM(lme_model_name)[[2]]
    neat_caption = paste('**Marginal *R*-squared: ',
                         round(model_marginal_r_squared,2), 
                         ". Conditional *R*-squared: ",
                         round(model_conditional_r_squared,2),".**",sep="")
    
    # return the table
    return(pander(neat_output, split.table = Inf, caption = neat_caption, style = 'rmarkdown'))
    
  } else { # or return a table without it
    return(pander(neat_output, split.table = Inf, style = 'rmarkdown'))
  }
}
