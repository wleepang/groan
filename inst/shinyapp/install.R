# install package dependencies
if (!require('shiny')) install.packages('shiny')
if (!require('groan')) {
  if (!require('devtools')) install.packages('devtools')
  library(devtools)
  install_github('groan', 'wleepang')
}