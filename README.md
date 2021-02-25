# seattleViz R package
R package for visualizing data for Seattle

## data-raw/DATASET.R
Script contains all loaded objects and parameters common across multiple functions. 
Changes made to values in this script will update for all functions that call these objects. 
When making any changes to this script, it must be run for objects to save to sysdata.rda.

## R/functions
Folder contains scripts for each function in seattleViz package. 
* When making any updates
to a function's documentation, run devtools::document() in the console to save
documentation. 
* To test/make changes to a function on your local machine before pushing to Github, 
run devtools::load_all() and library(oakViz) in the console before calling the function.
