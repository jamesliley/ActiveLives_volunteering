# README

The R scripts in this folder generate results in the report to ActiveLives.

Script 'read_raw.R' reads the original .sav and .xlsx files, determines training/validation/test status and saves files as .fst objects, which can be quickly read into R.
Script 'functions.R' contains auxiliary functions
Script 'initial_analysis.R' analyses the properties of the dataset
Script 'analysis.R' runs the general predictive model. Set variable 'yname' to either "Volint_ANY" or "VOLMTH_POP" (lines 47/48) to run the analysis for last-year or last-month respectively.
Script 'topic_analysis.R' generates and analyses a topic model for features. 

Outputs from all scripts are written to the folder 'Output'. R objects are saved in folder R_objects (currently empty) to speed up calculations.