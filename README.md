## Imputation and analysis of sports volunteering rates in English local areas using Sport England's ActiveLives survey

James Liley, Bilal Ashraf, Caroline Dodd-Reynolds, Lindsay Findlay-King, Iain Lindsey, Jochen Einbeck

#### Overview
This repository concerns rates of sports volunteering in English local areas, using data from Sport England's ActiveLives survey. Analyses are in R. Data is not included in this repository but can be downloaded from the UK data service. 


#### Structure
This repository contains two zipped files, each containing datasets and code in the R language. 

 - The first file (ActiveLives\_share.zip) contains a pipeline to impute individual-level volunteering status (namely, whether an invididual had engaged in sports volunteering in the past month or in the past year). Please see the report contained in the zip file for more details. 
 - The second file (ActiveLives\_shiny.zip) contains code to generate an R shiny app to visualise data. 

#### Usage

This repository does not contain any ActiveLives data. Relevant datasets are publically available from  [UKDA](https://doi.org/10.5255/UKDA-Series-2000120). 

To use this code, please do the following:
 1. Clone this repository locally
 2. Download the 2017-18 ActiveLives survey as a ```.sav``` file. The script will expect it to be called ```20220209 Active Lives Survey_May 17-18 data_Shared.sav``` and look for it in the subdirectory ```Analysis/Data/Original/```. The data also needs the codebook, which it will expect to be called ```"Active Lives Adult Code Book_Mid year 3_V1_final.xlsx" ```, also in the subdirectory ```Analysis/Data/Original/```. 
 3. In R, set the working directory to the folder ```Analysis```. 
 4. Please see the README in ```Analysis/Code``` for details on running analyses. The file ```Analysis/Code/read_raw.R``` will read the ```.sav``` file, and files ```Analysis/Code/initial_analysis.R``` and ```Analysis/Code/analysis.R``` will perform the main analyses. 

To use the app, please do the following (after doing the above):
 1. In R, set the working directory to the folder ```Shiny```
 2. Run the file ```la_associations.R```
 3. Run the file ```chloropleth.R```. This will generate the file ```Shiny/App/la_data.RData```.
 4. The app should now work. 