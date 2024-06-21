# TFM: Master in Computational Social Science 
*UC3M*
2023-24

## "Modeling the Relationship Between Wind Energy Generation and Energy Intensity in Spain’s Autonomous Communities"

### Reproduction 
Download the zip file or clone the repository to your computer. Follow these steps to reproduce my analysis: 

1) Run the file `collecting data.R`. This file will pull the data from Red Eléctrica's REData API. The data is already downloaded and in the repository in case you do not want to re-run the code (it will take some time to pull all the data). 
2) Run file `CollectingCovariateData.R`. Here you will manipulate and join the downloaded data into a format you can use for the analysis part. This file is necessary to save the variables in the global environment. 
3) Run file `Initial Descriptive Analysis.qmd`. This file will perform initial analysis, which includes several charts and some data wrangling necessary for the final modelling parts.
4) Run files `Modelling_Energy_Intensity-V3.qmd` and `Thesis report draft1.qmd` for the plots used in the final paper. 
