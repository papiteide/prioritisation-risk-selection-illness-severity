


clear all

version 16
set mat 600
set memory 400m
set more off
set linesize 80

*The British Household and Panel Survey data to run this code can be downloaded teh UK Data Archive:from https://beta.ukdataservice.ac.uk/datacatalogue/doi/?id=5151#!#2

*The dataset  "retail_price_index_dec2008.dta" must be saved in the folder "$maindir/STATA_replication Desktop/Datasets"

*Note that the individuals Tables are saved in the JUNK folder, except the instrumental variables ones that are saved in the Main Tables folder

*Enter here the main directory for the project
global maindir "C:/Users/uctpamv/OneDrive - University College London/Research/Prioritization_Risk_Selection/IsThereaLink_STATA"

*All orginal BHPS files should be put in the same folder:
global rawBHPS "$maindir/BHPS/UKDA-5151-stata/stata"


global figures "$maindir/STATA_replication Desktop/Figures"
global datasets "$maindir/STATA_replication Desktop/Datasets"
global tables "$maindir/STATA_replication Desktop/Tables"
global junk "$maindir/STATA_replication Desktop/junk folder"
global log "$maindir/STATA_replication Desktop/Log files"
global do "$maindir/STATA_replication Desktop/Do files"

global EMF "$maindir/STATA_replicaton Desktop/Figures/EMF"
global EPS "$maindir/STATA_replication Desktop/Figures/EPS"
global GPH "$maindir/STATA_replication Desktop/Figures/GPH"
global JPG "$maindir/STATA_replication Desktop/Figures/JPG"
global PNG "$maindir/STATA_replication Desktop/Figures/PNG"
global PS "$maindir/STATA_replication Desktop/Figures/PS"
global WMF "$maindir/STATA_replication Desktop/Figures/WMF"


cd "$do"

do "create_dataset.do"
* This saves the dataset "dataset_not_cleaned"

cd "$do"
do "clean_dataset.do"
* This saves the cleaned dataset "dataset_cleaned"

cd "$do" 

*The below executes the analysis
do "analysis_without_bootstrap.do"
cd "$do" 
do "analysis_with_bootstrap.do"

cap log close
exit 

