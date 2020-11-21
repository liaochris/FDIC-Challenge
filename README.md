# FDIC-Challenge

This is all the code I used for the 2020 FDIC Challenge. 
I zipped the csv files in the data folder because they were exceeding github's file size limit. Same with the ACdata.csv.zip. 
ACdata.csv is data from the FDIC. Data in the data folder is from the small business county patterns survey from the US census.
I also obtained the naics from there as well. 
FInding an FIPS to zip conversion spreadsheet is pretty simple - just google for one. 
The R files in Industries are the analysis I did on the 12 most frequently occurring industries from the aggregated dataset, the csv files are produced by the R files. 
The files with names ending in _percap are the ones that I used for statistical analysis, where I ran diff-in-diff regressions. There are four files that fall under category: cb/noncb ratio, construction, retail and real estate. 
Export contains the primary python code used for the analysis. Speciifcally, I used the  python code to clean my data. Notably, wihle both the smallbiz1.csv file and Changes1.csv file are in the folder, only Changes1.csv can be produced natively within the folder through Changes1.ipynb. Changes1.ipynb is the jupyter notebook that cleans the ACData.csv and transforms it into a usable format. See the file for more details.
Smallbiz - all.ipynb in the main directory produces smallbiz1.csv. Eventually I hope to get around to updating the code so that it can be run in the Export folder, but for now, one should just drag the file to the Export folder once smallbiz1.csv is produced and overwrite any changes made to the internal smallbiz1.csv file. 
If you wish to see the paper the team and I wrote, please contact me@ chrisliao (at) uchicago (dot) edu. 
