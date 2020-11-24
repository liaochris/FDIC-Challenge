# FDIC-Challenge

<h2> Introduction </h2>
This is all the code I used for the 2020 FDIC Challenge. In this challenge, the prompt was to analyze the impact of community banks on local economic development. We chosed to examine a more specific verison of this question - the effect of community banks on small business growth, specifically small business growth in a few key industries. 
The code in this repository contains three things:
1. the data used for the challenge
2. the jupyter notebooks that cleaned our data files
3. the analysis that we conducted in R
Further clarification on each of these will be provided in the below sections. 
<h3> Section 1: Data </h3>
For this challenge, I obtained data from three different sources. 
1. First, I used the FDIC Academic Challenge data that was provided to us by the FDIC. This data consisted of reports on all FDIC-insured banks across the United States from 1994 to 2018. These reports included data such as the bank name, year, whether it was a community bank, amount of loans given out, amount of equity held, amount of loans outstanding, etc. The codebook for the data "data-codebook.csv" contains more detailed information on the definition of each of the bank metrics and which metrics were included. This file is called <strong> "ACdata.csv.zip" </strong> and is zipped because of github upload requirements dictating all files uploaded to be less than 100 megabytes. Other files ending in .zip were also zipped to allow their upload. 
2. Second, I used data from the census on small business patterns on a county level from 1998 to 2018. This data contained information on the number of small businesses in a county, for a given year, as well as the number of smlal businesses in particular industries in a county. Finally, the small business counts are also categorized based off how many employees they have. They are denoted as <strong>zbp__detail.txt.zip</strong>, where __ corresponds to the last two digits of the year the data is from (ie: data from 2002 on small businesses is zbp02detail.txt.zip). In addition, along with this data, I also downloaded the NAICS industry classifications so I could categorize the industries using words instead of a numerical code. These files are denoted by <strong> naics__.txt <strong> where __ corresponds to the last two digits of the first year the classification was used (ie: naics17.txt means the code classifications in the text file were first used in 2017. Since the data is up to 2018, naics17.txt contains classification codes for 2017 and 2018). 

I zipped the csv files in the data folder because they were exceeding github's file size limit. Same with the ACdata.csv.zip. 

ACdata.csv is data from the FDIC. Data in the data folder is from the small business county patterns survey from the US census.

I also obtained the naics from there as well. 

FInding an FIPS to zip conversion spreadsheet is pretty simple - just google for one. 

The R files in Industries are the analysis I did on the 12 most frequently occurring industries from the aggregated dataset, the csv files are produced by the R files. 

The files with names ending in "percap" are the ones that I used for statistical analysis, where I ran diff-in-diff regressions. There are four files that fall under category: cb/noncb ratio, construction, retail and real estate. 

Export contains the primary python code used for the analysis. Speciifcally, I used the  python code to clean my data. Notably, wihle both the smallbiz1.csv file and Changes1.csv file are in the folder, only Changes1.csv can be produced natively within the folder through Changes1.ipynb. Changes1.ipynb is the jupyter notebook that cleans the ACData.csv and transforms it into a usable format. See the file for more details.

Smallbiz - all.ipynb in the main directory produces smallbiz1.csv. Eventually I hope to get around to updating the code so that it can be run in the Export folder, but for now, one should just drag the file to the Export folder once smallbiz1.csv is produced and overwrite any changes made to the internal smallbiz1.csv file. 

If you wish to see the paper the team and I wrote, please contact me@ chrisliao (at) uchicago (dot) edu. 
