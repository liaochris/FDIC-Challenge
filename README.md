# FDIC-Challenge

<h2> Introduction </h2>
This is all the code I used for the 2020 FDIC Challenge. In this challenge, the prompt was to analyze the impact of community banks on local economic development. We chosed to examine a more specific verison of this question - the effect of community banks on small business growth, specifically small business growth in a few key industries. 
The code in this repository contains three parts:

1. Data 

2. Cleaning the Data

3. Data Analysis

Further clarification on each of these will be provided in the below sections. 
<h3> Section 1: Data </h3>
For this challenge, I obtained data from three different sources. 

1. FDIC Academic Challenge Data.

First, I used the FDIC Academic Challenge data that was provided to us by the FDIC. This data consisted of reports on all FDIC-insured banks across the United States from 1994 to 2018. These reports included data such as the bank name, year, whether it was a community bank, amount of loans given out, amount of equity held, amount of loans outstanding, etc. The codebook for the data **data-codebook.csv** contains more detailed information on the definition of each of the bank metrics and which metrics were included. This file is called **ACdata.csv.zip** and is zipped because of github upload requirements dictating all files uploaded to be less than 100 megabytes. Other files ending in .zip were also zipped to allow their upload. 

2. Small business Data

Second, I used data from the census on small business patterns on a county level from 1998 to 2018. This data contained information on the number of small businesses in a county, for a given year, as well as the number of smlal businesses in particular industries in a county. Finally, the small business counts are also categorized based off how many employees they have. They are denoted as **zbp__detail.txt.zip**, where __ corresponds to the last two digits of the year the data is from (ie: data from 2002 on small businesses is zbp02detail.txt.zip). In addition, along with this data, I also downloaded the NAICS industry classifications so I could categorize the industries using words instead of a numerical code. These files are denoted by **naics__.txt** where __ corresponds to the last two digits of the first year the classification was used (ie: naics17.txt means the code classifications in the text file were first used in 2017. Since the last year that we have data for is 2018, naics17.txt contains classification codes for 2017 and 2018). The data can be found [here][1].
  
3. ZIP to FIPS County Conversion

Finally, I downloaded a csv file that matched zip codes to FIPS county labels. This is important because the small business data is categorized by zip code, but the bank data is matched to FIPS counties, and a single county contains multiple zip codes, so I would need to aggregate the small business data and reorgabnize it on a FIPS county basis. The datasheet can be found [here][2].

<h3> Section 2: Cleaning the Data </h3>

1. Cleaning and Reorganizing FDIC Data

First, I reorganized the FDIC data (ACdata.csv) by aggregating the quantitative bank metrics by county and year. That way, instead of having individual records for each FDIC-insured bank over the years, I would have data on all the banks within a county for a specified year. This was done in **Cleaning/FDIC_data.ipynb**

2. Cleaning and Reorganizing Small Business Data

Second, since the data given for small businesses was organized on an annual basis, I had to combine all twenty years of the data it into a single dataset. In addition, because we chose to focus our analysis on a few specific industries and small businesses as a whole (regardless of industry), I had to separate this combined dataset into separate files. The file **Cleaning/smallbiz-all.csv"** contains data for all small businesses aggregated together by year and county, regardless of industry. This file was created by **Cleaning/smallbiz-all.ipynb**. In addition, in the folder **Industry_Analysis/industry_data**, there are 11 csv files, of the format "##----" where ## is a two digit number and ##---- stands for the NAICS code of the industry that the corresponding file contains small business data on. These files wre created by **Industry_Analysis/smallbiz-industry.ipynb**. These 11 industries were chosen by picking the 11 industries that contained the most small businesses. 

<h3> Section 3: Data Analysis </h3>

1. Industry Analysis

All the analysis, conducted in R, is located in *Industry_Analysis*. Importantly, there are two types of files there. The first type, denoted using the industry name, such as **Industry_Analysis/past/retail.R** or **Industry_Analysis/past/manufacturing.R** contains analysis on small businesses. However, importantly, these files measure the impact of community banks on small businesses using % business growth in a county, a metric used in the early stages of naalysis. The final metric used to analyze the impact of community banks on small businesses is per capita increase in small business count in a county. These files end in "\_percap" to express the new metric used in analysis. Since we decided to ultimately only focus on a few industries out of the 11 largest, only a few files end in \_percap. All of these are located in **Industry\_Analysis**. 

2. Broader Economic Analysis

As part of the FDIC Challenge, our team also wrote a report that was eventually submitted to the FDIC challenge that included our empirical findings but also included policy suggestions. We found that prominent community bank presence had a positive effect overall on small businesses growth in a county and this effect was magnified post-2008 recession. 
If you wish to see a copy, please email me at chrisliao (at) uchicago (dot) edu. 

[1]: http://example.com/ "Title"
[2]: http://example.org/ "Title"
