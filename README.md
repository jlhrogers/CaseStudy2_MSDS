# MSDS_CaseStudy2_Master
Jose Torres and Lisa Street

### Introduction:

The files and folders contained within this repo were created to analyze Procrastination and HDI (Human Development Index) data for information on the procrastination tendencies of Company Corp.'s customer base.  Details below provide additional information of each file.

<br>

**Code Folder:**

* MSDS6306_Final_Case_Study_Functions.R - a set of custom functions used in the analysis by Cool DS Inc.
* MSDS_CaseStudy2_Master.Rmd - the R Markdown file created by Cool DS Inc.
* MSDS_CaseStudy2_Master.html - the html output file created when knitting the MSDS_CaseStudy2_Master.Rmd.
* MSDS_CaseStudy2_Master.md - the Markdown file that details the project analysis and corresponding visuals (graphs).
* MSDS_CaseStudy2_Master_files/figure-html folder - holds the images created when rendering the R Markdown. These images 
        can be seen in the Markdown file.
 
 <br>
 
**Data Folder:**

* HDI.csv - contains data collected from the UN's Human Development Index forecast.  This data was originally scraped from 
        a Wikipedia site, but was written to a csv file for data transparency.\
* Procrastination.csv - data gathered from a random sample of Company Corp.'s customer population.  
* MSDS_CaseStudy2_Master_Codebook.md - a Markdown file that details the data in both the HDI.csv and Procrastination.csv files.

<br>

**Output Folder:**

* MSDS_CaseStudy2_Master_Question6_All_Data.csv - a data dump of all the Procrastination data merged with all of the HDI data.
* MSDS_CaseStudy2_Master_Question6_HDI_Table_and_Country_Sort.csv - a data dump of the HDI table scraped from Wikipedia, with HDI       
        Categories (Very High, High, Medium, Low) and a listing of countries, sorted by mean DP and mean AIP, highest to lowest.
* Job_Keyword_Mapping.csv - details of the occupations listed in the Procrastination file and the category to which they were assigned
        for analysis.
* Remaining_Countries.csv - a list of the remaining countries not displayed in the Markdown file, that shows the top countries sorted by 
        survey participant count, hightest to lowest.  The top 20 countries are shown in the 'MSDS_CaseStudy2_Master.md' file.

<br>

### Session Info:

R version 3.3.2 (2016-10-31)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

locale:
LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
kableExtra_0.6.1 knitr_1.17       scales_0.5.0     stringr_1.2.0    reshape2_1.4.2   ggpubr_0.1.6     magrittr_1.5    
stargazer_5.2    ggplot2_2.2.1    tidyr_0.7.2      rvest_0.3.2      xml2_1.1.1       dplyr_0.7.4     

loaded via a namespace (and not attached):
Rcpp_0.12.10         highr_0.6            plyr_1.8.4           bindr_0.1            tools_3.3.2          digest_0.6.12       
evaluate_0.10.1      tibble_1.3.4         gtable_0.2.0         viridisLite_0.2.0    pkgconfig_2.0.1      rlang_0.1.2         
ggsci_2.8            curl_3.0             bindrcpp_0.2         httr_1.3.1           hms_0.3              cowplot_0.9.1       
rprojroot_1.2        grid_3.3.2           glue_1.1.1           R6_2.2.0             XML_3.98-1.9         rmarkdown_1.6.0.9007
selectr_0.3-1        purrr_0.2.4          readr_1.1.0          backports_1.1.0      htmltools_0.3.6      assertthat_0.1      
colorspace_1.3-2     labeling_0.3         stringi_1.1.6        lazyeval_0.2.0       munsell_0.4.3 

