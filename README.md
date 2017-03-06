# ms-cfa-lipids
Confirmatory factor analysis (CFA) of multivariate lipid distributions across time and race-ethnic groups: United States, 2003-2012

# CFA analyses in Mplus

1. Run [mplus-results.Rmd](mplus-results.Rmd) in R
    - Prior to running this program run [export-mplus.R](export-mplus.R) to get data in Mplus format using [MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html) R package.
        - Prior to running this program, get NHANES data in the [read-data.R](read-data.R) program, which pulls NHANES data off of the web.
    - Use templates with MplusAutomation package to make a batch of scripts to run each of the four models by year, race and sex groups.
        - [Template for model 1](mplus\compare-years-template\template_m1.txt)
        - [Template for model 2](mplus\compare-years-template\template_m2.txt)
        - [Template for model 3](mplus\compare-years-template\template_m3.txt)
        - [Template for model 4](mplus\compare-years-template\template_m4.txt)
    
