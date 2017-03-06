# ms-cfa-lipids
Confirmatory factor analysis (CFA) of multivariate lipid distributions across time and race-ethnic groups: United States, 2003-2012

# CFA analyses in Mplus

1. Run [mplus-results.Rmd](mplus-results.Rmd) in R
    - Prior to running this program run [export-mplus.R](export-mplus.R) to get data in Mplus format using [MplusAutomation](https://cran.r-project.org/web/packages/MplusAutomation/index.html) R package.
        - Prior to running this program, get NHANES data in the [read-data.R](read-data.R) program, which pulls NHANES data off of the web.
    - Use templates with MplusAutomation package (createModels function) to make a batch of scripts to run each of the four models by year, race and sex groups.
        - [Template for model 1](mplus/compare-years-template/template_m1.txt)
        - [Template for model 2](mplus/compare-years-template/template_m2.txt)
        - [Template for model 3](mplus/compare-years-template/template_m3.txt)
        - [Template for model 4](mplus/compare-years-template/template_m4.txt)
    - Run models in Mplus using MplusAutomation in batch mode in virtuallab.unc.edu.
        - Log into [virtual.lab.unc.edu](virtuallab.unc.edu)
        - Open Mplus
        - Open Windows File Explorer
            - Type in %userprofile% to get to folder Mplus access on the remote site
                - Note: I had to contact support on 9/23/2016. A special version of R on virtuallab was set up for me to be able to call Mplus in virtuallab.
            - Open new file explorer and open local folder specified in [Template for model 1](mplus/compare-years-template/template_m1.txt), currently 'C:/temp/models2'.
            - Transfer .inp files from c:/temp/models2/ local folder to remote folder so can run MPlus program in virtuallab.unc.edu
        - Open R in virtuallab.unc.edu and run [run-models.R](run-models.R). This will run all the Mplus programs one after another.
        - Got to remote folder and transfer the .out files to the local folder to save the Mplus output.
    - Take Mplus output and read in all output using extractModelParameters and extractModelSummaries functions in MplusAutomation in the [mplus-results.Rmd](mplus-results.Rmd) program.
    - Make tables in R with the Mplus results now in R data frame formats.
            
        
        
    
