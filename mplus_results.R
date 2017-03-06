# mplus_results.R

# All code borrowed from file to create html, mplus-results.Rmd
# >>>>---------------------------------------------------------
# Extract results from mplus output, .out 
# See https://cran.r-project.org/web/packages/MplusAutomation/vignettes/Vignette.pdf

library(knitr)
library(data.table)
library(readxl)
library(ggplot2)
library(plyr)
library("MplusAutomation")
library(ztable)
options(ztable.type="viewer")
library(htmlTable)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr read1
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# PUT into .Rda files. takes too long to run
# each time.

# read in results for differences across years (all ages) and racial/ethnic groups
# All data made in mplus-results.Rmd

# extract out all parameters from output
amp.years.params <- extractModelParameters("C:\\temp\\models2", 
                                           recursive=F)
warnings() # not sure why there are some problems reading in data -- missing values?
names(amp.years.params)
save(amp.years.params, file="201703-aha-poster-2\\ayp.Rda")

#NOTE: be careful with the recursive setting. will get repeats that mess up tables below.
# 
# ayp = amp.years.params # save a backup because it takes so long to run
# names.ayp = names(ayp)

# extract out model fit summaries from output
amp.years.sums <- extractModelSummaries("C:\\temp\\models2", recursive=F)
names(amp.years.sums)
save(amp.years.sums, file="201703-aha-poster-2\\ays.Rda")


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr data-handle1
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#rm(list=ls())

#load("201703-aha-poster-2\\ayp.Rda") # for debugging
load("ayp.Rda")
# check names of objects in list
#names(amp.years.params) # names too long, simplify
nms = names(amp.years.params) 

gender.g = ifelse(grepl("sex.all.", nms)==T, 'all',
                  ifelse(grepl("sex.fem", nms)==T, 'female',
                         ifelse(grepl("sex.mal", nms)==T, 'male', 
                                ifelse(grepl("groups.gender", nms)==T, 'gen', NA))))
table(gender.g) # check

ageg = ifelse(grepl("12to19",nms)==T, "12to19",
              ifelse(grepl("20to50",nms)==T,"20to50",
                     ifelse(grepl("50to80",nms)==T, "50to80", NA)))

table(ageg) #check

gps = ifelse(grepl("groups.year",nms)==T, "YR",
             ifelse(grepl("groups.race", nms)==T, "RE",
                    ifelse(grepl("groups.sex", nms)==T, "GE", NA)))
table(gps)

model = ifelse(grepl("m1.", nms)==T, 1,
               ifelse(grepl("m2.", nms)==T, 2,
                      ifelse(grepl("m3.", nms)==T, 3,
                             ifelse(grepl("m4.", nms)==T, 4, NA))))
table(model)

names(amp.years.params) = paste0("m", model, "y", ageg, "g", gps, "gender", gender.g)
names(amp.years.params) #check
head(amp.years.params)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr data-handle2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>---------------------------------------
# PREP model fit info
# >>>>---------------------------------------

#load("201703-aha-poster-2\\ays.Rda") # for debugging
load("ays.Rda")
head(amp.years.sums)


# add descriptors to model fit data

amp.years.sums$gender.g = with(amp.years.sums, {
  ifelse(grepl("-all-", Filename)==T, 'all',
                  ifelse(grepl("-female-", Filename)==T, 'female',
                         ifelse(grepl("-male-", Filename)==T, 'male', NA)))
})

table(amp.years.sums$gender.g) # check

amp.years.sums$agegrp = with(amp.years.sums, {
  ifelse(grepl("12to19",Filename)==T, "12to19",
                                 ifelse(grepl("20to50",Filename)==T,"20to50",
                                        ifelse(grepl("50to80",Filename)==T, "50to80", NA)))
})
table(amp.years.sums$agegrp)

table(amp.years.sums$nms)
names(amp.years.sums)

amp.years.sums$group = with(amp.years.sums, {
  ifelse(grepl("groups.year",nms)==T, "YR",
             ifelse(grepl("groups.race", nms)==T, "RE",
                    ifelse(grepl("groups.sex", nms)==T, "GE", NA)))
})

table(amp.years.sums$group)
names(amp.years.sums)

amp.years.sums$model = with(amp.years.sums, {
  ifelse(grepl("m1-", Filename)==T, 1,
               ifelse(grepl("m2-", Filename)==T, 2,
                      ifelse(grepl("m3-", Filename)==T, 3,
                             ifelse(grepl("m4-", Filename)==T, 4, NA))))
})

table(amp.years.sums$model)

# Get columns to print off table below
names(amp.years.sums)
head(amp.years.sums)
fit.vals = c("Filename",
             "ChiSqM_Value", "ChiSqM_DF", "ChiSqM_PValue", 
             "LL",
             "CFI", "TLI", "RMSEA_Estimate",
             "BIC", "AIC", 
             "gender.g", "agegrp", "group",
             "model"); length(fit.vals)

amp.years.sums.sub = amp.years.sums[colnames(amp.years.sums) %in% fit.vals]
length(names(amp.years.sums.sub))
names(amp.years.sums.sub)
amp.years.sums.sub = amp.years.sums.sub[fit.vals] # reorder column names 
colnames(amp.years.sums.sub)

fit.vals.renamed = c("Filename", 
                     "Chi-sq", "df", "p-value", 
                     "LL", "CFI", "TLI", "RMSEA",
                     "BIC", "AIC", 
                     "Gender", "Age", "Groups", 
                     "mod")

colnames(amp.years.sums.sub) = fit.vals.renamed

head(amp.years.sums.sub) #check

# make a model description to describe model invariance settings as described in
# van de Schoot R, Lugtig P, Hox J. A checklist for testing measurement invariance. European Journal of Developmental Psychology. 2012 Jul;9(4):486–92. 

amp.years.sums.sub = within(amp.years.sums.sub, {
  description = ifelse(grepl("m1", mod)==T, "M1: Metric invariance: diff int",
                       ifelse(grepl("m2", mod)==T, "M2: Diff factor loadings",
                              ifelse(grepl("m3", mod)==T, "M3: Scalar Invariance: eq loading and eq int",
                                     ifelse(grepl("m4", mod)==T, "M4: Full Uniqueness invariance: eq loadings, intercepts and res variance",
                                            ifelse(grepl("m5", mod)==T, "M5: Free loadings, int and res variance",
                                                   NA)))))
})

table(amp.years.sums.sub$Groups)

order = amp.years.sums.sub[order(amp.years.sums.sub$Age,
                         amp.years.sums.sub$Groups,
                         amp.years.sums.sub$description),]

  mod.1 = order[order$Gender=="all" & order$Groups %in% c("RE", "YR"), !(colnames(order) %in% c("Model"))]
  table(mod.1$Groups)
  head(mod.1)

  mod.1f = order[order$Gender=="female"& order$Groups %in% c("RE", "YR"),
                 !(colnames(order) %in% c("Model"))]
  head(mod.1f)
  table(mod.1f$Groups)
  
  mod.1m = order[order$Gender=="male"& order$Groups %in% c("RE", "YR"),
                 !(colnames(order) %in% c("Model"))]
  head(mod.1m)
  

  mod.ge = order[order$Gender=="all" & order$Groups %in% c("GE"),
                 !(colnames(order) %in% c("Model"))]
  head(mod.ge) # will have to go back and run this for gender if needed.
  
  mod.1.tot = rbind(mod.1[complete.cases(mod.1$df),], 
                    mod.ge) #includes gender groups (in addition to race/ethnic and year groups)
  
  table(mod.1.tot$Groups)
  mod.1.tot = mod.1.tot[order(mod.1.tot$Groups, mod.1.tot$Age, mod.1.tot$description),]
  mod.1.tot[1:10,]

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr fit1
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Note: All factors were adjusted for age and bmi (in the year comparisons).

  col.include = c("mod", "Chi-sq", "df", 
                  "LL", "CFI", "BIC", "RMSEA") # "TLI", "AIC",
  #,"description")
  head(mod.1.tot)
  mod.1.tot.sub = mod.1.tot[mod.1.tot$Age %in% c('12to19') &
                              !(mod.1.tot$Groups %in% c("GE")),]

  mod.2 = mod.1.tot.sub[col.include]
  mod.2$mod = as.integer(mod.1.tot.sub$mod)
  colnames(mod.2)[1] = "Model"
  
  head(mod.2)

  options(ztable.colnames.bold=TRUE)
  z = ztable(mod.2, digits = c(0,0,0,0,0,3,0,3), #zebra=4, zebra.color="pastelgray",
             include.rownames=FALSE)

  table(mod.1.tot.sub$Groups)
  mod.1.tot.sub$g2 = factor(mod.1.tot.sub$Groups,
                            label=c("Race/Ethnicity",
                                    "Time (year)"))
  
  # Add row labels
  rowlabs = with(mod.1.tot.sub, {
    unique(paste0("Groups: ", g2))
  })
  rowlabs

    rowlabs.n = with(mod.1.tot.sub, {
    table(paste0("Groups: ", Groups))
  })
    
  z = addrgroup(z, rgroup = rowlabs, 
                n.rgroup = rowlabs.n,
                cspan.rgroup=10)
  z = update_ztable(z, include.rownames=F)
  z = addRowColor(z,c(2,6),"pink")

  
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr fit-gender
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Model fit results for pooled gender
  # Note: All factors were adjusted for age and bmi (in the year comparisons).
  
  col.include = c("mod", "Chi-sq", "df", 
                  "LL", "CFI", "BIC", "RMSEA") # "TLI", "AIC",
  #,"description")
  head(mod.1.tot)
  table(mod.1.tot$Groups)
  mod.1.tot.sub = mod.1.tot[mod.1.tot$Age %in% c('12to19') &
                              (mod.1.tot$Groups %in% c("GE")),]
  
  mod.2 = mod.1.tot.sub[col.include]
  mod.2$mod = as.integer(mod.1.tot.sub$mod)
  colnames(mod.2)[1] = "Model"
  
  head(mod.2)
  
  options(ztable.colnames.bold=TRUE)
  z = ztable(mod.2, digits = c(0,0,0,0,0,3,0,3), #zebra=4, zebra.color="pastelgray",
             include.rownames=FALSE,
             tabular=T)
  
  table(mod.1.tot.sub$Groups)
  mod.1.tot.sub$g2 = factor(mod.1.tot.sub$Groups,
                            label=c("Gender"))
  
  # Add row labels
  rowlabs = with(mod.1.tot.sub, {
    unique(paste0("Groups: ", g2))
  })
  rowlabs
  
  rowlabs.n = with(mod.1.tot.sub, {
    table(paste0("Groups: ", Groups))
  })
  
  z = addrgroup(z, rgroup = rowlabs, 
                n.rgroup = rowlabs.n,
                cspan.rgroup=10)
  z = update_ztable(z, include.rownames=F)
  z = addRowColor(z,c(2),"pink")
  
  
    
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
## @knitr fit2
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Model fit results for age group 20-50 years
  
  # Note: All factors were adjusted for age and bmi (in the year comparisons).
  
  col.include = c("mod", "Chi-sq", "df", 
                  "LL", "CFI", "TLI", "BIC", "AIC", "RMSEA")
  #,"description")
  head(mod.1.tot)
  mod.1.tot.sub = mod.1.tot[mod.1.tot$Age %in% c('20to50') &
                              !(mod.1.tot$Groups %in% c("GE")),]
  mod.2 = mod.1.tot.sub[col.include]
  mod.2$mod = as.integer(mod.1.tot.sub$mod)
  colnames(mod.2)[1] = "Model"
  head(mod.2)
  
  options(ztable.colnames.bold=TRUE)
  z = ztable(mod.2, zebra=4, zebra.color="pastelgray",
             include.rownames=FALSE)
  
  table(mod.1.tot.sub$Groups)
  mod.1.tot.sub$g2 = factor(mod.1.tot.sub$Groups,
                            label=c("Race/Ethnicity",
                                    "Time (year)"))
  
  # Add row labels
  rowlabs = with(mod.1.tot.sub, {
    unique(paste0("Groups: ", g2))
  })
  rowlabs
  
  rowlabs.n = with(mod.1.tot.sub, {
    table(paste0("Groups: ", Groups))
  })
  
  z = addrgroup(z, rgroup = rowlabs, 
                n.rgroup = rowlabs.n,
                cspan.rgroup=10)
  z = update_ztable(z, include.rownames=F)
  z=addRowColor(z,c(2,9),"pink")
  
  

# Model parameters for all models

## Standardized factor loadings by age group, grouping (RE=race/ethnicity and YE=years) and model

## >>>>>>>>>>>>>==========================================
## @knitr loadings1
## >>>>>>>>>>>>>>==========================================
  
# YEARS
names(amp.years.params)

# pick off the standardized estimates, stdyx.standardized (3rd element in each list)
# and put in table for all models
names(amp.years.params)  # amp.years.params is a list of lists containing parameters from many different parts of the output for each model, indexed by model.
names(amp.years.params[[1]]$stdyx.standardized) # 7 columns
nrow(amp.years.params[[1]]$stdyx.standardized) # 130 rows
amp.years.params[[1]]$stdyx.standardized


# take out the standardized analysis from each of the model output (3rd element from each list, stdyx.standardized)
#res3 = do.call(rbind.data.frame, sapply(amp.years.params, function(x) x[3]))
#res3$names = rownames(res3)


res3 = rbindlist(sapply(amp.years.params, function(x) x[3]), 
                 use.names=TRUE, fill=TRUE, idcol="names")

head(res3)

# extract out model info (age and group)
#age.group = unlist(strsplit(rownames(res3), "[.]"))
res3$model = substr(res3$names,1,2)
res3$age.group = substr(res3$names,4,9)
res3$grouping = substr(res3$names,11,12)
res3$gender = substr(res3$names, 19, 21)

table(res3$model); table(res3$age.group); table(res3$grouping);
table(res3$gender)
head(res3[res3$Group=="FEMALE",])


# make a new value to put in table: est (se)
res3 = within(res3, {
  est.se = paste0(est, " (", se, ")")
})
head(res3$est.se)


# Look at factor loadings by model

fl = res3[res3$paramHeader %in% c("F1.BY"),]
nrow(fl) # 1065 rows
head(fl)

table(fl$gender)
t1 = data.frame(dcast(setDT(fl[fl$gender %in% c("all"),]), 
                      age.group + grouping + model + Group ~ param, 
                      value.var = "est.se"))

colnames(t1)[1:4] = c("Age", "Grouping", "Model", "Group value")
head(t1)
table(t1$"Group value")

# 
# # females
# t1f = data.frame(dcast(setDT(fl[fl$gender=="fem",]), age.group + grouping + model + Group ~ param, 
#                       value.var = "est.se"))
# colnames(t1f)[1:4] = c("Age", "Grouping", "Model", "Group value")
# head(t1f)
# 
# # males
# t1m = data.frame(dcast(setDT(fl[fl$gender=="mal",]), age.group + grouping + model + Group ~ param, 
#                       value.var = "est.se"))
# colnames(t1m)[1:4] = c("Age", "Grouping", "Model", "Group value")
# head(t1m)




## >>>>>>>==========================================
## @knitr combo1
## >>>>>>>==========================================

# NOTE: have to run loadings1 section first

# this section combines loading, intercept and sd for one model
# ages 12-19 years

## Standardized intercepts, factor loadings and residuals
## by age group, grouping (RE=race/ethnicity and YE=years) and model

head(res3)
table(res3$paramHeader)
table(res3$gender)


maketabcombo = function(group1, modeltype, df, agegroup1){
  res3=df
  #res3=res3.subyear; agegroup1="12to19"; group1="YR"; modeltype="m1" # for debugging, pull from next code chunk
    combo = res3[res3$age.group %in% c(agegroup1) &
                 res3$model %in% c(modeltype) &
                 res3$grouping %in% c(group1) &
                 res3$paramHeader %in% c("Intercepts", 
                                         "Residual.Variances",
                                         "F1.BY"),]


    t4 = data.frame(dcast(setDT(combo[combo$gender %in% c("all"),]), 
                          age.group + grouping + model + Group + paramHeader ~ param, 
                          value.var = "est.se"))
    colnames(t4)[1:5] = c("Age", "Grouping", "Model", "Group value", "Parameter")
    t4 = t4[,!(colnames(t4) %in% c("AGE", "BMI"))]

    levels(factor(t4$Parameter))
    t4$Parameter = factor(t4$Parameter, labels = c("Loading",
                                                   "Intercept",
                                                   "Residual variance"))

    dat=t4
    col.include = c("Parameter", "HDL", "LDL", "TG")
    col.names = c("Parameter", "HDL-C", "LDL-C", "TG")
    dat.2 = dat[col.include]
    colnames(dat.2) = col.names
    head(dat.2)
    
    z = ztable(dat.2, tabular=T)

    # Add row labels
    rowlabs = with(dat, {
      unique(paste0(`Group value`))
    })
    rowlabs
    
    rowlabs.n = with(dat, {
      table(paste0(`Group value`))
    })
    rowlabs.n
    
    z = addrgroup(z, rgroup=rowlabs, n.rgroup = rowlabs.n)
    z = update_ztable(z, include.rownames=F)
}


## >>>>>>>>>>>>==========================================
## @knitr print-combo-race
## >>>>>>>>>>>>==========================================

res3.subre = res3[res3$Group %in% c('MA', "NHW", "NHB", "OH"),]
res3.subre$Group = factor(res3.subre$Group, 
                          labels=c("Mexican American",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic White",
                                   "Other Hispanic"))
z = maketabcombo(group1="RE", modeltype = "m1",
                 df=res3.subre,
                 agegroup1="12to19")
z = addRowColor(z,c(3,6,9,12),"pink")


## ==========================================
## @knitr print-combo-gender
## ==========================================


res3.subge = res3[res3$Group %in% c('FEMALE',
                                    'MALE'),]
z=maketabcombo(group1="GE", modeltype="m1", 
               df=res3.subge,
               agegroup1="12to19")
z=addRowColor(z,c(3,6),"pink")
#z


## ==========================================
## @knitr print-combo-race-50
## ==========================================


res3.subre = res3[res3$Group %in% c('MA', "NHW", "NHB", "OH"),]
res3.subre$Group = factor(res3.subre$Group, 
                          labels=c("Mexican American",
                                   "Non-Hispanic Black",
                                   "Non-Hispanic White",
                                   "Other Hispanic"))
z = maketabcombo(group1="RE", modeltype = "m1",
                 df=res3.subre,
                 agegroup1="20to50")
z = addRowColor(z,c(3,6,9,12),"pink")

## ==========================================
## @knitr print-combo-year
## ==========================================

res3.subyr = res3[res3$Group %in% c("Y2003",
                                    "Y2005",
                                    "Y2007",
                                    "Y2009",
                                    "Y2011"),]
table(res3.subyr$Group)

res3.subyr$Group = factor(res3.subyr$Group, 
                          labels=c("2003-2004",
                                   "2005-2006",
                                   "2007-2008",
                                   "2009-2010",
                                   "2011-2012"))

z = maketabcombo(group1="YR", modeltype = "m1", 
                 agegroup1="12to19",
                 df=res3.subyr)

z = addRowColor(z,c(3,6,9,12,15),"pink")
