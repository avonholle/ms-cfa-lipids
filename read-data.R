# read-data.R ----------------------------------------------
# -----------------------------------------------------------

# example from the survey package documentation
# ---------------------------------------------

library(survey)
library(Hmisc)
library(nhanesA) # see https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
library(rgl)
library(devtools)
library(gridExtra)

#install_github("vqv/ggbiplot")
library(ggbiplot)

library(data.table)

# ----------------------------------------------------------------------------
# following adapted from vignette for nhanesA: https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html
# ----------------------------------------------------------------

# Use nhanesA package to read in NHANES data from web page
# --------------------------------------------------------

# Download demographic data for merge

demo = nhanes("DEMO") # http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm
demo_b = nhanes("DEMO_B") # http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.htm
demo_c = nhanes("DEMO_C") # http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.htm
demo_d = nhanes("DEMO_D") # http://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.htm
demo_e = nhanes("DEMO_E") # http://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.htm
demo_f = nhanes("DEMO_F")
demo_g = nhanes("DEMO_G")
demo_h = nhanes("DEMO_H")

# Get drug info -------------------------------
# -----------------------------------------------
nhanesTables('Q', 1999)

drug.info = nhanesSearch("Drug", ignore.case = T, ystart=1999, ystop=2014, data_group=c("QUESTIONNAIRE"))
drug.info # this is not very informative

rxq_rx = nhanes('RXQ_RX') # 1999-2000
rxq_rx_b = nhanes('RXQ_RX_B') # 2001-2002
rxq_rx_c = nhanes('RXQ_RX_C') # 2003-2004
rxq_rx_d = nhanes('RXQ_RX_D') # 2005-2006
rxq_rx_e = nhanes('RXQ_RX_E') # 2007-2008
rxq_rx_f = nhanes('RXQ_RX_F') # 2009-2010
rxq_rx_g = nhanes('RXQ_RX_G') # 2011-2012
#rxq_rx_h = nhanes('RXQ_RX_H') # 2013-2014. note it doesn't exist yet

rxq_drug = nhanes('RXQ_DRUG') # to identify cholesterol lowering drugs take the RXDDRGID
# variable and link to the persons file. should have a secondary level of drug category codes 
# with value = 19. so RXDDCI1B=19 or RXDDCI2B=19 or RXDDCI3B=19 or RXDDCI4B=19

# --------------------------------------

# Function to create a binary indicator if any indivudal is using chol lowering meds
# --------------------------------------
# --------------------------------------

# first, take the drug data file applying to all time periods and make indicator for 
# druges that are in cholesterol lowering class

table(table(rxq_drug$RXDDRGID)) # confirmed every row a unique RXDDRGID

names(rxq_drug)
head(rxq_drug[1:10,1:5])
rxq_drug[grep("OMEGA", rxq_drug$RXDDRUG),]

chol.lower = with(rxq_drug, ifelse(as.integer(RXDDCI1B)==19|
                                     as.integer(RXDDCI2B)==19|
                                     as.integer(RXDDCI3B)==19|
                                     as.integer(RXDDCI4B)==19|
                                     grepl("OMEGA", RXDDRUG)==T, 1, 0))
chol.lower = ifelse(is.na(chol.lower),0,chol.lower)
# check values -- look correct
rxq_drug[grep(1, chol.lower), colnames(rxq_drug) %in% c("RXDDRUG", "RXDDRGID", "RXDDCI1B","RXDDCI2B","RXDDCI3B","RXDDCI4B")]

# make a data frame I can merge with individual level data for each time period
chol.lower.dat = data.frame(chol.lower, RXDDRGID=rxq_drug$RXDDRGID)

make.chol = function(demo, orig.dat) {
  
  # match each drug in rxq_rx_d # 2005-2006 to chol lower status by RXDDRGID
  new = data.frame(SEQN=orig.dat$SEQN, RXDDRGID=orig.dat$RXDDRGID)

  new_d = merge(x=chol.lower.dat, y=new, by=c("RXDDRGID")) 
  new_d = merge(x=new_d, y=data.frame(SEQN=demo[,c('SEQN')]), by=c('SEQN'), all.y=T)

  # now that I have an indicator for chol lowering drug attached to each person-drug row need
  # to select out a unique row for each person indicating any chol lower drug
  # solution: sum chol.lower for each person and make new indicator if sum is >=1
  
  new.d = data.table(new_d)
  new.d.id = new.d[, list(chol.med =  ifelse(sum(chol.lower, na.rm=T)>0, 1, 0)), by=SEQN] # collapse prescription info on each person  
  return(new.d.id) # this data frame has the unique id, SEQN, and an indicator, chol.tot, for any chol lowering med use
}

drug.1999 = make.chol(demo, rxq_rx) # this data frame has two variables, one for id, SEQN and other an indicator for chol lowering med, chol.med
drug.2001 = make.chol(demo_b, rxq_rx_b)
drug.2003 = make.chol(demo_c, rxq_rx_c)
drug.2005 = make.chol(demo_d, rxq_rx_d)
drug.2007 = make.chol(demo_e, rxq_rx_e)
drug.2009 = make.chol(demo_f, rxq_rx_f)
drug.2011 = make.chol(demo_g, rxq_rx_g)
# no drug info available yet for 2013-2014
# make dummy variable for chol.med=0
drug.2013 = data.frame(SEQN=demo_h$SEQN, chol.med=0); head(drug.2013)

nrow(drug.2005)
nrow(demo_d) # checking that I get the same number of people in chol lowering data frame as  original data

# CHECK of cholesterol lowering indicator variable ---------------------
# --------------------------------------------------------------------------

# Now see if the percent using chol lowering med matches that estimated in cdc publication
# from 'Prescription Cholesterol-lowering Medication Use in Adults Aged 40 and Over: United States, 2003–2012'

# match figure 3 in this publication for 2011-2012 data and age>40 years
# age: RIAGEYR
# gender: RIDGENDR
# race/ethnicity: RIDRETH1
# pregnancy status: RIDEXPRG

match.1 = merge(drug.2011, demo_g[,colnames(demo_g) %in% c("SEQN",
                                                           "SDMVSTRA",
                                                           "SDMVPSU",
                                                           "WTMEC2YR",
                                                          "RIDAGEYR",
                                                          "RIAGENDR",
                                                          "RIDRETH1",
                                                          "RIDEXPRG")], by = "SEQN")
head(match.1)

d.40 = match.1[match.1$RIDAGEYR>39,]

d.40 = nhanesTranslate('DEMO_G', 'RIAGENDR', data=d.40)
d.40 = nhanesTranslate('DEMO_G', 'RIDRETH1', data=d.40)

names(d.40)
d.40$age.grp = cut(d.40$RIDAGEYR, c(40,59,74,max(d.40$RIDAGEYR)))
levels(d.40$age.grp)
d.40$age.grp = factor(d.40$age.grp, labels=c("40-59", "60-74", "75+"))

# Distribution of chol lowering drugs by gender, raw numbers
table.1 = with(d.40, table(RIAGENDR, chol.med))
prop.table(table.1, 1) # get row percentages

d.svy <- svydesign(strata=~SDMVSTRA, id=~SDMVPSU, weights=~WTMEC2YR, 
                   data=d.40, nest=T)

svyby(~chol.med, ~RIAGENDR, d.svy, svymean, na.rm=T) # check

# Distribution by age group
svyby(~chol.med, ~age.grp, d.svy, svymean, na.rm=T) # check

# Distribution by race and hispanic origin
svyby(~chol.med, ~RIDRETH1, d.svy, svymean, na.rm=T) # check
# Note: close but not exact. attributing differences to their age adjustment to 2000 predicted population.



# Get TG info --------------------------------------

tg.info = nhanesSearch("Triglyceride", ignore.case=TRUE, 
                       ystart=1999, ystop=2014, data_group=c('LAB'), nchar=50)
dim(tg.info)
tg.info

# Pick out the TG variables in mg/dL from the data files starting with cholesterol
tg.info.sub = tg.info[substr(tg.info$Data.File.Description,1,4)=="Chol" &
                        grepl("mg/dL)", tg.info$Variable.Description)==T,]

tg.info.sub

# Use TG info to extract out the TG data sets for each year.------------------
vars = tg.info.sub$Variable.Name
years = tg.info.sub$Begin.Year
names = tg.info.sub$Data.File.Name;names

for (i in 1:length(vars)){
  assign(paste(names[i], sep=""), nhanes(names[i]))
}

head(LAB13AM$LBXTR) # check
head(TRIGLY_H$LBDLDL)


# GET LDL info ------------------------------
ldl.info = nhanesSearch("LDL", ignore.case=TRUE, ystart=1999, ystop=2014, data_group=c('LAB'), nchar=50)

# Pick out the TG variables in mg/dL from the data files starting with cholesterol
ldl.info.sub = ldl.info[substr(ldl.info$Data.File.Description,1,4)=="Chol" &
                          grepl("mg/dL)", ldl.info$Variable.Description)==T,]
ldl.info.sub

# Use TG info to extract out the TG data sets for each year.

vars = ldl.info.sub$Variable.Name; vars
years = ldl.info.sub$Begin.Year; years
names = ldl.info.sub$Data.File.Name; names

# note: no need to extract out data frames here because they are the same as for triglycerides

# GET HDL info ------------------------------
hdl.info = nhanesSearch("hdl", ignore.case=TRUE, ystart=1999, ystop=2014, data_group=c('LAB'), nchar=50)
hdl.info

# Pick out the TG variables in mg/dL from the data files starting with cholesterol
hdl.info.sub = hdl.info[substr(hdl.info$Data.File.Description,1,4)=="Chol" &
                          grepl("mg/dL)", hdl.info$Variable.Description)==T,]
hdl.info.sub

# Use HDL info to extract out the hdl data sets for each year.------------------

vars = hdl.info.sub$Variable.Name; vars
years = hdl.info.sub$Begin.Year; years
names = hdl.info.sub$Data.File.Name; names

for (i in 1:length(vars)){
  assign(paste(names[i], sep=""), nhanes(names[i]))
}

head(HDL_D$LBDHDD) # check
head(HDL_E$LBDHDD) # check
head(HDL_F$LBDHDD) # check


# missing 1999 to 2004 info for HDL -- get these data frames manually
url.1999 = "http://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB13.XPT" # doc at 
HDL = sasxport.get(url.1999, lowernames=FALSE)
names(HDL_D); names(HDL)
head(HDL$LBDHDL)
HDL$LBDHDD = HDL$LBDHDL # NOTE: make a variable name to make consistent across data sets (and works in my functions
# although I realized that this HDL-C measure is different from post-2003 data 

url.2001 = "http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13_B.XPT" # doc at http://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13_B.htm
HDL_B = sasxport.get(url.2001, lowernames=FALSE)
names(HDL_B)
head(HDL_B$LBDHDL)
HDL_B$LBDHDD = HDL_B$LBDHDL  # NOTE: make a variable name to make consistent across data sets (and works in my functions
# although I realized that this HDL-C measure is different from post-2003 data 

url.2003 = "http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.XPT" # doc at http://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.htm
HDL_C = sasxport.get(url.2003, lowernames=FALSE)
head(HDL_C$LBXHDD)
names(HDL_C)
HDL_C$LBDHDD = HDL_C$LBXHDD  # NOTE: make a variable name to make consistent across data sets (and works in my functions
# although I realized that this HDL-C measure is different from post-2003 data 
head(HDL_C$LBXHDD)
head(HDL_C$LBDHDD)

# -------- GET body measures data ----------------------------


bmix = nhanes('BMX') # 1999-2000
bmx_b = nhanes('BMX_B') # 2001-2002
bmx_c = nhanes('BMX_C') # 2003-2004
bmx_d = nhanes('BMX_D') # 2005-2006
bmx_e = nhanes('BMX_E') # 2007-2008
bmx_f = nhanes('BMX_F') # 2009-2010
bmx_g = nhanes('BMX_G') # 2011-2012
bmx_h = nhanes('BMX_H') # 2013-2014


# put data together for each time period
# add ldl, hdl and tg values along with relevant survey variables

# Make function to combine demo, lab and drug data together--
# -----------------------------------------------------------

combine.dat = function(demo.name, demo.dat, hdl.dat, t.dat, drug.dat, body.dat){

  # get names for race/ethnic groups
  demo.dat <- nhanesTranslate(demo.name, 'RIDRETH1', data=demo.dat)
  demo.dat <- nhanesTranslate(demo.name, 'RIAGENDR', data=demo.dat)
  demo.dat <- nhanesTranslate(demo.name, 'RIDEXPRG', data=demo.dat)

  d = merge(hdl.dat, t.dat, by="SEQN") # see http://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list
  d = merge(d, demo.dat, by="SEQN")
  d = merge(d, drug.dat, by='SEQN')
  d = merge(d, body.dat, by='SEQN')
  
  return(d)
}

d.1999 = combine.dat('DEMO', demo, HDL, LAB13AM, drug.1999, bmix) # 1999 data
d.2001 = combine.dat('DEMO_B', demo_b, HDL_B, L13AM_B, drug.2001, bmx_b) # 2001 data
d.2003 = combine.dat('DEMO_C', demo_c, HDL_C, L13AM_C, drug.2003, bmx_c) # 2003 data
d.2005 = combine.dat('DEMO_D', demo_d, HDL_D, TRIGLY_D, drug.2005, bmx_d) # 2005 data
d.2007 = combine.dat('DEMO_E', demo_e, HDL_E, TRIGLY_E, drug.2007, bmx_e) # 2007 data
d.2009 = combine.dat('DEMO_F', demo_f, HDL_F, TRIGLY_F, drug.2009, bmx_f) # 2009 data
d.2011 = combine.dat('DEMO_G', demo_g, HDL_G, TRIGLY_G, drug.2011, bmx_g) # 2011 data
d.2013 = combine.dat('DEMO_H', demo_h, HDL_H, TRIGLY_H, drug.2013, bmx_h) # 2013 data. NOTE: no drug data available by 2016/08

# Check lipid values by race and gender (just raw numbers, not weighted)

check.dat1 = function(d){
  options(digits=4)
  df.name <- deparse(substitute(d))
  print(paste(df.name))
  print(aggregate(cbind(LBXTR, LBDLDL, LBDHDD)~RIAGENDR, d, mean)) # mean lipid levels by gender
  print(aggregate(cbind(LBXTR, LBDLDL, LBDHDD)~RIDRETH1, d, mean)) # mean lipid levels by race/ethnicity
}

check.dat1(d.2005); check.dat1(d.2007); check.dat1(d.2009); check.dat1(d.2011); check.dat1(d.2013)


# ----------------- Output data here for later use ===========================
# -----------------------------------------------------------------------------

save(d.1999, d.2001, d.2003, d.2005, d.2007, d.2009, d.2011, d.2013, tg.info, hdl.info, ldl.info, file="nhanes-dat.RData")