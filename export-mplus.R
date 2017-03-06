# export-mplus.R

# export nhanes data for mplus use (txt file)
library(foreign)
library("MplusAutomation")
library(survey)
library(reshape2)
library(data.table)
library(ggplot2)

setwd('C:/Users/vonholle/Dropbox/unc.grad.school/my-papers/ms-201608-2/programs-ms201608-2')
load("nhanes-dat.RData") # get data from read-data.R. Takes a while to run

# Look at a small test data set firs
# -------------------------------------------

head(d.2005)
names(d.2005)
#d.2005$RIDAGEYR


dat.2005 = subset(d.2005, chol.med==0 & RIDAGEYR>50, select=c(SEQN,
                                                LBXTR, LBDLDL, LBDHDD,
                                                RIDRETH1,
                                                WTMEC2YR, SDMVPSU, SDMVSTRA))
#table(dat.2005$SDMVPSU)
#table(dat.2005$SDMVSTRA)
head(dat.2005)

# format some variables

dat.2005$race = ifelse(dat.2005$RIDRETH1=="Non-Hispanic White", 1, 0)
colnames(dat.2005) = tolower(colnames(dat.2005))
#dat.2005$lbxtr = log(dat.2005$lbxtr)
head(dat.2005)
nrow(dat.2005)

head(dat.2005)
lipvars = c("lbxtr", "lbdldl", "lbdhdd")
d2005m = dat.2005[,colnames(dat.2005) %in% lipvars]
head(d2005m)
cor(d2005m,  use="pairwise.complete.obs")

dat.2005 = within(dat.2005, { # revise the lipid vars
  lbxtr = scale(log(lbxtr)) # center and standardize log of tg
  lbdldl = scale(lbdldl)
  lbdhdd = scale(lbdhdd) # scale the inverse of hdl so it's in same direction as others in terms of adverse effect (positive assn with adverse risk)
})


summary(dat.2005)

colnames(dat.2005)[colnames(dat.2005) %in% lipvars] = c("tg", "ldl", "hdl")

head(dat.2005)

prepareMplusData(dat.2005, "c:/temp/d2005.dat") # export data out to an mplus file


# do a simple check with Mplus results for a simple linear regression with
# ldl regressed on hdl

d.svy <- svydesign(strata=~SDMVSTRA, id=~SDMVPSU, weights=~WTMEC2YR, data=dat.2005, nest=T)
regmodel = svyglm(LBDLDL ~ LBDHDD, design=d.svy)
summary(regmodel)
# this matches results from Mplus

# ------------------------------------------------------
# Now export group of lipid measures for each time point
# -------------------------------------------------------

# list of data frames to loop over in mapply below
# -------------------------------------------
list.df = list(d.1999=d.1999, d.2001=d.2001, d.2003=d.2003,
               d.2005=d.2005, d.2007=d.2007, d.2009=d.2009, d.2011=d.2011)
n=names(list.df)

# FUNCTION to prepare data 
# -------------------------------------------

prep.dat = function(dat1, nms){
  
  # Do not subset here. subset when in mplus to ensure correct analysis
  # -------------------------------------------------------------------
  #dat1=d.2005; nms="d.2005" # check

  dat.1 = subset(dat1,
                 select=c(SEQN, LBXTR, LBDLDL, LBDHDD,
                          RIDRETH1,
                          RIDAGEYR,
                          RIAGENDR,
                          WTMEC2YR, SDMVPSU, SDMVSTRA,
                          BMXBMI,
                          chol.med))
  summary(dat.1)
  
  colnames(dat.1) = tolower(colnames(dat.1))
  
  # scale variables by age subgroups: 12-19, 20-49, 50-79, 80+
  dat.1$age.cut = cut(dat.1$ridageyr, c(0,11,19,49,79,120))
  
  dat.1.dt = data.table(dat.1)
  
  # scale variables by age subgroups, age.cut
  dat.1.dt[, tg2:=scale(log(lbxtr)), by=c("age.cut")]
  dat.1.dt[, ldl2:=scale(lbdldl), by=c("age.cut")]
  dat.1.dt[, hdl2:=scale(lbdhdd), by=c("age.cut")]
  
  # center variables by age subgroups, age.cut (no scaling)
  dat.1.dt[, tg3:=scale(log(lbxtr), scale=F), by=c("age.cut")]
  dat.1.dt[, ldl3:=scale(lbdldl, scale=F), by=c("age.cut")]
  dat.1.dt[, hdl3:=scale(lbdhdd, scale=F), by=c("age.cut")]
  
  dat.1 = data.frame(dat.1.dt) # convert back to data frame, makes data handling easier
  
  # Take out outliers outside of 4sd
  dat.1 = within(dat.1, {
    tg2 = ifelse(abs(tg2)>4, NA, tg2)
    ldl2 = ifelse(abs(ldl2)>4, NA, ldl2)
    hdl2 = ifelse(abs(hdl2)>4, NA, hdl2)

    tg3 = ifelse(abs(tg2)>4, NA, tg3)
    ldl3 = ifelse(abs(ldl2)>4, NA, ldl3)
    hdl3 = ifelse(abs(hdl2)>4, NA, hdl3)
  })

  #    dat.1.dt[, list(mean=mean(tg2, na.rm=T), sd=sd(tg2, na.rm=T)), by=c("age.cut")] #check
  #    dat.1.dt[, mean(ldl2, na.rm=T), by=c("age.cut")] #check
  #    dat.1.dt[, mean(hdl2, na.rm=T), by=c("age.cut")] #check

  # format and scale some variables for entire sample (tg,ldl, hdl)
  # Note: in the CFA these don't offer very good fit. Need to scale by age group
  # and subset the CFA analyses by age group.
  
  dat.1 = within(dat.1, {
    race = ifelse(ridreth1=="Mexican American", 1,
                  ifelse(ridreth1=="Other Hispanic", 2, 
                         ifelse(ridreth1=="Non-Hispanic White", 3,
                                ifelse(ridreth1=="Non-Hispanic Black", 4,
                                       ifelse(ridreth1=="Other Race - Including Multi-Rac", 5, NA)))))

    lbxtr = log(lbxtr) # ln transform tg
    
    tg = scale(lbxtr, scale=F) # center and standardize log of tg. have to be careful re adjustment within age group.
    ldl = scale(lbdldl, scale=F)
    hdl = scale(lbdhdd, scale=FALSE) 

#    lbxtr = ifelse(abs(lbxtr)>2, NA, lbxtr) # take out outliers
#    lbdldl = ifelse(abs(lbdldl)>2, NA, lbdldl)
#    lbdhdd = ifelse(abs(lbdhdd)>2, NA, lbdhdd)

    year = substr(paste(nms),3,6)
  })

  #lipvars = c("lbxtr", "lbdldl", "lbdhdd")
  
  colnames(dat.1)[colnames(dat.1) %in% c("ridageyr")] = c("age")
  colnames(dat.1)[colnames(dat.1) %in% c("riagendr")] = c("gender")
  colnames(dat.1)[colnames(dat.1) %in% c("bmxbmi")] = c("bmi")
  
  return(dat.1)
}

# make a list of objects with handled data
all.dat = mapply(prep.dat, dat1=list.df, nms=n, SIMPLIFY = F)

# take list of objects and combine into one data frame
all.dat.df = do.call(rbind.data.frame, all.dat) 
names(all.dat.df)
summary(all.dat.df$ldl2)
summary(all.dat.df$tg2)

# add unique ids according to numbering within year

all.dat.df$rn = substr(rownames(all.dat.df),3,6)
all.dat.df$id = sapply(strsplit(all.dat.df$rn, "[.]"), "[", 3) # take out string after 2nd period
all.dat.df$white = ifelse(all.dat.df$race==3, 1, 0)
head(all.dat.df)
all.dat.df$mec10yr = with(all.dat.df, {ifelse(year>2001, wtmec2yr/5, NA)})
all.dat.df$mec10yr[10000:10005] #check
summary(all.dat.df$tg)
names(all.dat.df)
all.dat.df$male = ifelse(all.dat.df$gender=="Male", 1, 0)
table(all.dat.df$male) # check

# reorder names of all.dat.df so I don't need to change names statement in mplus
# for original .inp files, cfa_m1.inp ... cfa_m4.inp
order1 = c('seqn', 'tg', 'ldl', 'hdl', 'ridreth1', 'age', 
           'gender', 'wtmec2yr',
           'sdmvpsu', 'sdmvstra', 'chol.med', 'year',
           'race', 'rn', 'id', 'white',
           'tg2', 'ldl2', 'hdl2',
           'tg3', 'ldl3', 'hdl3', 
           'lbxtr', 'lbdldl', 'lbdhdd',
           'age.cut', 'mec10yr', 'bmi', 'male')
length(order1)
length(names(all.dat.df))

all.dat.df = all.dat.df[order1]
colnames(all.dat.df) # check and make sure order is correct. Order should have newest variables last.

# check that mean is 0 and sd=1 by year and age.cut
test.1 = data.table(all.dat.df)
test.1[, list(mean=mean(tg2, na.rm=T), sd=sd(tg2, na.rm=T)), by=c("year", "age.cut")] #check

# Now take data frame and convert from long to wide by year. each lipid value will have a unique 
# column by year. ldl.1999, ldl.2001, ldl.2003, etc...
# NOTE: will not be using this. doesn't make sense to have repeated measures on unrelated groups

# #all.wide = dcast(setDT(all.dat.df), id ~ year, value.var=c('hdl', 'ldl', 'tg'))
# all.wide = dcast(setDT(all.dat.df),
#                  seqn + race + age + chol.med + gender +
#                    year + wtmec2yr + sdmvpsu + sdmvstra ~ year, value.var=c('hdl', 'ldl', 'tg')) # longer data frame, each time period missing lipids for other seqn numbers
# head(all.wide)
# summary(all.wide)
# nrow(all.wide)
# 
# head(all.wide)


# send out to Mplus ----------------------------------------
# ----------------------------------------------------------

# repeated measures data format (note: doesn't work in mplus)
#prepareMplusData(all.wide, "c:/temp/totdat.dat") # export data out to an mplus file

head(all.dat.df)
tail(all.dat.df)


# ----------------- Output data here for later use ===========================
# -----------------------------------------------------------------------------

save(all.dat.df,
     file="alldat.Rda")


# long style data. treat year as a multiple group in mplus with no correlation between years
prepareMplusData(all.dat.df, "c:/temp/models/totdatlong.dat") # export data out to an mplus file

# long style data. treat year as a multiple group in mplus with no correlation between years
prepareMplusData(all.dat.df[all.dat.df$gender=="Female",], 
                 "c:/temp/models/totdatlongfem.dat") # export data out to an mplus file

# long style data. treat year as a multiple group in mplus with no correlation between years
prepareMplusData(all.dat.df[all.dat.df$gender=="Male",], 
                 "c:/temp/models/totdatlongmale.dat") # export data out to an mplus file

# check vals
summary(all.dat.df$age)
summary(all.dat.df$chol.med)
head(all.dat.df$year)
table(all.dat.df$year)
table(all.dat.df$race)

sapply(all.dat.df, class)

# Look at histogram of ln(tg), hdl and ldl by year for ages 12 to 19
# they look ok

ggplot(data=all.dat.df, aes(x=tg3)) + geom_histogram() +
  facet_grid(~year)

ggplot(data=all.dat.df, aes(x=hdl3)) + geom_histogram() +
  facet_grid(~year)

ggplot(data=all.dat.df, aes(x=ldl3)) + geom_histogram() +
  facet_grid(~year)

# look at correlation between lipid measures
df = data.frame(all.dat.df)
lips = df[c("ldl", "hdl", "tg")]
head(lips)
c1 = cov2cor(cov(as.matrix(lips), use='complete.obs'))
c1

summary(lips)
summary(data.frame(all.dat.df$year))
summary(d.2005$LBXTR)

