## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
chooseCRANmirror(graphics=FALSE, ind=1)
knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_chunk$set(error = TRUE)
library(pastecs)
library(tidyverse) 
library(data.table)
library(stringr)
library(mfx)
library(MASS)
library(bayesm)
library(rmarkdown)
library(ggplot2)
library(dplyr)
library(lubridate)
setwd('C:/Users/sophi/Desktop/Econ613/A3')
options(knitr.purl.inline = TRUE)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ppl_ori = read.csv('population.csv')
crm_ori = read.csv('crime_long.csv')
off_ori = read.csv('officers.csv')

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(ppl_ori)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(ppl_ori)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(crm_ori)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex2 data manipulate
ppl = ppl_ori
crm = crm_ori
tot_crm_mon = aggregate(crimes~crime_month,crm,sum)
tot_crm_mon


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tot_crm_mon$crime_month = ymd(tot_crm_mon$crime_month)
plot1 = ggplot(tot_crm_mon, aes(x = crime_month, y = crimes)) + geom_line()+ scale_x_date(date_breaks = '24 months', date_labels = '%m %Y')
plot1

## ----include = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
# use the form of above aggregate(), would not have to rename the column anymore


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# merge
ppl_crm = left_join(ppl, crm, by = c('district'='district', 'month'='crime_month'))
ppl_crm


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# panel data of unit

# sum up the crime_type
a = ppl_crm %>%group_by( , month, period, district, tot_pop, tot_white, tot_black, tot_hisp, p50_inc, crime_type) %>% summarise(,crimes=sum(crimes))
b = spread(a, crime_type,crimes) %>% mutate(, tot_crimes = drug + other + property + violent, tot_crm_p_res = tot_crimes/tot_pop, vio_crm_p_res = violent/tot_pop, pro_crm_p_res = property/tot_pop, ro_black = tot_black/tot_pop, ro_hisp = tot_hisp/tot_pop, ro_white = tot_white/tot_pop)
panel5 = b[,c(1:3,15:17,8, 18:20)]#district,tot_crm_p_res,vio_crm_p_res, pro_crm_p_res, p50_inc,ro_black,ro_hisp, ro_white)
panel5

## ----include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
# total crimes per unit over time 
panel = ppl_crm[,1:8]
head(panel)
tot_crimes = aggregate(crimes~month+period+district+tot_pop,ppl_crm,sum) %>% rename(, 'tot_crimes' = 'crimes') %>% mutate(,tot_crm_p_res = tot_crimes/tot_pop)
tot_crimes


#share of diff residents
mutate(panel, ro_black = tot_black/tot_pop, ro_hisp = tot_hisp/tot_pop, ro_white = tot_white/tot_pop)

b = tot_crimes[,1:4]
c= panel[,1:4]
setdiff(c,b)
rm(b,c)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(off_ori)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
b


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex3
set.seed(10)
nind = 
nper = 3

eps =  matrix(rnorm(nind*nper,0,1),nind,nper)
z1 = b[,c()]

reg1 = lm(~)
summary(reg1)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex4



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ex5

