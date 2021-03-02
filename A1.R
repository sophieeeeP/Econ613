## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/sophi/Desktop/Econ613/A1")


## ----eval = FALSE---------------------------------------------------------------------------------------------------------------------------------------------
## install.packages('plyr')
## install.packages('dplyr')
## package_list = c('corrplot', 'caret', 'gridExtra', 'Rmisc', 'ggrepel', 'tidyverse', 'data.table', 'stringr', 'scrubr','AER','mfx','mice', 'matlib')
## install.packages(package_list)
## 


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
library(corrplot)
library(caret)
library(gridExtra)
library(Rmisc)
library(ggrepel)
library(tidyverse) # handy utility functions
library(scrubr)#for deduplication
library(mice) # package for categorical or numerical umputation
library(tidyr)
library(mfx)
library(matlib)


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
datjss = read.csv('datjss.csv')
datsss = read.csv('datsss.csv')
datstu = read.csv('datstu.csv')
dir()


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
# number of students
a1 = nrow(datstu)
cat('1. the number of students is:', a1, '\n')
# num of school
datsss2 = datsss[,-1] %>% unique()
datsss2 = datsss2[!duplicated(datsss2$schoolcode),]
a2 = nrow(datsss2)
cat('2. the number of school is:', a2, '\n')
# num of prog
prgm = dplyr::select(datstu, choicepgm1, choicepgm2, choicepgm3, choicepgm4, choicepgm5, choicepgm6, X)
prgm_V = gather(prgm, program, progname, choicepgm1, choicepgm2, choicepgm3, choicepgm4, choicepgm5, choicepgm6) 
prgm_V_uni = unique(prgm_V$progname) %>% length()
cat('3. the number of program is:', prgm_V_uni, '\n')
# num of choices(school.program)
schol_V = datstu %>% dplyr::select(schoolcode1, schoolcode2, schoolcode3, schoolcode4, schoolcode5, schoolcode6, X) %>%  gather(., schoolrank,schoolcode,schoolcode1, schoolcode2, schoolcode3, schoolcode4, schoolcode5, schoolcode6)
choices = cbind(schol_V[,3], prgm_V[,3])
choices_uni = unique(choices)
cat('4. the number of choices is:', nrow(choices_uni), '\n')
# Missing test score
ts_miss_count = sum(is.na(datstu$score) == 'TRUE', na.rm=TRUE)
cat('5. the number of missing test score is:', ts_miss_count, '\n')
# apply to the same schl(diff prog) (could be improved)
#tapply(samescl$schoolcode, samescl$X, count)
samescl = aggregate(schol_V$X, by = list(schol_V$schoolcode),length)
print(samescl)
# apply to less than 6 choices
ts_less_count = sum(is.na(datstu$schoolcode6) == 'TRUE', na.rm=TRUE)
cat('7. the number of student apply to less than 6 choices is:', ts_less_count, '\n')


## ---- eval = FALSE , echo=FALSE-------------------------------------------------------------------------------------------------------------------------------
## rapply(A)
## unlist(A)#charactor
## c(A) #list
## array(A)
## as.vector()


## ---- eval = FALSE, echo = FALSE------------------------------------------------------------------------------------------------------------------------------
## #a
## summary(datstu)
## dim(datstu)
## print(‘’)
## 


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
# the distric where school is located
# retype the choices and rename the col

choices_uni_df = as.data.frame(choices_uni)  # why cannot use %>% here?
choices_uni_df = plyr::rename(choices_uni_df, c('V1' = "schoolcode", 'V2' = "pregname")) %>% transform (., schoolcode = as.integer(schoolcode)) %>% unique()
ssl_sub = dplyr::select(datsss2, schoolcode, sssdistrict, ssslong, ssslat)

school_level = dplyr::left_join(choices_uni_df, ssl_sub, by = 'schoolcode') %>% unique()

# the lat of the dist.
# the long of the dist
# cutoff

admitschl = 0 * c(1:nrow(datstu))
admitprog = 0 * c(1:nrow(datstu))
datstu2 = data.frame(datstu, admitschl, admitprog)

for (i in 1:nrow(datstu2)) {
  if (is.na(datstu2[i, "rankplace"]) == FALSE){
    if (datstu2[i, "rankplace"] != 99){
      m =  as.integer(4 + datstu2[i, "rankplace"])
      n =  as.integer(10 + datstu2[i, "rankplace"])
      datstu2[i, 'admitschl'] = datstu2[i,m]
      datstu2[i, 'admitprog'] = datstu2[i,n]
      }
    }
}

cutoff = datstu2 %>% group_by(admitschl, admitprog) %>% summarize(min_score = min(score), avg_score = mean(score), num = length(X)) %>% plyr::rename(c('admitschl' = 'schoolcode', 'admitprog' = 'pregname', 'min_score' = 'cutoff', 'avg_score' = 'quality', 'num' = 'size' ))
print(cutoff)
school_level2= dplyr::left_join(school_level, cutoff, by = c('schoolcode', 'pregname')) 
school_level2 %>%  drop_na() %>% dim



## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
datstu3 = left_join(datstu2, datjss[2:4], by = 'jssdistrict') %>% plyr::rename(c('point_x' = 'jsslong', 'point_y' = 'jsslat'))
datsss3 = datsss2 %>% plyr::rename(c('schoolcode' = 'admitschl'))
datstu4 = datstu3 %>% select(c(1,2,17:22)) %>% left_join(., datsss3[2:5], by = 'admitschl' )
datstu4 = datstu4[!duplicated(datstu4$X),]#delete record by duplicated element in one col
datstu4$distance = sqrt(
  (69.172*(datstu4$ssslong-datstu4$jsslong)*cos(datstu4$jsslat/57.3))^2+(69.172*(datstu4$ssslat-datstu4$jsslat)^2)
)


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
datstu3$distance = datstu4$distance
cutoff2 = cutoff %>% plyr::rename(c('schoolcode' = 'admitschl', 'pregname' = 'admitprog'))
datstu5 = left_join(datstu3, cutoff2, by = c('admitschl', 'admitprog'))
report1 = datstu5 %>% group_by(rankplace) %>% summarize(avg_cutoff = mean(na.omit(cutoff)), sd_cutoff = sd(na.omit(cutoff)), avg_quality = mean(na.omit(quality)), sd_quality = sd(na.omit(quality)), avg_dis = mean(na.omit(distance)), sd_dis = sd(na.omit(distance)))
report1


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
datstu5 = mutate(datstu5, quantile_rank = ntile(datstu5$score,4))
report2 = datstu5 %>% group_by(quantile_rank) %>% summarize(avg_cutoff = mean(na.omit(cutoff)), sd_cutoff = sd(na.omit(cutoff)), avg_quality = mean(na.omit(quality)), sd_quality = sd(na.omit(quality)), avg_dis = mean(na.omit(distance)), sd_dis = sd(na.omit(distance)))
report2


## ----eval = FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------
## datstu %>%
## #reshape the variable
## 
## dat = select(datstu, con)
## View(dat)
## dat_long = gather(dat, 'key', 'value')
## length(datstu$X)


## ---- eval = FALSE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------
## dat_merge = left_join(datstu, datsss)
## view(dat_merge)
## dat_merge_na = drop_na(dat_merge)
## view(dat_merge_na)


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
set.seed(10000)
X1 = runif(10000, 1, 3)
#gamma 3, 2
X2 = rgamma(10000, shape = 3, scale = 2)
# b 0.3
X3 = rbinom(10000, 1, prob = 0.3)

eps = rnorm(10000, mean = 2, sd = 1)

Y = 0.5 + 1.2 * X1 - 0.9 * X2 + 0.1 * X3 + eps
ydum = ifelse(Y > mean(Y), 1, 0) # ydum = as.numeric(Y > mean(Y))


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
#calculate outcome
r = (sum((Y-mean(Y))*(X1- mean(X1))))/sqrt((sum((X1-mean(X1))^2))*(sum((Y-mean(Y))^2)))
r


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
Xa  = cbind(1, X1, X2, X3)
#calculate the coefficient
#
#cal se using standard fomulaas
olsestimate = function(x,y){
beta_hat =  inv(t(x) %*% x)  %*% t(x) %*% y
resid = y - x %*% beta_hat
sig2_hat = (t(resid) %*% resid) / (nrow(x) - ncol(x))
vcov_beta_hat = c(sig2_hat) * solve(t(x) %*% x)
se_hat = sqrt(diag(vcov_beta_hat))
coefficitent = cbind(beta_hat, se_hat)
return(coefficitent)
}
olsestimate(Xa, Y) 


## ----eval = FALSE, , echo= FALSE------------------------------------------------------------------------------------------------------------------------------
## #recycle bin
## 
##   t_stat = function(x, betahat) {
##   iter = c(1:length(betahat))
##   t_statistic = iter * 0
## 
##   for (i in iter) {
##     t_statistic[i] = betahat[i] / se_betahat[i]
##   }
##   return(t_statistic)
##   }
##     beta_t_statistic = t_stat(X, betahat)
## 
##   se2_hat =(t(y - X %*% betahat) %*% (y - X %*% betahat)) /(nrow(X)-ncol(X))
##   se_betahat = sqrt(se^2 * diag(inv(t(X) %*% X)))```{r}
## #+++++++++++++++++++++++++++++++++++++++++++++++++++
## betahat = probitmodel(Xa, ydum)  $ par
## temp = diag(inv(t(Xa) %*% Xa))
## se2_hat =(t(ydum - Xa %*% betahat) %*% (ydum - Xa %*% betahat)) /(nrow(Xa)-ncol(Xa))
## # t_stat = as.vector(betahat) / as.vector(sqrt(se2_hat * diag(inv(t(Xa) %*% Xa)))) doesn't work
## 
## t_stat = function(x, betahat) {
##   se_betahat = sqrt(se2_hat * diag(inv(t(x) %*% x)))
##   iter = c(1:length(betahat))
##   t_statistic = iter * 0
##   for (i in iter) {
##     t_statistic[i] = betahat[i] / se_betahat
##   }
##   return(t_statistic)
## }
## 
## t_1 = t_stat(Xa, betahat)
## DOG = nrow(Xa) - 1
## p_value_func = function(t, df){
##   pvalue = 2 * (1 - pt(q = abs(t), df = df))
##   return(pvalue)
## }
## p_value_func(t_1, DOG)
## 
## 
## t_stat_P
## p.value = 2 * (1- pt(q = abs(t_1), df = DOG))
## p.value
## #+++++++++++++++++++++++++++++
## eps_hat_sqr = sum((ydum - Xa %*% betahat)^2)
## beta_se = 1/(nrow(Xa)-ncol(a)) *


## ---- echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------
# Exercise 7 Discrete choice
# write and optimize probit, logit, linear prob
probitmodel = function(X, y){
  loglik = function(betas, X, y) {     #loglikelihood
    xbeta = X %*% betas
    llh = - sum(y * log(pnorm(xbeta)) + (1-y) * log(1-pnorm(xbeta))) 
    return(llh)
  }

  begin = 0 * c(1:ncol(X)) # do write it wrong!!!, why different from 0000 and 1234
  fit = optim(begin, loglik, X = X, y = y, hessian = TRUE) #optimiza
  
  betahat = fit $ par
  vcv = solve(fit$hessian)
  se = sqrt(diag(vcv)) #se of betahat
  beta_t_statistic = betahat / se
  DOG = nrow(X) - 1
  
  p_value_func = function(t, df){
  pvalue = 2 * (1 - pt(q = abs(t), df = df))
  return(pvalue)
  }
  beta_pvalue = p_value_func(beta_t_statistic, DOG)
  result = cbind(betahat, se, beta_t_statistic, beta_pvalue)
  return(result)
}
probit_table = probitmodel(Xa,  ydum)
print(probit_table)


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------------------------------
#logit 
logitmodel = function(X, y){
  loglik = function(betas, X, y) {     #loglikelihood
    xbeta = X %*% betas
    llh = - sum(y * log(1/(1+exp(-xbeta))) + (1-y) * log(1-1/(1+exp(-xbeta))))

    return(llh)
  }

  begin = 0 * rep(1,ncol(X)) # do write it wrong!!!, why different from 0000 and 1234
  fit = optim(begin, loglik, X = X, y = y, hessian = TRUE) #optimizar
  
  betahat = fit $ par
  vcv = solve(fit$hessian)
  se = sqrt(diag(vcv)) #se of betahat
  beta_t_statistic = betahat / se
  DOG = nrow(X) - 1
  
  p_value_func = function(t, df){
  pvalue = 2 * (1 - pt(q = abs(t), df = df))
  return(pvalue)
  }
  beta_pvalue = p_value_func(beta_t_statistic, DOG)

  result = cbind(betahat, se, beta_t_statistic, beta_pvalue)
  return(result)
}
  logittable = logitmodel(Xa, ydum)
  print(logittable)

## ----  echo = TRUE--------------------------------------------------------------------------------------------------------------------------------------------
# linear probabiity
olstable = function(X, y){
  fit = olsestimate(X,y)
  betahat = fit[,1]
  se2_hat =as.numeric((t(y - X %*% betahat) %*% (y - X %*% betahat)) /(nrow(X)-ncol(X)))
  se_betahat = sqrt(se2_hat * diag(inv(t(X) %*% X)))

  
  t_stat = function(X, betahat) {
  diagele  = inv(t(X) %*% X)
  # se_betahat = sqrt(se2_hat * diag(inv(t(X) %*% X)))

  iter = c(1:length(betahat))
  t_statistic = iter * 0
  
  for (i in iter) {
    t_statistic[i] = betahat[i] / se_betahat[i]
  }
  return(t_statistic)
  }
  
  beta_t_statistic = t_stat(X, betahat)
  DOG = nrow(X) - 1
  p_value_func = function(t, df){
  pvalue = 2 * (1 - pt(q = abs(t), df = df))
  return(pvalue)
  }
  beta_pvalue = p_value_func(beta_t_statistic, DOG)
  
  result = cbind(betahat, beta_t_statistic, beta_pvalue)
  return(result)
}
ols_table = olstable(Xa, ydum)
print(ols_table)


## ----echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------

#probit
probit = glm(ydum ~ X1 + X2 + X3, family = binomial(link = 'probit'))
summary(probit)

#logit
logit = glm(ydum ~ X1 + X2 + X3, family = binomial(link = 'logit')  )
summary(logit)
# lin prob
linearprob = lm(ydum ~ X1 + X2 + X3)
summary(linearprob)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------
### Exercise 8 Marginal Effects

# The marginal effect of X according to Probit and Logit
data = data.frame(ydum,Xa)
probitmfx(formula = ydum ~ X1 + X2 + X3, data = data)
logitmfx(formula = ydum ~ X1 + X2 + X3, data = data)


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------------------------------
# Standard error of marginal effect
betahat_logit = as.vector(logittable[,1])
xbeta_logit = as.vector(Xa %*% betahat_logit)
lambda_Xbeta = exp(xbeta_logit)/(1 + exp(xbeta_logit)) 
maginef_logit = lambda_Xbeta * (1-lambda_Xbeta)  %*% t(betahat_logit)
avg_marginef_logit = colMeans(maginef_logit)
avg_marginef_logit

betahat_probit =as.vector(probit_table[,1])
xbeta_probit = as.vector(Xa %*% betahat_probit)
maginef_probit = 1 /sqrt(2*pi) * exp(-(xbeta_probit)^2/2) %*%  t(betahat_probit)
avg_marginef_probit= colMeans(maginef_probit)
avg_marginef_probit

## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
R = 999
nvar = length(betahat_probit)
outs2 = mat.or.vec(R,nvar)
outs3 = mat.or.vec(R,nvar)

for (i in 1:R)
{
  samp = sample(1:10000,200,rep=TRUE)
  X_samp = X[samp,]
  
  probit = exp(-(X_samp %*% betahat_probit)^2/2) %*% t(betahat_probit) / sqrt(2 * pi)
  outs2[i,] = colMeans(probit)
  
  f_boo = 1/(1+exp(- X_samp %*% betahat_logit))
  logit = (f_boo * (1 - f_boo)) %*% t(betahat_logit)
  outs3[i,] = colMeans(logit)
}

sd_probit = apply(outs2, 2, sd)
sd_probit
sd_logit = apply(outs3, 2, sd)
sd_logit

