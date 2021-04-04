## ----setup, include = FALSE--------------------------------------------------------------------------------------
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
data(margarine)
setwd('C:/Users/sophi/Desktop/Econ613/A2')


## ---- include=FALSE----------------------------------------------------------------------------------------------
##margarine$choicePrice.head(5)
#head(margarine,n =5)
#Average of Prices
#summary(margarine$choicePrice[,3])


## ----------------------------------------------------------------------------------------------------------------
mat1 = apply(as.matrix(margarine$choicePrice[,3:12]), 2, mean)
print(mat1)
#stat.desc(margarine$choicePrice[,3:12])
d1 = margarine$choicePrice[,1:12] %>% mutate(d1, Stk = rowSums(d1[,3:8]), Tub= rowSums(d1[,9:12]), PPk = d1[,3]+d1[,10], PBB = d1[,4], PFl = d1[,5]+d1[,11], PHse = d1[,6]+d1[,12], PGen =d1[,7], PImp = d1[,8])
head(d1,5)
# Would that be a better way to calculate the sum of row? could we use sperate or apply?
#could use ends_with/begjins_with
# for output, have to split %>% or using rmarkdown::render()


## ----------------------------------------------------------------------------------------------------------------
stat.desc(d1[,13:14])


## ----------------------------------------------------------------------------------------------------------------
stat.desc(d1[,15:20])


## ----------------------------------------------------------------------------------------------------------------
stat.desc(d1[,3:12])


## ----------------------------------------------------------------------------------------------------------------
marketshare = data.frame()
marketshare = aggregate(d2$hhid, by = list(d2$choice), length) %>% rename(, c('count'='x'))

marketshare['freq'] = marketshare$count / sum(marketshare$count)
marketshare
#s = sum(marketshare$count)
#marketshare['Choicefreq'] = marketshare$count / s
#marketshare = apply(margarine$choicePrice[,2],)


## ----------------------------------------------------------------------------------------------------------------
brandname = c( 'PPk', 'PBB', 'PFI', 'PHse','PGen', 'Plmp')
type  = c('Stk', 'Tub')
#d2 = d1[c(-1, -3:-12)] %>% group_by(choice)
d2 =  d1[c( -3:-12)] 
msbyproduct_sep = aggregate(d2[,3:dim(d2)[2]], by = list(d1$choice),sum)
msbyproduct = aggregate(d1[,2:12], by = list(d1$choice), sum )
msbyproduct
#d2 = select(d1, d1[,1:2])
#d2 = d1[c(-0), c(-3:-12)] %>% group_by('choice')



## ----------------------------------------------------------------------------------------------------------------
map = d1[,1:12] %>% group_by(hhid,choice) %>% summarize(count = n())
map


## ----------------------------------------------------------------------------------------------------------------
mapping = d1[, 1:12] %>% filter(hhid == 2100016) %>% print()
mapping2 = d1[, 1:12] %>% filter(hhid == 2100024) %>% print()
mapping3 = d1[, 1:12] %>% filter(hhid == 2100495) %>% print()
mapping4 = d1[, 1:12] %>% filter(hhid == 2100685) %>% print()
mapping5 = d1[, 1:12] %>% filter(hhid == 2100693) %>% print()




## ----------------------------------------------------------------------------------------------------------------
uReshape <- function(data, id.vars, var.stubs, sep) {   
  # vectorized version of grep
  vGrep <- Vectorize(grep, "pattern", SIMPLIFY = FALSE)

  # Isolate the columns starting with the var.stubs
  temp <- names(data)[names(data) %in% unlist(vGrep(var.stubs, names(data), value = TRUE))]
  
  # Split the vector and reasemble into a data.frame
  x <- do.call(rbind.data.frame, strsplit(temp, split = sep))
  names(x) <- c("VAR", paste(".time", 1:(length(x)-1), sep = "_"))

  # Prep to decide whether normal reshape or unbalanced reshape
  xS <- split(x$.time_1, x$VAR)
  xL <- unique(unlist(xS))
  
  if (isTRUE(all(sapply(xS, function(x) all(xL %in% x))))) {
    # Everything looks ok for normal `reshape` to work
    reshape(data, direction = "long", idvar = id.vars, 
            varying = lapply(vGrep(var.stubs, names(data), value = TRUE), sort), 
            sep = sep, v.names = var.stubs)
  } else {
    # Padding required to "balance" the data

    # Find out which variables need to be padded
    newVars <- unlist(lapply(names(xS), function(y) {
      temp <- xL[!xL %in% xS[[y]]]
      if (length(temp) == 0) {
        temp <- NULL
      } else {
        paste(y, temp, sep = sep)
      }
    }))

    # Create matrix of NAs
    myMat <- setNames(data.frame(matrix(NA, nrow = nrow(data), ncol = length(newVars))), newVars)

    # Bind with original data.frame
    out <- cbind(data, myMat)

    # Use `reshape` as normal
    reshape(out, direction = "long", idvar = id.vars,
            varying = lapply(vGrep(var.stubs, names(out), 
                                   value = TRUE), sort), 
            sep = sep, v.names = var.stubs)
  }
}
#`reshape()` for "unbalanced" datasets. from https://gist.github.com/mrdwab/6123681


## ----------------------------------------------------------------------------------------------------------------
library(mlogit)
d3 = d1[, 1:12]
d3['Var']= c(1:dim(d3)[1])
d3Tr = uReshape(d3, id.vars = c("hhid","Var"),var.stubs = c ('PPk',), sep = "_")

d3logitdat1 = aggregate(d3, )
d3logitdat1 = mlogit.data(d3, id = c('var','hhid'), varying = 3:12, shape = "wide", sep = "_", alt.levels = c('Stk','Tub'))

d3logitdata = mlogit.data(d3, choice = "choice", id ='hhid', varying = 3:12, shape = "wide", sep = "_",  v.names = c( "PPk_Stk","PBB_Stk","PFl_Stk","PHse_Stk","PGen_Stk","PImp_Stk","PSS_Tub","PPk_Tub","PFl_Tub" ,"PHse_Tub")) # how to choose varying? what is the shaoe(long or wide), what is sep?


model1_logit <- mlogit(choice ~ PPk_Stk + PBB_Stk + PFl_Stk + PHse_Stk + PGen_Stk + PImp_Stk + PSS_Tub + PPk_Tub + PFl_Tub + PHse_Tub, data = d3logitdata)
#Error in dfidx::dfidx(data = data, dfa$idx, drop.index = dfa$drop.index, : the two indexes don't define unique observations


## ----------------------------------------------------------------------------------------------------------------
d32 = cbind(d1[,1:2], d1[,13:20]) 
#d32 <- lapply(d32, function(x) if (is.integer(x)) as.integer(x) else x)
head(d32,5)
d32logitdata <- mlogit.data(d3,id.var = 'hhid', shape = 'long'varying = 3:10 )  
d32logit <- mlogit(choice ~ PPk + PBB + PFl + PHse + PGen + PImp+ PSS + Stk + Tub, data = d32)


## ----------------------------------------------------------------------------------------------------------------
names = c("hhid"  , "choice",  "PPk_Stk", "PBB_Stk","PFl_Stk","PHse_Stk", "PGen_Stk", "PImp_Stk", "PSS_Tub",  "PPk_Tub",  "PFl_Tub",  "PHse_Tub")

for (i in names){
  print(dim(d3[names[i]])
}

## ----------------------------------------------------------------------------------------------------------------
for (i in 1:ncol(d3)){
  print(dim(d3$[i])
}


## ----------------------------------------------------------------------------------------------------------------
set.seed(100)
like_fun=function(param,CP)
{
  ni=nrow(CP)
  nj=length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  price<-as.matrix(CP[,3:12])
  
  ut[,1] = param[nj]*CP[,3]
  ut[,2] = param[nj]*CP[,4] + param[1]
  for(j in c(3:nj)){
    ut[,j] = param[j-1] + param[nj] * CP[,j+2]
  }
  
  
  prob= exp(ut)
  sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

model1_resultoptim  = optim(rep(10,-10,10), fn = like_fun, CP = d3)
model1_resultoptim$par


## ----------------------------------------------------------------------------------------------------------------
library(nloptr)
npar=10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)
model1_result_2   = nloptr(start,eval_f=like_fun, lb=lower,ub=upper,
                          opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=10000),
                          CP=d3)

model1_result_2$solution
  


## ----------------------------------------------------------------------------------------------------------------
set.seed(100)
#data should be in the form of (id, choice, choicePrice)
computellh = function(param,data){
  n = nrow(data)
  m = length(unique(data$choice))
  ut = mat.or.vec(n, m)
  aug = cbind(data,rep(1,n))

  # m(1,10) --->data(,2:12)
  for (j in 1:m ){
    ut[, j] = aug[,j+2] * param[j] + param[m]
  }
    
  prob= exp(ut)
  # sprob=rowSums(prob)
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:n)
  {
    probc[i] = prob[i,data$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

result1optim  = optim(rep(10,-10, 10), fn = computellh, data = d3)
result1optim$par


## ----------------------------------------------------------------------------------------------------------------
library(nloptr)# optimization 
npar= 10
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar)
model1_result   = nloptr(start,eval_f=computellh, lb=lower,ub=upper,
                          opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=10000),data = d3)

model1_result$solution


## ----------------------------------------------------------------------------------------------------------------
m = nrow(map)
y = data$
llh1  = function(par, data,choice)


## ----------------------------------------------------------------------------------------------------------------
d4 <- margarine$demo
d4 <- d4[,1:2]
d4 <- left_join(d3[,1:2], d4,by = 'hhid')
d4_1 <- d4[,1:3]
head(d4)


## ----------------------------------------------------------------------------------------------------------------
#data should be the form of (hhid, choice, income)
computellh2 = function(param, data){
  n = nrow(data)
  ut = mat.or.vec(n,1)
  X = data[,3]
  param[1] = 0 # use product 1 as reference
  Xbeta = X * param
  ut <- Xbeta


  probc = exp(ut)
  probc = probc / Xbeta  
  

  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

library(nloptr)
npar = 2
lower  = rep(-10, npar)
upper  = rep(10, npar)
start  = runif(npar)
model2_result_2 = nloptr(start,eval_f=computellh2, lb=lower,ub=upper, opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10,"maxeval"=10000),
                          data = d4)

model2_result_2$solution
  



## ---- echo = True------------------------------------------------------------------------------------------------
#data form(hhid, choice, ....), at least size = {n, m+2}
#delta method
margeff_eval= function(param,data,model){   
  m = length(unique(data$choice))
  n = nrow(data)
  xbeta = rep(0,n)
   for (j in 1:m){
     xbeta = xbeta + data[, j+2] * param[j]
   }
  pr = dlogis(mean(xbeta))
  marg = param * pr
  return(marg)
  }
# delta method
heps = 0.00001
gprime = NULL
out = NULL
out_bench = margeff_eval(model1_result_2$solution,d3, "like_fun")
#model1_margeff = margeff(model1_result_2$result, d3, yvar,'like_fun')

for (i in 2:length(model1_result_2$solution))
{
  parh = model1_result_2$solution
  parh[i] = model1_result_2$solution[i] + heps 
  res = margeff_eval(parh, d3,"like_fun")
  out[i-1] = res[i-1]
}
gprime = (out - out_bench)/heps
#==================
# sds
#==================
res_logit   = optim(runif(10,-10,10),fn = like_fun,method="BFGS",control=list(trace=6,maxit=1000),CP = d3,hessian=TRUE)
fisher_info = solve(res_logit$hessian)       # standard formula is -res$hessian but flike is return -like
var_probit  = sqrt(diag(fisher_info))
var_marg = gprime*var_probit[-1]

# close enough .. may need the second type of finite difference
tabf = cbind(out_bench,var_marg)
tabf


## ----------------------------------------------------------------------------------------------------------------
heps = 0.00001
gprime = NULL
out = NULL
margeff_eval2= function(param,data,model){   
  m = length(unique(data$choice))
  n = nrow(data)

  X = data[,3]
  Xbeta = X * param
  pr = dlogis(mean(xbeta))
  marg = param * pr
  return(marg)
}

out_bench = margeff_eval2(model2_result_2$solution, d4, "computellh2")
#model1_margeff = margeff(model1_result_2$result, d3, yvar,'like_fun')

for (i in 2:length(model2_result_2$solution))
{
  parh = model2_result_2$solution
  parh[i] = model2_result_2$solution[i] + heps 
  res = margeff_eval2(parh, d4,"computellh2")
  out[i-1] = res[i-1]
}
gprime = (out - out_bench)/heps
#==================
# sds
#==================
res_logit   = optim(runif(2,-10,10),fn = computellh2,method="BFGS",control=list(trace=6,maxit=1000),data = d4,hessian=TRUE)
fisher_info = solve(res_logit$hessian)       # standard formula is -res$hessian but flike is return -like
var_probit  = sqrt(diag(fisher_info))
var_marg = gprime*var_probit[-1]

# close enough .. may need the second type of finite difference
tabf = cbind(out_bench,var_marg)
tabf


## ----------------------------------------------------------------------------------------------------------------
like_fun_5=function(param,data){
  
  n = nrow(data)
  m = length(unique(data$choice))
  ut = mat.or.vec(n,m)
  
  
  intercept <- append(0,param[1:9])
  beta <- append(0,param[10:18])
  alpha <- param[19]
  for (j in 1:m){
    ut[,j] = CP$Income*beta[j]+intercept[j]+alpha * CP[,j+2]
  }
  
  
  prob = exp(ut)
  sprob = rowSums(prob)
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:ni){
    probc[i] = prob[i,CP$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}

npar=19
lower  = rep(-10,npar)
upper  = rep(5,npar)
start  = runif(npar)

model3_result  = nloptr(start,eval_f=like_fun_5, lb=lower,ub=upper,
                        opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                        CP=CPDM)

model3_result$solution




#beta_r
#delete first choice

CPDM_temp <- subset(CPDM, choice != 1)



## ----------------------------------------------------------------------------------------------------------------

like_fun_6=function(param,CP){
  
  ni = nrow(CP)
  nj = length(unique(CP$choice))
  ut = mat.or.vec(ni,nj)
  
  
  intercept <- append(0,param[1:8])
  beta <- append(0,param[9:16])
  alpha <- param[17]
  for (j in 1:nj){
    ut[,j] = CP$Income*beta[j]+intercept[j]+alpha * CP[,j+3]
  }
  
  
  prob = exp(ut)
  sprob = rowSums(prob)
  prob  = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob))
  probc = NULL
  
  for (i in 1:ni){
    probc[i] = prob[i,CP$choice[i]-1]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  
  like = sum(log(probc))
  return(-like)
}

npar=17
lower  = rep(-10,npar)
upper  = rep(5,npar)
start  = runif(npar)

model4_result  = nloptr(start,eval_f=like_fun_6, lb=lower,ub=upper,
                        opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10,"maxeval"=10000),
                        CP=CPDM_temp)

model4_result$solution


## ----------------------------------------------------------------------------------------------------------------
HM_test = function(beta1, beta2){
  
  
  
  
  
  
  
  
}


## ----------------------------------------------------------------------------------------------------------------



## ----------------------------------------------------------------------------------------------------------------
rmarkdown::render('Yifei.Peng_A2.Rmd', 'html_document')

