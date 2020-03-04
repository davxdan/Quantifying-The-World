### Quantifying the world - Case 4
### Davieau, Jiang, Rollins

# load packages
library(tswge)
library(tseries)


# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio
this_directory = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this_directory)

data_file = "price.csv"
df = read.csv(data_file)

# realization, autocorrelations, spectral density
plotts.sample.wge(df$Close)

# difference once
df_d1 = artrans.wge(df$Close,phi.tr=1) # appears stationary
plotts.sample.wge(df_d1)
adf.test(df_d1) # dickey fuller test reject, i.e. stationary

# top combinations of p and q
aic5.wge(df_d1) # ARMA(0,0) is top choice

# residuals pass as white noise
ljung.wge(df_d1,p=0,q=0)
ljung.wge(df_d1,K=48,p=0,q=0)

f = fore.aruma.wge(df$Close,phi=0,theta=0,d=1,n.ahead=100)
