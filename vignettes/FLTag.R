## ----knitr_init, echo=FALSE, results="hide"------------------------------
library(knitr)
## Global options
opts_chunk$set(echo    =!TRUE,
               eval    =TRUE,
               prompt  =FALSE,
               comment =NA,
               message =FALSE,
               tidy    =FALSE,
               warnings=FALSE,
               fig.height=4.5,
               fig.width =4.5)

## ---- pkgs, echo=FALSE, message=FALSE------------------------------------
warn=options()$warn
options(warn=-1)
library(ggplot2)
library(kobe)

theme_set(theme_bw())
options(digits=3)
options(warn=warn)

## ----install,echo=TRUE,eval=FALSE----------------------------------------
## install.packages("FLTage", repos = "http://cloud.r-project.org/")

## ----src,echo=TRUE,eval=FALSE--------------------------------------------
## source("http://flr-project.org/R/instFLR.R")

## ----inst,echo=TRUE,eval=FALSE-------------------------------------------
## install.packages(repos="http://flr-project.org/R")

## ----lib,echo=TRUE-------------------------------------------------------
library(FLTag)

## ----data,echo=TRUE------------------------------------------------------
data(releases)
data(recoveries)

## ----data-rel,echo=TRUE,eval=FALSE---------------------------------------
## head(releases)

## ----kpp,echo=TRUE-------------------------------------------------------

## ----plyr,echo=TRUE------------------------------------------------------

## ----plyr2,echo=TRUE,eval=FALSE------------------------------------------
## trks=ddply(yft, .(method,scenario), with, quantile(stock))

## ----plyr3,echo=TRUE,eval=FALSE------------------------------------------
## trks=ddply(yft, .(method,scenario,year), with, quantile(stock))
## 
## head(trks)

## ----plyr4,echo=TRUE,eval=FALSE------------------------------------------
## head(trks)

## ----plyr5,echo=TRUE,fig.height=4,fig.width=8----------------------------

## ---- devtools, echo=TRUE, eval=FALSE------------------------------------
## 	library(devtools)
## 	install_github('flr/FLPKG')

