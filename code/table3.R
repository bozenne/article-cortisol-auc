### table3.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 16 2021 (18:18) 
## Version: 
## Last-Updated: nov 16 2021 (18:35) 
##           By: Brice Ozenne
##     Update #: 8
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(xtable)
library(data.table)

## * Path
path <- "." ## put path to Github directory
path.code <- file.path(path,"code")
path.data <- file.path(path,"source")
path.report <- file.path(path,"report")
path.results <- file.path(path,"results")

## * Load results
ls.table3 <- readRDS(file = file.path(path.results,"ls-table3.rds"))
## or
## source(file.path("code","re-analysis.R"))

## * display
## ** console
ls.table3
## $Jak2016_pal
##                     Estimate Std. Error   Pr(>|t|)     2.5 %    97.5 %
## AUC with 5 samples -328.2406   123.2851 0.01313013 -581.6568 -74.82453
## AUC with 3 samples -284.2505   108.1232 0.01418977 -506.5008 -62.00016
## LM with 3 samples  -289.4799   108.8211 0.01320065 -513.1647 -65.79498

## $Jak2016_hip
##                     Estimate Std. Error  Pr(>|t|)     2.5 %   97.5 %
## AUC with 5 samples -334.6063   411.0139 0.4229817 -1179.457 510.2448
## AUC with 3 samples -315.3216   358.7619 0.3874977 -1052.767 422.1241
## LM with 3 samples  -334.7316   361.4060 0.3628638 -1077.612 408.1491

## $Fro2014
##                    Estimate Std. Error    Pr(>|t|)    2.5 %   97.5 %
## AUC with 5 samples 1715.996   594.2885 0.005899568 519.7552 2912.237
## AUC with 3 samples 1676.618   646.6305 0.012719381 375.0186 2978.218
## LM with 3 samples  1716.492   656.8608 0.012154163 393.5063 3039.478

## ** latex
lapply(ls.table3, xtable, digits = 3)

##----------------------------------------------------------------------
### table3.R ends here