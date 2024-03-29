### table1.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 16 2021 (16:38) 
## Version: 
## Last-Updated: nov 16 2021 (18:35) 
##           By: Brice Ozenne
##     Update #: 6
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
trainL.AUC <- readRDS(file.path(path.results,"trainL_AUC.rds"))
## or
## source(file.path("code","analysis-AUC-cortisol.R"))

## * Display
## ** console
ls.tablelm <- lapply(attr(trainL.AUC,"model"),function(iM){cbind(summary(iM$lm)$coef, confint(iM$lm))})
M.tablelm <- do.call(cbind,lapply(ls.tablelm, function(iT){paste0(round(iT[,"Estimate"],2)," [",round(iT[,"2.5 %"],2),";",round(iT[,"97.5 %"],2),"]")}))
rownames(M.tablelm) <- c("(Intercept)","sample 1","sample 2","sample 3")
colnames(M.tablelm) <- c("0-15-30","0-15-45","0-15-60","0-30-45","0-30-60","0-45-60")
M.tablelm
##             0-15-30             0-15-45               0-15-60              
## (Intercept) "48.96 [26.91;71]"  "30.43 [15.74;45.12]" "41.37 [19.43;63.32]"
## sample 1    "0.01 [-0.26;0.28]" "0.04 [-0.13;0.22]"   "-0.02 [-0.29;0.24]" 
## sample 2    "0.07 [-0.09;0.23]" "-0.02 [-0.08;0.04]"  "-0.08 [-0.14;-0.01]"
## sample 3    "3.14 [2.89;3.38]"  "0.91 [0.84;0.98]"    "0.17 [0.1;0.23]"    
##             0-30-45              0-30-60              0-45-60              
## (Intercept) "18.67 [2.95;34.39]" "9.28 [-7.97;26.53]" "42.74 [18.14;67.34]"
## sample 1    "-0.05 [-0.13;0.03]" "-0.05 [-0.13;0.04]" "-0.05 [-0.13;0.03]" 
## sample 2    "0.07 [0;0.14]"      "-0.02 [-0.07;0.03]" "0.02 [-0.07;0.1]"   
## sample 3    "1.6 [1.4;1.79]"     "0.09 [0;0.19]"      "0.15 [-0.21;0.5]"   


## ** latex
xtable(t(M.tablelm))

##----------------------------------------------------------------------
### table1.R ends here
