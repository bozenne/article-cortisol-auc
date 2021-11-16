### table2.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 16 2021 (16:53) 
## Version: 
## Last-Updated: nov 16 2021 (21:32) 
##           By: Brice Ozenne
##     Update #: 18
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
dt.evalPredictor <- as.data.table(readRDS(file = file.path(path.results,"dt-evalPredictor.rds")))
## or
## source(file.path("code","analysis-AUC-cortisol.R"))

## * Processing
AUCg0.tablePerf <- dt.evalPredictor[,.(median = paste0(round(median(AUC.error),0), " (",round(100*median(AUC.error/AUCg.pracma),1),"%)"),
                                      "2.5% quantile" = paste0(round(quantile(AUC.error,probs = 0.025),0), " (",round(100*quantile(AUC.error/AUCg.pracma,probs =0.025),1),"%)"),
                                      "97.5% quantile" = paste0(round(quantile(AUC.error,probs = 0.975),0), " (",round(100*quantile(AUC.error/AUCg.pracma,probs =0.975),1),"%)"),
                                      IQR = paste0(round(IQR(AUC.error),0), " (",round(100*IQR(AUC.error/AUCg.pracma),1),"%)"),
                                      correlation = round(cor(AUCg.estimate,AUCg.pracma),2)),
                                   by = c("dataset","method","timepoint")]
setkeyv(AUCg0.tablePerf, c("dataset","method","timepoint"))

AUCg.tablePerf <- copy(AUCg0.tablePerf)
AUCg.tablePerf[, method := ifelse(duplicated(method),"",method) , by = "dataset"]
AUCg.tablePerf[duplicated(dataset), dataset := ""]

AUCi0.tablePerf <- dt.evalPredictor[,.(median = round(median(AUC.error),0),
                                      "2.5% quantile" = round(quantile(AUC.error,probs = 0.025),0),
                                      "97.5% quantile" = round(quantile(AUC.error,probs = 0.975),0),
                                      IQR = round(IQR(AUC.error),0),
                                      correlation = round(cor(AUCi.estimate,AUCi.pracma),2)),
                                   by = c("dataset","method","timepoint")]
setkeyv(AUCi0.tablePerf, c("dataset","method","timepoint"))
AUCi.tablePerf <- copy(AUCi0.tablePerf)
AUCi.tablePerf[, method := ifelse(duplicated(method),"",method) , by = "dataset"]
AUCi.tablePerf[duplicated(dataset), dataset := ""]

## * Display
## ** console
AUCg.tablePerf
##                dataset method timepoint        median 2.5% quantile 97.5% quantile         IQR correlation
##  1: CV on training set    auc   0-15-30 -440 (-50.5%) -847 (-65.7%)  -114 (-37.2%) 256 (10.3%)        0.94
##  2:                             0-15-45 -221 (-26.8%) -456 (-39.5%)     -46 (-13%)  156 (8.7%)        0.97
##  3:                             0-15-60   -45 (-5.8%) -241 (-24.7%)     92 (15.7%) 105 (11.7%)        0.97
##  4:                             0-30-45 -210 (-24.5%) -451 (-38.3%)      -33 (-8%)  138 (7.5%)        0.97
##  5:                             0-30-60    -8 (-1.1%) -139 (-18.1%)    107 (17.2%)   65 (8.3%)        0.98
##  6:                             0-45-60   -36 (-4.9%) -248 (-28.4%)    105 (17.2%) 118 (13.4%)        0.96
##  7:                        lm   0-15-30        8 (1%) -199 (-20.1%)    173 (25.8%)  99 (12.8%)        0.97
##  8:                             0-15-45      4 (0.5%)   -118 (-12%)    111 (16.9%)     57 (7%)        0.98
##  9:                             0-15-60        9 (1%) -185 (-17.9%)    151 (28.7%) 101 (12.7%)        0.97
## 10:                             0-30-45      2 (0.2%)   -119 (-14%)    119 (19.5%)   64 (8.1%)        0.98
## 11:                             0-30-60        0 (0%) -131 (-16.9%)    122 (18.1%)     68 (8%)        0.98
## 12:                             0-45-60     14 (1.7%) -185 (-20.8%)    156 (30.2%)   116 (15%)        0.96
## 13:           test set    auc   0-15-30   -463 (-50%)  -1092 (-65%)  -119 (-36.9%) 314 (10.1%)        0.94
## 14:                             0-15-45 -244 (-25.8%) -536 (-39.4%)   -55 (-11.7%)  186 (8.2%)        0.98
## 15:                             0-15-60   -38 (-4.3%) -248 (-22.7%)    107 (14.2%) 113 (11.5%)        0.97
## 16:                             0-30-45 -231 (-25.1%) -528 (-38.9%)   -44 (-10.5%)  167 (7.5%)        0.98
## 17:                             0-30-60   -12 (-1.5%) -182 (-18.6%)     97 (15.1%)   78 (8.3%)        0.99
## 18:                             0-45-60   -62 (-6.3%) -299 (-28.2%)    119 (17.2%) 118 (11.2%)        0.97
## 19:                        lm   0-15-30     10 (1.3%) -232 (-18.8%)    143 (21.9%) 113 (12.7%)        0.97
## 20:                             0-15-45      3 (0.3%) -115 (-11.5%)    116 (17.6%)     61 (8%)        0.99
## 21:                             0-15-60     11 (1.5%) -186 (-16.7%)    160 (26.6%) 101 (11.5%)        0.97
## 22:                             0-30-45    -8 (-0.7%) -149 (-13.4%)    103 (12.6%)   71 (7.5%)        0.99
## 23:                             0-30-60    -3 (-0.4%) -166 (-17.2%)    105 (16.5%)   79 (8.6%)        0.99
## 24:                             0-45-60    -8 (-0.8%) -241 (-20.9%)    177 (28.8%)   116 (12%)        0.97
##                dataset method timepoint        median 2.5% quantile 97.5% quantile         IQR correlation
AUCi.tablePerf
##                dataset method timepoint median 2.5% quantile 97.5% quantile IQR correlation
##  1: CV on training set    auc   0-15-30   -440          -847           -114 256        0.90
##  2:                             0-15-45   -221          -456            -46 156        0.96
##  3:                             0-15-60    -45          -241             92 105        0.94
##  4:                             0-30-45   -210          -451            -33 138        0.97
##  5:                             0-30-60     -8          -139            107  65        0.97
##  6:                             0-45-60    -36          -248            105 118        0.93
##  7:                        lm   0-15-30      8          -199            173  99        0.94
##  8:                             0-15-45      4          -118            111  57        0.97
##  9:                             0-15-60      9          -185            151 101        0.94
## 10:                             0-30-45      2          -119            119  64        0.97
## 11:                             0-30-60      0          -131            122  68        0.97
## 12:                             0-45-60     14          -185            156 116        0.93
## 13:           test set    auc   0-15-30   -463         -1092           -119 314        0.92
## 14:                             0-15-45   -244          -536            -55 186        0.97
## 15:                             0-15-60    -38          -248            107 113        0.96
## 16:                             0-30-45   -231          -528            -44 167        0.97
## 17:                             0-30-60    -12          -182             97  78        0.98
## 18:                             0-45-60    -62          -299            119 118        0.95
## 19:                        lm   0-15-30     10          -232            143 113        0.94
## 20:                             0-15-45      3          -115            116  61        0.98
## 21:                             0-15-60     11          -186            160 101        0.96
## 22:                             0-30-45     -8          -149            103  71        0.98
## 23:                             0-30-60     -3          -166            105  79        0.98
## 24:                             0-45-60     -8          -241            177 116        0.95
##                dataset method timepoint median 2.5% quantile 97.5% quantile IQR correlation

## ** latex
print.tablePerf <- copy(AUCg.tablePerf)
print.tablePerf[1, dataset := "training"]
print.tablePerf[2, dataset := "(with CV)"]
names(print.tablePerf)[3] <- ""
names(print.tablePerf)[5] <- "q. 2.5%"
names(print.tablePerf)[6] <- "q. 97.5%"
names(print.tablePerf)[8] <- "cor."
print(xtable(print.tablePerf), include.rownames = FALSE)

print.tablePerf <- copy(AUCi.tablePerf)
print.tablePerf[1, dataset := "training"]
print.tablePerf[2, dataset := "(with CV)"]
names(print.tablePerf)[3] <- "" 
names(print.tablePerf)[5] <- "q. 2.5%"
names(print.tablePerf)[6] <- "q. 97.5%"
names(print.tablePerf)[8] <- "cor."
print(xtable(print.tablePerf), include.rownames = FALSE)

##----------------------------------------------------------------------
### table2.R ends here
