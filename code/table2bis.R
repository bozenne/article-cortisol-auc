### table2bis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 16 2021 (16:53) 
## Version: 
## Last-Updated: mar  1 2022 (17:19) 
##           By: Brice Ozenne
##     Update #: 26
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
library(officer)

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
AUCg0.tablePerf <- dt.evalPredictor[,.(median = paste0(round(median(AUCg.estimate-AUCg.pracma),0),
                                                       " (",round(100*median((AUCg.estimate-AUCg.pracma)/AUCg.pracma),1),"%)"),
                                       "2.5% quantile" = paste0(round(quantile(AUCg.estimate-AUCg.pracma, probs = 0.025),0),
                                                                " (",round(100*quantile((AUCg.estimate-AUCg.pracma)/AUCg.pracma,probs =0.025),1),"%)"),
                                      "97.5% quantile" = paste0(round(quantile(AUCg.estimate-AUCg.pracma, probs = 0.975),0),
                                                                " (",round(100*quantile((AUCg.estimate-AUCg.pracma)/AUCg.pracma,probs =0.975),1),"%)"),
                                      IQR = paste0(round(IQR(AUCg.estimate-AUCg.pracma),0), " (",round(100*IQR((AUCg.estimate-AUCg.pracma)/AUCg.pracma),1),"%)"),
                                      correlation = round(cor(AUCg.estimate,AUCg.pracma),2)),
                                   by = c("dataset","method","timepoint","Gender")]
setkeyv(AUCg0.tablePerf, c("Gender","dataset","method","timepoint"))

AUCg.tablePerf <- copy(AUCg0.tablePerf)
AUCg.tablePerf[, method := ifelse(duplicated(method),"",method) , by = "dataset"]
AUCg.tablePerf[duplicated(dataset), dataset := ""]

AUCi0.tablePerf <- dt.evalPredictor[,.(median = round(median(AUCi.estimate-AUCi.pracma),0),
                                      "2.5% quantile" = round(quantile(AUCi.estimate-AUCi.pracma,probs = 0.025),0),
                                      "97.5% quantile" = round(quantile(AUCi.estimate-AUCi.pracma,probs = 0.975),0),
                                      IQR = round(IQR(AUCi.estimate-AUCi.pracma),0),
                                      correlation = round(cor(AUCi.estimate,AUCi.pracma),2)),
                                   by = c("dataset","method","timepoint","Gender")]
setkeyv(AUCi0.tablePerf, c("dataset","method","timepoint"))
AUCi.tablePerf <- copy(AUCi0.tablePerf)
AUCi.tablePerf[, method := ifelse(duplicated(method),"",method) , by = "dataset"]
AUCi.tablePerf[duplicated(dataset), dataset := ""]

## * Display
## ** console
AUCg.tablePerfFM <- data.table(dataset = AUCg.tablePerf[Gender == "Female",dataset],
                               timepoint = AUCg.tablePerf[Gender == "Female",timepoint],
                               median = paste(AUCg.tablePerf[Gender == "Male",median],
                                              "vs",
                                              AUCg.tablePerf[Gender == "Female",median]),
                               IQR = paste(AUCg.tablePerf[Gender == "Male",IQR],
                                           "vs",
                                           AUCg.tablePerf[Gender == "Female",IQR]),
                               correlation = paste(AUCg.tablePerf[Gender == "Male",correlation],
                                                   "vs",
                                                   AUCg.tablePerf[Gender == "Female",correlation])
                               )
AUCg.tablePerfFM
##     dataset timepoint                         median                        IQR  correlation
##  1:           0-15-30   -397 (-48.8%) vs -472 (-52%)   238 (8.8%) vs 265 (9.4%) 0.94 vs 0.94
##  2:           0-15-45 -204 (-25.7%) vs -238 (-27.7%)   126 (9.1%) vs 166 (8.1%) 0.97 vs 0.98
##  3:           0-15-60       -44 (-6%) vs -45 (-5.7%)   106 (12%) vs 105 (10.9%) 0.96 vs 0.97
##  4:           0-30-45 -192 (-23.9%) vs -224 (-24.8%)   130 (7.8%) vs 146 (6.9%) 0.97 vs 0.98
##  5:           0-30-60      -13 (-1.5%) vs -6 (-0.7%)     62 (9.1%) vs 69 (7.7%) 0.97 vs 0.99
##  6:           0-45-60       -52 (-7%) vs -29 (-3.5%) 121 (14.4%) vs 116 (13.1%) 0.95 vs 0.97
##  7:           0-15-30        26 (3.2%) vs -8 (-0.9%)   98 (14.6%) vs 95 (11.3%) 0.96 vs 0.97
##  8:           0-15-45             7 (0.8%) vs 0 (0%)     56 (7.1%) vs 59 (6.7%) 0.98 vs 0.99
##  9:           0-15-60          3 (0.4%) vs 11 (1.2%)   102 (13.3%) vs 101 (12%) 0.96 vs 0.97
## 10:           0-30-45           4 (0.5%) vs 1 (0.1%)     65 (8.5%) vs 63 (7.5%) 0.98 vs 0.99
## 11:           0-30-60         -5 (-0.7%) vs 5 (0.6%)       68 (9%) vs 69 (7.7%) 0.97 vs 0.99
## 12:           0-45-60        -2 (-0.4%) vs 23 (2.5%) 117 (16.9%) vs 112 (13.5%) 0.95 vs 0.97
## 13:           0-15-30 -440 (-48.3%) vs -488 (-50.7%)  271 (9.5%) vs 346 (10.6%) 0.94 vs 0.94
## 14:           0-15-45 -233 (-25.4%) vs -250 (-26.2%)   149 (9.1%) vs 198 (7.9%) 0.97 vs 0.98
## 15:           0-15-60     -38 (-4.1%) vs -38 (-4.4%) 121 (13.1%) vs 107 (10.9%) 0.96 vs 0.98
## 16:           0-30-45 -211 (-23.7%) vs -238 (-25.7%)     118 (7%) vs 177 (7.8%) 0.98 vs 0.98
## 17:           0-30-60        -8 (-0.9%) vs -17 (-2%)     73 (9.2%) vs 83 (8.8%) 0.99 vs 0.99
## 18:           0-45-60     -69 (-6.6%) vs -59 (-6.2%) 113 (11.8%) vs 117 (10.9%) 0.96 vs 0.97
## 19:           0-15-30            21 (2%) vs 4 (0.6%)  88 (10.1%) vs 120 (13.4%) 0.98 vs 0.97
## 20:           0-15-45             0 (0%) vs 4 (0.6%)     57 (7.2%) vs 66 (8.3%) 0.99 vs 0.99
## 21:           0-15-60            7 (0.7%) vs 13 (2%)    104 (12.9%) vs 99 (11%) 0.97 vs 0.98
## 22:           0-30-45       -3 (-0.2%) vs -9 (-1.2%)     80 (9.5%) vs 67 (6.9%) 0.99 vs 0.99
## 23:           0-30-60       -2 (-0.2%) vs -4 (-0.7%)     76 (8.5%) vs 84 (8.6%) 0.99 vs 0.99
## 24:           0-45-60      -17 (-1.7%) vs -5 (-0.5%) 116 (13.6%) vs 109 (11.4%) 0.96 vs 0.97
##     dataset timepoint                         median                        IQR  correlation

AUCi.tablePerfFM <- data.table(dataset = AUCi.tablePerf[Gender == "Male",dataset],
                               timepoint = AUCi.tablePerf[Gender == "Male",timepoint],
                               median = paste(AUCi.tablePerf[Gender == "Male",median],
                                              "vs",
                                              AUCi.tablePerf[Gender == "Female",median]),
                               IQR = paste(AUCi.tablePerf[Gender == "Male",IQR],
                                           "vs",
                                           AUCi.tablePerf[Gender == "Female",IQR]),
                               correlation = paste(AUCi.tablePerf[Gender == "Male",correlation],
                                                   "vs",
                                                   AUCi.tablePerf[Gender == "Female",correlation])
                               )
AUCi.tablePerfFM
##                dataset timepoint      median        IQR  correlation
##  1: CV on training set   0-15-30 -61 vs -105 172 vs 222 0.87 vs 0.93
##  2:                      0-15-45  -36 vs -60 122 vs 129 0.95 vs 0.97
##  3:                      0-15-60  -44 vs -45 106 vs 105 0.92 vs 0.96
##  4:                      0-30-45  -26 vs -46 104 vs 127 0.94 vs 0.98
##  5:                      0-30-60   -13 vs -6   62 vs 69 0.95 vs 0.98
##  6:                      0-45-60  -52 vs -29 121 vs 116  0.9 vs 0.95
##  7:                      0-15-30    32 vs -7 101 vs 109 0.92 vs 0.95
##  8:                      0-15-45      8 vs 0   65 vs 61 0.96 vs 0.98
##  9:                      0-15-60     3 vs 11 102 vs 101 0.92 vs 0.96
## 10:                      0-30-45     7 vs -1   69 vs 60 0.95 vs 0.98
## 11:                      0-30-60     -5 vs 5   68 vs 69 0.95 vs 0.98
## 12:                      0-45-60    -2 vs 23 117 vs 112  0.9 vs 0.95
## 13:           test set   0-15-30  -44 vs -92 237 vs 264 0.92 vs 0.91
## 14:                      0-15-45  -25 vs -53 159 vs 166 0.97 vs 0.97
## 15:                      0-15-60  -38 vs -38 121 vs 107 0.96 vs 0.96
## 16:                      0-30-45  -20 vs -51 128 vs 147 0.98 vs 0.97
## 17:                      0-30-60   -8 vs -17   73 vs 83 0.99 vs 0.97
## 18:                      0-45-60  -69 vs -59 113 vs 117 0.95 vs 0.95
## 19:                      0-15-30    27 vs 15  97 vs 123 0.97 vs 0.93
## 20:                      0-15-45     5 vs 11   59 vs 81 0.98 vs 0.98
## 21:                      0-15-60     7 vs 13  104 vs 99 0.96 vs 0.96
## 22:                      0-30-45     9 vs -2   83 vs 78 0.99 vs 0.97
## 23:                      0-30-60    -2 vs -4   76 vs 84 0.99 vs 0.97
## 24:                      0-45-60   -17 vs -5 116 vs 109 0.95 vs 0.95
##                dataset timepoint      median        IQR  correlation


## ** docx
myTable2bis.doc <- body_add_table(x = read_docx(), 
                               value =  AUCg.tablePerfFM)
myTable2bis.doc <- body_add_break(myTable2bis.doc)
myTable2bis.doc <- body_add_table(x = myTable2bis.doc, 
                               value =  AUCi.tablePerfFM)
myTable2bis.doc <- body_end_section_landscape(myTable2bis.doc)
print(myTable2bis.doc, target = file.path(path.report,"/Table2bis.docx"))


##----------------------------------------------------------------------
### table2bis.R ends here
