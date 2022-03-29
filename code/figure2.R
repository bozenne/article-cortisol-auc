### figure2.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 16 2021 (10:52) 
## Version: 
## Last-Updated: mar 29 2022 (11:57) 
##           By: Brice Ozenne
##     Update #: 23
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(ggplot2)
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

## * Data processing
dt.evalPredictor[, dataset := factor(dataset, levels = c("CV on training set","test set"), labels = c("Healthy subjects","Case subject"))]
dt.evalPredictor[, estimator := factor(estimator, levels = c("trapezoidal rule","linear regression"))]

AUCg0.tablePerf <- dt.evalPredictor[,.(correlation = round(cor(AUCg.estimate,AUCg.pracma),2)),
                                    by = c("dataset","method","timepoint")]
setkeyv(AUCg0.tablePerf, c("dataset","method","timepoint"))
AUCg0.tablePerf[, estimator := factor(method, levels = c("auc","lm"), labels  = c("trapezoidal rule","linear regression"))]
AUCg0.tablePerf[, timepoint.plot := paste("samples:", timepoint)]

AUCi0.tablePerf <- dt.evalPredictor[,.(correlation = round(cor(AUCi.estimate,AUCi.pracma),2)),
                                    by = c("dataset","method","timepoint")]
AUCi0.tablePerf[, estimator := factor(method, levels = c("auc","lm"), labels  = c("trapezoidal rule","linear regression"))]
AUCi0.tablePerf[, timepoint.plot := paste("samples:", timepoint)]

## * Display
gg.AUCg.cor <- ggplot(dt.evalPredictor, aes(x = AUCg.pracma, y = AUCg.estimate))
gg.AUCg.cor <- gg.AUCg.cor + geom_abline(intercept=0, slope = 1, color = "purple", size = 1.5)
gg.AUCg.cor <- gg.AUCg.cor + geom_point(alpha = 0.1)
gg.AUCg.cor <- gg.AUCg.cor + facet_grid(estimator~timepoint.plot)
gg.AUCg.cor <- gg.AUCg.cor + geom_smooth(size = 1.5, aes(color = dataset))
gg.AUCg.cor <- gg.AUCg.cor + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.AUCg.cor <- gg.AUCg.cor + labs(color = "") + ylab("3-sample AUCg") + xlab("5-sample AUCg")
gg.AUCg.cor <- gg.AUCg.cor + geom_text(data = AUCg0.tablePerf[dataset == "Healthy subjects"],
                                       mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset, x = 500, y = 2500))
gg.AUCg.cor <- gg.AUCg.cor + geom_text(data = AUCg0.tablePerf[dataset == "Case subject"],
                                       mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset, x = 1500, y = 2500))
gg.AUCg.cor

gg.AUCi.cor <- ggplot(dt.evalPredictor, aes(x = AUCi.pracma, y = AUCi.estimate))
gg.AUCi.cor <- gg.AUCi.cor + geom_abline(intercept=0, slope = 1, color = "purple", size = 1.5)
gg.AUCi.cor <- gg.AUCi.cor + geom_point(alpha = 0.1)
gg.AUCi.cor <- gg.AUCi.cor + facet_grid(estimator~timepoint.plot)
gg.AUCi.cor <- gg.AUCi.cor + geom_smooth(size = 1.5, aes(color = dataset))
gg.AUCi.cor <- gg.AUCi.cor + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.AUCi.cor <- gg.AUCi.cor + labs(color = "") + ylab("3-sample AUCi") + xlab("5-sample AUCi")
gg.AUCi.cor <- gg.AUCi.cor + geom_text(data = AUCi0.tablePerf[dataset == "Healthy subjects"], mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset), x = 500, y = 2000)
gg.AUCi.cor <- gg.AUCi.cor + geom_text(data = AUCi0.tablePerf[dataset == "Case subject"], mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset), x = -500, y = 2000)
gg.AUCi.cor

## * export
ggsave(gg.AUCg.cor, filename = file.path(path.results,"figure2a.pdf"), width = 13, device = cairo_pdf)
ggsave(gg.AUCi.cor, filename = file.path(path.results,"figure2b.pdf"), width = 13, device = cairo_pdf)

ggsave(gg.AUCg.cor, filename = file.path(path.report,"figures/AUCg-perf-cor.pdf"), width = 13, device = cairo_pdf)
ggsave(gg.AUCi.cor, filename = file.path(path.report,"figures/AUCi-perf-cor.pdf"), width = 13, device = cairo_pdf)

##----------------------------------------------------------------------
### figure2.R ends here
