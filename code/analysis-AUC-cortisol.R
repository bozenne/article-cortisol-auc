### analysis-AUC-cortisol.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: maj 11 2020 (10:01) 
## Version: 
## Last-Updated: sep 29 2022 (12:15) 
##           By: Brice Ozenne
##     Update #: 150
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * path
path <- "." ## put path to Github directory
path.code <- file.path(path,"code")
path.data <- file.path(path,"source")
path.report <- file.path(path,"report")
path.results <- file.path(path,"results")


## * library
library(readxl)
library(data.table)
library(ggplot2)
library(ggpubr)
library(pracma)
library(mgcv)
library(lcmm) ## install.packages("lcmm")
library(xtable)
library(kmlShape) ## install.packages("kmlShape")
source(file.path(path.code,"FCT-calcAUCg.R"))
source(file.path(path.code,"FCT-plotTraj.R"))

## * data management
## ** load
df.data <- readxl::read_xlsx(file.path(path.data,"All_CAR_Arafat_second export.xlsx"))
## names(df.data)

## ** process
## -----Original message-----
## date: Tue, 21 Apr 2020 14:38:50 +0200
## from: Arafat Nasser <arafat.nasser@nru.dk>
## to: Brice Ozenne <brice.ozenne@nru.dk>
## subject: Re: Cortisol awakening response project

## [...] You should probably only look at the healthy subjects for the simulation model. [...]
## Relevant columns are R+T+V+X+Z (cortisol data for t = 0 to 60 min), AQ (AUCg) and AR (AUCi).

## *** rename columns
## df.data
col.newname <- c("CIMBI ID" = "id",
                 "Date of cortisol home samples" = "date",
                 "Person status" = "diseaseGroup",
                 "Gender" = "gender",
                 "Time wake up on day for cortisol home samples" = "time_wake",
                 "Time of cortisol home sample 1" = "time_t0",
                 "Time of cortisol home sample 2" = "time_t15",
                 "Time of cortisol home sample 3" = "time_t30",
                 "Time of cortisol home sample 4" = "time_t45",
                 "Time of cortisol home sample 5" = "time_t60",
                 "Cortisol levels (nmol/L) for home sample #1 (at wake up)" = "cortisol_t0",
                 "Cortisol levels (nmol/L) for home sample #2 (wake up + 15 min)" = "cortisol_t15",
                 "Cortisol levels (nmol/L) for home sample #3 (wake up + 30 min)" = "cortisol_t30",
                 "Cortisol levels (nmol/L) for home sample #4 (wake up + 45 min)" = "cortisol_t45",
                 "Cortisol levels (nmol/L) for home sample #5 (wake up +60 min)" = "cortisol_t60",
                 "Area (nmol/L*min) under the cort. curve with respect to ground" = "AUCg",
                 "Area (nmol/L*min) under the cort. curve with respect to increase" = "AUCi",
                 "Area (nmol/L*min) under the cort. curve above baseline" = "AUCb")

dtW.data <- as.data.table(df.data[,names(col.newname)])
names(dtW.data) <- col.newname
dtW.data[, id2 := if(.N>1){paste0(.SD$id,"_I",1:.N)}else{as.character(.SD$id)}, by = "id", .SDcols = "id"]

## range(c(dtW.data$cortisol_t0,dtW.data$cortisol_t15,dtW.data$cortisol_t30,dtW.data$cortisol_t45,dtW.data$cortisol_t60),na.rm=TRUE)
## 0.7  429.6
##  table(rowSums(is.na(dtW.data[,.(cortisol_t0,cortisol_t15,cortisol_t30,cortisol_t45,cortisol_t60)])))
##    0   1   2   3   4   5 
## 898  50  12   2   3  19 

## *** compute time interval
dtW.data[, interval_awaket0 := as.numeric(diff(as.difftime(c(.SD$time_wake,.SD$time_t0), format = "%H:%M:%S", units = "mins"))), by = "id2"]
dtW.data[, interval_t0t15 := as.numeric(diff(as.difftime(c(.SD$time_t0,.SD$time_t15), format = "%H:%M:%S", units = "mins"))), by = "id2"]
dtW.data[, interval_t15t30 := as.numeric(diff(as.difftime(c(.SD$time_t15,.SD$time_t30), format = "%H:%M:%S", units = "mins"))), by = "id2"]
dtW.data[, interval_t30t45 := as.numeric(diff(as.difftime(c(.SD$time_t30,.SD$time_t45), format = "%H:%M:%S", units = "mins"))), by = "id2"]
dtW.data[, interval_t45t60 := as.numeric(diff(as.difftime(c(.SD$time_t45,.SD$time_t60), format = "%H:%M:%S", units = "mins"))), by = "id2"]

dtW.data[, min_t0 := 0]
dtW.data[, min_t15 := interval_t0t15]
dtW.data[, min_t30 := interval_t0t15 + interval_t15t30]
dtW.data[, min_t45 := interval_t0t15 + interval_t15t30 + interval_t30t45]
dtW.data[, min_t60 := interval_t0t15 + interval_t15t30 + interval_t30t45 + interval_t45t60]

dtW.data[, weight_t0 := interval_t0t15/2]
dtW.data[, weight_t15 := interval_t0t15/2 + interval_t15t30/2]
dtW.data[, weight_t30 := interval_t15t30/2 + interval_t15t30/2]
dtW.data[, weight_t45 := interval_t15t30/2 + interval_t30t45/2]
dtW.data[, weight_t60 := interval_t45t60/2]

## *** restrict to cortisol trajectories with full data and not too delayed baseline
##dtW.data[interval_awaket0<0,.(id,date,time_wake,time_t0,time_t15,AUCg)]
##       id       date time_wake time_t0 time_t15     AUCg
## 1: 50473 15/03/2011  10:00:00 9:42:00     <NA>       NA
## 2: 50542 09/01/2011   9:30:00 6:20:00  6:35:00  375.525
## 3: 52514 03/07/2012  11:00:00 9:40:00  7:17:00 -513.490
## dtW.data[interval_awaket0>15,.(id,date,time_wake,time_t0,AUCg)]

ls.rm <- list(interval_awaket0 = dtW.data[interval_awaket0<0 | interval_awaket0>15,id2],
              interval_t0t15 = dtW.data[interval_t0t15<0 | interval_t0t15>20,id2],
              interval_t15t30 = dtW.data[interval_t15t30<0 | interval_t15t30>20,id2],
              interval_t30t45 = dtW.data[interval_t30t45<0 | interval_t30t45>20,id2],
              interval_t45t60 = dtW.data[interval_t45t60<0 | interval_t45t60>20,id2],
              na.cortisol = dtW.data[is.na(cortisol_t0) | is.na(cortisol_t15) | is.na(cortisol_t30) | is.na(cortisol_t45) | is.na(cortisol_t60),id2],
              na.time = dtW.data[is.na(time_t0) | is.na(time_t15) | is.na(time_t30) | is.na(time_t45) | is.na(time_t60),id2]
              )
## lapply(ls.rm,length)

dtW.fulldata <-  dtW.data[id2 %in% unlist(ls.rm) == FALSE,]
## dtW.fulldata[, .(n = .N , n.id = length(unique(id)))]
##      n n.id
## 1: 815  540
## dtW.fulldata[, .(n = .N , n.id = length(unique(id))), by = "diseaseGroup"]
##       diseaseGroup   n n.id
## 1: Healthy Control 470  325
## 2:            Case 345  215
## dtW.fulldata[, .(n = .N , n.id = length(unique(id))), by = "gender"]
##    gender   n n.id
## 1:   Male 318  214
## 2: Female 497  326

dtL.fulldata <- melt(dtW.fulldata,
                     measure.vars = patterns(cortisol="^cortisol_", time="^min_t", weight="^weight_t"),
                     id.vars = c("id","id2","diseaseGroup","gender","AUCg","AUCi","AUCb"),
                     variable.name = "sample")
dtL.fulldata[,AUCg.pracma := trapz(x = time, y = cortisol), by = "id2"]
dtL.fulldata[,AUCb.pracma := cortisol[1]*max(time), by = "id2"]
dtL.fulldata[,.(cortisol[1],max(time)), by = "id2"]
dtL.fulldata[,AUCi.pracma := AUCg.pracma - AUCb.pracma, by = "id2"]
## dtL.fulldata[abs(AUCg.pracma-AUCg)>1e-5]
## dtL.fulldata[abs(AUCb.pracma-AUCb)>1e-5]
## dtL.fulldata[abs(AUCi.pracma-AUCi)>1e-5]

## *** select HC
## table(dtW.data$diseaseGroup)

dtL.HC <- dtL.fulldata[diseaseGroup=="Healthy Control"]
dtL.HC[, id2.num := as.numeric(as.factor(id2))]
dtL.HC[,diseaseGroup:=NULL]
## table(dtL.HC$test.remove, useNA = "always")
## sum(dtL.HC$AUCg.pracma > 2000) ## 25
dtLR.HC <- dtL.HC[AUCg.pracma <= 2000]

## sum(is.na(dtL.HC$cortisol))
setkeyv(dtL.HC,c("AUCg.pracma","id2"))
setkeyv(dtLR.HC,c("AUCg.pracma","id2"))

dtW.HC <- dcast(dtL.HC, value.var = "cortisol", formula = id + id2 + AUCg + AUCg.pracma ~ sample)
setnames(dtW.HC, old = c("1","2","3","4","5"), new =  c("c0","c15","c30","c45","c60"))

## *** select Case
## table(dtW.data$diseaseGroup)

dtL.Case <- dtL.fulldata[diseaseGroup=="Case"]
dtL.Case[, id2.num := as.numeric(as.factor(id2))]
dtL.Case[,diseaseGroup:=NULL]
## table(dtL.HC$test.remove, useNA = "always")
## sum(dtL.Case$AUCg.pracma > 5000) ## 5
dtLR.Case <- dtL.Case[AUCg.pracma <= 5000]

## dtL.Case[AUCg.pracma > 5000,table(id)]
## 52395 
##     5 

##  length(unique(c(dtLR.Case$id2,dtLR.HC$id2)))
## 809


## length(unique(dtL.Case$id2))
## df.data.Case <- df.data[df.data[["CIMBI ID"]] %in% unique(dtL.Case$id),]
## table(df.data.Case[["Project (primary)"]])
## ttt <- table(df.data.Case[["Project (primary)"]][!duplicated(df.data.Case[["CIMBI ID"]])])
## rbind(cbind(ttt), total = sum(ttt))

## ** check
table(duplicated(dtW.fulldata[,paste0(id,date)]))
## FALSE 
##   898 

range(dtL.HC$cortisol, na.rm = TRUE)
## [1]   0.7 284.1


length(unique(dtLR.Case$id))+length(unique(dtLR.HC$id))
## [1] 537

length(unique(dtLR.Case$id2))+length(unique(dtLR.HC$id2))
## [1] 809

## ** number of repetitions
table(c(table(dtLR.Case$id), table(dtLR.HC$id)))
##   5  10  15  20 
## 319 174  34  10 

## Number of samples break down per number of individual repetitions
319 + 2*174 + 3*34 + 4*10 
## 809


## * descriptive graphs
## dtSum <- rbind(dtLR.HC, dtLR.Case)[,.(median = median(cortisol),
##                                       lower = quantile(cortisol,0.025),
##                                       upper = quantile(cortisol,0.975)),
##                                    by=c("gender","sample")]
## gg_sumTraj <- ggplot(dtSum, aes(x=sample, group = gender, color = gender))
## gg_sumTraj <- gg_sumTraj + geom_point(aes(y = median))
## gg_sumTraj <- gg_sumTraj + geom_errorbar(aes(ymin = lower, ymax = upper))
## gg_sumTraj

IdU <- unique(dtL.HC$id2)
n.Id <- length(IdU)


gg_traj <- ggplot(dtL.HC, aes(x = time, y = cortisol, group = id2, color = AUCg.pracma >= 2000))
gg_traj <- gg_traj + geom_line(alpha = 0.5)
gg_traj <- gg_traj + ylab("cortisol (nmol/L)")
gg_traj <- gg_traj + scale_colour_manual(name = "Excluded",
                                         labels = c("TRUE" = "Yes", "FALSE" = "No"),
                                         values = c("TRUE" = "red",
                                                    "FALSE" = "darkgreen"))
ggTrajInd <-  ggarrange(gg_traj, gg_traj + coord_cartesian(ylim = c(0,35)), ncol = 2, common.legend = TRUE, legend = "bottom")

##  file.remove(file.path(path.report,"cortisol-individual-trajectories.pdf"))
pdf(file.path(path.report,"figures/cortisol-individual-traj.pdf"))
## png(file.path(path.report,"cortisol-individual-trajectories.png"))
ls.groups25 <- lapply(1:ceiling(n.Id/25), function(iG){IdU[(25*(iG-1)+1):min(25*iG,n.Id)]})
vec.ggIndiv <- vector(mode = "list", length = length(ls.groups25))
for(iG in 1:length(ls.groups25)){ ## iG <- 4
    vec.ggIndiv[[iG]] <- ggplot(dtL.HC[id2 %in% ls.groups25[[iG]]][!is.na(cortisol)], aes(x = time, y = cortisol, group = id2, color = (AUCg.pracma > 2000)))
    vec.ggIndiv[[iG]] <- vec.ggIndiv[[iG]] + geom_line() + geom_point()
    vec.ggIndiv[[iG]] <- vec.ggIndiv[[iG]] + facet_wrap(~id2, scales = "free_y")
    vec.ggIndiv[[iG]] <- vec.ggIndiv[[iG]] + theme(legend.position="bottom") + scale_colour_manual(name = "Excluded",
                                                                                                   labels = c("TRUE" = "Yes", "FALSE" = "No"),
                                                                                                   values = c("TRUE" = "red",
                                                                                                              "FALSE" = "darkgreen"))
    print(vec.ggIndiv[[iG]])
}
dev.off()


## length(unique(dtL.HC$id))+length(unique(dtL.Case$id))


## * clustering

## ** kmlShape
if(FALSE){
    myClds <- cldsWide(as.matrix(dtW.HC[id2 %in% unique(dtLR.HC$id2),.(c0,c15,c30,c45,c60)]))
    e.kmlShakpe <- kmlShape(myClds, nbClusters = 2, toPlot = "none")

    e.kmlShakpe <- kmlShape(myClds, nbClusters = 4, toPlot = "none")
    e.kmlShakpe <- kmlShape(myClds, nbClusters = 5)
}
## ** lcmm

if(FALSE){
    nNG <- 6
    ls.hlme <- vector(mode = "list", length = nNG)
    ls.hlme[[1]] <- hlme(cortisol ~ sample, subject = "id2.num", data = dtLR.HC,
                         ng = 1, random =~ sample)
    for(iNG in 2:nNG){
        cat("iNG=",iNG," ")
        ls.hlme[[iNG]] <- hlme(cortisol ~ sample, subject = "id2.num", data = dtLR.HC,
                               ng = iNG, mixture =~ sample, random =~ sample)
        saveRDS(ls.hlme, file.path(path.results,"ls_hlme.rds"))
    }
    cat("\n")

    nNG <- 6
    ls.hlme2 <- vector(mode = "list", length = nNG)
    ls.hlme2[[1]] <- hlme(cortisol ~ sample, subject = "id2.num", data = dtLR.HC,
                          ng = 1, random =~ sample)
    for(iNG in 2:nNG){
        cat("iNG=",iNG," ")
        ls.hlme2[[iNG]] <- hlme(cortisol ~ sample, subject = "id2.num", data = dtLR.HC,
                                ng = iNG, nwg = TRUE, mixture =~ sample, random =~ sample)

        ## lapply(ls.hlme2,"[[","niter")
        ## xx <- gridsearch(hlme(cortisol ~ time, subject = "id2.num", data = dtLR.HC,
        ##                       ng = 6, nwg = TRUE, mixture =~ time, random =~ time), rep=10, maxiter=100, minit = ls.hlme2[[1]])
        saveRDS(ls.hlme2, file.path(path.results,"ls_hlme2.rds"))
    }
    cat("\n")

}else{
    ls.hlme <- readRDS(file.path(path.results,"ls_hlme.rds"))

    ggTraj_hlme(ls.hlme[[1]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme[[2]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme[[3]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme[[4]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_homo <- ggTraj_hlme(ls.hlme[[5]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme[[6]], color = "prob", facet = TRUE, nrow = 1)

    
    ls.hlme2 <- readRDS(file.path(path.results,"ls_hlme2.rds"))

    ggTraj_hlme(ls.hlme2[[1]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme2[[2]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme2[[3]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hlme(ls.hlme2[[4]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hetero <- ggTraj_hlme(ls.hlme2[[5]], color = "prob", facet = TRUE, nrow = 1)
    ggTraj_hetero2 <- ggTraj_hlme(ls.hlme2[[6]], color = "prob", facet = TRUE, nrow = 1)

    ggTrajLCMM <- ggarrange(ggTraj_homo$plot + ggtitle("LCMM with 5 classes - common residual variance-covariance") + ylab("cortisol (nmol/L)"),
                     ggTraj_hetero$plot + ggtitle("LCMM with 5 classes - group specific residual variance-covariance") + ylab("cortisol (nmol/L)"), nrow = 2)
    
    ## cv
    compare.hlme <- summarytable(ls.hlme[[1]], bb = ls.hlme[[2]], ls.hlme[[3]], ls.hlme[[4]], ls.hlme[[5]], ls.hlme[[6]],
                                 which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy"))
    compare.hlme[,"conv"] <- compare.hlme[,"conv"] == 1
    colnames(compare.hlme)[1] <- "nb. classes"
    colnames(compare.hlme)[3] <- "cv"
    colnames(compare.hlme)[4] <- "nb. parameters"
    ##         nb. classes    loglik cv nb. parameters      AIC      BIC    SABIC   entropy
    ## [[                1 -6635.304  1             21 13312.61 13399.59 13332.94 1.0000000
    ## ls.hlme           2 -6586.737  1             27 13227.47 13339.31 13253.62 0.9068225
    ## 1                 3 -6587.364  1             33 13240.73 13377.42 13272.68 0.7004746
    ## [[                4 -6512.689  1             39 13103.38 13264.92 13141.14 0.8814889
    ## ls.hlme           5 -6515.216  1             45 13120.43 13306.82 13164.00 0.8746931
    ## 2                 6 -6456.579  1             51 13015.16 13226.40 13064.54 0.8546314
    ## print(xtable(compare.hlme, type = "latex"), include.rownames = FALSE)
    
    compare.hlme2 <- summarytable(ls.hlme2[[1]], ls.hlme2[[2]], ls.hlme2[[3]], ls.hlme2[[4]], ls.hlme2[[5]], ls.hlme2[[6]],
                                  which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy"))
    compare.hlme2[,"conv"] <- compare.hlme2[,"conv"] == 1
    colnames(compare.hlme2)[1] <- "nb. classes"
    colnames(compare.hlme2)[3] <- "cv"
    colnames(compare.hlme2)[4] <- "nb. parameters"
    ##          nb. classes    loglik cv nb. parameters      AIC      BIC    SABIC   entropy
    ## [[                 1 -6635.304  1             21 13312.61 13399.59 13332.94 1.0000000
    ## ls.hlme2           2 -6505.252  1             28 13066.50 13182.48 13093.62 0.5827591
    ## 1                  3 -6435.973  1             35 12941.95 13086.92 12975.84 0.7871611
    ## [[                 4 -6409.372  1             42 12902.74 13076.71 12943.41 0.7933647
    ## ls.hlme2           5 -6389.418  1             49 12876.84 13079.80 12924.28 0.7134285
    ## 2                  6 -6393.029  1             56 12898.06 13130.01 12952.28 0.7546520
    ## print(xtable(compare.hlme2, type = "latex"), include.rownames = FALSE)
}

## gg <- ggplot(ggTraj_hetero$data[class == 1], aes(x = time, y = obs, group = id2.num))
## gg <- gg + geom_line()
## gg


## * assessement
set.seed(10)
trainL.AUC <- calcAUCgi(data = dtLR.HC,
                        method = c("auc","lm"),
                        timepoint = list("0-15-30" = c(1,2,3),
                                         "0-15-45" = c(1,2,4),
                                         "0-15-60" = c(1,2,5),
                                         "0-30-45" = c(1,3,4),
                                         "0-30-60" = c(1,3,5),
                                         "0-45-60" = c(1,4,5)),
                        var.timepoint = "sample",
                        var.X = "time",
                        var.Y = "cortisol",
                        var.id = "id2",
                        var.truth = c("AUCg.pracma","AUCi.pracma"))

## trainL.AUC[,.(.(range(AUCi.estimate-AUCi.pracma-AUC.error))),by = "timepoint"]

testL.AUC <- calcAUCgi(data = dtLR.HC,
                       newdata = dtLR.Case,
                       method = c("auc","lm"),
                       timepoint = list("0-15-30" = c(1,2,3),
                                        "0-15-45" = c(1,2,4),
                                        "0-15-60" = c(1,2,5),
                                        "0-30-45" = c(1,3,4),
                                        "0-30-60" = c(1,3,5),
                                        "0-45-60" = c(1,4,5)),
                       var.timepoint = "sample",
                       var.X = "time",
                       var.Y = "cortisol",
                       var.id = "id2",
                       var.truth = c("AUCg.pracma","AUCi.pracma"))

dt.evalPredictor <- rbind(cbind(trainL.AUC, dataset = "CV on training set"),
                          cbind(testL.AUC, dataset = "test set"))
dt.evalPredictor[, estimator := factor(method,
                                       levels = c("auc","lm"),
                                       labels = c("trapezoidal rule","linear regression"))]
dt.evalPredictor[, timepoint.plot := paste0("samples: ",timepoint)]


dt.cov <- as.data.table(unique(df.data[,c("CIMBI ID","Gender")]))
dt.cov[["CIMBI ID"]] <- as.character(dt.cov[["CIMBI ID"]])
dt.evalPredictor <- merge(x = cbind(id = gsub("_I1|_I2|_I3|_I4","",dt.evalPredictor$id2), dt.evalPredictor),
                          y = dt.cov,
                          by.x = "id", by.y = "CIMBI ID", all.x = TRUE, all.y = FALSE)

## dt.evalPredictor$AUCi.estimate - dt.evalPredictor$AUCi.pracma - dt.evalPredictor$AUC.error
## range(dt.evalPredictor$AUCg.estimate - dt.evalPredictor$AUCg.pracma - dt.evalPredictor$AUC.error) 

## ** Plot error training set
gg.error <- ggplot(dt.evalPredictor, aes(x = timepoint, y = AUC.error, color = estimator))
gg.error <- gg.error + geom_hline(yintercept=0)
gg.error <- gg.error + geom_boxplot()
gg.error <- gg.error + scale_y_continuous() + theme(text = element_text(size=18))
gg.error <- gg.error + facet_grid(~dataset)
gg.error <- gg.error + ylab("")
gg.error <- gg.error + xlab("3-samples (first-second-third)")
gg.error

gg.AUCg.Rerror <- ggplot(dt.evalPredictor, aes(x = timepoint, y = AUC.error/AUCg.pracma, color = estimator))
gg.AUCg.Rerror <- gg.AUCg.Rerror + geom_hline(yintercept=0)
gg.AUCg.Rerror <- gg.AUCg.Rerror + geom_boxplot()
gg.AUCg.Rerror <- gg.AUCg.Rerror + facet_grid(~dataset)+ theme(text = element_text(size=18))
gg.AUCg.Rerror <- gg.AUCg.Rerror + scale_y_continuous(labels = scales::percent, n.breaks = 7)
gg.AUCg.Rerror <- gg.AUCg.Rerror + ylab("Relative difference in AUCg")
gg.AUCg.Rerror <- gg.AUCg.Rerror + xlab("3-samples (first-second-third)")
gg.AUCg.Rerror

out.error <- ggarrange(gg.error + ylab("Difference in AUCg (nmol.h/L)"), gg.AUCg.Rerror, nrow = 2, common.legend = TRUE, legend = "bottom")
   

gg.AUCg.cor <- ggplot(dt.evalPredictor, aes(x = AUCg.pracma, y = AUCg.estimate))
gg.AUCg.cor <- gg.AUCg.cor + geom_abline(intercept=0, slope = 1, color = "purple", size = 1.5)
gg.AUCg.cor <- gg.AUCg.cor + geom_point(alpha = 0.25)
gg.AUCg.cor <- gg.AUCg.cor + facet_grid(estimator~timepoint.plot)
gg.AUCg.cor <- gg.AUCg.cor + geom_smooth(size = 1.5, aes(color = dataset))
gg.AUCg.cor <- gg.AUCg.cor + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.AUCg.cor <- gg.AUCg.cor + labs(color = "") + ylab("3-sample AUCg") + xlab("5-sample AUCg")
gg.AUCg.cor

gg.AUCi.cor <- ggplot(dt.evalPredictor, aes(x = AUCi.pracma, y = AUCi.estimate))
gg.AUCi.cor <- gg.AUCi.cor + geom_abline(intercept=0, slope = 1, color = "purple", size = 1.5)
gg.AUCi.cor <- gg.AUCi.cor + geom_point(alpha = 0.25)
gg.AUCi.cor <- gg.AUCi.cor + facet_grid(estimator~timepoint.plot)
gg.AUCi.cor <- gg.AUCi.cor + geom_smooth(size = 1.5, aes(color = dataset))
gg.AUCi.cor <- gg.AUCi.cor + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.AUCi.cor <- gg.AUCi.cor + labs(color = "") + ylab("3-sample AUCi") + xlab("5-sample AUCi")
gg.AUCi.cor

gg.blandError <- ggplot(dt.evalPredictor, aes(x = AUCg.pracma, y = AUC.error))
gg.blandError <- gg.blandError + geom_abline(intercept=0, slope = 0, color = "purple", size = 1.5)
gg.blandError <- gg.blandError + geom_point(alpha = 0.25)
gg.blandError <- gg.blandError + facet_grid(estimator~timepoint.plot)
gg.blandError <- gg.blandError + geom_smooth(size = 1.5, aes(color = dataset))
gg.blandError <- gg.blandError + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.blandError <- gg.blandError + labs(color = "") + ylab("Difference in AUCg (nmol.h/L)") + xlab("5-sample AUCg")
gg.blandError

gg.blandRerror <- ggplot(dt.evalPredictor, aes(x = AUCg.pracma, y = AUC.error/AUCg.pracma))
gg.blandRerror <- gg.blandRerror + geom_abline(intercept=0, slope = 0, color = "purple", size = 1.5)
gg.blandRerror <- gg.blandRerror + geom_point(alpha = 0.25)
gg.blandRerror <- gg.blandRerror + facet_grid(estimator~timepoint.plot)
gg.blandRerror <- gg.blandRerror + geom_smooth(size = 1.5, aes(color = dataset))
gg.blandRerror <- gg.blandRerror + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.blandRerror <- gg.blandRerror + labs(color = "") + ylab("Relative difference in AUCg (nmol.h/L)") + xlab("5-sample AUCg")
gg.blandRerror

## * export
## results
## saveRDS(AUCg0.tablePerf, file = file.path(path.results,"AUCg-tablePerf.rds"))
## saveRDS(AUCi0.tablePerf, file = file.path(path.results,"AUCi-tablePerf.rds"))
saveRDS(dt.evalPredictor, file = file.path(path.results,"dt-evalPredictor.rds"))
saveRDS(trainL.AUC, file = file.path(path.results,"trainL_AUC.rds"))

## plots
ggsave(ggTrajInd, filename = file.path(path.report,"figures/cortisol-individual-alltraj.pdf"), width = 10, height = 5)
ggsave(ggTrajLCMM, filename = file.path(path.report,"figures/trajLCMM-5groups.pdf"), width = 13)
ggsave(gg.error + ylab("Difference in AUCi (nmol.h/L)") + theme(legend.position = "bottom"), filename = file.path(path.report,"figures/AUCi-perf-boxplot.pdf"), width = 13, height = 4.5)
ggsave(out.error, filename = file.path(path.report,"figures/AUCg-perf-boxplot.pdf"), width = 13)
ggsave(gg.AUCg.cor, filename = file.path(path.report,"figures/AUCg-perf-cor.pdf"), width = 13)
ggsave(gg.AUCi.cor, filename = file.path(path.report,"figures/AUCi-perf-cor.pdf"), width = 13)
ggsave(gg.blandRerror, filename = file.path(path.report,"figures/AUCg-perf-blandRelative.pdf"), width = 13)


## training dataset
dtTrain <- dtLR.HC[,.(id2,sample,time,cortisol,AUCg.pracma,AUCi.pracma)]
dtTrain$id2 <- paste0("id",as.numeric(as.factor(dtTrain$id2)))
setnames(dtTrain,old="id2",new="id")
saveRDS(dtTrain, file = file.path(path.results,"input_calcAUCgi.rds"))

## * Review comments
## ** multicolinearity
M.vif <- do.call(rbind,lapply(attr(trainL.AUC,"model"), function(iM){car::vif(iM$lm)}))
range(M.vif)
## [1] 1.253987 3.868554

## ** repeated measurements

## *** process data
rmRepData <- function(data){
    idI1 <- unique(data$id[grepl("I1",data$id2)])
    idI2 <- unique(data$id[grepl("I2",data$id2)])
    idI3 <- unique(data$id[grepl("I3",data$id2)])
    idRep <- unique(c(intersect(idI1,idI2),intersect(idI2,idI3),intersect(idI2,idI3)))
    idI1.2select <- intersect(idRep,idI1)
    idI2.2select <- setdiff(intersect(idRep,idI2), idI1.2select)

    out <- rbind(data[data$id %in% idRep == FALSE,],
                 data[data$id2 %in% paste0(idI1.2select,"_I1"),],
                 data[data$id2 %in% paste0(idI2.2select,"_I2"),])
    return(out)
}

## HC
length(unique(dtLR.HC$id))
## [1] 322
dtLR.HC1 <- rmRepData(dtLR.HC)

length(unique(dtLR.HC1$id))
## [1] 322

## Case
length(unique(dtLR.Case$id))
## [1] 215
dtLR.Case1 <- rmRepData(dtLR.Case)

length(unique(dtLR.Case1$id))
## [1] 215


## *** train and test model
trainL1.AUC <- calcAUCgi(data = dtLR.HC1,
                         method = c("auc","lm"),
                         timepoint = list("0-15-30" = c(1,2,3),
                                          "0-15-45" = c(1,2,4),
                                          "0-15-60" = c(1,2,5),
                                          "0-30-45" = c(1,3,4),
                                          "0-30-60" = c(1,3,5),
                                          "0-45-60" = c(1,4,5)),
                         var.timepoint = "sample",
                         var.X = "time",
                         var.Y = "cortisol",
                         var.id = "id2",
                         var.truth = c("AUCg.pracma","AUCi.pracma"))

testL1.AUC <- calcAUCgi(data = dtLR.HC1,
                       newdata = dtLR.Case1,
                       method = c("auc","lm"),
                       timepoint = list("0-15-30" = c(1,2,3),
                                        "0-15-45" = c(1,2,4),
                                        "0-15-60" = c(1,2,5),
                                        "0-30-45" = c(1,3,4),
                                        "0-30-60" = c(1,3,5),
                                        "0-45-60" = c(1,4,5)),
                       var.timepoint = "sample",
                       var.X = "time",
                       var.Y = "cortisol",
                       var.id = "id2",
                       var.truth = c("AUCg.pracma","AUCi.pracma"))

## *** reproduce figure 1
dt.evalPredictor1 <- rbind(cbind(trainL1.AUC, dataset = "CV on training set"),
                           cbind(testL1.AUC, dataset = "test set"))
dt.evalPredictor1[, dataset := factor(dataset, levels = c("CV on training set","test set"), labels = c("Healthy subjects","Case subject"))]
dt.evalPredictor1[, estimator := factor(method,
                                        levels = c("auc","lm"),
                                        labels = c("trapezoidal rule","linear regression"))]
dt.evalPredictor1[, timepoint.plot := paste0("samples: ",timepoint)]
AUCg0.tablePerf1 <- dt.evalPredictor1[,.(correlation = round(cor(AUCg.estimate,AUCg.pracma),2)),
                                     by = c("dataset","method","estimator","timepoint","timepoint.plot")]

gg.AUCg.cor1 <- ggplot(dt.evalPredictor1, aes(x = AUCg.pracma, y = AUCg.estimate))
gg.AUCg.cor1 <- gg.AUCg.cor1 + geom_abline(intercept=0, slope = 1, color = "purple", size = 1.5)
gg.AUCg.cor1 <- gg.AUCg.cor1 + geom_point(alpha = 0.1)
gg.AUCg.cor1 <- gg.AUCg.cor1 + facet_grid(estimator~timepoint.plot)
gg.AUCg.cor1 <- gg.AUCg.cor1 + geom_smooth(size = 1.5, aes(color = dataset))
gg.AUCg.cor1 <- gg.AUCg.cor1 + theme(legend.position = "bottom", text = element_text(size=15), axis.text.x=element_text(angle=90))
gg.AUCg.cor1 <- gg.AUCg.cor1 + labs(color = "") + ylab("3-sample AUCg") + xlab("5-sample AUCg")
gg.AUCg.cor1 <- gg.AUCg.cor1 + geom_text(data = AUCg0.tablePerf1[dataset == "Healthy subjects"],
                                         mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset, x = 500, y = 2500))
gg.AUCg.cor1 <- gg.AUCg.cor1 + geom_text(data = AUCg0.tablePerf1[dataset == "Case subject"],
                                         mapping = aes(label = paste0("\u03C1=",round(correlation,3)), color = dataset, x = 1500, y = 2500))
gg.AUCg.cor1

## ggsave(gg.AUCg.cor1, filename = file.path(path.results,"figure2aReviewer.pdf"), width = 13, device = cairo_pdf)

## * Sanity check
if(FALSE){

    ##  lm approach for AUCi (0-15-30)
    dtTrainW <- dcast(dtLR.HC[sample %in% 1:3], formula = id2+AUCg.pracma+AUCi.pracma+AUCb.pracma~sample, value.var = c("cortisol","time"))
    dtTrainW <- dcast(dtLR.HC[sample %in% 1:3], formula = id2+AUCg.pracma+AUCi.pracma+AUCb.pracma~sample, value.var = c("cortisol","time"))

    dtTrainW[, AUC3.auc := pracma::trapz(x = c(.SD$time_1,.SD$time_2,.SD$time_3),
                                        y = c(.SD$cortisol_1,.SD$cortisol_2,.SD$cortisol_3)),
             by = "id2"]
    dtTrainW[, c("ct_1","ct_2","ct_3") := .(cortisol_1*(time_2-time_1)/2,cortisol_2*((time_2-time_1)/2+(time_3-time_2)/2),cortisol_3*(time_3-time_2)/2)]
    
    e.lm <- lm(AUCg.pracma ~ offset(AUC3.auc) + ct_1 + ct_2 + ct_3, data = dtTrainW)
    ## coef(attr(trainL.AUC,"model")[["1-2-3"]][["lm"]]) - coef(e.lm)

    cor(fitted(e.lm), dtTrainW$AUCg.pracma)
    
    ## 
    setnames(dtTrain,old="id2",new="id")
    dtTest <- dtLR.Case[,.(id2,sample,time,cortisol,AUCg.pracma,AUCi.pracma)]
    setnames(dtTest,old="id2",new="id")

    testL.AUC.bis <- calcAUCgi(data = dtTrain,
                               newdata = dtTest,
                               method = c("auc","lm"),
                               timepoint = list("0-15-30" = c(1,2,3),
                                                "0-15-45" = c(1,2,4),
                                                "0-15-60" = c(1,2,5),
                                                "0-30-45" = c(1,3,4),
                                                "0-30-60" = c(1,3,5),
                                                "0-45-60" = c(1,4,5)),
                               var.timepoint = "sample",
                               var.X = "time",
                               var.Y = "cortisol",
                               var.id = "id",
                               var.truth = c("AUCg.pracma","AUCi.pracma"))
    range(testL.AUC.bis$AUCg.estimate-testL.AUC$AUCg.estimate)
    range(testL.AUC.bis$AUCi.estimate-testL.AUC$AUCi.estimate)
    range(testL.AUC.bis$AUCb.estimate-testL.AUC$AUCb.estimate)
}


######################################################################
### analysis-AUC-cortisol.R ends here
