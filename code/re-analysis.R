### re-analysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep 10 2021 (09:20) 
## Version: 
## Last-Updated: nov 16 2021 (21:21) 
##           By: Brice Ozenne
##     Update #: 88
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
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

library(pracma)

library(ggplot2)
library(ggpubr)
library(xtable)

source(file.path(path.code,"FCT-calcAUCg.R"))
dtTrain <- readRDS(file = file.path(path.results,"input_calcAUCgi.rds"))

##
## Check that the AUC is matching 
## 
calcAUCgi2 <- function(newdata){
    newdata <- copy(as.data.table(newdata))
    if("AUCg.pracma" %in% names(newdata) == FALSE){
        newdata$AUCg.pracma <- NA
        rm.AUCg.pracma <- TRUE
    }else{
        rm.AUCg.pracma <- FALSE
    }
    if("AUCi.pracma" %in% names(newdata) == FALSE){
        newdata$AUCi.pracma <- NA
        rm.AUCi.pracma <- TRUE
    }else{
        rm.AUCi.pracma <- FALSE
    }
    out <- calcAUCgi(data = dtTrain,
                     newdata = newdata,
                     method = c("auc","lm"),
                     timepoint = list(
                         ## "0-15-30" = c(1,2,3),
                         ## "0-15-45" = c(1,2,4),
                         ## "0-15-60" = c(1,2,5),
                         ## "0-30-45" = c(1,3,4),
                         "0-30-60" = c(1,3,5)#,
                         ## "0-45-60" = c(1,4,5)
                     ),
                     var.timepoint = "sample",
                     var.X = "time",
                     var.Y = "cortisol",
                     var.id = "id",
                     var.truth = c("AUCg.pracma","AUCi.pracma"))
    if(rm.AUCg.pracma){
        out$AUCg.pracma <- NULL
    }
    if(rm.AUCi.pracma){
        out$AUCi.pracma <- NULL
    }
    return(out)
}
## * Jakobsen et al 2016

## ** data

## *** import
dfW.Jak00 <- read.csv2(file.path(path.data,"SB_HRRT_CAR_Gustav.csv", dec = "."))
dtW.Jak0 <- as.data.table(dfW.Jak00)
## dfW.Jak00[which(dfW.Jak00$CIMBI.ID %in% c("50678","50524")),c("CIMBI.ID",grep("Time|Seconds",names(dfW.Jak00), value = TRUE))]
## dfW.Jak00[which(dfW.Jak00$CIMBI.ID %in% c("50524")),c("CIMBI.ID",grep("Time|Seconds",names(dfW.Jak00), value = TRUE))]
## table(dtW.Jak0[["Seconds.from.wake.up.to.CAR..1"]],dtW.Jak0[["Seconds.from.wake.up.to.CAR..1.1"]], useNA = "always")


## *** rename columns
old2new <- c("Age.at.SB.scan"="Age",
             "Area..nmol.L.min..under.the.cort..curve.with.respect.to.ground"="AUCg",
             "Area..nmol.L.min..under.the.cort..curve.with.respect.to.increase"="AUCi",
             "Area..nmol.L.min..under.the.full.cort..curve.wrt..ground"="AUCg24h",
             "AUCs.Available"="AUCs.Available",
             "CIMBI.ID"="id",
             "Time.wake.up.on.day.for.cortisol.home.samples"="time.wakeUp",
             "Seconds.from.wake.up.to.CAR..1"="dtimeS.sample1",
             "Cortisol.levels..nmol.L..for.home.sample..1..at.wake.up."="cortisol.sample1",
             "Time.of.cortisol.home.sample.2"="time.sample2",                                  
             "Cortisol.levels..nmol.L..for.home.sample..2..wake.up...15.min."="cortisol.sample2",
             "Time.of.cortisol.home.sample.3"="time.sample3",                                  
             "Cortisol.levels..nmol.L..for.home.sample..3..wake.up...30.min."="cortisol.sample3",
             "Time.of.cortisol.home.sample.4"="time.sample4",                                  
             "Cortisol.levels..nmol.L..for.home.sample..4..wake.up...45.min."="cortisol.sample4",
             "Time.of.cortisol.home.sample.5"="time.sample5",                                  
             "Cortisol.levels..nmol.L..for.home.sample..5..wake.up..60.min."="cortisol.sample5",
             "HighBinding_SB_BPnd_NonPV_GM"="BP.highBinding",
             "Total_hippocampus_SB_BPnd_NonPV_GM"="BP.hippocampus",                              
             "LA.LA"="LA.LA",
             "Length..minutes..of.interval.between.samples.1.and.8"="Length..minutes..of.interval.between.samples.1.and.8",
             "PET.scan.type"="PET.scan.type")
## names(old2new)[names(old2new) %in% names(dfW.Jak00)==FALSE]
setnames(dtW.Jak0, old = names(old2new), new = as.character(old2new))

## *** subset
dtW.Jak <- dtW.Jak0[AUCs.Available== 1 & PET.scan.type== 'Baseline' & id != 50313,.SD,.SDcols = old2new]
dtW.Jak[,id := as.character(id)]

## *** compute times
dtW.Jak[,timeMin.wakeUp := as.numeric(as.difftime(trimws(time.wakeUp),"%H:%M:%S",units = "min"))]
dtW.Jak[,dtimeMin.sample1 := dtimeS.sample1/60]
dtW.Jak[,timeMin.sample2 := as.numeric(as.difftime(trimws(time.sample2),"%H:%M:%S",units = "min"))]
dtW.Jak[,timeMin.sample3 := as.numeric(as.difftime(trimws(time.sample3),"%H:%M:%S",units = "min"))]
dtW.Jak[,timeMin.sample4 := as.numeric(as.difftime(trimws(time.sample4),"%H:%M:%S",units = "min"))]
dtW.Jak[,timeMin.sample5 := as.numeric(as.difftime(trimws(time.sample5),"%H:%M:%S",units = "min"))]

## fix missing values

## PB 1
dtW.Jak[is.na(timeMin.wakeUp),id]
## [1] 50678
dtW.Jak[,timeMinImp.wakeUp := timeMin.wakeUp]
dtW.Jak[is.na(timeMin.wakeUp), timeMinImp.wakeUp := timeMin.sample2 - mean(c(timeMin.sample3-timeMin.sample2,timeMin.sample4-timeMin.sample3,timeMin.sample5-timeMin.sample4))]
dtW.Jak[is.na(timeMin.wakeUp), timeMinImp.wakeUp]
## [1] 360
## dtW.Jak[, timeMinImp.wakeUp]
## dtW.Jak[is.na(timeMin.wakeUp) | is.na(dtimeS.sample1), ]

## PB 2
dtW.Jak[is.na(dtimeMin.sample1),id]
## [1] 50678
dtW.Jak[,dtimeMinImp.sample1 := dtimeMin.sample1]
dtW.Jak[is.na(dtimeMin.sample1), dtimeMinImp.sample1 := mean(15-c(timeMin.sample3-timeMin.sample2,timeMin.sample4-timeMin.sample3,timeMin.sample5-timeMin.sample4))]
dtW.Jak[is.na(dtimeMin.sample1), dtimeMinImp.sample1]
## [1] 0

dtW.Jak[,timeMin.sample1 := timeMinImp.wakeUp + dtimeMinImp.sample1]

## NOTE: dtimeMin.sampleX is the time gap between sample X and wake-up
##     : dtime1Min.sampleX is the time gap between sample X and sample 1
dtW.Jak[,dtime1Min.sample1 := 0]
dtW.Jak[,dtime1Min.sample2 := timeMin.sample2-timeMin.sample1]
dtW.Jak[,dtime1Min.sample3 := timeMin.sample3-timeMin.sample1]
dtW.Jak[,dtime1Min.sample4 := timeMin.sample4-timeMin.sample1]
dtW.Jak[,dtime1Min.sample5 := timeMin.sample5-timeMin.sample1]

## dtW.Jak[id=="50678"]

## *** reshape
dtL.Jak <- melt(dtW.Jak,
                id.vars = c("id","Age","LA.LA","BP.highBinding","BP.hippocampus","AUCg","AUCi"),
                measure=patterns("dtime1Min.","cortisol."),
                variable.name = "sample", value.name = c("time","cortisol"))
sum(is.na(dtL.Jak))
## [1] 0

dtL.Jak[,expectedTime := c(0,15,30,45,60)[sample]]

## *** compute AUC
dtL.Jak[id == "50678"]

dtL.Jak[, AUCg.pracma := pracma::trapz(x=time, y=cortisol), by = "id"]
dtL.Jak[, AUCb.pracma := cortisol[1]*time[5], by = "id"]
dtL.Jak[, AUCi.pracma := AUCg.pracma - AUCb.pracma, by = "id"]

dtL.Jak[,.(AUCg = range(AUCg-AUCg.pracma), AUCi = range(AUCi-AUCi.pracma))]
##             AUCg          AUCi
## 1: -2.273737e-13 -2.273737e-13
## 2:  2.273737e-13  1.847411e-13
## dtL.Jak[,.(GS = AUCg[1], pracma = AUCg.pracma[1], diff = AUCg[1]-AUCg.pracma[1]), by = "id"]


## ** descriptive
ggTime.Jak <- ggplot(dtL.Jak, aes(x=sample,y=time,group=id,color=id))
ggTime.Jak <- ggTime.Jak + geom_abline(slope = 15, intercept = -15, color = "black")
ggTime.Jak <- ggTime.Jak + geom_point() + geom_line()
ggTime.Jak

ggTime2.Jak <- ggplot(dtL.Jak, aes(x=sample,y=expectedTime-time,group=id,color=id))
ggTime2.Jak <- ggTime2.Jak + geom_abline(slope = 0, intercept = 0, color = "black")
ggTime2.Jak <- ggTime2.Jak + geom_point() + geom_line()
ggTime2.Jak

ggCortisol.Jak <- ggplot(dtL.Jak, aes(x=sample,y=cortisol,group=id,color=id))
ggCortisol.Jak <- ggCortisol.Jak + geom_point() + geom_line()
ggCortisol.Jak


## ** compute 3 point AUC
AUC3W.Jak <- copy(calcAUCgi2(dtL.Jak))
AUC3W.Jak[,AUCg.error := AUCg.pracma - AUCg.estimate]
AUC3W.Jak[,AUCi.error := AUCi.pracma - AUCi.estimate]

## visualize error
AUC3L.Jak <- melt(AUC3W.Jak, id.vars = c("id","method","timepoint"),
                  measure = list(c("AUCg.pracma","AUCi.pracma"),c("AUCg.estimate","AUCi.estimate"),c("AUCg.error","AUCi.error")),
                  value.name = c("pracma","estimate","error"), variable.name = "type")   
AUC3L.Jak[,type := as.character(factor(type,levels = 1:2, labels = c("AUCg","AUCi")))]

ggError.Jak <- ggplot(AUC3L.Jak, aes(x = method, y = error))
ggError.Jak <- ggError.Jak + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + facet_wrap(~type)
ggError.Jak

ggError2.Jak <- ggplot(AUC3L.Jak, aes(x = pracma, y = error, color = method))
ggError2.Jak <- ggError2.Jak + geom_point() + geom_smooth() + facet_wrap(~type, ncol = 1)
ggError2.Jak

## AUC3L.Jak[method == "auc" & type=="AUCg", .(abs = range(error), relative = range(100*error/estimate))]
##       abs  relative
## 1: -66.35 -13.61165
## 2: 139.53  17.96794

## bias?
summary(lm(error ~ 1, data = AUC3L.Jak[method == "auc" & type=="AUCg"]))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   20.983      9.421   2.227   0.0339 *
summary(lm(error ~ estimate, data = AUC3L.Jak[method == "auc" & type=="AUCg"]))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -4.98126   25.17659  -0.198    0.845
## estimate     0.03264    0.02937   1.111    0.276
summary(lm(error ~ estimate, data = AUC3L.Jak[method == "auc" & type=="AUCi"]))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 12.39183    8.39780   1.476  0.15121   
## estimate     0.10642    0.03073   3.463  0.00174 **
t.test(AUC3L.Jak[method == "auc" & type=="AUCg",estimate],AUC3L.Jak[method == "lm" & type=="AUCg",estimate])
## t = -0.10199, df = 58, p-value = 0.9191

## double check
range(AUC3L.Jak[method=="auc"&type=="AUCg",estimate]-dtL.Jak[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol), by = "id"][[2]])
range(AUC3L.Jak[method=="auc"&type=="AUCi",estimate]-dtL.Jak[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol) - time[3]*cortisol[1], by = "id"][[2]])

## ggplot(perfAUCg.AUC, aes(x=GS,y=diff)) + geom_point()

## save
dtW.Jak$AUCi.estimateLM <- AUC3W.Jak[method=="lm",AUCi.estimate]
dtW.Jak$AUCi.estimateAUC <- AUC3W.Jak[method=="auc",AUCi.estimate]

## ** reproduce analysis
## Pallidostriatum
lm8 <- lm(AUCi ~  LA.LA + Age  + BP.highBinding, data=dtW.Jak)
## Expected: -328 [-582; -75] 0.01(0.04)
summary(lm8)$coef["BP.highBinding",,drop=FALSE]
##                 Estimate Std. Error   t value   Pr(>|t|)
## BP.highBinding -328.2406   123.2851 -2.662452 0.01313013
lm8.estimateAUC <- lm(AUCi.estimateAUC ~  LA.LA + Age  + BP.highBinding, data=dtW.Jak)
summary(lm8.estimateAUC)$coef["BP.highBinding",,drop=FALSE]
##                 Estimate Std. Error   t value   Pr(>|t|)
## BP.highBinding -284.2505   108.1232 -2.628951 0.01418977
lm8.estimateLM <- lm(AUCi.estimateLM ~  LA.LA + Age  + BP.highBinding, data=dtW.Jak)
summary(lm8.estimateLM)$coef["BP.highBinding",,drop=FALSE]
##                 Estimate Std. Error   t value   Pr(>|t|)
## BP.highBinding -289.4799   108.8211 -2.660146 0.01320065

table.PalJak <- cbind(rbind("AUC with 5 samples" = summary(lm8)$coef["BP.highBinding",c("Estimate","Std. Error","Pr(>|t|)")],
                            "AUC with 3 samples" = summary(lm8.estimateAUC)$coef["BP.highBinding",c("Estimate","Std. Error","Pr(>|t|)")],
                            "LM with 3 samples" = summary(lm8.estimateLM)$coef["BP.highBinding",c("Estimate","Std. Error","Pr(>|t|)")]),
                      rbind(confint(lm8)["BP.highBinding",],confint(lm8.estimateAUC)["BP.highBinding",],confint(lm8.estimateLM)["BP.highBinding",])
                      )
xtable(table.PalJak, digits = 3)

## hippocampus
lm38 <- lm(AUCi ~  LA.LA + Age  + BP.hippocampus, data=dtW.Jak)
## Expected: -328 [-582; -75] 0.01(0.04)
summary(lm38)$coef["BP.hippocampus",,drop=FALSE]
##                 Estimate Std. Error    t value  Pr(>|t|)
## BP.hippocampus -334.6063   411.0139 -0.8140997 0.4229817
lm38.estimateAUC <- lm(AUCi.estimateAUC ~  LA.LA + Age  + BP.hippocampus, data=dtW.Jak)
summary(lm38.estimateAUC)$coef["BP.hippocampus",,drop=FALSE]
##                 Estimate Std. Error   t value  Pr(>|t|)
## BP.hippocampus -315.3216   358.7619 -0.878916 0.3874977
lm38.estimateLM <- lm(AUCi.estimateLM ~  LA.LA + Age  + BP.hippocampus, data=dtW.Jak)
summary(lm38.estimateLM)$coef["BP.hippocampus",,drop=FALSE]
##                 Estimate Std. Error    t value  Pr(>|t|)
## BP.hippocampus -334.7316    361.406 -0.9261926 0.3628638


table.HipJak <- cbind(rbind("AUC with 5 samples" = summary(lm38)$coef["BP.hippocampus",c("Estimate","Std. Error","Pr(>|t|)")],
                            "AUC with 3 samples" = summary(lm38.estimateAUC)$coef["BP.hippocampus",c("Estimate","Std. Error","Pr(>|t|)")],
                            "LM with 3 samples" = summary(lm38.estimateLM)$coef["BP.hippocampus",c("Estimate","Std. Error","Pr(>|t|)")]),
                      rbind(confint(lm38)["BP.hippocampus",],confint(lm38.estimateAUC)["BP.hippocampus",],confint(lm38.estimateLM)["BP.hippocampus",])
                      )
xtable(table.HipJak, digits = 3)


## * Frokjaer et al 2014
library(xlsx)

## ** data

## *** load main dataset
keep.cols0 <- c("AUCi_notcorrected" = "AUCi",
                "AUCgFullday" = "AUCg",
                "FrontCort_DASB_BPnd_NonPV_GM" = "DASB",
                "MDMAuserVsNot" = "MDMA",
                "Age.at.DASB.scan" = "Age")
dfW.Fro <- read.csv2(file.path(path.data,"Klaus-MDMA_N18ogN32_final.csv"), dec = ".", sep = ",", header = TRUE)
dtW.Fro <- as.data.table(dfW.Fro[,c("CIMBI.ID",names(keep.cols0))])


## *** add CAR values over time
keep.cols1 <-  c("Time.of.cortisol.home.sample..1" = "time.sample1",
                "Cortisol.levels..nmol.L..for.home.sample..1..at.wake.up." = "cortisol.sample1",
                "Time.of.cortisol.home.sample..2" = "time.sample2",
                "Cortisol.levels..nmol.L..for.home.sample..2..wake.up...15.min." = "cortisol.sample2",
                "Time.of.cortisol.home.sample..3" = "time.sample3",
                "Cortisol.levels..nmol.L..for.home.sample..3..wake.up...30.min." = "cortisol.sample3",
                "Time.of.cortisol.home.sample..4" = "time.sample4",
                "Cortisol.levels..nmol.L..for.home.sample..4..wake.up...45.min." = "cortisol.sample4",
                "Time.of.cortisol.home.sample..5" = "time.sample5",
                "Cortisol.levels..nmol.L..for.home.sample..5..wake.up..60.min." = "cortisol.sample5")
## names(keep.cols1) %in% names(dfW.FroControl)

dfW.FroCase <- read.xlsx(file.path(path.data,"Vibe_SERT_Cort_MDMAcase.xls"), sheetName = "VIBE_SERT_CORT_MDMACASE")
dtW.FroCase <- as.data.table(dfW.FroCase[!is.na(dfW.FroCase$CIMBI.ID),c("CIMBI.ID",names(keep.cols1))])
## intersect(dfW.FroCase$CIMBI.ID,dfW.Fro$CIMBI.ID)

dfW.FroControl <- read.xlsx(file.path(path.data,"DASB_Cort_data-dec2010_fullN32ControlstilMDMA_CAR.xls"), sheetName = "VIBE_DASB_CORT_HealthyN=32")
dtW.FroControl <- as.data.table(dfW.FroControl[!is.na(dfW.FroControl$CIMBI.ID),c("CIMBI.ID",names(keep.cols1))])
## intersect(dfW.FroControl$CIMBI.ID,dfW.FroControl$CIMBI.ID)


dtW.FroT <- merge(x=dtW.Fro,y=rbind(dtW.FroCase,dtW.FroControl), by = "CIMBI.ID")

## dfW.FroCase[dfW.FroCase$CIMBI.ID == "11083",grep("level",colnames(dfW.FroCase),value = TRUE)]
## dfW.FroControl[dfW.FroControl$CIMBI.ID == "11101",grep("level",colnames(dfW.FroControl),value = TRUE)]

## 1: 11083  956.820               NA            11.29
## 2: 11101 -182.475            10.15               NA


## *** rename columns
setnames(dtW.FroT,
         old = c("CIMBI.ID",names(keep.cols0), names(keep.cols1)),
         new = c("id",keep.cols0,keep.cols1),
         )


## *** compute time difference
## dtW.FroT$time.sample1
dtW.FroT[,time.sample1 := as.numeric(as.difftime(trimws(time.sample1),"%H:%M:%S",units = "min"))]
dtW.FroT[,time.sample2 := as.numeric(as.difftime(trimws(time.sample2),"%H:%M:%S",units = "min"))]
dtW.FroT[,time.sample3 := as.numeric(as.difftime(trimws(time.sample3),"%H:%M:%S",units = "min"))]
dtW.FroT[,time.sample4 := as.numeric(as.difftime(trimws(time.sample4),"%H:%M:%S",units = "min"))]
dtW.FroT[,time.sample5 := as.numeric(as.difftime(trimws(time.sample5),"%H:%M:%S",units = "min"))]

dtW.FroT[,dtime.sample1 := 0]
dtW.FroT[,dtime.sample2 := time.sample2-time.sample1]
dtW.FroT[,dtime.sample3 := time.sample3-time.sample1]
dtW.FroT[,dtime.sample4 := time.sample4-time.sample1]
dtW.FroT[,dtime.sample5 := time.sample5-time.sample1]

## *** reshape
dtL.FroT <- melt(dtW.FroT,
                 id.vars = c("id","Age","DASB","MDMA","AUCi","AUCg"),
                 measure=patterns("dtime.","cortisol."),
                 variable.name = "sample", value.name = c("time","cortisol"))
sum(is.na(dtL.FroT))
## [1] 2
## dtL.FroT[rowSums(is.na(dtL.FroT)),]
## dtW.FroT[which(rowSums(is.na(dtW.FroT))==1),.(id,AUCi,cortisol.sample4,cortisol.sample5)]
##       id     AUCi cortisol.sample4 cortisol.sample5
## 1: 11083  956.820               NA            11.29
## 2: 11101 -182.475            10.15               NA

dtL.FroT[,expectedTime := c(0,15,30,45,60)[sample]]
dtL.FroT[,id := as.character(id)]

## remove missing value
dtL.FroT <- na.omit(dtL.FroT)

## *** compute AUC
dtL.FroT[, AUCg.pracma := pracma::trapz(x=time, y=cortisol), by = "id"]
dtL.FroT[, AUCb.pracma := cortisol[1]*time[length(time)], by = "id"]
## dtL.FroT[, AUCb.pracma := cortisol[1]*time[5], by = "id"]
dtL.FroT[, AUCi.pracma := AUCg.pracma - AUCb.pracma, by = "id"]

dtL.FroT[,.(AUCi = range(AUCi-AUCi.pracma, na.rm=TRUE), AUCg = range(AUCg-AUCg.pracma, na.rm=TRUE))]
##             AUCi     AUCg
## 1: -2.842171e-13   395.78
## 2:  3.410605e-13 10807.00



## ** descriptive
ggTime.Fro <- ggplot(dtL.FroT, aes(x=sample,y=time,group=id,color=id))
ggTime.Fro <- ggTime.Fro + geom_abline(slope = 15, intercept = -15, color = "black")
ggTime.Fro <- ggTime.Fro + geom_point() + geom_line()
ggTime.Fro

ggTime2.Fro <- ggplot(dtL.FroT, aes(x=sample,y=expectedTime-time,group=id,color=id))
ggTime2.Fro <- ggTime2.Fro + geom_abline(slope = 0, intercept = 0, color = "black")
ggTime2.Fro <- ggTime2.Fro + geom_point() + geom_line()
ggTime2.Fro

ggCortisol.Fro <- ggplot(dtL.FroT, aes(x=sample,y=cortisol,group=id,color=id))
ggCortisol.Fro <- ggCortisol.Fro + geom_point() + geom_line()
ggCortisol.Fro



## ** compute 3 point AUC
AUC3W.FroT <- copy(calcAUCgi2(dtL.FroT))
AUC3W.FroT[,AUCg.error := AUCg.pracma - AUCg.estimate]
AUC3W.FroT[,AUCi.error := AUCi.pracma - AUCi.estimate]

## visualize error
AUC3L.FroT <- melt(AUC3W.FroT, id.vars = c("id","method","timepoint"),
                   measure = list(c("AUCg.pracma","AUCi.pracma"),c("AUCg.estimate","AUCi.estimate"),c("AUCg.error","AUCi.error")),
                   value.name = c("pracma","estimate","error"), variable.name = "type")
     
AUC3L.FroT[,type := as.character(factor(type,levels = 1:2, labels = c("AUCg","AUCi")))]

ggError.Fro <- ggplot(AUC3L.FroT, aes(x = method, y = error))
ggError.Fro <- ggError.Fro + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + facet_wrap(~type)
ggError.Fro

ggError2.Fro <- ggplot(AUC3L.FroT, aes(x = pracma, y = error, color = method))
ggError2.Fro <- ggError2.Fro + geom_point() + geom_smooth() + facet_wrap(~type, ncol = 1)
ggError2.Fro

## AUC3L.FroT[method == "auc" & type=="AUCg", .(abs = range(error), relative = range(100*error/estimate))]
##       abs  relative
## 1: -501.0 -38.21510
## 2:  322.4  54.97016
## AUC3L.FroT[method == "auc" & type=="AUCg" & abs(error)>200, id]
## AUC3L.FroT[method == "auc" & type=="AUCg" & id %in% c("10852","10909","11041","11131") == FALSE, .(abs = range(error), relative = range(100*error/estimate))]
##        abs  relative
## 1: -141.60 -24.32990
## 2:  123.75  51.06667


## bias?
summary(lm(error ~ 1, data = AUC3L.FroT[method == "auc" & type=="AUCg"]))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    16.77      16.16   1.038    0.304
t.test(AUC3L.FroT[method == "auc" & type=="AUCg",estimate],AUC3L.FroT[method == "lm" & type=="AUCg",estimate])
## t = -0.16573, df = 96.937, p-value = 0.8687

## double check
range(AUC3L.FroT[method=="auc"&type=="AUCg",estimate]-dtL.FroT[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol), by = "id"][[2]], na.rm=TRUE)
range(AUC3L.FroT[method=="auc"&type=="AUCi",estimate]-dtL.FroT[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol) - time[3]*cortisol[1], by = "id"][[2]], na.rm=TRUE)

## save
dtW.FroT$AUCi.estimateLM <- AUC3W.FroT[method=="lm",AUCi.estimate]
dtW.FroT$AUCi.estimateAUC <- AUC3W.FroT[method=="auc",AUCi.estimate]

## ** primary model
e.lm <- lm(AUCi ~ DASB + MDMA + Age, data= dtW.FroT)
## EXPECTED: 1716 [520;2912] 0.006
summary(e.lm)$coef["DASB",,drop=FALSE]
##      Estimate Std. Error t value    Pr(>|t|)
## DASB 1715.996   594.2885 2.88748 0.005899568
confint(e.lm)
## DASB          519.755201 2912.23650

## lrob <- glm(AUCi ~ DASB + MDMA +Age, data=d)
## estimate(lrob)

e.lm.estimateAUC <- lm(AUCi.estimateAUC ~ DASB + MDMA +Age, data= dtW.FroT)
summary(e.lm.estimateAUC)$coef["DASB",,drop=FALSE]
##      Estimate Std. Error  t value   Pr(>|t|)
## DASB 1676.618   646.6305 2.592854 0.01271938
e.lm.estimateLM <- lm(AUCi.estimateLM ~ DASB + MDMA +Age, data= dtW.FroT)
summary(e.lm.estimateLM)$coef["DASB",,drop=FALSE]
##      Estimate Std. Error  t value   Pr(>|t|)
## DASB 1716.492   656.8608 2.613174 0.01215416
table.FroT <- cbind(rbind("AUC with 5 samples" = summary(e.lm)$coef["DASB",c("Estimate","Std. Error","Pr(>|t|)")],
                          "AUC with 3 samples" = summary(e.lm.estimateAUC)$coef["DASB",c("Estimate","Std. Error","Pr(>|t|)")],
                          "LM with 3 samples" = summary(e.lm.estimateLM)$coef["DASB",c("Estimate","Std. Error","Pr(>|t|)")]),
                    rbind(confint(e.lm)["DASB",],confint(e.lm.estimateAUC)["DASB",],confint(e.lm.estimateLM)["DASB",])
                    )
xtable(table.FroT, digits = 3)

## * export
ggsave(ggarrange(ggTime.Jak, ggCortisol.Jak, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-jak-descriptive.pdf"), width = 10)
ggsave(ggarrange(ggError.Jak, ggError2.Jak, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-jak-errorAUC.pdf"), width = 10)
ggsave(ggarrange(ggTime.Fro, ggCortisol.Fro, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-fro-descriptive.pdf"), width = 10)
ggsave(ggarrange(ggError.Fro, ggError2.Fro, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-fro-errorAUC.pdf"), width = 10)

saveRDS(list(Jak2016_pal = table.PalJak,
             Jak2016_hip = table.HipJak,
             Fro2014 = table.FroT),
        file = file.path(path.results,"ls-table3.rds"))

##----------------------------------------------------------------------
### re-analysis.R ends here
