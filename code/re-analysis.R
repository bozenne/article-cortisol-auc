### re-analysis.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: sep 10 2021 (09:20) 
## Version: 
## Last-Updated: jan 31 2022 (15:48) 
##           By: Brice Ozenne
##     Update #: 112
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
### Code:


## * path
if(Sys.info()["login"] == "hpl802"){
    path <- "~/Github/article-cortisol-auc/"
}else{
    path <- "C:/Users/arafatn/Documents/Github/article-cortisol-auc" ## put path to Github directory
}
path.code <- file.path(path,"code")
path.data <- file.path(path,"data")
path.report <- file.path(path,"report")
path.results <- file.path(path,"results")


## * library
library(readxl)
library(data.table)
library(pracma)
library(ggplot2)
library(ggpubr)
library(xtable)
library(plyr)
library(lubridate)
library(nlme)

source(file.path(path.code,"FCT-calcAUCg.R"))
dtTrain <- readRDS(file = file.path(path.results,"input_calcAUCgi.rds"))

##
## Fonction used to compute the 3-point AUC with trapezoidal rule ("auc") or linear regression ("lm")
## 
calcAUCgi2 <- function(newdata, timepoint = c(1,3,5)){
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
    ls.timepoint <- list(timepoint)
    names(ls.timepoint) <- paste(c("0","15","30","45","60")[timepoint], collapse="-")
    
    out <- calcAUCgi(data = dtTrain,
                     newdata = newdata,
                     method = c("auc","lm"),
                     timepoint = ls.timepoint,
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

#### Jakobsen et al 2016 ####
## * Jakobsen et al 2016

## ** data
dtL.Jak <- as.data.table(read.csv(file = file.path(path.data,"Jakobsen_2016-process_dataL.csv", dec = ".")))
dtW.Jak <- as.data.table(read.csv(file = file.path(path.data,"Jakobsen_2016-process_dataW.csv", dec = ".")))

## ** compute 5-points AUC
## dtL.Jak[id == "50678"]

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


## ** compute 3-points AUC
AUC3W.Jak <- copy(calcAUCgi2(dtL.Jak))
AUC3W.Jak[,AUCg.error := AUCg.pracma - AUCg.estimate]
AUC3W.Jak[,AUCi.error := AUCi.pracma - AUCi.estimate]

## save
dtW.Jak$AUCi.estimateLM <- AUC3W.Jak[method=="lm",AUCi.estimate]
dtW.Jak$AUCi.estimateAUC <- AUC3W.Jak[method=="auc",AUCi.estimate]

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
summary(lm(error ~ 1, data = AUC3L.Jak[method == "auc" & type=="AUCi"]))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)   20.983      9.421   2.227   0.0339 *
summary(lm(error ~ estimate, data = AUC3L.Jak[method == "auc" & type=="AUCi"]))
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 12.39183    8.39780   1.476  0.15121   
## estimate     0.10642    0.03073   3.463  0.00174 **
t.test(AUC3L.Jak[method == "auc" & type=="AUCg",estimate],AUC3L.Jak[method == "lm" & type=="AUCg",estimate],
       paired = TRUE)
## boxplot(AUC3L.Jak[method == "auc" & type=="AUCg",estimate],AUC3L.Jak[method == "lm" & type=="AUCg",estimate])

## double check
range(AUC3L.Jak[method=="auc"&type=="AUCg",estimate]-dtL.Jak[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol), by = "id"][[2]])
range(AUC3L.Jak[method=="auc"&type=="AUCi",estimate]-dtL.Jak[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol) - time[3]*cortisol[1], by = "id"][[2]])

## ggplot(perfAUCg.AUC, aes(x=GS,y=diff)) + geom_point()


## ** reproduce analysis
## Pallidostriatum
lm8 <- lm(AUCi ~  LA.LA + Age  + BP.highBinding, data=dtW.Jak)
## Expected: -328 [-582; -75] 0.01(0.04)
summary(lm8)$coef["BP.highBinding",,drop=FALSE]
##                 Estimate Std. Error   t value   Pr(>|t|)
## BP.highBinding -328.2406   123.2851 -2.662452 0.01313013
lm8.estimateAUC <- lm(AUCi.estimateAUC ~  LA.LA + Age  + BP.highBinding, 
                      data=dtW.Jak)
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

#### Frokjaer et al 2014 ####
## * Frokjaer et al 2014

## ** data
dtL.FroT <- as.data.table(read.csv(file.path(path.data,"Frokjaer_2014-process_dataL.csv", dec = ".")))
dtW.FroT <- as.data.table(read.csv(file.path(path.data,"Frokjaer_2014-process_dataW.csv", dec = ".")))

## ** compute 5-points AUC
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

## ** compute 3-points AUC
AUC3W.FroT <- copy(calcAUCgi2(dtL.FroT))
AUC3W.FroT[,AUCg.error := AUCg.pracma - AUCg.estimate]
AUC3W.FroT[,AUCi.error := AUCi.pracma - AUCi.estimate]

## save
dtW.FroT$AUCi.estimateLM <- AUC3W.FroT[method=="lm",AUCi.estimate]
dtW.FroT$AUCi.estimateAUC <- AUC3W.FroT[method=="auc",AUCi.estimate]

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
summary(lm(error ~ 1, data = AUC3L.FroT[method == "auc" & type=="AUCi"]))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    13.47      16.52   0.816    0.419
summary(lm(error ~ 1, data = AUC3L.FroT[method == "lm" & type=="AUCi"]))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    8.532     16.402    0.52    0.605

t.test(AUC3L.FroT[method == "auc" & type=="AUCg",estimate],AUC3L.FroT[method == "lm" & type=="AUCg",estimate],
       paired = TRUE)
## 	Paired t-test

## data:  AUC3L.FroT[method == "auc" & type == "AUCg", estimate] and AUC3L.FroT[method == "lm" & type == "AUCg", estimate]
## t = -11.969, df = 48, p-value = 5.134e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -9.781890 -6.968032
## sample estimates:
## mean of the differences 
##               -8.374961 

## double check
range(AUC3L.FroT[method=="auc"&type=="AUCg",estimate]-dtL.FroT[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol), by = "id"][[2]], na.rm=TRUE)
range(AUC3L.FroT[method=="auc"&type=="AUCi",estimate]-dtL.FroT[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol) - time[3]*cortisol[1], by = "id"][[2]], na.rm=TRUE)


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

#### Hogsted et al 2021 ####
## * Hogsted et al 2021
## ** data
dtL.Hog <- as.data.table(read.csv(file.path(path.data,"Hogsted_2021-process_dataL.csv", dec = ".")))
dtW.Hog <- as.data.table(read.csv(file.path(path.data,"Hogsted_2021-process_dataW.csv", dec = ".")))

## ** compute 5-points AUC
dtL.Hog[, AUCg.pracma := pracma::trapz(x=time, y=cortisol), by = "id"]
dtL.Hog[, AUCb.pracma := cortisol[1]*time[length(time)], by = "id"]
## dtL.Hog[, AUCb.pracma := cortisol[1]*time[5], by = "id"]
dtL.Hog[, AUCi.pracma := AUCg.pracma - AUCb.pracma, by = "id"]

dtL.Hog[,.(AUCi = range(AUC.increase-AUCi.pracma, na.rm=TRUE))]
##             AUCi
## 1: -3.410605e-13
## 2:  3.410605e-13

## ** descriptive
ggTime.Hog <- ggplot(dtL.Hog, aes(x=sample,y=time,group=id,color=id))
ggTime.Hog <- ggTime.Hog + geom_abline(slope = 15, intercept = -15, color = "black")
ggTime.Hog <- ggTime.Hog + geom_point() + geom_line()
ggTime.Hog

ggTime2.Hog <- ggplot(dtL.Hog, aes(x=sample,y=expectedTime-time,group=id,color=id))
ggTime2.Hog <- ggTime2.Hog + geom_abline(slope = 0, intercept = 0, color = "black")
ggTime2.Hog <- ggTime2.Hog + geom_point() + geom_line()
ggTime2.Hog

ggCortisol.Hog <- ggplot(dtL.Hog, aes(x=sample,y=cortisol,group=id,color=id))
ggCortisol.Hog <- ggCortisol.Hog + geom_point() + geom_line()
ggCortisol.Hog

## ** compute 3-points AUC
AUC3W.Hog <- copy(calcAUCgi2(dtL.Hog, timepoint = c(1,3,5)))
AUC3W.Hog[,AUCg.error := AUCg.pracma - AUCg.estimate]
AUC3W.Hog[,AUCi.error := AUCi.pracma - AUCi.estimate]

## save
dtW.Hog$AUCi.estimateLM <- AUC3W.Hog[method=="lm",AUCi.estimate]
dtW.Hog$AUCi.estimateAUC <- AUC3W.Hog[method=="auc",AUCi.estimate]

## visualize error
AUC3L.Hog <- melt(AUC3W.Hog, id.vars = c("id","method","timepoint"),
                   measure = list(c("AUCg.pracma","AUCi.pracma"),c("AUCg.estimate","AUCi.estimate"),c("AUCg.error","AUCi.error")),
                   value.name = c("pracma","estimate","error"), variable.name = "type")
     
AUC3L.Hog[,type := as.character(factor(type,levels = 1:2, labels = c("AUCg","AUCi")))]

ggError.Fro <- ggplot(AUC3L.Hog, aes(x = method, y = error))
ggError.Fro <- ggError.Fro + geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) + facet_wrap(~type)
ggError.Fro

ggError2.Fro <- ggplot(AUC3L.Hog, aes(x = pracma, y = error, color = method))
ggError2.Fro <- ggError2.Fro + geom_point() + geom_smooth() + facet_wrap(~type, ncol = 1)
ggError2.Fro

## AUC3L.Hog[method == "auc" & type=="AUCg", .(abs = range(error), relative = range(100*error/estimate))]
##        abs  relative
## 1: -284.25 -27.98641
## 2:  487.50  72.22222


## bias?
summary(lm(error ~ 1, data = AUC3L.Hog[method == "auc" & type=="AUCg"]))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    6.447      9.592   0.672    0.504
summary(lm(error ~ 1, data = AUC3L.Hog[method == "auc" & type=="AUCi"]))
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)    4.139      8.276     0.5    0.618
summary(lm(error ~ 1, data = AUC3L.Hog[method == "lm" & type=="AUCi"]))
##             Estimate Std. Error t value Pr(>|t|)  
## (Intercept)  -12.109      7.235  -1.674   0.0983 .

t.test(AUC3L.Hog[method == "auc" & type=="AUCg",estimate],AUC3L.Hog[method == "lm" & type=="AUCg",estimate],
       paired = TRUE)
## 	Paired t-test

## data:  AUC3L.Hog[method == "auc" & type == "AUCg", estimate] and AUC3L.Hog[method == "lm" & type == "AUCg", estimate]
## t = -12.949, df = 76, p-value < 2.2e-16
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -14.20186 -10.41561
## sample estimates:
## mean of the differences 
##               -12.30874 

## double check
range(AUC3L.Hog[method=="auc"&type=="AUCg",estimate]-dtL.Hog[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol), by = "id"][[2]], na.rm=TRUE)
range(AUC3L.Hog[method=="auc"&type=="AUCi",estimate]-dtL.Hog[sample %in% c(1,3,5), pracma::trapz(x=time, y=cortisol) - time[3]*cortisol[1], by = "id"][[2]], na.rm=TRUE)


## ** primary model

## *** MODEL A TABLE 2 (GLS model CAR no adjustments)
e.gls1 <- gls(AUC.increase ~ OC.use, data = dtW.Hog, weight = varIdent(form=~1|OC.use))
summary(e.gls1)
##                   Value Std.Error   t-value p-value
## (Intercept)    260.6070  47.22868  5.517982   0e+00
## OC.useOC.user -228.3758  64.07765 -3.564048   6e-04
confint(e.gls1)
##                   2.5 %    97.5 %
## (Intercept)    168.0405  353.1735
## OC.useOC.user -353.9657 -102.7859

e.gls1.estimateAUC <- gls(AUCi.estimateAUC ~ OC.use, data = dtW.Hog, weight = varIdent(form=~1|OC.use))
summary(e.gls1.estimateAUC)
##                   Value Std.Error   t-value p-value
## (Intercept)    257.3732  44.10695  5.835208   0e+00
## OC.useOC.user -231.2008  62.46192 -3.701468   4e-04
confint(e.gls1.estimateAUC)
##                   2.5 %    97.5 %
## (Intercept)    170.9252  343.8212
## OC.useOC.user -353.6239 -108.7777

## WARNING: complete case analysis here 
e.gls1.estimateLM <- gls(AUCi.estimateLM ~ OC.use, data = dtW.Hog, weight = varIdent(form=~1|OC.use), na.action = na.omit)
summary(e.gls1.estimateLM)
##                   Value Std.Error   t-value p-value
## (Intercept)    269.4270  45.89051  5.871082   0e+00
## OC.useOC.user -232.7205  64.42681 -3.612169   5e-04
confint(e.gls1.estimateLM)
##                   2.5 %    97.5 %
## (Intercept)    179.4832  359.3707
## OC.useOC.user -358.9948 -106.4463

## *** MODEL B TABLE 2 (GLS model CAR)
e.gls2 <- gls(AUC.increase ~ OC.use + Age + wstatus + BMI, data = dtW.Hog, weight = varIdent(form=~1|OC.use))
summary(e.gls2)
##                      Value Std.Error    t-value p-value
## (Intercept)      -44.88349  367.4974 -0.1221328  0.9031
## OC.useOC.user   -203.24329   71.4228 -2.8456366  0.0057
## Age               -9.73147    8.4041 -1.1579448  0.2507
## wstatusWork.day    0.17965   67.2962  0.0026696  0.9979
## BMI               23.69287   14.6592  1.6162445  0.1104
confint(e.gls2)
##                       2.5 %    97.5 %
## (Intercept)     -765.165111 675.39813
## OC.useOC.user   -343.229369 -63.25720
## Age              -26.203177   6.74024
## wstatusWork.day -131.718430 132.07774
## BMI               -5.038657  52.42439

e.gls2.estimateAUC <- gls(AUCi.estimateAUC ~ OC.use + Age + wstatus + BMI, data = dtW.Hog, weight = varIdent(form=~1|OC.use))
summary(e.gls2.estimateAUC)
##                      Value Std.Error   t-value p-value
## (Intercept)     -143.33384  348.6780 -0.411078  0.6822
## OC.useOC.user   -190.51156   69.1221 -2.756159  0.0074
## Age               -8.48362    7.9004 -1.073821  0.2864
## wstatusWork.day   34.05100   66.0334  0.515663  0.6076
## BMI               25.45714   13.9323  1.827209  0.0718
confint(e.gls2.estimateAUC)
##                       2.5 %     97.5 %
## (Intercept)     -826.730083 540.062408
## OC.useOC.user   -325.988450 -55.034679
## Age              -23.968137   7.000892
## wstatusWork.day  -95.372169 163.474179
## BMI               -1.849579  52.763852

e.gls2.estimateLM <- gls(AUCi.estimateLM ~ OC.use + Age + wstatus + BMI, data = dtW.Hog, weight = varIdent(form=~1|OC.use), na.action = na.omit)
summary(e.gls2.estimateLM)
##                      Value Std.Error    t-value p-value
## (Intercept)     -217.61364  376.1933 -0.5784623  0.5648
## OC.useOC.user   -181.17626   72.8362 -2.4874476  0.0152
## Age               -7.95552    8.1560 -0.9754185  0.3326
## wstatusWork.day   41.65951   68.3909  0.6091382  0.5444
## BMI               28.25589   14.6981  1.9224118  0.0585
confint(e.gls2.estimateLM)
##                        2.5 %    97.5 %
## (Intercept)     -954.9388776 519.71161
## OC.useOC.user   -323.9326050 -38.41991
## Age              -23.9410014   8.02996
## wstatusWork.day  -92.3841880 175.70321
## BMI               -0.5519472  57.06372


table.Hog <- cbind(rbind("AUC with 5 samples" = summary(e.gls2)$tTable["OC.useOC.user",c("Value","Std.Error","p-value")],
                         "AUC with 3 samples" = summary(e.gls2.estimateAUC)$tTable["OC.useOC.user",c("Value","Std.Error","p-value")],
                         "LM with 3 samples" = summary(e.gls2.estimateLM)$tTable["OC.useOC.user",c("Value","Std.Error","p-value")]),
                   rbind(confint(e.gls2)["OC.useOC.user",],confint(e.gls2.estimateAUC)["OC.useOC.user",],confint(e.gls2.estimateLM)["OC.useOC.user",])
                   )
xtable(table.Hog, digits = 3)

## * export
if(FALSE){
    ggsave(ggarrange(ggTime.Jak, ggCortisol.Jak, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-jak-descriptive.pdf"), width = 10)
    ggsave(ggarrange(ggError.Jak, ggError2.Jak, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-jak-errorAUC.pdf"), width = 10)
    ggsave(ggarrange(ggTime.Fro, ggCortisol.Fro, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-fro-descriptive.pdf"), width = 10)
    ggsave(ggarrange(ggError.Fro, ggError2.Fro, common.legend = TRUE, legend = "bottom"), filename = file.path(path.report,"figures","gg-fro-errorAUC.pdf"), width = 10)

    saveRDS(list(Jak2016_pal = table.PalJak,
                 Jak2016_hip = table.HipJak,
                 Fro2014 = table.FroT,
                 Hog2021 = table.Hog),
            file = file.path(path.results,"ls-table3.rds"))
}


##----------------------------------------------------------------------
### re-analysis.R ends here
