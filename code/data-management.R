### data-management.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: jan 25 2022 (13:31) 
## Version: 
## Last-Updated: jan 25 2022 (15:59) 
##           By: Brice Ozenne
##     Update #: 12
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

## * path
if(Sys.info()["login"] == "hpl802"){
    path <- "~/Github/article-cortisol-auc/"
}else{
    path <- "C:/Users/arafatn/Documents/Github/article-cortisol-auc" ## put path to Github directory
}
path.code <- file.path(path,"code")
path.source <- file.path(path,"source")
path.data <- file.path(path,"data")
path.report <- file.path(path,"report")
path.results <- file.path(path,"results")

## * library
library(readxl)
library(data.table)
library(plyr)
library(lubridate)
library(xlsx)


#### Jakobsen et al 2016 ####
## * Jakobsen et al 2016
## ** import
dfW.Jak00 <- read.csv2(file.path(path.source,"SB_HRRT_CAR_Gustav.csv", dec = "."))
dtW.Jak0 <- as.data.table(dfW.Jak00)
## dfW.Jak00[which(dfW.Jak00$CIMBI.ID %in% c("50678","50524")),c("CIMBI.ID",grep("Time|Seconds",names(dfW.Jak00), value = TRUE))]
## dfW.Jak00[which(dfW.Jak00$CIMBI.ID %in% c("50524")),c("CIMBI.ID",grep("Time|Seconds",names(dfW.Jak00), value = TRUE))]
## table(dtW.Jak0[["Seconds.from.wake.up.to.CAR..1"]],dtW.Jak0[["Seconds.from.wake.up.to.CAR..1.1"]], useNA = "always")


## ** rename columns
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

## ** subset
dtW.Jak <- dtW.Jak0[AUCs.Available== 1 & PET.scan.type== 'Baseline' & id != 50313,.SD,.SDcols = old2new]
dtW.Jak[,id := as.character(id)]

## ** compute times
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

## ** reshape
dtL.Jak <- melt(dtW.Jak,
                id.vars = c("id","Age","LA.LA","BP.highBinding","BP.hippocampus","AUCg","AUCi"),
                measure=patterns("dtime1Min.","cortisol."),
                variable.name = "sample", value.name = c("time","cortisol"))
sum(is.na(dtL.Jak))
## [1] 0

dtL.Jak[,expectedTime := c(0,15,30,45,60)[sample]]

## ** export
write.csv(x = dtL.Jak, file = file.path(path.data,"Jakobsen_2016-process_dataL.csv", dec = "."))
write.csv(x = dtW.Jak, file = file.path(path.data,"Jakobsen_2016-process_dataW.csv", dec = "."))

#### Frokjaer et al 2014 ####
## * Frokjaer et al 2014

## ** load main dataset
keep.cols0 <- c("AUCi_notcorrected" = "AUCi",
                "AUCgFullday" = "AUCg",
                "FrontCort_DASB_BPnd_NonPV_GM" = "DASB",
                "MDMAuserVsNot" = "MDMA",
                "Age.at.DASB.scan" = "Age")
dfW.Fro <- read.csv2(file.path(path.source,"Klaus-MDMA_N18ogN32_final.csv"), dec = ".", sep = ",", header = TRUE)
dtW.Fro <- as.data.table(dfW.Fro[,c("CIMBI.ID",names(keep.cols0))])


## ** add CAR values over time
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

dfW.FroCase <- read.xlsx(file.path(path.source,"Vibe_SERT_Cort_MDMAcase.xls"), sheetName = "VIBE_SERT_CORT_MDMACASE")
dtW.FroCase <- as.data.table(dfW.FroCase[!is.na(dfW.FroCase$CIMBI.ID),c("CIMBI.ID",names(keep.cols1))])
## intersect(dfW.FroCase$CIMBI.ID,dfW.Fro$CIMBI.ID)

dfW.FroControl <- read.xlsx(file.path(path.source,"DASB_Cort_data-dec2010_fullN32ControlstilMDMA_CAR.xls"), sheetName = "VIBE_DASB_CORT_HealthyN=32")
dtW.FroControl <- as.data.table(dfW.FroControl[!is.na(dfW.FroControl$CIMBI.ID),c("CIMBI.ID",names(keep.cols1))])
## intersect(dfW.FroControl$CIMBI.ID,dfW.FroControl$CIMBI.ID)


dtW.FroT <- merge(x=dtW.Fro,y=rbind(dtW.FroCase,dtW.FroControl), by = "CIMBI.ID")

## dfW.FroCase[dfW.FroCase$CIMBI.ID == "11083",grep("level",colnames(dfW.FroCase),value = TRUE)]
## dfW.FroControl[dfW.FroControl$CIMBI.ID == "11101",grep("level",colnames(dfW.FroControl),value = TRUE)]

## 1: 11083  956.820               NA            11.29
## 2: 11101 -182.475            10.15               NA


## ** rename columns
setnames(dtW.FroT,
         old = c("CIMBI.ID",names(keep.cols0), names(keep.cols1)),
         new = c("id",keep.cols0,keep.cols1),
         )


## ** compute time difference
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

## ** reshape
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

## ** export
write.csv(x = dtL.FroT, file = file.path(path.data,"Frokjaer_2014-process_dataL.csv", dec = "."))
write.csv(x = dtW.FroT, file = file.path(path.data,"Frokjaer_2014-process_dataW.csv", dec = "."))

#### Hogsted et al 2021 ####
## * Hogsted et al 2021

## ** data
df.Hog <- read.csv2(file = file.path(path.source,"DBsample_FirstCAR..csv"),sep=";")

## ** change column names
colnames(df.Hog)[1] <- 'CIMBI.ID'
colnames(df.Hog)[colnames(df.Hog) == 'SLC6A4.5HTTLPR..rs4795541.'] <- 'SLC6A4.5HTTLP'
colnames(df.Hog)[colnames(df.Hog) == 'SLC6A4.5HTTLPR.A.G..s.allele.'] <- '5HTTLPR.S.allele'
colnames(df.Hog)[colnames(df.Hog) == 'SLC6A4.5HTTLPR.A.G..l.allele.'] <- '5HTTLPR.L.allele'
colnames(df.Hog)[colnames(df.Hog) == 'Use.of.contraceptive..Y.N.'] <- 'Contraceptive.use'
colnames(df.Hog)[colnames(df.Hog) == 'Use.of.medicine.other.than.contraceptive..Y.N.'] <- 'Other.medication'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.physical.examination'] <- 'Date.phys.exam'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.education.examination'] <- 'Date.education'
colnames(df.Hog)[colnames(df.Hog) == 'Education.score.individual..1.5.'] <- 'Education.score'
colnames(df.Hog)[colnames(df.Hog) == 'Number.of.years.in.school..max..12.'] <- 'Years.in.school'
colnames(df.Hog)[colnames(df.Hog) == 'Education.score.dad..1.5.'] <- 'Education.score.dad'
colnames(df.Hog)[colnames(df.Hog) == 'Education.score.mom..1.5.'] <- 'Education.score.mom'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.stimulants.examination'] <- 'Date.cig.exam'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.Cohen.PSS..at.home..examination'] <- 'Date.Cohen.exam'
colnames(df.Hog)[colnames(df.Hog) == 'Cohen.PSS..at.home.'] <- 'Cohen.PSS'
colnames(df.Hog)[colnames(df.Hog) == 'Cigarettes.per.day'] <- 'Smoking.status'
colnames(df.Hog)[colnames(df.Hog) == 'Exact.cigarettes.per.day'] <- 'Exact.cig.day'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.cortisol.home.samples'] <- 'Date.cortisol.samples'
colnames(df.Hog)[colnames(df.Hog) == 'Number.of.samples.in.this.version.of.test'] <- 'Number.of.samples'
colnames(df.Hog)[colnames(df.Hog) == 'Status.of.day.for.cortisol.home.samples'] <- 'Work.Rest.Status'
colnames(df.Hog)[colnames(df.Hog) == 'Time.wake.up.on.day.for.cortisol.home.samples'] <- 'Wakeup.Time.Csamples'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.1'] <- 'Time.csample1'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.2'] <- 'Time.csample2'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.3'] <- 'Time.csample3'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.4'] <- 'Time.csample4'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.5'] <- 'Time.csample5'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.6'] <- 'Time.csample6'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.7'] <- 'Time.csample7'
colnames(df.Hog)[colnames(df.Hog) == 'Time.of.cortisol.home.sample.8'] <- 'Time.csample8'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..1..at.wake.up.'] <- 'Levels.C1'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..2..wake.up...15.min.'] <- 'Levels.C2'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..3..wake.up...30.min.'] <- 'Levels.C3'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..4..wake.up...45.min.'] <- 'Levels.C4'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..5..wake.up..60.min.'] <- 'Levels.C5'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..6..at.noon...12.00.'] <- 'Levels.C6'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..7..at.6.p.m..'] <- 'Levels.C7'
colnames(df.Hog)[colnames(df.Hog) == 'Cortisol.levels..nmol.L..for.home.sample..8..at.11.p.m..'] <- 'Levels.C8'
colnames(df.Hog)[colnames(df.Hog) == 'Area..nmol.L.min..under.the.cort..curve.with.respect.to.increase'] <- 'AUC.increase'
colnames(df.Hog)[colnames(df.Hog) == 'Letter.number.sequencing.total.score'] <- 'LNS'
colnames(df.Hog)[colnames(df.Hog) == 'Letter-number sequencing'] <- 'LNS.oldbattery'
colnames(df.Hog)[colnames(df.Hog) == 'SDMT "correct" score'] <- 'SDMT.c'
colnames(df.Hog)[colnames(df.Hog) == 'RIST...Index'] <- 'RIST'
colnames(df.Hog)[colnames(df.Hog) == 'P.Ã.stradiol'] <- 'Estradiol'
colnames(df.Hog)[colnames(df.Hog) == 'P.Progesteron'] <- 'Progesteron'
colnames(df.Hog)[colnames(df.Hog) == 'P.Testosteron'] <- 'Testosteron'
colnames(df.Hog)[colnames(df.Hog) == 'P.Follitropin..FSH...IRP.78.549.'] <- 'FSH'
colnames(df.Hog)[colnames(df.Hog) == 'PSQI.global.score'] <- 'PSQI'
colnames(df.Hog)[colnames(df.Hog) == 'PSQI.global.score..SEMO.1.week.'] <- 'PSQI.1week'
colnames(df.Hog)[colnames(df.Hog) == 'PSQI.global.score..SEMO.2.week.'] <- 'PSQI.2week'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.POMS.examination'] <- 'Date.POMS'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.MDI.examination'] <- 'Date.MDI'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.PSQI.examination'] <- 'Date.PSQI'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.PSQI..SEMO.1.week..examination'] <- 'Date.PSQI.1week'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.PSQI..SEMO.2.week..examination'] <- 'Date.PSQI.2week'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.neuropsychological.examination..new.battery.'] <- 'Date.neuropsyc'
colnames(df.Hog)[colnames(df.Hog) == 'Date.of.PSQI.examination'] <- 'Date.PSQI'
colnames(df.Hog)[colnames(df.Hog) == 'Sample.collection.date'] <- 'Bloodsample.collection.date'
colnames(df.Hog)[colnames(df.Hog) == 'Age.at.CAR'] <- 'Age'
colnames(df.Hog)[colnames(df.Hog) == '`5HTTLPR.L.allele`'] <- 'L.allele'
colnames(df.Hog)[colnames(df.Hog) == 'Total.Mood.Disturbance..TMD..score...32.200.'] <- 'TMD'

## ** Change variable names OC use
df.Hog$Name.of.contraceptive  <- revalue(df.Hog$Name.of.contraceptive, c("P-PILLER, NOVYNETTE" = "OC", "P-PILLER (HARMONET)" = "OC", "MALONETTA" = "OC", 
                                                                 "P-PILLER (GESTONETTE)" = "OC", "JASMINEL" = "OC", "GESTONYL" = "OC", "P-piller" = "OC", 
                                                                 "P-piller, milvane" = "OC",  "P-PILLER" = "OC", "P-PILLER (FEMICEPT)" = "OC", "JASMINELLE" = "OC", "LINDYNETTE" = "OC", 
                                                                 "P-PILLER (DIANOVA MITE)" = "OC", "GESTONETTE" = "OC", "NOVYNETTE" = "OC", 
                                                                 "P-piller, novynette" = "OC", "P-piller, navn ej angivet" = "OC", "P-piller, gestonette" = "OC", "YASMIN" = "OC"))


## ** Change variable name cigarettes
df.Hog$Smoking.status  <- revalue(df.Hog$Smoking.status,
                                  c("Intermediate smoker (5 incl to max 15 exl)" = "Smoker", "Smoked earlier in life" = "No smoker", "Light smoker (max 5 exl daily)" = "Smoker"))

df.Hog$Name.of.contraceptive  <- revalue(df.Hog$Name.of.contraceptive, c("MINIPILLER" = "minipiller", "Minipiller - carzette" = "minipiller", "NUVORING (P-RING)" = "P-ring"))

## df.Hog$Estradiol <- as.character(df.Hog$Estradiol)
## df.Hog$Estradiol <- as.numeric(sub(",", ".", sub(".", ",", df.Hog$Estradiol, fixed = TRUE)))

df.Hog$Progesteron <- as.character(df.Hog$Progesteron)
df.Hog$Progesteron <- as.numeric(sub(",", ".", sub(".", ",", df.Hog$Progesteron, fixed = TRUE)))


## ** Remove cimbiIDs with missing CAR.AUCi
df.Hog <- subset(df.Hog, AUC.increase != '')

## ** Remove cimbiIDs with progesterone-only pills and nuvoring
df.Hog <- subset(df.Hog, Name.of.contraceptive != 'P-ring')
df.Hog <- subset(df.Hog, Name.of.contraceptive != 'minipiller')

## ** Delete women with unknown IUD
df.Hog <- subset(df.Hog, Name.of.contraceptive != 'SPIRAL')

## ** Remove cimbiIDs with cortisol batch 1df
df.Hog <- subset(df.Hog, Cortisol.Batch != '1')

## ** Forloop sort date
dftime <- rep(0)
for(ID in unique(df.Hog$CIMBI.ID)) {
  tmp <- subset(df.Hog, CIMBI.ID == ID)
  sort(df.Hog$Age)
  tmp$time[1] <- 1  
  df.Hog$time[df.Hog$CIMBI.ID == ID] <- tmp$time
}

## ** Delete one duplication (CIMBI-ID: 11140)
df.Hog <- df.Hog[!duplicated(df.Hog$CIMBI.ID), ]

## ** Delete rows with later CAR dates . 
df.Hog <- subset(df.Hog, time != 'NA')


## ** Factor: work day status
df.Hog$wstatus <- as.factor(ifelse(df.Hog$Work.Rest.Status == "Study day" | df.Hog$Work.Rest.Status == "Working day", "Work.day", "Day off"))

## ** Factor: 5HTTLPR genotype
df.Hog$genvs <- as.factor(ifelse(df.Hog$`5HTTLPR.L.allele` == "AA", "LALA", "S-carrier"))

## ** For main analysis
df.Hog <- subset(df.Hog, Name.of.contraceptive != 'HORMONSPIRAL')
df.Hog$OC.use <- as.factor(ifelse(df.Hog$Name.of.contraceptive == "OC", "OC.user", "Non.user"))

dtW.Hog <- as.data.table(df.Hog[!duplicated(df.Hog$CIMBI.ID), c("CIMBI.ID", "AUC.increase", "OC.use", "Gender", "Age", "wstatus", "BMI",
              "Time.csample1","Time.csample2","Time.csample3","Time.csample4","Time.csample5",
              "Levels.C1","Levels.C2","Levels.C3","Levels.C4","Levels.C5")])

setnames(dtW.Hog, old = paste0("Time.csample",1:5), new = paste0("time.sample",1:5))
setnames(dtW.Hog, old = paste0("Levels.C",1:5), new = paste0("cortisol.sample",1:5))
setnames(dtW.Hog, old = "CIMBI.ID", new = "id")

dtW.Hog[,time.sample1 := as.numeric(as.difftime(trimws(time.sample1),"%H:%M:%S",units = "min"))]
dtW.Hog[,time.sample2 := as.numeric(as.difftime(trimws(time.sample2),"%H:%M:%S",units = "min"))]
dtW.Hog[,time.sample3 := as.numeric(as.difftime(trimws(time.sample3),"%H:%M:%S",units = "min"))]
dtW.Hog[,time.sample4 := as.numeric(as.difftime(trimws(time.sample4),"%H:%M:%S",units = "min"))]
dtW.Hog[,time.sample5 := as.numeric(as.difftime(trimws(time.sample5),"%H:%M:%S",units = "min"))]

dtW.Hog[,dtime.sample1 := 0]
dtW.Hog[,dtime.sample2 := time.sample2-time.sample1]
dtW.Hog[,dtime.sample3 := time.sample3-time.sample1]
dtW.Hog[,dtime.sample4 := time.sample4-time.sample1]
dtW.Hog[,dtime.sample5 := time.sample5-time.sample1]


## ** reshape
dtL.Hog <- melt(dtW.Hog,
                 id.vars = c("id","OC.use","Age","Gender","wstatus","BMI","AUC.increase"),
                 measure=patterns("dtime.","cortisol."),
                 variable.name = "sample", value.name = c("time","cortisol"))
sum(is.na(dtL.Hog))
## [1] 4
## dtL.Hog[rowSums(is.na(dtL.Hog))>0,]
## dtW.FroT[which(rowSums(is.na(dtW.FroT))==1),.(id,AUCi,cortisol.sample4,cortisol.sample5)]
##       id     AUCi cortisol.sample4 cortisol.sample5
## 1: 11083  956.820               NA            11.29
## 2: 11101 -182.475            10.15               NA

dtL.Hog[,expectedTime := c(0,15,30,45,60)[sample]]
dtL.Hog[,id := as.character(id)]

## remove missing value
dtL.Hog <- na.omit(dtL.Hog)

## ** export
write.csv(x = dtL.Hog, file = file.path(path.data,"Hogsted_2021-process_dataL.csv", dec = "."))
write.csv(x = dtW.Hog, file = file.path(path.data,"Hogsted_2021-process_dataW.csv", dec = "."))

##----------------------------------------------------------------------
### data-management.R ends here
