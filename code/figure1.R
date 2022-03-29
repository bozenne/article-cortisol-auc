### figure1.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar 28 2022 (12:05) 
## Version: 
## Last-Updated: mar 29 2022 (18:32) 
##           By: Brice Ozenne
##     Update #: 13
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:


library(ggplot2)
library(ggpubr)
library(data.table)

## * Path
path <- "." ## put path to Github directory
path.code <- file.path(path,"code")
path.data <- file.path(path,"source")
path.report <- file.path(path,"report")
path.results <- file.path(path,"results")

## * Load data
## ** Data used in the article
base::source(file.path(path.code,"analysis-AUC-cortisol.R")) ## no need to wait for all of it to run

## ** original data (no exclusion criteria)
if(FALSE){

    df.data <- readxl::read_xlsx(file.path(path.data,"All_CAR_Arafat_second export.xlsx"))
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
    dtL.fulldata <- melt(dtW.data,
                         measure.vars = patterns(cortisol="^cortisol_", time="^time_t"),
                         id.vars = c("id","diseaseGroup","gender","AUCg","AUCi","AUCb"),
                         variable.name = "sample")
    dtL.HC2 <- as.data.table(dtL.fulldata)[diseaseGroup=="Healthy Control"]


}

## * process
dt.fig1a <- dtLR.HC[, .(nId = length(unique(id[!is.na(cortisol)])),
                        nSample=sum(!is.na(cortisol)),
                        time = c(0,15,30,45,60)[sample],
                        cortisol = mean(cortisol, na.rm = TRUE),
                        lower = t.test(na.omit(cortisol))$conf.int[1],
                        upper = t.test(na.omit(cortisol))$conf.int[2]),
                    by = c("sample")]
##    sample nId nSample time cortisol    lower    upper
## 1:      1 322     465    0 11.88422 11.37274 12.39570
## 2:      2 322     465   15 14.23009 13.64310 14.81707
## 3:      3 322     465   30 16.08942 15.47151 16.70733
## 4:      4 322     465   45 15.06789 14.45109 15.68469
## 5:      5 322     465   60 13.18699 12.62377 13.75021

dt.fig1b <- dtLR.HC[, .(nId = length(unique(id[!is.na(cortisol)])),
                        nSample=sum(!is.na(cortisol)),
                        time = c(0,15,30,45,60)[sample],
                        cortisol = mean(cortisol, na.rm = TRUE),
                        sd = sd(cortisol, na.rm = TRUE),
                        lower = t.test(na.omit(cortisol))$conf.int[1],
                        upper = t.test(na.omit(cortisol))$conf.int[2]),
                    by = c("sample","gender")]

##     sample gender nId nSample time cortisol       sd    lower    upper
##  1:      1   Male 145     210    0 11.44949 5.530137 10.69718 12.20180
##  2:      2   Male 145     210   15 13.97790 6.745494 13.06026 14.89555
##  3:      3   Male 145     210   30 15.36643 6.540753 14.47664 16.25622
##  4:      4   Male 145     210   45 13.69381 6.330563 12.83261 14.55501
##  5:      5   Male 145     210   60 11.72229 5.362461 10.99279 12.45178
##  6:      1 Female 177     255    0 12.24224 5.665632 11.54352 12.94095
##  7:      2 Female 177     255   15 14.43776 6.185237 13.67497 15.20056
##  8:      3 Female 177     255   30 16.68482 6.928170 15.83040 17.53924
##  9:      4 Female 177     255   45 16.19949 6.918490 15.34627 17.05272
## 10:      5 Female 177     255   60 14.39322 6.548622 13.58560 15.20083


## * display
gg.fig1a <- ggplot(dt.fig1a, aes(x = time, y = cortisol)) + geom_point(size = 3) + geom_line(size = 1.25)
gg.fig1a <- gg.fig1a + geom_errorbar(aes(ymin = lower, ymax = upper), width = 2)
gg.fig1a <- gg.fig1a + xlab("time (in minutes)") + ylab("Cortisol (nmol/L)") + scale_x_continuous(breaks=c(0,15,30,45,60))
gg.fig1a <- gg.fig1a + coord_cartesian(ylim = c(9,19))
gg.fig1a <- gg.fig1a + theme(text = element_text(size=15), 
                             axis.line = element_line(size = 1.25),
                             axis.ticks = element_line(size = 2),
                             axis.ticks.length=unit(.25, "cm"))

gg.fig1b <- ggplot(dt.fig1b, aes(x = time+2*(gender=="Male")-1, y = cortisol, color = gender, group = gender))
gg.fig1b <- gg.fig1b + geom_point(size = 3, aes(shape = gender)) + geom_line(size = 1.25, aes(linetype = gender))
gg.fig1b <- gg.fig1b + geom_errorbar(aes(ymin = lower, ymax = upper), width = 2)
gg.fig1b <- gg.fig1b + xlab("time (in minutes)") + ylab("Cortisol (nmol/L)") + scale_x_continuous(breaks=c(0,15,30,45,60))
gg.fig1b <- gg.fig1b + coord_cartesian(ylim = c(9,19)) + labs(color="", shape = "", linetype = "")
gg.fig1b <- gg.fig1b + theme(text = element_text(size=15), 
                             axis.line = element_line(size = 1.25),
                             axis.ticks = element_line(size = 2),
                             axis.ticks.length=unit(.25, "cm"),
                             legend.position=c(0.88,0.96))

## dtL.example <- dtL.HC[id == "52619"]
dtL.example <- dtL.HC[id2 == "50678_I1"]
dtLA.example <- rbind(dtL.example[,.(time,cortisol)],data.table(time=60,cortisol=dtL.example[sample==1,cortisol]))
gg.fig1c <- ggplot(dtL.example, aes(x = time, y = cortisol))
gg.fig1c <- gg.fig1c + geom_polygon(data = dtLA.example, alpha = 0.3, fill = "orange")
gg.fig1c <- gg.fig1c + geom_polygon(data = rbind(data.table(time=0,cortisol=8.5),dtLA.example[c(1,6)],data.table(time=60,cortisol=8.5)), alpha = 0.3, fill = "green")
## gg.fig1c <- gg.fig1c + geom_segment(x = 0, xend = 0, y = 0, yend = dtL.example[sample==1,cortisol], linetype = 2, size = 1.15, color = "orange")
## gg.fig1c <- gg.fig1c + geom_segment(x = 60, xend = 60, y = 0, yend = dtL.example[sample==5,cortisol], linetype = 2, size = 1.15, color = "orange")
## gg.fig1c <- gg.fig1c + geom_segment(x = 0, xend = 60, y = dtL.example[sample==1,cortisol], yend = dtL.example[sample==1,cortisol], linetype = 2, size = 1.15, color = "orange")
gg.fig1c <- gg.fig1c + xlab("time (in minutes)") + ylab("Cortisol (nmol/L)") + scale_x_continuous(breaks=c(0,15,30,45,60))
gg.fig1c <- gg.fig1c + geom_line(size = 1.25) + geom_point(size = 3) + coord_cartesian(ylim = c(9,19))
gg.fig1c <- gg.fig1c + geom_text(x = 30, y = 9.5, label = "AUC[b]", parse = TRUE, size = 7, color = "darkgreen")
gg.fig1c <- gg.fig1c + geom_text(x = 30, y = 12, label = "AUC[i]", parse = TRUE, size = 7, color = "orange")
## gg.fig1c <- gg.fig1c + geom_text(x = 30, y = 18, label = "phantom(AUC[g]~`=`~)AUC[i]+AUC[b]", parse = TRUE, size = 10, color = "blue")
## gg.fig1c <- gg.fig1c + geom_text(x = 30, y = 18, label = "AUC[g]~`=`~phantom(AUC[i]+AUC[b])", parse = TRUE, size = 10, color = "black")
gg.fig1c <- gg.fig1c + annotate("text", x = 10, y = 18.5, hjust = 0, label = "AUC[g]~`=`~phantom(AUC[i])+phantom(AUC[b])", parse = TRUE, size = 7, color = "black")
gg.fig1c <- gg.fig1c + annotate("text", x = 10, y = 18.5, hjust = 0, label = "phantom(a.AUC[g])~AUC[i]", parse = TRUE, size = 7, color = "orange")
gg.fig1c <- gg.fig1c + annotate("text", x = 10, y = 18.5, hjust = 0, label = "phantom(a.AUC[g]+AUC[i])~AUC[b]", parse = TRUE, size = 7, color = "darkgreen")
gg.fig1c <- gg.fig1c + theme(text = element_text(size=15), 
                             axis.line = element_line(size = 1.25),
                             axis.ticks = element_line(size = 2),
                             axis.ticks.length=unit(.25, "cm"))

gg.figure1 <- ggarrange(gg.fig1a + ggtitle("Average across both sex"),
                        gg.fig1b + ggtitle("Average Female vs. Male"),
                        gg.fig1c + ggtitle(paste0("Example of ",unique(dtL.example$gender))),
                        nrow = 1)

## * export
ggsave(gg.figure1, filename = file.path(path.results,"figure1.pdf"), width = 13, device = cairo_pdf)
ggsave(gg.figure1, filename = file.path(path.results,"figure1.png"), width = 13)

## 50489
## 52363_I4
## dd <- dtL.HC[id %in% dtL.HC[(sample == 3 & time==30)]$id,]

## ggplot(dd[dd$id %in% unique(dd$id)[151:175],], aes(x = time, y = cortisol, group = id2)) +geom_line() + facet_wrap(~id) + coord_cartesian(ylim = c(10,18))



## dtL.HC[(sample == 1 & time==0) & (sample == 2 & time==15) & (sample == 3 & time==30) & (sample == 4 & time==45) & (sample == 5 & time==60)]
## dtL.HC[(sample == 1 & time==0) & (sample == 2 & time==15) & (sample == 3 & time==30) & (sample == 4 & time==45) & (sample == 5 & time==60)]

## 
##----------------------------------------------------------------------
### figure1.R ends here
