### figureS1.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: mar  9 2022 (15:19) 
## Version: 
## Last-Updated: mar  9 2022 (16:13) 
##           By: Brice Ozenne
##     Update #: 2
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
ls.hlme <- readRDS(file.path(path.results,"ls_hlme.rds"))

## ggTraj_hlme(ls.hlme[[1]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[2]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[3]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[4]], color = "prob", facet = TRUE, nrow = 1)
ggTraj_homo <- ggTraj_hlme(ls.hlme[[5]], color = "prob", facet = TRUE, nrow = 1)
## ggTraj_hlme(ls.hlme[[6]], color = "prob", facet = TRUE, nrow = 1)

   
ggsave(ggTraj_homo$plot + scale_x_discrete(name ="time", labels=c(c("0 min","15 min","30 min","45 min","60 min"))) + ylab("corstisol (nmol/L)"),
       filename = "report/figures/trajLCMM-5groups-bis.pdf", height = 4, width = 12)


##----------------------------------------------------------------------
### figureS1.R ends here
