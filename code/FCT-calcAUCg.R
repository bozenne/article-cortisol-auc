### FCT-calcAUCg.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: maj 11 2020 (10:53) 
## Version: 
## Last-Updated: mar 29 2022 (18:25) 
##           By: Brice Ozenne
##     Update #: 200
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

calcAUCgi <- function(data, newdata = NULL, method, timepoint,
                      var.X = "X", var.Y = "Y", var.id = "id", var.timepoint = "timepoint", var.truth = NULL,
                      CV.k = 10, guess.timemax = NA){

    ## ** normalize arguments
    if(!is.data.table(data)){
        data <- data.table::as.data.table(data)
    }else{
        data <- data.table::copy(data)
    }
    if("auc" %in% var.X){
        stop("\"auc\" is used internally and should be in argument \'varX\' \n")
    }
    if(var.timepoint %in% names(data) == FALSE){
        stop("Argument \'var.timepoint\' does not match any column of argument \'data\' \n")
    }
    if(!is.null(newdata) && var.timepoint %in% names(newdata) == FALSE){
        stop("Argument \'var.timepoint\' does not match any column of argument \'newdata\' \n")
    }
    if(var.X %in% names(data) == FALSE){
        stop("Argument \'var.X\' does not match any column of argument \'data\' \n")
    }
    if(!is.null(newdata) && var.X %in% names(newdata) == FALSE){
        stop("Argument \'var.X\' does not match any column of argument \'newdata\' \n")
    }
    if(var.Y %in% names(data) == FALSE){
        stop("Argument \'var.Y\' does not match any column of argument \'data\' \n")
    }
    if(var.id %in% names(data) ==  FALSE){
        stop("Argument \'var.id\' does not match any column of argument \'data\' \n")
    }    
    if(!is.null(newdata) && var.id %in% names(newdata) ==  FALSE){
        stop("Argument \'var.id\' does not match any column of argument \'newdata\' \n")
    }    
    if(!is.null(var.truth) && length(var.truth) != 2){
        stop("Argument \'var.truth\' must have length 2 (AUCg and AUCi) \n")
    }    
    if(!is.null(var.truth) && any(var.truth %in% names(data) == FALSE)){
        stop("Argument \'var.truth\' does not match any column of argument \'data\' \n")
    }    
    if(!is.null(newdata) && !is.null(var.truth) && any(var.truth %in% names(newdata) == FALSE)){
        stop("Argument \'var.truth\' does not match any column of argument \'newdata\' \n")
    }    

    valid.timepoint <- unique(data[[var.timepoint]])
    if(!is.numeric(data[[var.X]])){
        stop("Argument \'var.X\' must refer to numeric values in \'data\' \n")
    }
    n.timepoint <- length(timepoint)
    if(!is.list(timepoint)){
        timepoint <- list(timepoint)
    }
    lapply(timepoint, function(iT){
        if(any(iT %in% valid.timepoint == FALSE)){
            stop("Invalid value in argument \'timepoint\' \n",
                 "valid timepoints: ",paste(valid.timepoint, collapse = " "),"\n")
        }
    })
    
    method <- match.arg(method,
                        choices = c("mean", "auc", "lm", "lmI", "gam", "gamI"),
                        several.ok = TRUE)

    ## ** reshape data
    if(is.null(var.truth)){
        var.truth <- c("XXtruthXX_AUCg","XXtruthXX_AUCi)")
                       
        if("XXtruthXX_AUCg" %in% names(data) || "XXtruthXX_AUCi" %in% names(data)){
            stop("Names \"XXtruthXX_AUCg\" and \"XXtruthXX_AUCi\" should not name a colum of argument \'data\' since they are used internally \n")
        }
        data[["XXtruthXX_AUCg"]] <- NA
        data[["XXtruthXX_AUCi"]] <- NA

        if(!is.null(newdata)){
            if("XXtruthXX_AUCg" %in% names(newdata) || "XXtruthXX_AUCi" %in% names(newdata)){
                stop("Names \"XXtruthXX_AUCg\" and \"XXtruthXX_AUCi\" should not name a colum of argument \'newdata\' since they are used internally \n")
            }
            newdata[["XXtruthXX_AUCg"]] <- NA
            newdata[["XXtruthXX_AUCi"]] <- NA
        }
    }

    if(any(c("lm","lmI","gam","gamI") %in% method)){
        dataW <- dcast(data, value.var = c(var.X,var.Y), formula = as.formula(paste0(var.id,"+",var.truth[1],"+",var.truth[2], "~", var.timepoint)))
        if("XXaucXX" %in% names(data)){
            stop("Name \"XXaucXX\" should not name a colum of argument \'data\' since it is used internally \n")
        }

        if(!is.null(newdata)){
            newdataW <- dcast(newdata, value.var = c(var.X,var.Y), formula = as.formula(paste0(var.id,"+",var.truth[1],"+",var.truth[2], "~", var.timepoint)))
            if("XXaucXX" %in% names(newdata)){
                stop("Name \"XXaucXX\" should not name a colum of argument \'newdata\' since it is used internally \n")
            }
        }
    }

    ## ** prepare
    if(is.null(newdata) && any(c("lm","lmI","gam","gamI") %in% method)){
        n.obs <- NROW(dataW)
        ls.folds <- caret::createFolds(1:n.obs, k = CV.k)
    }

    indexmax.time <- max(unlist(timepoint))
    var.timemax <- paste0(var.X,"_",indexmax.time)
    if(is.na(guess.timemax)){
        guess.timemax <- median(dataW[[var.timemax]])
    }
    
    ## ** compute auc
    out <- NULL
    out.model <- list()
    for(iTimepoint in 1:n.timepoint){
        cat("timepoints: ",paste(timepoint[[iTimepoint]],collapse= " "),"\n")
        iName.timepoint <- paste(timepoint[[iTimepoint]],collapse="-")
        out.model[[iName.timepoint]] <- list()

        iName.X.long <- paste0(var.X,"_",timepoint[[iTimepoint]])
        iName.Y.long <- paste0(var.Y,"_",timepoint[[iTimepoint]])
        
        for(iMethod in method){
            iDataW <- dataW[, .SD, .SDcols = c(var.id, var.truth, iName.X.long, iName.Y.long)]

            if(!is.null(newdata)){
                iNewdataL <- newdata[newdata[[var.timepoint]] %in% timepoint[[iTimepoint]],.SD, .SDcols = c(var.id, var.truth, var.X, var.Y)]
                iNewdataW <- newdataW[, .SD, .SDcols = c(var.id, var.truth, iName.X.long, iName.Y.long)]
            }else{
                iNewdataL <- data[data[[var.timepoint]] %in% timepoint[[iTimepoint]],.SD, .SDcols = c(var.id, var.truth, var.X, var.Y)]
                iNewdataW <- iDataW
            }
            if(iMethod == "mean"){                
                iPred <- iNewdataL[, .(method = "mean",
                                       timepoint = iName.timepoint,
                                       AUCg.estimate = mean(.SD[[2]]),
                                       AUCb.estimate = .SD[[2]][1]*.SD[[1]][.N],
                                       AUCg.truth = unique(.SD[[3]]),
                                       AUCi.truth = unique(.SD[[4]])
                                       ),
                                   by = var.id, .SDcols = c(var.X,var.Y,var.truth)]
            }else if(iMethod == "auc"){
                iPred <- iNewdataL[, .(method = "auc",
                                       timepoint = iName.timepoint,
                                       AUCg.estimate = pracma::trapz(x=.SD[[1]], y=.SD[[2]]),
                                       AUCb.estimate = .SD[[2]][1]*.SD[[1]][.N],
                                       AUCg.truth = unique(.SD[[3]]),
                                       AUCi.truth = unique(.SD[[4]])
                                       ),
                                   by = var.id, .SDcols = c(var.X,var.Y,var.truth)]
            }else{ 
                ## ** compute AUC
                iDataW[, XXaucXX := pracma::trapz(x = unlist(.SD[,.SD,.SDcols = iName.X.long]),
                                                  y = unlist(.SD[,.SD,.SDcols = iName.Y.long])),
                       by = var.id]
                ## iDataW[, XXaucXX  - pracma::trapz(x = c(.SD$time_1, .SD$time_2, .SD$time_3),
                ##                       y = c(.SD$cortisol_1, .SD$cortisol_2, .SD$cortisol_3)),
                ##       by = var.id]
                iNewdataW[, XXaucXX := pracma::trapz(x = unlist(.SD[,.SD,.SDcols = iName.X.long]),
                                                     y = unlist(.SD[,.SD,.SDcols = iName.Y.long])),
                          by = var.id]
                
                ## ** weight cortisol values by the measurement time
                iName.WY.long <- paste0("W",iName.Y.long)
                iWeight <- cbind(t(apply(iDataW[,.SD,.SDcols = iName.X.long],1,diff)),0)/2 + cbind(0,t(apply(iDataW[,.SD,.SDcols = iName.X.long],1,diff)))/2
                iDataW[, c(iName.WY.long) := iWeight*.SD, .SDcols = iName.Y.long] ## weighted Y

                iNewweight <- cbind(t(apply(iNewdataW[,.SD,.SDcols = iName.X.long],1,diff)),0)/2 + cbind(0,t(apply(iNewdataW[,.SD,.SDcols = iName.X.long],1,diff)))/2
                iNewdataW[, c(iName.WY.long) := iNewweight*.SD, .SDcols = iName.Y.long] ## weighted Y
                ## ** fit global model
                if(iMethod == "lm"){
                    ff.lm <- as.formula(paste0(var.truth[1],"~offset(XXaucXX)+", paste(iName.WY.long, collapse =" + ")))
                    out.model[[iName.timepoint]]$lm <- stats::lm(ff.lm, data = iDataW)
                }else if(iMethod == "lmI"){
                    ff.lmI <- as.formula(paste0(var.truth[1],"~offset(XXaucXX)+", paste(iName.WY.long, collapse =" * ")))
                    out.model[[iName.timepoint]]$lmI <- stats::lm(ff.lmI, data = iDataW)
                }else if(iMethod == "gam"){
                    ff.gam <- as.formula(paste0(var.truth[1],"~offset(XXaucXX)+", paste("s(",iName.WY.long,")", collapse = " + ")))
                    out.model[[iName.timepoint]]$gam <- mgcv::gam(ff.gam, data = iDataW)
                }else if(iMethod == "gamI"){
                    ff.gamI <- as.formula(paste0(var.truth[1],"~offset(XXaucXX)+ti(",paste(iName.WY.long, collapse = ","),")"))
                    out.model[[iName.timepoint]]$gamI <- mgcv::gam(ff.gamI, data = iDataW)
                }

                ## ** assessment
                if(!is.null(newdata)){
                    if(iName.X.long[length(iName.X.long)]==var.timemax){
                        iTime.AUCb <- iNewdataW[[iName.X.long[length(iName.X.long)]]]
                    }else{
                        iTime.AUCb <- guess.timemax
                    }
                    iPred <- data.table(method = iMethod,
                                        timepoint = iName.timepoint,
                                        id = iNewdataW[[var.id]],
                                        AUCg.estimate = predict(out.model[[iName.timepoint]][[iMethod]], newdata = iNewdataW),
                                        AUCb.estimate = iNewdataW[[iName.Y.long[1]]]*iTime.AUCb,
                                        AUCg.truth = iNewdataW[[var.truth[1]]],
                                        AUCi.truth = iNewdataW[[var.truth[2]]]
                                        )
                }else{
                    iPred <- NULL
                    for(iFold in 1:CV.k){ ## iFold <- 1
                        index.test <- ls.folds[[iFold]]
                        index.train <- setdiff(1:n.obs,index.test)
                        iData.test <- iDataW[index.test]
                        iData.train <- iDataW[index.train]

                        if(iMethod == "lm"){
                            iE.fit <- stats::lm(ff.lm, data = iData.train)
                        }else if(iMethod == "lmI"){
                            iE.fit <- stats::lm(ff.lmI, data = iData.train)
                        }else if(iMethod == "gam"){
                            iE.fit <- mgcv::gam(ff.gam, data = iData.train)
                        }else if(iMethod == "gamI"){
                            iE.fit <- mgcv::gam(ff.gamI, data = iData.train)
                        }
                        if(iName.X.long[length(iName.X.long)]==var.timemax){
                            iTime.AUCb <- iData.test[[iName.X.long[length(iName.X.long)]]]
                        }else{
                            iTime.AUCb <- guess.timemax
                        }
                        iPred <- rbind(iPred,
                                       data.table(method = iMethod,
                                                  timepoint = iName.timepoint,
                                                  id = iData.test[[var.id]],
                                                  AUCg.estimate = as.double(predict(iE.fit, newdata = iData.test)),
                                                  AUCb.estimate = iData.test[[iName.Y.long[1]]]*iTime.AUCb,
                                                  AUCg.truth = iData.test[[var.truth[1]]],
                                                  AUCi.truth = iData.test[[var.truth[2]]]))
                    }
                }
            }
            names(iPred)[names(iPred)=="id"] <- var.id
            names(iPred)[names(iPred)=="AUCg.truth"] <- var.truth[1]
            names(iPred)[names(iPred)=="AUCi.truth"] <- var.truth[2]

            if(var.truth[1]=="XXtruthXX_AUCg"){
                iPred$XXtruthXX_AUCg <- NULL
            }
            if(var.truth[2]=="XXtruthXX_AUCi"){
                iPred$XXtruthXX_AUCi <- NULL
            }
            out <- rbind(out, iPred)
                
        }
    }

    out[, AUCi.estimate := AUCg.estimate - AUCb.estimate]
    ## out[, AUCb.estimate := NULL]
    if(var.truth[1]!="XXtruthXX_AUCg"){
        out[, AUC.error := .SD[["AUCg.estimate"]] - .SD[[var.truth[1]]]]
    }
    setcolorder(out, c(var.id,"method","timepoint","AUCg.estimate","AUCi.estimate",var.truth,"AUC.error"))

    ## ** export
    attr(out,"model") <- out.model
    if(!is.null(names(timepoint))){
        oldname <- unlist(lapply(timepoint, paste, collapse = "-"))
        newname <- names(timepoint)
        out[,timepoint := factor(timepoint, levels = oldname, labels = newname)]
    }
    return(out)
}



######################################################################
### FCT-calcAUCg.R ends here
