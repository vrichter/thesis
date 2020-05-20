library(tidyverse)
library(parallelMap)
library(lubridate)
library(rcompanion)
library(mlr)
library(parallel)

## for some reason the full calculation with four sets of variables fails when done in one session

# setup
setwd("~/sync/projects/publications/diss/data")
load("study-addressee-workspace.RData")

# meta switch
draft = TRUE
if (Sys.getenv("CALCULATE_NONDRAFT") != ""){
  draft = FALSE
}
use_cores = detectCores(all.tests = FALSE, logical = TRUE)
if (Sys.getenv("USE_CORES") != ""){
  use_cores = as.integer(Sys.getenv("USE_CORES"))
}
varset = "All"
if (Sys.getenv("ADDRESSEE_RF_VARSET") != ""){
  varset = Sys.getenv("ADDRESSEE_RF_VARSET")
}

fun_calculate_rf <- function (drop_name) {
  ## settings for drafting
  rf.conf.tune.it = 100
  rf.conf.tune.resample = 2
  ## parameters to optimize
  set.seed(1) # need this to be reproducible
  resample_result <- NULL
  
  if(draft) {
      resample_desc <- makeResampleDesc("CV", iters=5)
      model = makeLearner("classif.randomForest")
      ml_task <- makeClassifTask(data = data.frame(data_eval), target = "Ar")
      if(!is.null(eval_drop_sets[[drop_name]])) {
        ml_task  <- dropFeatures(task = ml_task,features = eval_drop_sets[[drop_name]])
      }
      resample_result <- resample(model, ml_task, resampling=resample_desc, show.info = TRUE)
  } else {
    ps <- makeParamSet(
      makeIntegerParam("ntree",lower=1,upper=2000),
      makeIntegerParam("mtry", lower=1, ncol(data_eval)-1-length(eval_drop_sets[[drop_name]])),
      makeIntegerParam("nodesize", lower=1,upper=100),
      makeIntegerParam("maxnodes", lower=2,upper=100) # maxnodes = 1 seems to block
    )
    param_ctrl = makeTuneControlRandom(maxit=rf.conf.tune.it)
    param_resample_desc = makeResampleDesc("Subsample", iters=rf.conf.tune.resample)
    resample_desc <- makeResampleDesc("LOO")
    ml_task <- makeClassifTask(data = data.frame(data_eval), target = "Ar")
    if(!is.null(eval_drop_sets[[drop_name]])) {
      ml_task  <- dropFeatures(task = ml_task,features = eval_drop_sets[[drop_name]])
    }
    model = makeTuneWrapper("classif.randomForest", resampling = param_resample_desc, par.set = ps, control = param_ctrl, show.info = TRUE)
    resample_result <- resample(model, ml_task, resampling=resample_desc, extract = getTuneResult, show.info = TRUE)
  }
  return(resample_result)
}

start_time <- now()
parallelStartSocket(use_cores, logging=TRUE, storagedir = sprintf("logging-%s", varset))
rf_result_local <- fun_calculate_rf(varset)
parallelStop()
end_time <- now()
print(sprintf("Random forest evaluation of %s done in:",varset))
print(end_time-start_time)

if(!draft) {
  saveRDS(rf_result_local, file = sprintf("study-addressee-rf-%s.rds", varset))
}
