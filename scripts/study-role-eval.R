## required libraries
library(readr)
library(ggplot2)
library(tikzDevice)
library(rlang)
library(tidyverse)
library(bnlearn)
library(Rgraphviz)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)
keras_python <- "../scripts/.venv_keras/bin/python"
if (Sys.getenv("KERAS_PYTHON") != ""){
  keras_python = Sys.getenv("KERAS_PYTHON")
}
reticulate::use_python(keras_python)
library(kerasR)

load("study-role-data-full.RData")

train_model <- "data-role-cv/study-role-cv-0-300-keras_full_dropout_0.5_epochs_60_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0_seed_1-data.RData"

if (Sys.getenv("TRAIN_MODEL") != ""){
  train_model = Sys.getenv("TRAIN_MODEL")
}

fun_get_last_checkpoint <- function(name){
  elems <- str_match(name,"(.*/)?([^/]+_epochs_)([0-9]+)(_.+)")
  if (is.na(elems[[1,1]])){ return(NA) }
  if(is.na(elems[[1,2]])) { elems[[1,2]] <- "./"}
  epochs <- parse_integer(elems[[1,4]])
  pattern <- paste(".*",elems[[1,3]],"[0-9]+",elems[[1,5]], sep = "")
  files <- list.files(elems[[1,2]]) %>% str_subset(pattern) %>% lapply(function(x) {
    ep <- (x %>% str_match(".*_epochs_([0-9]+)_.*"))[[1,2]]
    return(list("epoch" = parse_integer(ep), file=x))
  }) %>% 
    bind_rows() %>%
    filter(epoch <= epochs) %>% 
    arrange(desc(epoch))
  if(nrow(files) == 0){ 
    return(NA) 
  } else {
    return(str_replace_all(paste(elems[[1,2]],files[[1,2]], sep  = "/"),"/+","/"))
  }
}

fun_update_functions_form_new <- function(model, newmodel){
  # overwrite functions, copy all new elements
  for(i in names(newmodel)){
    if(typeof(newmodel[[i]]) == "closure" | !(i %in% names(model))){
      model[[i]] <- newmodel[[i]]
    }
  }
  return(model)
}

fun_save_model <- function(){
  save(file=filename_model,list=c("trained","modelname","num_seconds_in_set","set_number"))
  if(typeof(trained$model) == "closure"){
    keras_save(trained$model, path = filename_keras_model)
    params <- trained$model$count_params()
  }
}

# load model
load(train_model)
train <- data_roles_full %>% filter(second != set_number)
test <- data_roles_full %>% filter(second == set_number)

# files
log <- str_replace(train_model,"data.RData","log.csv")
config <- str_remove_all(train_model,"data-role-cv/study-role-cv-|-data.RData")
plot_file <- str_replace(train_model,"data.RData","plot.jpg")
filename_model <- train_model %>% str_replace("-data.RData","-model.RData")
filename_keras_model <- train_model %>% str_replace("-data.RData","-model.h5")
filename_last_checkpoint <- fun_get_last_checkpoint(filename_keras_model)
filename_result <- paste("data-role-cv/study-role-cv-",set_number,"-",num_seconds_in_set,"-",modelname,"-result.RData",sep = "")

training_time <- NA
if(is.na(filename_last_checkpoint)){
  #train model
  print(paste("training model",modelname,"from data",set_number,"in",num_seconds_in_set,"from the start","file:",train_model))
  training_time <- Sys.time()
  model$logname <- log
  trained <- model$train(train,model)
  fun_save_model()
  training_time <- Sys.time() - training_time
} else if (filename_last_checkpoint == filename_keras_model) {
  print(paste("training model",modelname,"not required. already exists."))
  e <- new.env()
  load(filename_model, envir = e)
  newmodel <- model
  trained <- fun_update_functions_form_new(e[["trained"]],newmodel)
  trained$model <- keras_load(filename_keras_model)
  if(file.exists(filename_result)){
    e2 <- new.env()
    load(filename_result, envir=e2)
    training_time <- e2[["training_time"]]
  }
} else {
  print(paste("training model",modelname,"from data",set_number,"in",num_seconds_in_set,"from",filename_last_checkpoint,"file:",train_model))
  training_time <- Sys.time()
  e <- new.env()
  load(str_replace(filename_last_checkpoint,"h5$","RData"), envir = e)
  newmodel <- model
  trained <- fun_update_functions_form_new(e[["trained"]],newmodel)
  trained$model <- keras_load(filename_last_checkpoint)
  trained$logname <- log
  file.copy(from = str_replace(filename_last_checkpoint, "model.h5$", "log.csv"), to = log)
  trained <- trained$continue(data = train, m = trained, until = newmodel$epochs)
  fun_save_model()
  training_time <- Sys.time() - training_time
}

params <- ifelse(typeof(trained$model) == "closure", trained$model$count_params(), NA)

#evaluate log
if(file.exists(log)){
  log_data <- read_csv(log)
  (
    log_data %>% 
      mutate(epoch = seq(1,length(epoch))) %>%
      gather(measure,"value","categorical_accuracy","loss") %>% 
      ggplot() +
      ggtitle(config) +
      geom_line(aes(x=epoch, y=value, color=measure)) +
      scale_y_continuous(minor_breaks = seq(0 , 1, 0.025), breaks = seq(0, 1, 0.05), limits=c(0,1))
  )
  ggsave(plot_file)
}

# test model
result <- trained$predict(test,trained) %>% mutate(time = test$time, agent = test$agent, model=modelname, set=set_number, sets=num_seconds_in_set)
save(file=filename_result,list=c("result","modelname","num_seconds_in_set","set_number","filename_model","filename_keras_model", "training_time", "params"))

