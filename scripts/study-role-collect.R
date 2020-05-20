## required libraries
library(readr)
library(rlang)
library(tidyverse)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)

e <- new.env()
load(file="study-group-workspace.RData", envir = e)
for(var in names(e)[startsWith(names(e),"fun_")]){
  .GlobalEnv[[var]] <- e[[var]]
}
rm(e)

fun_calc_acc_w <- function(t){
  acc <- sum(diag(t))/sum(t)
  e <- 1-acc
  wd <- 1.96*sqrt((e*(1-e))/sum(t))
  return(list("accuracy"=acc, "wd" = wd))
}

fun_calc_ci <- function(x){
  result <- list()
  result$mean <- mean(x)
  if(length(x) > 1){
    result$error <- qt(0.975,df=(length(x))-1)*sd(x)/sqrt(length(x))
  } else {
    result$error <- NA
  }
  result$lower <- result$mean-result$error
  result$upper <- result$mean+result$error
  return(result)
}

load("study-role-data-full.RData")
result_dir = "data-role-cv"
result_file_pattern = "study-role-cv-([0-9]+)-([0-9]+)-(.+)(_seed_[0-9]+)?-result.RData"
result_file_filter = result_file_pattern
#result_file_filter = "study-role-cv-([0-9]+)-([0-9]+)-(rule|bnmrulec|(lstm|keras).+epochs_50_units_128.*_hl_1_.*)-result.RData"

fun_load_with_filter <- function(filter = result_file_filter, calculate_sets = TRUE, print_debug = TRUE){
  roles_eval_result_data <- 
    lapply(
      list.files(result_dir) %>% str_subset(filter), 
      function(x) {
        m <- str_match(x,result_file_pattern)
        filename <- paste(result_dir,m[[1]], sep="/")
        set <- m[[2]]
        sets <- m[[3]]
        model <- m[[4]]
        if (print_debug) {print(paste("loading:",filename))}
        e <- new.env()
        load(file=filename, envir = e)
        result <- e[["result"]]
        if(str_count(model,"_") > 0){
          params <- str_split(model, "_")[[1]]
          result <- result %>% mutate(model.type = params[1], model.data = params[2])
          if(length(params) > 2){
            for (i in seq(3,length(params),2)) {
              result <- result %>% mutate(!!paste("model",params[i],sep=".") := params[i+1])
            }
          }
        } else {
          result <- result %>% mutate(model.type = model)
        }
        result <- result %>% mutate(model.training_time = e[["training_time"]])
        result <- result %>% mutate(model.params = e[["params"]])
        return(result)
      }) %>%  bind_rows()
  
  model_performance <- lapply(unique(roles_eval_result_data$model), function(mod){
    if(print_debug){print(paste("processing model",mod))}
    data <- roles_eval_result_data %>% filter(model==mod)
    data$role.predicted[is.na(data$role.predicted)] <- "non-member"
    acc <- fun_calc_acc_w(table(data$role.agent,data$role.predicted))
    result <- lapply(levels(data$role.agent),function(role){
      m <- fun_create_cm_and_measurements(data$role.agent==role,data$role.predicted==role)
      for (predictedrole in levels(data$role.predicted)) {
        m[[str_c("predicted.",predictedrole)]] <- data %>% filter(role.agent==role, role.predicted==predictedrole) %>% nrow()
      }
      m %>% bind_rows() %>% 
        mutate(class=role) %>% 
        return()
    }) %>% bind_rows() %>% 
      mutate(model=mod, 
             mean.precision=mean(precision),
             mean.recall=mean(recall),
             mean.f1 = mean(f1),
             accuracy = acc$accuracy,
             accuracy.wd = acc$wd,
             mean.markedness = mean(markedness),
             mean.informedness = mean(informedness),
             observations=nrow(data), 
             sets=toString(unique(data$set)),
             model.type = first(data$model.type),
             model.data = first(data$model.data),
             model.dropout = first(data$model.dropout),
             model.hist = first(data$model.hist),
             model.epochs = first(data$model.epochs),
             model.units = first(data$model.units),
             model.regularize = first(data$model.regularize),
             model.hl = first(data$model.hl),
             model.lr = first(data$model.lr),
             model.base = str_remove(first(data$model),"_seed_[0-9]+$"),
             model.seed = first(data$model.seed)
      )
  }) %>% bind_rows() %>% filter(observations == 101668) %>% select(model, class, everything())

  if (!"model.seed" %in% names(model_performance)) {
    model_performance <- model_performance %>% mutate(model.seed = "0")
  }

  model_performance <- lapply(unique(model_performance$model.base), function(mb) {
    data <- model_performance %>% filter(model.base==mb) %>% arrange(model.seed)
    data_addr <- data %>% filter(class=="addressee")
    assertthat::assert_that(nrow(data_addr)==length(unique(data$model.seed)))
    acc.ci <- fun_calc_ci(data_addr$accuracy)
    f1.ci <- fun_calc_ci(data_addr$mean.f1)
    fi <- lapply(unique(data$class), function(c) {
      dc <- data %>% filter(class == c)
      f <- fun_calc_ci(dc$f1)
      r <- list()
      r[paste(c,"lower",sep=".")] <- f$lower
      r[paste(c,"upper",sep=".")] <- f$upper
      return(r)
    }) %>% unlist(recursive = FALSE)
    data %>% mutate(
      seed.mean.accuracy = acc.ci$mean, seed.mean.accuracy.lower = acc.ci$lower, seed.mean.accuracy.upper = acc.ci$upper,
      seed.mean.f1 = f1.ci$mean, seed.mean.f1.lower = f1.ci$lower, seed.mean.f1.upper = f1.ci$upper, 
      seed.mean.f1.speaker.lower = fi$speaker.lower, seed.mean.f1.speaker.upper = fi$speaker.upper,
      seed.mean.f1.addressee.lower = fi$addressee.lower, seed.mean.f1.addressee.upper = fi$addressee.upper,
      seed.mean.f1.member.lower = fi$member.lower, seed.mean.f1.member.upper = fi$member.upper,
      `seed.mean.f1.non-member.lower` = fi$`non-member.lower`, `seed.mean.f1.non-member.upper` = fi$`non-member.upper`,
      seed.seeds = length(data_addr$model.seed)
    ) %>%
    return()
  }) %>% bind_rows()


  model_performance_sets <- NA
  if (calculate_sets) {
    model_performance_sets <- lapply(unique(roles_eval_result_data$model), function(mod){
      if(print_debug){print(paste("processing setwise model",mod))}
      data <- roles_eval_result_data %>% filter(model==mod)
      data$role.predicted[is.na(data$role.predicted)] <- "non-member"
      acc <- sum(data$role.agent==data$role.predicted)/nrow(data)
      result <- lapply(unique(data$set), function (s) {
        sdata <- data %>% filter(set==s)
        sacc <-  sum(sdata$role.agent==sdata$role.predicted)/nrow(sdata)
        lapply(levels(data$role.agent), function(role){
          fun_create_cm_and_measurements(sdata$role.agent==role,sdata$role.predicted==role) %>% 
            bind_rows() %>% 
            mutate(class=role) %>% 
            return()
        }) %>% bind_rows() %>% 
          mutate(set=s, set.accuracy = sacc,
                 set.mean.f1 = mean(f1),
                 set.mean.precision = mean(precision),
                 set.mean.recall = mean(recall)
          ) %>% 
          return()
      }) %>% bind_rows()
      result %>%  
        mutate(model=mod, 
               mean.precision=mean(precision),
               sd.precision=sd(precision),
               mean.recall=mean(recall),
               sd.recall=sd(recall),
               mean.f1 = mean(f1),
               sd.f1 = sd(f1),
               accuracy = acc,
               set.accuracy = set.accuracy,
               set.accuracy.mean = mean(set.accuracy),
               set.accuracy.sd = sd(set.accuracy),
               mean.markedness = mean(markedness),
               mean.informedness = mean(informedness),
               observations=nrow(data), 
               sets=toString(unique(data$set)),
               model.type = first(data$model.type),
               model.data = first(data$model.data),
               model.dropout = first(data$model.dropout),
               model.hist = first(data$model.hist),
               model.epochs = first(data$model.epochs),
               model.units = first(data$model.units),
               model.regularize = first(data$model.regularize),
               model.hl = first(data$model.hl),
               model.lr = first(data$model.lr),
               model.base = str_remove(first(data$model),"_seed_[0-9]+$"),
               model.seed = first(data$model.seed)
        ) %>% return()
    }) %>% bind_rows() %>% select(model, class, everything())
  }
  return(list("model_performance"=model_performance, "model_performance_sets"=model_performance_sets))
}

# split processing otherwize it takes too much memory
models <- lapply(unique(list.files(result_dir) %>% str_subset(result_file_filter)), function(pathname) {
  m <- str_match(pathname,"(study-role-cv-)([0-9]+-[0-9]+)-(.+)_seed_[0-9]+-result.RData")
  if (!is.na(m[1])){
    return(paste(m[[2]],'[0-9]+-[0-9]+-',m[[4]],'_seed_[0-9]+-result.RData$', sep = ""))
  } else {
    m <- str_match(pathname,"(study-role-cv-)([0-9]+-[0-9]+)-(.+)-result.RData")
    return(paste(m[[2]],'[0-9]+-[0-9]+-',m[[4]],'-result.RData$', sep = ""))
  }
}) %>% unlist() %>% unique() %>% lapply(., function(filter) {
  print(str_c("processing match: ",filter))
  return(fun_load_with_filter(filter = filter, calculate_sets = TRUE, print_debug = FALSE))
})

model_performance <- lapply(seq(length(models)), function(i){
  models[[i]]$model_performance
}) %>% bind_rows()

model_performance_sets <- lapply(seq(length(models)), function(i){
  models[[i]]$model_performance_sets
}) %>% bind_rows()

save(file = "study-role-eval-performance-data.RData", list = c("model_performance", "model_performance_sets"))
print("...done")