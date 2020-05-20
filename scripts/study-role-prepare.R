library(readr)
library(tidyverse)
library(rlang)
library(bnlearn)
library(gRain)
library(Rgraphviz)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)
keras_python <- "../scripts/.venv_keras/bin/python"
if (Sys.getenv("KERAS_PYTHON") != ""){
  keras_python = Sys.getenv("KERAS_PYTHON")
}
reticulate::use_python(keras_python)
library(kerasR)

load("study-group-workspace.RData")

# meta switch
num_seconds_in_set = 300
keras_verbose = 1

if (Sys.getenv("SECONDS_IN_SET") != ""){
  num_seconds_in_set = as.integer(Sys.getenv("SECONDS_IN_SET"))
}
if (Sys.getenv("KERAS_VERBOSE") != ""){
  keras_verbose = as.integer(Sys.getenv("KERAS_VERBOSE"))
}

fun_landmarks_distances <- function(data, cutoff=50){
  parts <- c("inner_lips", "outer_lips", "jaw", "left_brow", "right_brow", "left_eye", "right_eye", "nose", "nose_wings")
  for (i in parts) {
    for (j in seq(0,100)) {
      name <- paste("distance",i,j, sep = ".")
      xname <- paste("landmarks",i,j,"x", sep = ".")
      yname <- paste("landmarks",i,j,"y", sep = ".")
      if(xname %in% names(data)){
        xdist <- (data[[xname]] - lag(data[[xname]]))
        ydist <- (data[[yname]] - lag(data[[yname]]))
        xdist <- replace(xdist, `>`(abs(xdist),cutoff), 0)
        ydist <- replace(ydist, `>`(abs(ydist),cutoff), 0)
        dist <- sqrt(xdist**2 + ydist**2) 
        data <- mutate(data, !!name := dist)
      } else {
        break
      }
    }
  }
  return(data)
}

fun_focus_speaking <- function(data, frames=9){
  var.p = function(x){var(x)*(length(x)-1)/length(x)}
  d <- sqrt((data$landmarks.inner_lips.2.x-data$landmarks.inner_lips.6.x)^2+(data$landmarks.inner_lips.2.y-data$landmarks.inner_lips.6.y)^2)
  d[data$num_faces==0] <- NA
  S <- sapply(seq(1,length(d)),function(n){
    return(var.p(na.omit(d[seq(max(1,n-frames),n)])))
  })
  data %>% mutate(focus.speaking=S) %>% return()
}

data_roles_full <- 
  data_roles %>%
  inner_join(data_ffm_gco, by=c("time","agent","observation.type")) %>% 
  inner_join(data_faces, by=c("time","agent")) %>% 
  select(time,agent,speaking,role.agent,
         faces.size, faces.angle.w,
         starts_with("cl"),
         starts_with("role"),starts_with("landmarks")) %>%
  mutate(
         agent=recode(agent, flobi_assistance="Flobi Assistance", flobi_entrance="Flobi Entrance"),
         faces.angle.w = replace_na(faces.angle.w,90),
         cl.agent.distance.cost = (int(!cl.in.group) * max(data_ffm_gco$cl.agent.distance.cost)) + cl.agent.distance.cost,
         cl.agent.visibility.cost = (int(!cl.in.group) * max(data_ffm_gco$cl.agent.visibility.cost)) + cl.agent.visibility.cost,
         cl.group.distance.cost = (int(!cl.in.group) * max(data_ffm_gco$cl.group.distance.cost)) + cl.group.distance.cost,
         cl.group.visibility.cost = (int(!cl.in.group) * max(data_ffm_gco$cl.group.visibility.cost)) + cl.group.visibility.cost,
         assignment.cost.agent = cl.agent.distance.cost + cl.agent.visibility.cost,
         assignment.cost.group = cl.group.distance.cost + cl.group.visibility.cost,
         num_faces = replace_na(landmarks.faces,0),
         face.detected = num_faces > 0,
         faces.size = replace_na(faces.size,0)
         ) %>% 
  fun_landmarks_distances() %>% 
  fun_focus_speaking() %>%
  replace(., is.na(.), 0) %>%
  mutate(addressee.mutual_gaze = faces.angle.w < 12,
         addressee.focus_speaking = focus.speaking > 1.5)


# model creation and evaluation functions
fun_create_model_rule <- function(...){
  model <- list()
  # load create addressee recognition from meka data
  e <- new.env()
  load(file="study-meka-workspace.RData", envir = e)
  model$addressee.model <- as.grain(
    bn.fit(model2network("[an.addressed][cl.mutualgaze|an.addressed][cl.mouthmovement|an.addressed]"),
           data = data.frame(e[["data_eval_sets"]]$Classification %>% 
                               select(cl.mutualgaze, cl.mouthmovement, an.addressed)))
  )
  n <- setEvidence(model$addressee.model, evidence=list(cl.mutualgaze="TRUE", cl.mouthmovement="TRUE"))
  model$addressee.model.tt <- (querygrain(n, nodes=c("an.addressed"), type = "marginal"))$an.addressed[[1]] > 0.5
  n <- setEvidence(model$addressee.model, evidence=list(cl.mutualgaze="TRUE", cl.mouthmovement="FALSE"))
  model$addressee.model.tf <- (querygrain(n, nodes=c("an.addressed"), type = "marginal"))$an.addressed[[1]] > 0.5
  n <- setEvidence(model$addressee.model, evidence=list(cl.mutualgaze="FALSE", cl.mouthmovement="TRUE"))
  model$addressee.model.ft <- (querygrain(n, nodes=c("an.addressed"), type = "marginal"))$an.addressed[[1]] > 0.5
  n <- setEvidence(model$addressee.model, evidence=list(cl.mutualgaze="FALSE", cl.mouthmovement="FALSE"))
  model$addressee.model.ff <- (querygrain(n, nodes=c("an.addressed"), type = "marginal"))$an.addressed[[1]] > 0.5
  rm(e)
  model$predict.addressee <- function(gaze, mouth, m){
    return(ifelse(gaze & mouth,
                  m$addressee.model.tt,
                  ifelse(gaze,
                         m$addressee.model.tf,
                         ifelse(mouth,
                                m$addressee.model.ft,
                                m$addressee.model.ff
                         )
                  )
    ))
  }
  model$train <- function(train, m){
    return(m)
  }
  model$predict <- function(test, m) {
    result <- test %>% mutate(role.predicted="member")
    result$role.predicted[m$predict.addressee(result$addressee.mutual_gaze, result$addressee.focus_speaking, m)] = "addressee"
    result$role.predicted[result$speaking]="speaker"
    result$role.predicted[!result$cl.in.group]="non-member"
    result %>% 
      select(role.agent,role.predicted) %>% 
      mutate(
        role.predicted = factor(role.predicted, levels=levels(role.agent)),
        speaker = int(role.predicted=="speaker"),
        addressee = int(role.predicted=="addressee"),
        member = int(role.predicted=="member"),
        `non-member` = int(role.predicted=="non-member")
      ) %>% 
      return()
  }
  return(model)
}

fun_resample_bn <- function(x, type){
  if(is.na(type)) { return(x) }
  if (type == "undersample") {
    keep_samples <- c(
      seq(1,nrow(x))[x$role.agent=="speaker"], # all speaker
      seq(1,nrow(x))[x$role.agent=="addressee"], # all addressee
      seq(1,nrow(x))[x$role.agent=="member"], # all member
      sample(seq(1,nrow(x))[x$role.agent=="non-member"],sum(x$role.agent=="member")+1, replace = FALSE) # number of member+1 from non-member. extra sample to prevent tie.
    ) %>% sort()
    return(x[keep_samples,])
  }
  if (type == "undersample2") {
    nspeaker <- sum(x$role.agent=="speaker")
    keep_samples <- c(
      seq(1,nrow(x))[x$role.agent=="speaker"], # all speaker
      sample(seq(1,nrow(x))[x$role.agent=="addressee"],nspeaker+1, replace = FALSE), # number of speaker from addressee-member. extra sample to prevent tie
      sample(seq(1,nrow(x))[x$role.agent=="member"],nspeaker+2, replace = FALSE), # number of speaker from member. extra sample to prevent tie
      sample(seq(1,nrow(x))[x$role.agent=="non-member"],nspeaker+3, replace = FALSE) # number of speaker from non-member. extra sample to prevent tie
    ) %>% sort()
    return(x[keep_samples,])
  }
}
 
fun_create_model_bn <- function(modelstring=NA, autovars_in=NA, resample=NA){
  model <- list()
  model$modelstring <- modelstring
  model$autovars_in <- autovars_in
  model$fun_resample <- fun_resample_bn
  model$resample <- resample
  model$transform_data <- function(data, m) {
    result <- data %>% select(m$vars)
    for (v in m$vars) {
      if (typeof(result[[v]])=="logical") {
        result[[v]] <- factor(result[[v]], levels = c("FALSE", "TRUE"))
      } else if(typeof(result[[v]])=="integer" & !is.factor(result[[v]])) {
        result[[v]] <- factor(result[[v]])
      }
    }
    return(data.frame(result))
  }
  model$train <- function(data, m){
    set.seed(1)
    if(!is.na(m$modelstring)){
      m$vars <- names((model2network(m$modelstring))$nodes)
    } else if (!anyNA(m$autovars_in)) {
      if("role.agent" %in% m$autovars_in){
        m$vars <- m$autovars_in
      } else {
        m$vars <- c("role.agent",m$autovars_in)
      }
    } else {
      m$vars <- c("speaking", "role.agent", "faces.size", "faces.angle.w", "cl.agent.visibility.cost","assignment.cost.agent", "assignment.cost.group")
    }
    train_data <- m$fun_resample(m$transform_data(data, m), m$resample)
    if(is.na(m$modelstring)){
      m$bn <- hc(train_data,debug = FALSE, restart = 1000, perturb = 1000)
    } else {
      m$bn <- model2network(m$modelstring)
    }
    m$fit <- bn.fit(m$bn,train_data)
    return(m)
  }
  model$predict <- function(test, m){
    t <- m$transform_data(test,m)
    l <- capture.output(p <- predict(m$fit,node="role.agent",data=t,debug = TRUE))
    l <- l[2:length(l)]
    l <- l[seq(2,length(l),2)]
    l <- str_extract_all(l," 0.[0-9]+",simplify = TRUE)
    colnames(l) <- c("speaker","addressee","member","non-member")
    l <- as_tibble(l)
    test %>% 
      select(role.agent) %>% 
      mutate(
        role.predicted=factor(p, levels=levels(test$role.agent)), 
        speaker=as.double(l$speaker), 
        addressee=as.double(l$addressee), 
        member=as.double(l$member), 
        `non-member`=as.double(l$`non-member`)
      ) %>% 
      select(speaker,addressee,member,`non-member`,role.agent,role.predicted) %>%
      return()
  }
  return(model)
}

fun_resample <- function(x, y, type){
  if (type == "undersample") {
    keep_samples <- c(
      seq(1,nrow(y))[y[,1]==1], # all speaker
      seq(1,nrow(y))[y[,2]==1], # all addressee
      seq(1,nrow(y))[y[,3]==1], # all member
      sample(seq(1,nrow(y))[y[,4]==1],sum(y[,3]), replace = FALSE) # number of member from non-member
    ) %>% sort()
    return(list("x"=x[keep_samples,],"y"=y[keep_samples,]))
  }
  if (type == "undersample2") {
    keep_samples <- c(
      seq(1,nrow(y))[y[,1]==1], # all speaker
      sample(seq(1,nrow(y))[y[,2]==1],sum(y[,1]), replace = FALSE), # number of speaker from addressee-member
      sample(seq(1,nrow(y))[y[,3]==1],sum(y[,1]), replace = FALSE), # number of speaker from member
      sample(seq(1,nrow(y))[y[,4]==1],sum(y[,1]), replace = FALSE) # number of speaker from non-member
    ) %>% sort()
    return(list("x"=x[keep_samples,],"y"=y[keep_samples,]))
  }
}
  
fun_create_model_keras <- function(
  units=128, dropout=0.25, epochs=5, verbose=1, vars_in=NA, lr=0.001, 
  resample = NA, regularize=NA, hidden.layers=1, validation.split=0.1, 
  seed=1, leaky=FALSE)
{
  model <- list()
  model$vars <- vars_in
  model$units <- units
  model$dropout <- dropout
  model$epochs <- epochs
  model$verbose <- verbose
  model$lr <- lr
  model$resample <- resample
  model$logname <- NA
  model$fun_resample <- fun_resample
  model$regularize <- regularize
  model$hidden.layers <- hidden.layers
  model$validation.split <- validation.split
  model$scale.center <- TRUE
  model$scale.scale <- TRUE
  model$seed = seed
  model$leaky = leaky
  model$transform <- function(data, m){
    t <- data
    t$assignment.cost.agent[t$assignment.cost.agent>5000] <- 5000
    t$assignment.cost.group[t$assignment.cost.group>5000] <- 5000
    t$faces.size[!t$cl.in.group] <- 0
    t$faces.angle.w[!t$cl.in.group] <- 45
    u <- t %>% select(m$vars) %>% data.matrix() %>% scale(center = m$scale.center, scale=m$scale.scale)
    return(list("scale.center" = attr(u, "scaled:center"), "scale.scale" = attr(u, "scaled:scale"), "data" = u))
  } 
  # train model
  model$train <- function(data, m){
    set.seed(m$seed)
    if(anyNA(m$vars)){
      m$vars <- data %>% select(
        faces.size,
        faces.angle.w,
        speaking,
        assignment.cost.agent,
        assignment.cost.group,
        cl.in.group,
        starts_with("landmarks"),
        starts_with("distance")
      ) %>% names()
    } 
    m$roles <- levels(data$role.agent)
    train_data_scaled <- m$transform(data, m)
    m$scale.center <- train_data_scaled$scale.center
    m$scale.scale <- train_data_scaled$scale.scale
    train_data_x <- train_data_scaled$data
    train_data_y <- data %>% select(role.agent) %>% data.matrix() %>% `-`(1) %>% to_categorical()
    if(!is.na(m$resample)){
      rs <- m$fun_resample(train_data_x, train_data_y, m$resample)
      train_data_x <- rs$x
      train_data_y <- rs$y
    }
    # setup model
    reg <- NULL
    reg <- if(!is.na(m$regularize)) { reg <- l2(m$regularize) }
    mod <- Sequential()
    for (i in seq(1,m$hidden.layers)){
      mod$add(Dense(units = m$units, input_shape = dim(train_data_x)[2], kernel_regularizer=reg, bias_regularizer=reg))
      if(m$leaky){
        mod$add(LeakyReLU())
      }
      mod$add(Dropout(m$dropout))
    }
    mod$add(Dense(4, kernel_regularizer=reg, bias_regularizer=reg))
    mod$add(Activation("softmax"))
    m$model <- mod
    callbacks <- list()
    if (!is.na(m$logname)){
      callbacks <- list(CSVLogger(m$logname))
    }
    keras_compile(m$model,  loss = 'categorical_crossentropy', optimizer = RMSprop(lr=m$lr), metrics=c('categorical_accuracy'))
    keras_fit(m$model, train_data_x, train_data_y,
              batch_size = 32, epochs = m$epochs,
              verbose = m$verbose, validation_split = m$validation.split, callbacks = callbacks)
    return(m)
  }
  model$continue <- function(data,m,until) {
    set.seed(m$seed)
    assertthat::assert_that(m$epochs < until)
    train_data_x <- (m$transform(data, m))$data
    train_data_y <- data %>% select(role.agent) %>% data.matrix() %>% `-`(1) %>% to_categorical()
    if(!is.na(m$resample)){
      rs <- m$fun_resample(train_data_x, train_data_y, m$resample)
      train_data_x <- rs$x
      train_data_y <- rs$y
    }
    callbacks <- list()
    if (!is.na(m$logname)){
      callbacks <- list(CSVLogger(m$logname, append=TRUE))
    }
    keras_fit(m$model, train_data_x, train_data_y,
              batch_size = 32, epochs = (until-m$epochs),
              verbose = m$verbose, validation_split = m$validation.split, callbacks = callbacks)
    m$epochs <- until
    return(m)
  }
  # predict function
  model$predict <- function(test, m){
    test_data_x <- model$transform(test,m)$data
    test_data_y <- test %>% select(role.agent) %>% data.matrix() 
    test_result_y <- int(keras_predict_classes(m$model, test_data_x)) %>% `+`(1)
    test_result_p <- keras_predict_proba(m$model, test_data_x)
    colnames(test_result_p) <- c("speaker","addressee","member","non-member")
    test_result_p %>% 
      as_tibble() %>% 
      mutate(role.predicted=recode(
        factor(int(test_result_y),levels=c("1","2","3","4")),
        `1`="speaker", `2`="addressee", `3`="member", `4`="non-member")) %>% 
      mutate(role.agent=test$role.agent) %>% 
      return()
  }
  return(model)
}

fun_resample_timed <- function(x, y, type){
  if (type == "undersample") {
    keep_samples <- c(
      seq(1,nrow(y))[y[,1]==1], # all speaker
      seq(1,nrow(y))[y[,2]==1], # all addressee
      seq(1,nrow(y))[y[,3]==1], # all member
      sample(seq(1,nrow(y))[y[,4]==1],sum(y[,3]), replace = FALSE) # number of member from non-member
    ) %>% sort()
    return(list("x"=x[keep_samples,,],"y"=y[keep_samples,]))
  }
  if (type == "undersample2") {
    keep_samples <- c(
      seq(1,nrow(y))[y[,1]==1], # all speaker
      sample(seq(1,nrow(y))[y[,2]==1],sum(y[,1]), replace = FALSE), # number of speaker from addressee-member
      sample(seq(1,nrow(y))[y[,3]==1],sum(y[,1]), replace = FALSE), # number of speaker from member
      sample(seq(1,nrow(y))[y[,4]==1],sum(y[,1]), replace = FALSE) # number of speaker from non-member
    ) %>% sort()
    return(list("x"=x[keep_samples,,],"y"=y[keep_samples,]))
  }}

fun_create_model_keras_lstm <- function(
  units=128, epochs=5, dropout=0.25, size_hist=15, verbose=1, vars_in=NA, 
  lr=0.001, resample = NA, regularize = NA, convolute=FALSE, hidden.layers=1, 
  validation.split=0.1, seed=1, leaky=FALSE, distributed=FALSE)
{
  model <- list()
  model$vars <- vars_in
  model$units <- units
  model$epochs <- epochs
  model$dropout <- dropout
  model$size_hist <- size_hist
  model$verbose <- verbose
  model$lr <- lr
  model$resample <- resample
  model$fun_resample <- fun_resample_timed
  model$logname <- NA
  model$regularize <- regularize
  model$convolute <- convolute
  model$hidden.layers = hidden.layers
  model$validation.split <- validation.split
  model$scale.center <- TRUE
  model$scale.scale <- TRUE
  model$seed=seed
  model$leaky=leaky
  model$distributed = distributed
  model$transform <- function(data, m){
    t <- data
    t$assignment.cost.agent[t$assignment.cost.agent>5000] <- 5000
    t$assignment.cost.group[t$assignment.cost.group>5000] <- 5000
    t$faces.size[!t$cl.in.group] <- 0
    t$faces.angle.w[!t$cl.in.group] <- 45
    t <- t %>% select(m$vars) %>% data.matrix() %>% scale(center = m$scale.center, scale=m$scale.scale)
    u <- sapply(1:(nrow(t)-(m$size_hist-1)), function(x) t[x:(x+m$size_hist-1),]) %>% t()
    dim(u) <- c(dim(u)[1],m$size_hist,ncol(t))
    return(list("scale.center" = attr(t, "scaled:center"), "scale.scale" = attr(t, "scaled:scale"), "data" = u))
  }
  model$transform_y <- function(data,m){
    t <- data %>% select(role.agent) %>% data.matrix() %>% `-`(1) %>% to_categorical()
    if (m$distributed) {
      u <- sapply(1:(nrow(t)-(m$size_hist-1)), function(x) t[x:(x+m$size_hist-1),]) %>% t()
      dim(u) <- c(dim(u)[1],m$size_hist,4)
      return(u)
    } else {
      # first prediction is last element of first size_hist
      u <- t[(m$size_hist):nrow(t),] 
      return(u)
    }
  }
  model$train <- function(data, m){
    set.seed(m$seed)
    m$roles <- levels(data$role.agent)
    if(anyNA(m$vars)){
      m$vars <- data %>% select(
        faces.size,
        faces.angle.w,
        speaking,
        assignment.cost.agent,
        assignment.cost.group,
        cl.in.group,
        starts_with("landmarks"),
        starts_with("distance")
      ) %>% names()
    }
    train_data_scaled <- m$transform(data, m)
    m$scale.center <- train_data_scaled$scale.center
    m$scale.scale <- train_data_scaled$scale.scale
    train_data_x <- train_data_scaled$data
    train_data_y <- m$transform_y(data,m)
    if(!is.na(m$resample)){
      rs <- m$fun_resample(train_data_x, train_data_y, m$resample)
      train_data_x <- rs$x
      train_data_y <- rs$y
    }
    mod <- Sequential()
    if(m$convolute){
      mod$add(Conv1D(128, c(64), activation='relu', padding='same', input_shape=c(dim(train_data_x)[2],dim(train_data_x)[3])))
      mod$add(MaxPooling1D(pool_size=c(8)))
    }
    reg <- NULL
    reg <- if(!is.na(m$regularize)) { reg <- l2(m$regularize) }
    for (i in seq(1,m$hidden.layers)){
      mod$add(LSTM(m$units, return_sequences = (i != m$hidden.layers) | m$distributed, input_shape=c(dim(train_data_x)[2],dim(train_data_x)[3]),
                   kernel_regularizer=reg, recurrent_regularizer=reg, bias_regularizer=reg))
      if(m$leaky){
        mod$add(LeakyReLU())
      }
      mod$add(Dropout(m$dropout))
    }
    if(m$distributed){
      mod$add(TimeDistributed(Dense(4, kernel_regularizer=reg, bias_regularizer=reg)))
    } else {
      mod$add(Dense(4, kernel_regularizer=reg, bias_regularizer=reg))
    }
    mod$add(Activation("softmax"))
    m$model <- mod
    callbacks <- list()
    if (!is.na(m$logname)){
      callbacks <- list(CSVLogger(m$logname))
    }
    keras_compile(mod,  loss = 'categorical_crossentropy', optimizer = RMSprop(lr=m$lr), metrics=c('categorical_accuracy'))
    keras_fit(mod, train_data_x, train_data_y,
              batch_size = 32, epochs = m$epochs,
              verbose = m$verbose, validation_split = m$validation.split,
              callbacks = callbacks)
    return(m)
  }
  model$continue <- function(data,m,until) {
    set.seed(m$seed)
    assertthat::assert_that(m$epochs < until)
    train_data_x <- (m$transform(data, m))$data
    train_data_y <- m$transform_y(data,m)
    if(!is.na(m$resample)){
      rs <- m$fun_resample(train_data_x, train_data_y, m$resample)
      train_data_x <- rs$x
      train_data_y <- rs$y
    }
    callbacks <- list()
    if (!is.na(m$logname)){
      callbacks <- list(CSVLogger(m$logname, append = TRUE))
    }
    keras_fit(m$model, train_data_x, train_data_y,
              batch_size = 32, epochs = (until-m$epochs),
              verbose = m$verbose, validation_split = m$validation.split, callbacks = callbacks)
    m$epochs <- until
    return(m)
  }
  # predict function
  model$predict <- function(test, m){
    test_data_x <- m$transform(test,m)$data
    test_data_y <- test %>% select(role.agent) %>% data.matrix()
    if (m$distributed){
      test_result_y <- int(keras_predict_classes(m$model, test_data_x)[,m$size_hist]) %>% `+`(1)
      test_result_p <- keras_predict_proba(m$model, test_data_x)[,m$size_hist,]
    } else {
      test_result_y <- int(keras_predict_classes(m$model, test_data_x)) %>% `+`(1)
      test_result_p <- keras_predict_proba(m$model, test_data_x)
    }
    colnames(test_result_p) <- c("speaker","addressee","member","non-member")
    bind_rows(
      lapply(1:(m$size_hist-1), function(x) 
        return(list("speaker"=NA,"addressee"=NA,"member"=NA,"non-member"=NA,"role.predicted"=NA))
      ) %>% bind_rows(),
      test_result_p %>% 
        as_tibble() %>% 
        mutate(role.predicted=recode(
          factor(int(test_result_y),levels=c("1","2","3","4")),
          `1`="speaker", `2`="addressee", `3`="member", `4`="non-member"))
    ) %>% 
      mutate(role.agent=test$role.agent) %>% 
      return()
  }
  return(model)
}

fun_create_model_list_keras <- function(invars, keras_dropout, keras_epochs, keras_lr, keras_units, resample=c(NA), regularize=c(NA), hidden.layers=c(NA), validation.split=0.1, leaky=c(FALSE), seeds=c(1)){
  mname <- "keras"
  lapply(names(invars), function(vname) {
    vars <- invars[[vname]]
    result <- list()
    keras <- lapply(keras_dropout,function(dropout){
      lapply(keras_epochs,function(epochs){
        lapply(keras_lr,function(lr){
          lapply(keras_units,function(units){
            lapply(regularize, function(reg) {
              lapply(resample, function(res) {
                lapply(hidden.layers, function(hidden.layers) {
                  lapply(leaky, function(leak) {
                    lapply(seeds, function(seed) {
                      result <- list()
                      name <- paste(mname,vname,"dropout",dropout,"epochs",epochs,"units",units,"lr",format(lr,scientific = FALSE),sep = "_")
                      if(!is.na(reg)) { name <- paste(name,"regularize",format(reg,scientific = FALSE),sep = "_") }
                      if(!is.na(res)) { name <- paste(name,"resample",res,sep = "_") }
                      if(!is.na(hidden.layers)) { name <- paste(name,"hl",hidden.layers,sep = "_") }
                      name <- paste(name,"leaky",leak,"vsplit",validation.split,"seed",seed,sep = "_")
                      result[name] <- list(fun_create_model_keras(epochs=epochs, verbose=keras_verbose, vars_in = vars, dropout = dropout, units = units, lr = lr, resample = res, regularize = reg, hidden.layers = hidden.layers, validation.split = validation.split, leaky=leak, seed = seed))
                      return(result)
                    }) %>% unlist(recursive = FALSE)
                  }) %>% unlist(recursive = FALSE)
                }) %>% unlist(recursive = FALSE)
              }) %>% unlist(recursive = FALSE)
            }) %>% unlist(recursive = FALSE)
          }) %>% unlist(recursive = FALSE)
        }) %>% unlist(recursive = FALSE)
      }) %>% unlist(recursive = FALSE)
    }) %>% unlist(recursive = FALSE)
    return(append(result,keras))
  }) %>% unlist(recursive = FALSE) %>% return()
}

fun_create_model_list_lstm <- function(invars, keras_dropout, keras_epochs, keras_lr, keras_units, keras_size_hist, resample=c(NA), regularize=c(NA), convolute=c(FALSE), hidden.layers=c(NA), validation.split=0.1, leaky=c(FALSE), seeds=c(1), distributed=c(FALSE)){
  mname <- "lstm"
  lapply(names(invars), function(vname) {
    vars <- invars[[vname]]
    result <- list()
    keras <- lapply(keras_dropout,function(dropout){
      lapply(keras_epochs,function(epochs){
        lapply(keras_lr,function(lr){
          lapply(keras_units,function(units){
            lapply(keras_size_hist, function(size_hist) {
              lapply(regularize, function(reg) {
                lapply(resample, function(res) {
                  lapply(convolute, function(conv) {
                    lapply(hidden.layers, function(hidden.layers) {
                      lapply(leaky, function(leak) {
                        lapply(seeds, function(seed) {
                          lapply(distributed, function(dist) {
                            result <- list()
                            if(conv){
                              mname <- paste(mname,"conv",sep = "")
                            }
                            if(dist){
                              mname <- paste(mname,"dist",sep = "")
                            }
                            name <- paste(mname,vname,"dropout",dropout,"hist",size_hist,"epochs",epochs,"units",units,"lr",format(lr,scientific = FALSE),sep = "_")
                            if(!is.na(reg)) { name <- paste(name,"regularize",format(reg, scientific = FALSE),sep = "_") }
                            if(!is.na(res)) { name <- paste(name,"resample",res,sep = "_") }
                            if(!is.na(hidden.layers)) { name <- paste(name,"hl",hidden.layers,sep = "_") }
                            name <- paste(name,"leaky",leak,"vsplit",validation.split,"seed",seed, sep = "_")
                            result[name] <- list(fun_create_model_keras_lstm(epochs=epochs, verbose=keras_verbose, vars_in = vars, dropout = dropout, size_hist = size_hist, units = units, lr = lr, resample = res, regularize = reg, convolute = conv, hidden.layers = hidden.layers, validation.split = validation.split, leaky = leak, seed = seed, distributed = dist))
                            return(result)
                          }) %>% unlist(recursive = FALSE)
                        }) %>% unlist(recursive = FALSE)
                      }) %>% unlist(recursive = FALSE)
                    }) %>% unlist(recursive = FALSE)
                  }) %>% unlist(recursive = FALSE)
                }) %>% unlist(recursive = FALSE)
              }) %>% unlist(recursive = FALSE)
            }) %>% unlist(recursive = FALSE)
          }) %>% unlist(recursive = FALSE)
        }) %>% unlist(recursive = FALSE)
      }) %>% unlist(recursive = FALSE)
    }) %>% unlist(recursive = FALSE)
    return(append(result,keras))
  }) %>% unlist(recursive = FALSE) %>% return()
}

fun_create_model_list_bn <- function(invars, manual, resample){
  mname <- "bn"
  lapply(resample,function(res){
    lapply(names(manual),function(man){
      if(is.na(manual[[man]])){
        mname <- paste(mname,man,sep = "")
        lapply(names(invars), function(vname) {
          name <- paste(mname,vname,sep = "_")
          if(!is.na(res)) { name <- paste(name,"resample",res,sep = "_") }
          result <- list()
          result[name] <- list(fun_create_model_bn(autovars_in = invars[[vname]], resample = res))
          return(result)
        }) %>% unlist(recursive = FALSE) %>% return()
      } else {
        mname <- paste(mname,"m",sep = "")
        name <- paste(mname,man,sep="")
        if(!is.na(res)) { name <- paste(name,"resample",res,sep = "_") }
        result <- list()
        result[name] <- list(fun_create_model_bn(modelstring = manual[[man]], resample = res))
        return(result)
      }
    }) %>% unlist(recursive = FALSE)
  }) %>% unlist(recursive = FALSE) %>% return()
}

# input variable sets
invars_full <- data_roles_full %>% select(-starts_with("distance."), -starts_with("role."), -time, -focus.speaking, -addressee.focus_speaking, -addressee.mutual_gaze, -num_faces , -face.detected, -cl.in.group, -assignment.cost.agent, -assignment.cost.group) %>% names()
invars_rule <- data_roles_full %>% select(cl.in.group, speaking, starts_with("addressee.")) %>% names()
invars_rule_raw <- data_roles_full %>% select(assignment.cost.agent, speaking, faces.angle.w, focus.speaking) %>% names()
invars <- list(
  "full" = invars_full,
  "rule" = invars_rule,
  "ruleraw" = invars_rule_raw
)

## training and test splits
data_roles_full <- data_roles_full %>% mutate(second=floor((time-min(time))/(1000*num_seconds_in_set)))

# basic rule model
models <- list("rule" = fun_create_model_rule())

# bayesian models
bn_manual <- list(
  "1" = "[role.agent][speaking|role.agent][face.detected][faces.size|face.detected:role.agent][faces.angle.w|face.detected:role.agent][focus.speaking|face.detected:role.agent][assignment.cost.agent|role.agent:cl.in.group][assignment.cost.group|role.agent:cl.in.group][cl.in.group|role.agent]",
  "2" = "[role.agent|face.detected:cl.in.group:speaking][face.detected][cl.in.group][faces.size|role.agent][faces.angle.w|role.agent][speaking][assignment.cost.agent|role.agent]",
  "rulep" = "[role.agent][cl.in.group|role.agent][speaking|role.agent][addressee.mutual_gaze|role.agent][addressee.focus_speaking|role.agent]",
  "rulec" = "[role.agent|cl.in.group:speaking:addressee.mutual_gaze:addressee.focus_speaking][cl.in.group][speaking][addressee.mutual_gaze][addressee.focus_speaking]",
  "rulerawp" = "[role.agent][assignment.cost.agent|role.agent][speaking|role.agent][faces.angle.w|role.agent][focus.speaking|role.agent]",
  "rulerawc" = "[role.agent|speaking][assignment.cost.agent|role.agent][speaking][faces.angle.w|role.agent][focus.speaking|role.agent]",
  "auto" = NA
)
models <- append(
  models,
  fun_create_model_list_bn(
    invars=list("rule" = invars$rule, "ruleraw" = invars$ruleraw),
    manual = bn_manual,
    resample = c(NA, "undersample", "undersample2"))
)

fun_plot_bn_model <- function(model.string, rename_nodes=TRUE){
  if(rename_nodes){
    model.string <- model.string %>% 
      str_replace_all("role.agent","Role") %>% 
      str_replace_all("face.detected","Face Detected") %>% 
      str_replace_all("faces.size","Face Size") %>% 
      str_replace_all("addressee.mutual.gaze","Mutual Gaze") %>% 
      str_replace_all("faces.angle.w","Gaze Angle") %>% 
      str_replace_all("addressee.focus.speaking","Mouth Movement") %>% 
      str_replace_all("focus.speaking","Lip Variance") %>% 
      str_replace_all("speaking","Speech Production") %>% 
      str_replace_all("cl.in.group","In Group") %>% 
      str_replace_all("assignment.cost.agent","Participation Cost") %>% 
      str_replace_all("assignment.cost.group","Assignment Cost") 
  }
  return(graphviz.plot(x = model2network(model.string), highlight = list("nodes"=c("Role")), shape = "rectangle"))
}

# grid search for keras / lstm modelskeras_epochs = c(10,50)
keras_lr = c(0.0001)
keras_units = c(128,512)
keras_dropout = c(0.5)
keras_size_hist = c(15)
keras_resample = c(NA) # undersampling does not help with deep networks
keras_regularize = c(NA)
keras_hl = c(1,4)
keras_epochs = c(5, 10, 20, 30, 40, 50)
keras_seeds = seq(1,8)
models <- append(
  models,
  fun_create_model_list_keras(
    invars = invars, keras_dropout = keras_dropout, keras_epochs = keras_epochs,
    keras_lr = keras_lr, keras_units = keras_units, resample = keras_resample,
    regularize = keras_regularize, hidden.layers = keras_hl, validation.split=0, leaky=c(FALSE),
    seeds=keras_seeds
  )
)
models <- append(
  models,
  fun_create_model_list_lstm(
    invars = invars, keras_dropout = keras_dropout, keras_epochs = keras_epochs,
    keras_lr = keras_lr, keras_units = keras_units, keras_size_hist = keras_size_hist,
    resample = keras_resample, regularize = keras_regularize, convolute = c(FALSE),
    hidden.layers = keras_hl, validation.split=0, leaky=c(FALSE),
    distributed = c(TRUE), seeds=keras_seeds
  )
)
# train promising models a little bit longer
models <- append(
  models,
  fun_create_model_list_keras(
    invars = invars["full"], keras_dropout = keras_dropout, keras_epochs = seq(60,500,10),
    keras_lr = keras_lr, keras_units = c(128), resample = keras_resample,
    regularize = keras_regularize, hidden.layers = c(1), validation.split=0, leaky=c(FALSE),
    seeds=keras_seeds
  )
)
models <- append(
  models,
  fun_create_model_list_lstm(
    invars = invars["full"], keras_dropout = keras_dropout, keras_epochs = seq(60,500,10),
    keras_lr = keras_lr, keras_units = c(512), keras_size_hist = keras_size_hist,
    resample = keras_resample, regularize = keras_regularize, convolute = c(FALSE),
    hidden.layers = c(1), validation.split=0, leaky=c(FALSE),
    distributed = c(TRUE), seeds=keras_seeds
  )
)

fun_run_model_test <- function(model, data=data_roles_full, set=5, table=TRUE){
  train <- data %>% filter(second != set)
  test <- data %>% filter(second == set)
  trained <- model$train(train, model)
  result <- trained$predict(test, trained)
  if(table){
    return(result %>% select(role.agent, role.predicted) %>% table())
  } else {
    return(result)
  }
}

fun_run_model_test_full <- function(model, data=data_roles_full){
  lapply(unique(data$second), function(x) {
    fun_run_model_test(model,data=data,set=x,table=FALSE)
  }) %>% bind_rows() %>% select(role.agent, role.predicted) %>% table() %>% return()
}

roles_eval_configs <- lapply(unique(data_roles_full$second), function(x) {
  print(paste("creating models for set",x))
  lapply(names(models), function(modelname) {
    model <- models[[modelname]]
    set_number <- x
    filename <- paste("data-role-cv/study-role-cv-",x,"-",num_seconds_in_set,"-",modelname,"-data.RData",sep="")
    if(file.exists(filename)){
      print(paste("skip creation of",filename,"as it already exists"))
    } else {
      print(paste("saving model in",filename))
      save(file=filename, list=c("num_seconds_in_set","model","set_number","modelname"))
    }
    return(list("set"=x,"sets"=num_seconds_in_set,"model"=modelname))
  }) %>% bind_rows() %>% return()
}) %>% bind_rows()

save(file="study-role-data-full.RData", 
     list=c(
       "data_roles_full", 
       "roles_eval_configs",
       "num_seconds_in_set"
     ))
