## required libraries
fun_install_required_packages <- function(){
  install.packages('readr')
  install.packages('tidyverse')
  install.packages('ggplot2')
  install.packages('reshape2')
  install.packages('irr')
  install.packages('tikzDevice')
  install.packages('rcompanion')
  install.packages('fancycut')
  install.packages('mlr')
  install.packages('FSelector')
  install.packages('bnlearn')
  install.packages('kernlab')
  source('http://bioconductor.org/biocLite.R')
  biocLite('Rgraphviz')
  biocLite('gRain')
  install.packages('ggsignif')
}

library(readr)
library(reshape2)
library(irr)
library(rcompanion)
library(rlang)
library(tidyverse)
library(stats)
library(fancycut)
library(mlr)
library(FSelector)
library(bnlearn)
library(Rgraphviz)
library(gRain)
library(parallelMap)
library(ggsignif)
library(lubridate)

datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)

# meta switch
draft = FALSE
write_out = FALSE
use_cores = detectCores(all.tests = FALSE, logical = TRUE)

if (Sys.getenv("CALCULATE_NONDRAFT") == "ON"){
  draft = FALSE
}
if (Sys.getenv("WRITE_OUT") == "ON"){
  write_out = TRUE
}
if (Sys.getenv("USE_CORES") != ""){
  use_cores = as.integer(Sys.getenv("USE_CORES"))
}

fun_assert_equals_print <- function(x,y, msg="Unexpected amount of entries. got: %s != %s expected.", pre=''){
  assertthat::assert_that(x==y,msg=sprintf(paste(pre,msg), x, y))
}

fun_load_data <- function() {
  ## read the data
  result <- read_tsv("study-meka.csv",
                     col_types = cols(
                       .default = col_character(),
                       start_time = col_integer(),
                       trial = col_factor(),
                       filename = col_factor(),
                       start_time = col_integer(),
                       date = col_datetime(format="%Y-%m-%d_%H-%M-%S")
                     ))
  return(result)
}

fun_clean_data <- function(data) {
  gaze_addressee_filter <- "( |lightOff|negation|nameRequest|timeRequest|falsche|[Pp]erson|packetRequest|lightOn|callRequest|dataRequest|gardenRequest|experimentsRequest|-)"
  meka_addressee_filter <- "( |-|schaut|falsche|falsch|[Pp]erson|an)"
  ## fix some annotations
  data %>%  
    ## ignore cases when all/dialogact is na. can no work with them
    filter(!is.na(`/meka/eval/all/dialogact/`)) %>%
    # na are all before/after trial when the addressee recognition is not working
    filter(!is.na(`/meka/addressee-meka/scatter/Viewing_Person0_Meka/`)) %>%
    # confirmations/negations are ignored either way and therefore not annotated
    filter(!(`/meka/eval/all/dialogact/` %in% c('confirmation','negation','ok'))) %>% 
    ##  entries and create new columns
    mutate(
      # nas in robot-speaking mean n
      `robot-speaking` = replace_na(`robot-speaking`,"n"),
      # falsch in an annotation means robot looks at the wrong person
      `an.wrong.person` = 
        str_match(`meka-addressee`,"falsch") == "falsch" | 
        str_match(`gaze/addressee`,"falsch") == "falsch" | 
        str_match(`mouth/addressee`,"falsch") == "falsch",
      `an.wrong.person` = replace_na(`an.wrong.person`,FALSE),
      # na in mouth/addresee means yes
      `mouth/addressee` = replace_na(`mouth/addressee`,"y"),
      # remove clutter from gaze/addressee, na means yes
      `gaze/addressee` = str_replace_all(`gaze/addressee`,gaze_addressee_filter,""),
      `gaze/addressee` = replace_na(`gaze/addressee`,"y"),
      # remove clutter from meka-addressee
      `meka-addressee` = str_replace_all(`meka-addressee`,meka_addressee_filter,""),
      # deduce new columns
      `dialogact.id` = coalesce(`/meka/dialogact/dialogActInformer/`,`/meka/rejected/dialogact/dialogActInformer/`),
      `dialogact` = as.factor(str_replace_all(`/meka/eval/all/dialogact/`,"nameRequest ","")),
      `an.addressed` = `meka-addressee` == "y",
      `an.mutualgaze` = `gaze/addressee` == "y",
      `an.mouthmovement` = `mouth/addressee` == "y",
      `an.robotspeaking` = `robot-speaking` == "y",
      `cl.addressed` = `/meka/addressee-meka/scatter/Addressee/`=='Meka',
      `cl.mouthmovement`  = !is.na(`/meka/eval/mouth/dialogact/`),
      `cl.mutualgaze` = !is.na(`/meka/eval/gaze/dialogact/`),
      # create human readable names for dialog acts
      `h.dialogact` = fct_recode(`dialogact`,
        "Data Request"       = "dataRequest"       ,
        "Light On"           = "lightOn"           ,
        "Time Request"       = "timeRequest"       ,
        "Parcel Request"     = "packetRequest"     ,
        "Exhibits request"   = "experimentsRequest",
        "Light Off"          = "lightOff"          ,
        "Garden Request"     = "gardenRequest"     ,
        "Call Request"       = "callRequest"          
        ),
      start_time = dmilliseconds(start_time)
    ) %>% # drop irrelevant and no more relevant columns
  select(
    "dialogact.id",
    "dialogact",
    "h.dialogact",
    "an.addressed",
    "an.mutualgaze",
    "an.mouthmovement",
    "an.robotspeaking",
    "an.wrong.person",
    "cl.addressed",
    "cl.mouthmovement",
    "cl.mutualgaze",
    "trial",
    "start_time",
    "filename",
    "date"
    ) %>%
    return(.)
}

fun_write_data_filtered <- function(data, ... ){
  write_csv(dplyr::arrange(data, trial, start_time), path='data.all_study-meka.csv')
}

fun_calc_precision_recall <- function(cm, correct=0) {
  # catch cases where the confusion matrix is incomplete
  fp <- correct
  tp <- correct
  tn <- correct
  fn <- correct
  if (("TRUE" %in% rownames(cm)) & ("TRUE" %in% colnames(cm))) {
    tp <- tp + cm["TRUE","TRUE"]
  }
  if (("TRUE" %in% rownames(cm)) & ("FALSE" %in% colnames(cm))) {
    fp <- fp + cm["TRUE","FALSE"]
  }
  if (("FALSE" %in% rownames(cm)) & ("TRUE" %in% colnames(cm))) {
    fn <- fn + cm["FALSE","TRUE"]
  }
  if (("FALSE" %in% rownames(cm)) & ("FALSE" %in% colnames(cm))) {
    tn <- tn + cm["FALSE","FALSE"]
  }
  result = list()
  result$tp <- tp
  result$fp <- fp
  result$tn <- tn
  result$fn <- fn
  result$sum=sum(cm)
  result$sum_pos <- sum(tp+fn)
  result$sum_neg <- sum(tn+fp)
  result$sum_pred_pos <- sum(tp+fp)
  result$sum_pred_neg <- sum(tn+fn)
  result$prevalence <- result$sum_pos / result$sum
  # rates with upper and lower intervals
  assign_without_ci <- function(r,val,name){
      r[name] <- val
      r[paste(name,"_lower",sep="")] <- NA
      r[paste(name,"_upper",sep="")] <- NA
      return(r)
  }
  assign_with_ci <- function(r,num,sum,name){
    if (sum > 0) {
      r[name] <- num/sum
      test <- binom.test(num,sum)
      r[paste(name,"_lower",sep="")] <- test$conf.int[1]
      r[paste(name,"_upper",sep="")] <- test$conf.int[2]
    } else {
      r[name] <- NA
      r[paste(name,"_lower",sep="")] <- NA
      r[paste(name,"_upper",sep="")] <- NA
    }
    return(r)
  }
  result <- result %>%
  assign_with_ci(result$tp,result$sum_pos,"tpr") %>% 
  assign_with_ci(result$fn,result$sum_pos,"fnr") %>%
  assign_with_ci(result$fp,result$sum_neg,"fpr") %>%
  assign_with_ci(result$tn,result$sum_neg,"tnr") %>%
  assign_with_ci(result$tp,result$sum_pred_pos,"ppv") %>%
  assign_with_ci(result$fp,result$sum_pred_pos,"fdr") %>%
  assign_with_ci(result$fn,result$sum_pred_neg,"for") %>%
  assign_with_ci(result$tn,result$sum_pred_neg,"npv") %>%
  assign_with_ci((result$tp + result$tn),result$sum,"accuracy")
  #
  result <- result %>% assign_without_ci(2*((result$ppv*result$tpr)/(result$ppv+result$tpr)),"f1") %>%
  assign_without_ci(result$tpr/result$fpr,"lr_plus") %>%
  assign_without_ci(result$fnr/result$tnr,"lr_minus")
  result <- result %>% assign_without_ci(result$lr_plus/result$lr_minus,"dor") %>%
  #log_dor_ci = 1.96*sqrt(1/tp + 1/fn + 1/fp + 1/fn)
  #result$dor_upper <- exp(log(result$dor)+log_dor_ci)
  #result$dor_lower <- exp(log(result$dor)-log_dor_ci)
  # further measurements
  assign_without_ci((result$ppv + result$npv - 1),"markedness") %>%
  assign_without_ci((result$tpr + result$tnr - 1),"informedness")
  # additional names
  result$precision <- result$ppv
  result$precision_lower <- result$ppv_lower
  result$precision_upper <- result$ppv_upper
  result$recall <- result$tpr
  result$recall_lower <- result$tpr_lower
  result$recall_upper <- result$tpr_upper
  # confidence intervals
  return(result)
}

fun_long_format_precision_recall <- function(pr_result, scale_dor=1.) {
  var_set = c("precision", "recall", "accuracy", "f1", "markedness", "informedness", "dor")
  #var_set = c("precision", "recall", "accuracy", "f1", "dor", "tpr", "fnr", "fpr", "tnr", "ppv", "fdr", "for", "npv", "lr_plus", "lr_minus")
  result <- tribble(~value_type, ~value, ~lower, ~upper)
  for (i in var_set) {
    factor = 1.0
    vt <- i
    if (i == "dor") { 
      factor = scale_dor
      vt = "dor_scaled"
    }
    result <- add_row(result, 
                      value_type=vt, 
                      value=factor*pr_result[[i]],
                      lower=factor*pr_result[[paste(i,"_lower",sep="")]],
                      upper=factor*pr_result[[paste(i,"_upper",sep="")]])
  }
  result <- result %>% mutate(
      "tp" = pr_result[["tp"]], 
      "fp" = pr_result[["fp"]], 
      "tn" = pr_result[["tn"]], 
      "fn" = pr_result[["fn"]], 
      "sum" = pr_result[["sum"]], 
      "sum_pos" = pr_result[["sum_pos"]], 
      "sum_neg" = pr_result[["sum_neg"]], 
      "sum_pred_pos" = pr_result[["sum_pred_pos"]], 
      "sum_pred_neg" = pr_result[["sum_pred_neg"]], 
      "dor_factor" = scale_dor, 
      "prevalence" = pr_result[["prevalence"]]
      ) %>% return(.)
}

fun_create_model_confusion_matrix <- function(p, observations, type="non_equal") {
  if (type=="equal") {
    # draw with same distribution as passed
    positive = round(observations*p)
    negative = observations - positive
    tp = round(positive*p)
    fp = positive-tp
    tn = round(negative*p)
    fn = negative-tn
    return(matrix(c(tn,fp,fn,tp),nrow=2, dimnames=list(list(FALSE,TRUE),list(FALSE,TRUE))))
  } else {
    # all positive. result according to distribution.
    tp = observations*p
    fp = observations-tp
    return(matrix(c(0,fp,0,tp),nrow=2, dimnames=list(list(FALSE,TRUE),list(FALSE,TRUE))))
  }
}

# read data
info <- list()
data_raw <- fun_load_data()
fun_assert_equals_print(dim(data_raw)[1],841,pre='initial entries')
data <- fun_clean_data(data_raw)
fun_assert_equals_print(dim(data)[1],176,pre='initial entries')

# write info
info[['sum.dialogacts']] <- nrow(data_raw) 
info[['sum.entries']] <- nrow(data)
info[['prev.mouthmovements']] <- round(sum(data$an.mouthmovement)/nrow(data),3)
info[['prob.mouthmovements']] <- round(sum(data$an.mouthmovement)/nrow(data),3)*100
info[['prev.mutualgaze']] <- round(sum(data$an.mutualgaze)/nrow(data),3)
info[['prob.mutualgaze']] <- round(sum(data$an.mutualgaze)/nrow(data),3)*100
info[['prev.addressed']] <- round(sum(data$an.addressed)/nrow(data),3)
info[['prob.addressed']] <- round(sum(data$an.addressed)/nrow(data),3)*100

correct_tables = 0
baseline_model_type="non_equal" # "non_equal" or "equal"
fun_create_eval_model_and_baseline <- function(result, gt, dor_factor) {
  pr <- fun_calc_precision_recall(table(result,gt), correct=correct_tables)
  r1 <- fun_long_format_precision_recall(pr, scale_dor = dor_factor)
  r2 <- fun_long_format_precision_recall(fun_calc_precision_recall(fun_create_model_confusion_matrix(pr$prevalence,pr$sum, type=baseline_model_type), correct = correct_tables), scale_dor=dor_factor)
  return(bind_rows(r1 %>% mutate(base="Study"), r2 %>% mutate(base="Accept-All")))
}
#data <- data %>% filter(an.wrong.person==TRUE)
cl_performance <- 
  bind_rows(
    fun_create_eval_model_and_baseline(data$cl.mouthmovement,data$an.mouthmovement, 1/50) %>% mutate(cl="mouthmovement"),
    fun_create_eval_model_and_baseline(data$cl.mutualgaze,data$an.mutualgaze, 1/50) %>% mutate(cl="mutualgaze"),
    fun_long_format_precision_recall(fun_calc_precision_recall(table(data$cl.mutualgaze,data$an.addressed), correct = correct_tables), scale_dor = 1/120) %>% mutate(base="Recognition", cl="mutualgaze-addressee"),
    fun_long_format_precision_recall(fun_calc_precision_recall(table(data$an.mutualgaze,data$an.addressed), correct = correct_tables), scale_dor = 1/120) %>% mutate(base="Annotation", cl="mutualgaze-addressee")
    ) %>% 
  mutate(
    value_type=as_factor(value_type),
    value_type=recode(value_type,
                      `precision`="Prec.", 
                      `recall`="Rec.", 
                      `accuracy`="Acc.", 
                      `f1`="F1", 
                      `dor_scaled`="DOR",
                      `markedness` = "Marked.",
                      `informedness` = "Inform."
                      )
  )

# write some info about the models
for (row in 1:nrow(cl_performance)) {
  name <- paste('model',cl_performance[[row,"cl"]],cl_performance[[row,"base"]],cl_performance[[row,"value_type"]],sep=".")
  value <- cl_performance[[row,"value"]]
  if (cl_performance[[row,"value_type"]]=="DOR") {
    value <- value / cl_performance[[row,"dor_factor"]]
  }
  info[[name]] <- round(value,2)
  if(cl_performance[[row,"value_type"]] %in% c("Prec.", "Rec.", "Acc.")) {
    info[[paste(name,"100",sep="")]] <- round(value,2)*100
  }
}

# eval addressee classification
data_eval_sets = list(
  "Classification" = data %>% 
  mutate(
    an.addressed=factor(an.addressed, levels=c("TRUE","FALSE")),
    an.robotspeaking=factor(an.robotspeaking, levels=c("TRUE","FALSE")),
    an.wrong.person=factor(an.wrong.person, levels=c("TRUE","FALSE")),
    cl.mouthmovement=factor(cl.mouthmovement, levels=c("TRUE","FALSE")),
    cl.mutualgaze=factor(cl.mutualgaze, levels=c("TRUE","FALSE"))
  ) %>%
  select(h.dialogact,
         an.addressed,
         an.robotspeaking,
         an.wrong.person,
         cl.mouthmovement,
         cl.mutualgaze,
         trial),
  "Annotation" = data %>% 
  mutate(
    an.addressed=factor(an.addressed, levels=c("TRUE","FALSE")),
    an.robotspeaking=factor(an.robotspeaking, levels=c("TRUE","FALSE")),
    an.wrong.person=factor(an.wrong.person, levels=c("TRUE","FALSE")),
    cl.mouthmovement=factor(an.mouthmovement, levels=c("TRUE","FALSE")),
    cl.mutualgaze=factor(an.mutualgaze, levels=c("TRUE","FALSE"))
  ) %>%
  select(h.dialogact,
         an.addressed,
         an.robotspeaking,
         an.wrong.person,
         cl.mouthmovement,
         cl.mutualgaze,
         trial)
)


# hand made bayes
bn_models = list(
  "Mouth" =      model2network("[h.dialogact][cl.mutualgaze][cl.mouthmovement|an.addressed][an.wrong.person][an.addressed][an.robotspeaking]"),
  "Gaze" =       model2network("[h.dialogact][cl.mutualgaze|an.addressed][cl.mouthmovement][an.wrong.person][an.addressed][an.robotspeaking]"),
  "Both" =       model2network("[h.dialogact][cl.mutualgaze|an.addressed][cl.mouthmovement|an.addressed][an.wrong.person][an.addressed][an.robotspeaking]"),
  "Both+Self" =  model2network("[h.dialogact][cl.mutualgaze|an.addressed][cl.mouthmovement|an.addressed][an.wrong.person][an.addressed][an.robotspeaking|an.addressed]"),
  "All" =        model2network("[h.dialogact|an.addressed][cl.mutualgaze|an.addressed][cl.mouthmovement|an.addressed][an.wrong.person][an.addressed][an.robotspeaking|an.addressed]")
)

set.seed(1)
result <- lapply(names(data_eval_sets), function(data_eval_name) {
  data_eval <- data_eval_sets[[data_eval_name]]
  lapply(names(bn_models), function (bn_model_name) {
    bn_model <- bn_models[[bn_model_name]]
    predictions <- lapply(c("1","2","3","4","5"), function(trial) {
      train.set <- data_eval[data_eval$trial!=trial,] %>% select(-trial)
      test.set <- data_eval[data_eval$trial==trial,] %>% select(-trial)
      fitted <- bn.fit(bn_model, data = data.frame(train.set), method='bayes')
      prediction <- predict(
        fitted,
        data=data.frame(test.set),
        node="an.addressed",method="bayes-lw", 
        prob = TRUE
        )
      test.set %>% 
        mutate(trial=trial,
              predicted.true=attributes(prediction)$prob[1,],
              predicted.false=attributes(prediction)$prob[2,]
              ) %>%
        return(.)
    }) %>% bind_rows()
    roc <- lapply(seq(from=0,to=1,length.out=500), function (threshold) {
      threshold_prediction <- predictions %>% mutate(pt=predicted.true>threshold)
      tb <- table(threshold_prediction$pt, threshold_prediction$an.addressed)
      val <- fun_calc_precision_recall(tb)
      val$threshold <- threshold
      return(val)
    }) %>% bind_rows() %>% mutate(model = bn_model_name)
  }) %>% bind_rows() %>% mutate(data = data_eval_name)
}) %>% bind_rows()
set.seed(NULL)

# collect data: study models gaze, mouth, mouth & gaze, mouth | gaze tpr / fpr
base_result <- bind_rows(
  append(list("data"="Classification", "model"="Gaze"),   fun_calc_precision_recall(table(data$cl.mutualgaze,data$an.addressed), correct = 0)),
  append(list("data"="Classification", "model"="Mouth"),  fun_calc_precision_recall(table(data$cl.mouthmovement,data$an.addressed), correct = 0)),
  append(list("data"="Classification", "model"="Both"),   fun_calc_precision_recall(table(data$cl.mouthmovement & data$cl.mutualgaze,data$an.addressed), correct = 0)),
  append(list("data"="Classification", "model"="Either"), fun_calc_precision_recall(table(data$cl.mouthmovement | data$cl.mutualgaze,data$an.addressed), correct = 0)),
  append(list("data"="Annotation",     "model"="Gaze"),   fun_calc_precision_recall(table(data$an.mutualgaze,data$an.addressed), correct = 0)),
  append(list("data"="Annotation",     "model"="Mouth"),  fun_calc_precision_recall(table(data$an.mouthmovement,data$an.addressed), correct = 0)),
  append(list("data"="Annotation",     "model"="Both"),   fun_calc_precision_recall(table(data$an.mouthmovement & data$an.mutualgaze,data$an.addressed), correct = 0)),
  append(list("data"="Annotation",     "model"="Either"), fun_calc_precision_recall(table(data$an.mouthmovement | data$an.mutualgaze,data$an.addressed), correct = 0))
) %>% mutate(model=factor(model,c("Mouth","Gaze","Either","Both")))

fun_calc_auc <- function(xdata,ydata){
  cleaned <- tibble(xdata,ydata) %>% arrange(xdata) %>% filter(!is.na(xdata)) %>% group_by(xdata) %>% summarise(val=mean(ydata))
  calc_area <- function (i) {
    x1 <- 0
    y1 <- 0
    if (i == 1) {
      x1 <- 0
      y1 <- min(cleaned[,2])
      x2 <- cleaned[[i,1]]
      y2 <- cleaned[[i,2]]
    } else if (i <= nrow(cleaned)) {
      x1 <- cleaned[[i-1,1]]
      y1 <- cleaned[[i-1,2]]
      x2 <- cleaned[[i,1]]
      y2 <- cleaned[[i,2]]
    } else {
      x1 <- cleaned[[i-1,1]]
      y1 <- cleaned[[i-1,2]]
      x2 <- 1
      y2 <- max(cleaned[,2])
    }
    imin <- (x2-x1)*y1
    imax <- (x2-x1)*y2
    itriangle <- (imax-imin)/2
    #print(c("vars",i,x1,x2,y1,y2))
    #print(c("vals",i,imin,imax,itriangle))
    return(imin+itriangle)
  }
  sapply(1:(nrow(cleaned)+1), calc_area) %>% 
    sum() %>%
    return()
}

#write plots to files
fun_write_all_out <- function(){
  fun_write_data_filtered(data)
}
if(write_out) { 
  fun_write_all_out() 
}
if(!draft) {
  # save workspace
  save.image(file="study-meka-workspace.RData")
}
if (FALSE) {
 load("study-meka-workspace.RData")
}
