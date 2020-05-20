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
  source('http://bioconductor.org/biocLite.R')
  biocLite('Rgraphviz')
  biocLite('gRain')
  install.packages('ggsignif')
}

library(readr)
library(reshape2)
library(irr)
library(rcompanion)
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

## fürFragebögen
## 1 internal consistency
## - zwischen den beobachtungen
## cronbachs alpha für test ob mehrere Fragen zusammengefasst wreden können (paket psych)
## - Daten aufbereiten in long format
## - visueller check ob daten normalverteilt sind (qqnorm & qqline)
## - shappiro.test auf normalverteilung
## - wenn normalverteilt -> darf t-test angewendet werden (t.test)
## apa-norm zum aufschreiben von t-test result \emph(t)(df-val) t-value, p < p-value
## effektstärke über cohens d

# wenn nicht normalverteilt
## wilcox.test 
## apa-norm zum aufschreben \emph(W)=95, p < 0.01

setwd("~/sync/projects/publications/diss/data")

# meta switch
draft = TRUE
load_rf = FALSE
draft_rf = FALSE
write_out = FALSE
use_cores = detectCores(all.tests = FALSE, logical = TRUE)
if (Sys.getenv("CALCULATE_NONDRAFT") == "ON"){
  draft = FALSE
}
if (Sys.getenv("DRAFT_RF") == "ON"){
  draft_rf=TRUE
}
if (Sys.getenv("LOAD_RF") == "ON"){
  load_rf=TRUE
}
if (Sys.getenv("WRITE_OUT") == "ON"){
  write_out = TRUE
}
if (Sys.getenv("USE_CORES") != ""){
  use_cores = as.integer(Sys.getenv("USE_CORES"))
}

# some config parameters
max_time_seconds = 2
chi.square.replicates = ifelse(draft,2000,100000)
tiers_metainfo = c(
  "Vp_num",
  "Wizard task",
  "Condition",
  "Order",
  "Annotator"
)
tiers_annotated = c(
  "Addressee final",
  "Wizard addressee",
  "Focus of attention",
  "Method",
  "Expression (facial, gestural, verbal)",
  "Speech form of address",
  "Speech politeness",
  "Speech type of sentence",
  "Speech specific",
  "Expression specific",
  "Method specific"
)
tiers_freeform = c(
  "Speech specific",
  "Expression specific",
  "Method specific"
)
tiers_deduced = c(
  "Time_adapted",
  "Addressee final (reduced)",
  "Focus of attention (reduced)"
)
tiers_ignore = c(
  "Speech intention",
  "Study progress (coarse)",
  "Study progress (detailed)",
  "Robot speech",
  "Apartment time",
  "Apartment door state",
  "Apartment call",
  "Cupboard door state",
  "Cupboard handle sound",
  "Displayed Text",
  "Cupboard handle light",
  "Apartment parcel",
  "Radio",
  "Robot gesture",
  "Filename",
  "Wizard"
)
tiers_irr = tiers_annotated %>% setdiff(tiers_freeform) %>% setdiff(c('Wizard addressee'))

fun_inter_rater_classification <- function(aggreement){
## according to altman:
  agr <- list(
    list(0.21,'poor'),
    list(0.41,'fair'),
    list(0.61,'moderate'),
    list(0.81,'good'),
    list(1.0,'very good')
    )
  for(i in agr){
    if (aggreement <= i[1]){ return(i[2]) }
  }
  assertthat::assert_that(FALSE,msg=sprintf('unexpected agreement of %f',aggreement))
}

fun_assert_equals_print <- function(x,y, msg="Unexpected amount of entries. got: %s != %s expected.", pre=''){
  assertthat::assert_that(x==y,msg=sprintf(paste(pre,msg), x, y))
}

fun_load_data <- function() {
  ## read the data
  result <- read_tsv("study-addressing-apartment.csv",
                     col_types = cols(
                       .default = col_character(),
                       Time_adapted = col_integer(),
                       Vp_num = col_integer(),
                       Order = col_integer(),
                       `Wizard task` = col_integer()
                     ))
  fun_assert_equals_print(dim(result)[1],437,pre='initial entries')
  return(result)
}

fun_create_addressees_table <- function(data, ...) {
  sadr <- c(
    summary(data$`Addressee final Code`),
    summary(data$`Addressee final (reduced) Code`)["Ap"])
  sfoa <- c(
    summary(data$`Focus of attention Code`),
    summary(data$`Focus of attention (reduced) Code`)["Ap"])
  addressees <- tibble(
    Addressee = names(sadr),
    Addressed = sadr,
    Attention = sfoa,
    Reduced = names(sadr) %in% names(summary(data$`Addressee final (reduced) Code`)))
  addressees <- dplyr::arrange(addressees, desc(Addressed))
  return(addressees) 
}

fun_write_csv_addressees <- function(data, ... ){
  write_csv(data, path='addressees-counted_study-addressing-apartment.csv')
  return(data)
}

fun_write_data_filtered <- function(data, ... ){
  write_csv(dplyr::arrange(data, Vp_num, `Wizard task`), path='data.all_study-addressing-apartment.csv')
}

fun_filter_wizard_error <- function(data, ...){
  remove <- tibble::tibble(v=integer(),w=integer())
  remove <- add_row(remove,v=25, w=1) # triggered without action
  remove <- add_row(remove,v=57, w=3) # wizard addressee is none
  result <- data
  for (i in 1:nrow(remove)) {
    n = nrow(result)
    result <- filter(result,!(Vp_num==remove$v[i] & `Wizard task` == remove$w[i]))
    fun_assert_equals_print(nrow(result), n-1, pre=sprintf('filtering vp %d task %d from data.',remove$v[i],remove$w[i]))
  }
  return(result)
}

fun_fix_annotations <- function(data, ... ){
  data$Method[data$Vp_num==48 & data$`Wizard task`==1] <- 'gesture' # pointed and asked a question
  data$Method[data$Vp_num==63 & data$`Wizard task`==2] <- 'speech' # touched screen but told to switch
  data$Method[data$Vp_num==63 & data$`Wizard task`==7] <- 'touch' # multiple touches
  data$`Speech specific`[data$Vp_num==21 & data$`Wizard task`==4] <- 'ist ein Paket für mich angekommen?' # not annotated for some reason
  data[data$`Expression (facial, gestural, verbal)`=='unobserved',]$`Expression (facial, gestural, verbal)` <- 'neutral' # neutral expressions are not annotated
  return(data)
}

fun_filter_time <- function(data, ... ){
  # remove all entries where the time between annotation and wizard action is too high (>2s)
  ## Why? Because the time in between is too high. This happens mostly when Wizard makes an error
  ## or the participant does a task repeatedly and the wizard cannot keep up
  # remove all entries where Time_delta is < 0
  ## these are wizard actions without a solution attempt and therefore errors 
  result <- data[data$Time_adapted<max_time_seconds*1000 & data$Time_adapted>=0,]
  assertthat::assert_that(dim(result)[1]==dim(data)[1]-30, msg='Unexpected amount of entries.')
  return(result)
}

fun_filter_addressee_wizard_error <- function(data, ... ){
  ## there is one case where wizard adressee is none, this is a wizard error
  result <- data[data$`Wizard addressee` != 'None',]
  fun_assert_equals_print(dim(result)[1],dim(data)[1]-1,pre = 'filter wizard addressee na')
  return(result)
}

fun_filter_addressee_na <- function(data, ... ){
  # remove all entries where addressee final or attention is na
  ## there are some cases without such annotations because of failed interactions
  result <- data[data$`Addressee final` != 'Not discernible',]
  fun_assert_equals_print(dim(result)[1],dim(data)[1]-9,pre = 'filter addressee na')
  return(result)
}

fun_filter_corrected <- function(data, ... ){
  # replace all entries by the corrected entries if they exist
  ## There is one correction on annotation vp-49, annotator ES
  assertthat::assert_that(dim(data[data$Annotator=='PH_korr',])[1]==0, msg='Expect no corrections of PH.')
  es_korr_vp <- unique(data[data$Annotator=='ES_korr',]$Vp_num)
  assertthat::assert_that(es_korr_vp==49, msg='Expect vp 49 correction of ES.')
  ## remove all entries with corrections
  result = data[!(data$Vp_num %in% es_korr_vp & data$Annotator=='ES'),]
  ## remove the korr from annotators name
  result[result$Annotator=='ES_korr',]$Annotator <- replicate(sum(result$Annotator=='ES_korr'),'ES')
  assertthat::assert_that(dim(result)[1]==dim(data)[1]-6, msg='Unexpected amount of entries.')
  return(result)
}

fun_filter_only_both_annotators <- function(data, ... ) {
  data_ph = data[data$Annotator=='PH',]
  data_es = data[data$Annotator=='ES',]
  vpns_both_annotators = intersect(unique(data_es$Vp_num), unique(data_ph$Vp_num))
  data_both_annotators = filter(data,Vp_num %in% vpns_both_annotators)
  return(data_both_annotators)
}

fun_filter_remove_paired_data <- function(data, filter_expect = c(7,8,6,24,9,6,23), ... ){
  # filter_expect are the expected count of removed task solutions
  # for each combination of wizard_task and vp_num keep only the first
  result = data[0,]
  for (task in 1:7){
    data_task <- data[data$`Wizard task` == task,]
    entries <- dim(data_task)[1]
    data_task <- data_task[c(TRUE, diff(data_task$Vp_num)) != 0,]
    fun_assert_equals_print(dim(data_task)[1],entries-filter_expect[task],pre=sprintf('remove paired in task %d',task))
    result <- full_join(result,data_task,by=names(result))
  }
  assertthat::assert_that(dim(result)[1]==dim(data)[1]-sum(filter_expect), msg='Unexpected amount of entries.')
  return(result)
}

fun_create_inter_rater_table <- function(data, columns, ... ) {
  data_es = fun_filter_remove_paired_data(filter(data,Annotator == 'ES'), filter_expect=c(0,0,0,0,1,0,4))
  data_ph = fun_filter_remove_paired_data(filter(data,Annotator == 'PH'), filter_expect=c(0,0,0,0,1,0,4))
  result = tibble::tibble(es = factor(), ph=factor())
  for (task in unique(data$`Wizard task`)){
    for (vpnum in unique(data$Vp_num)){
      entry_es = filter(data_es,`Wizard task`==task,`Vp_num`==vpnum)
      entry_ph = filter(data_ph,`Wizard task`==task,`Vp_num`==vpnum)
      if ((dim(entry_es)[1] + dim(entry_ph)[1]) == 0) {
      } else if(dim(entry_es)[1] == dim(entry_ph)[1] && dim(entry_ph)[1] > 0) {
        fun_assert_equals_print(dim(entry_es)[1],1)
        for (r in columns) {
          result = add_row(result,ph=entry_ph[[r]],es=entry_es[[r]])
        }
      } else {
        if (dim(entry_ph)[1] > 0) {
          for (r in columns) {
            result = add_row(result,ph=entry_ph[[r]],es='not-existing')
          }
        } else {
          for (r in columns) {
            result = add_row(result,ph='not-existing',es=entry_es[[r]])
          }
        }
      }
    }
  }
  return(result)
}

fun_make_factors <- function(data, ... ) {
  helper_make_factor <- function(data, cols) {
    values <- stack(select(data,cols))
    values <- sort(unique(values$values))
    for (i in cols) {
      data[[i]] <- factor(data[[i]], levels = values)
    }
    return(data)
  }
  data <- helper_make_factor(data,c('Addressee final', 'Focus of attention'))
  data <- helper_make_factor(data,c('Addressee final (reduced)', 'Focus of attention (reduced)'))
  data <- helper_make_factor(data,c('Wizard addressee'))
  data <- helper_make_factor(data,c('Wizard task'))
  data <- helper_make_factor(data,c('Vp_num'))
  data <- helper_make_factor(data,c('Order'))
  data <- helper_make_factor(data,c('Method'))
  data <- helper_make_factor(data,c('Expression (facial, gestural, verbal)'))
  data <- helper_make_factor(data,c('Speech form of address'))
  data <- helper_make_factor(data,c('Speech politeness'))
  data <- helper_make_factor(data,c('Speech type of sentence'))
  data <- helper_make_factor(data,c('Condition'))
  data <- helper_make_factor(data,c('Annotator'))
  fct_recode(data$`Addressee final`,L_F='Floor lamp \\(L_F\\)')
  return(data)
}

fun_reduce_addressees <- function(data) {
  mapping <- c(
    "Parts of the apartment"         = "Apartment as a whole",
    "Parts of the apartment"         = "Furniture of apartment", 
    "Parts of the apartment"         = "Loudspeaker (by the fridge)",
    "Parts of the apartment"         = "Screen (entrance)",
    "Parts of the apartment"         = "Screen (kitchen on worktop)",
    "Parts of the apartment"         = "Screen (living room table)",
    "Parts of the apartment"         = "Screen (living room wall)",
    "Parts of the apartment"         = "Screen (living room window)",
    "Parts of the apartment"         = "Sliding door (between hallway and kitchen)",
    "Parts of the apartment"         = "Switch (living room)",
    "Parts of the apartment"         = "Switches (entrance)",
    "Parts of the apartment"         = "Switches (kitchen by the fridge)",
    "Parts of the apartment"         = "Switches (living room by the kitchen)",
    "Parts of the apartment"         = "Switches (living room lamp)",
    "Light in the hallway \\(L_H\\)" = "Light in the hallway \\(L_H\\)",
    "Robot"                          = "Robot",
    "Unspecific"                     = "Not discernible",
    "Unspecific"                     = "Self",
    "Unspecific"                     = "Unspecific"
  )
  data %>% 
    mutate(`Addressee final (reduced)` = fct_recode(`Addressee final`, !!!mapping)) %>%
    mutate(`Focus of attention (reduced)` = fct_recode(`Focus of attention`, !!!mapping)) %>%
    return(.)
}

fun_sort_addressee_factors <- function(data) {
  sorted_addressees = summary(data$`Addressee final`) %>% sort(decreasing=TRUE) %>% names()
  sorted_addressees_reduced = summary(data$`Addressee final (reduced)`) %>% sort(decreasing=TRUE) %>% names()
  data %>%
    mutate(`Addressee final` = factor(`Addressee final`, levels=sorted_addressees),
           `Focus of attention` = factor(`Focus of attention`, levels=sorted_addressees),
           `Addressee final (reduced)` = factor(`Addressee final (reduced)`, levels=sorted_addressees_reduced),
           `Focus of attention (reduced)` = factor(`Focus of attention (reduced)`, levels=sorted_addressees_reduced)
           ) %>%
    return(.)
}

fun_shorten_addressee_names <- function(data){
  result <- data
  for (i in c('Addressee final', 'Focus of attention')){
    iname <- str_c(i," Code")
    result <- mutate(result,!!iname := fct_recode(select(result,i)[[1]],
                                           `A`      = "Apartment as a whole",
                                           `LF`     = "Floor lamp \\(L_F\\)",
                                           `F`      = "Furniture of apartment",
                                           `LH`     = "Light in the hallway \\(L_H\\)",
                                           `Lo`     = "Loudspeaker (by the fridge)",
                                           `Nd`     = "Not discernible",
                                           `R`      = "Robot",
                                           `ScE`   = "Screen (entrance)",
                                           `ScK`   = "Screen (kitchen on worktop)",
                                           `ScLt`  = "Screen (living room table)",
                                           `ScLwa` = "Screen (living room wall)",
                                           `ScLwi` = "Screen (living room window)",
                                           `S`      = "Self",
                                           `D`      = "Sliding door (between hallway and kitchen)",
                                           `SwL`   = "Switch (living room)",
                                           `SwE`   = "Switches (entrance)",
                                           `SwK`   = "Switches (kitchen by the fridge)",
                                           `SwLk`  = "Switches (living room by the kitchen)",
                                           `SwLl`  = "Switches (living room lamp)",
                                           `U`      = "Unspecific"
    ))
  }
  for (i in c('Addressee final (reduced)', 'Focus of attention (reduced)')){
    iname <- str_c(i," Code")
    result <- mutate(result,!!iname := fct_recode(select(result,i)[[1]],
                                           `LF`     = "Floor lamp \\(L_F\\)",
                                           `LH`     = "Light in the hallway \\(L_H\\)",
                                           `Ap`     = "Parts of the apartment",
                                           `R`      = "Robot",
                                           `U`      = "Unspecific"
    ))
  }
  return(result)
}

fun_shorten_variable_names <- function(names) {
  map <- tribble(
    ~long, ~short,
    #--|--|----
    "Addressee final"                       , "Ad",
    "Focus of attention"                    , "Foa",
    "Method"                                , "M",
    "Expression (facial, gestural, verbal)" , "E",
    "Expression (reduced)"                  , "Er",
    "Speech form of address"                , "Sf",
    "Speech politeness"                     , "Sp",
    "Speech type of sentence"               , "St",
    "Speech type of sentence (reduced)"     , "Str",
    "Speech phrasing"                       , "Sph",
    "Method specific"                       , "Ms",
    "Speech specific"                       , "Ss",
    "Expression specific"                   , "Es",
    "Addressee final (reduced)"             , "Ar",
    "Focus of attention (reduced)"          , "Fr",
    "Addressee final (reduced) Code"        , "Ar",
    "Focus of attention (reduced) Code"     , "Fr",
    "Wizard addressee"                      , "Aw",
    "Wizard task"                           , "T",
    "Condition"                             , "C",
    "Order"                                 , "O",
    "Vp_num"                                , "Pid",
    "Speech specific (reduced)"             , "Ssr",
    "Method specific (reduced)"             , "Msr",
    "Addressee equals focus"                , "Aef"
  )
  return((names %>% enframe(name = NULL) %>% left_join(map, by=c("value" = "long")))$short)
}

fun_summarise_boolean_vector <- function(data){
  return(list(
    'sum' = length(data),
    'true' = sum(data),
    'false' = length(data) - sum(data),
    'p' = sum(data) / length(data)
  ))
}

fun_calc_grouped_bool_table <- function(data, group=`Wizard task`, equation=(`Addressee final` == `Focus of attention`)) {
  var = enquo(equation)
  group_q <- enquo(group)
  print(quo(data %>% 
              mutate(`equals` = !!var) %>% 
              select(`equals`,!!group_q) %>%
              table(.)))
  return(data %>% 
    mutate(`equals` = !!var) %>% 
    select(`equals`,!!group_q) %>%
    table(.))
}

fun_calc_grouped_bool_count_test <- function(data, group=`Wizard task`, equation=(`Addressee final` == `Focus of attention`), simulate=FALSE) {
  var = enquo(equation)
  group_q <- enquo(group)
  return(chisq.test(fun_calc_grouped_bool_table(data,group=!!group_q, equation=!!var), simulate.p.value = simulate))
}

fun_calc_significance_all <- function(data, variable=`Addressee final (reduced)`, variables=names(data)) {
  var = enquo(variable)
  levels = sapply(variables, 
                  function (x) { 
                    tryCatch(
                      (data %>% 
                         fun_calc_grouped_bool_table(., group=!!as.name(x), equation=!!var) %>% 
                         chisq.test(.))$p.value, 
                      warning=function(w) NaN )
                  }
  )
  return(levels)
}

fun_add_significance_to_info <- function(info, significance, prefix='--') {
  for (i in names(significance)) {
    p = significance[[i]]
    result = ""
    if (is.nan(p)) {
      result = "NaN"
    } else if (p < 0.001) {
      result = "< 0.001"
    } else {
      result = sprintf("%.3f", round(p,3))
    }
    name = i %>% str_remove_all(.,"([(].*,.*[)]|[()])") %>% str_trim(.) %>% str_to_lower(.) %>% str_replace_all(.," +","_")
    name = str_c(c(prefix,name), collapse="")
    if (!is.null(info[[name]])){
      warning(sprintf('overwriting existing value "%s" in info',name))
    }
    info[[name]] = result
  }
  return(info)
}

fun_mean_col_sorted_names <- function(data, decreasing=FALSE) {
  is.nan.data.frame <- function(x)
    do.call(cbind, lapply(x, is.nan))
  internal <- data
  internal[is.nan(internal)] <- 0
  internal %>%
    select(-Names) %>%
    sapply(., function(x) mean(x) ) %>%
    sort(., decreasing) %>%
    names(.) %>% 
    return(.)
}

fun_extract_reduced_method_specific <- function(data){
  extract_pattern = '(Bildschirm|Wand|Tür|Tisch|Lampe|Klatscht|Winkt|Wisch|Zeigt)'
  extract_pattern = '(Klatscht|Winkt|Wisch|Zeigt)'
  data %>% 
    mutate(`Method specific (reduced)` = `Method specific`) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(tür)","Tür")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(tisch)","Tisch")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(Stehlampe)","Lampe")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(klatscht)","Klatscht")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(winkt|winken)","Winkt")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(wisch)","Wisch")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(zeigt)","Zeigt")) %>%
    mutate(`Method specific (reduced)` = str_replace_all(`Method specific (reduced)`,"(schnippt)","Schnippt")) %>%
    mutate(`Method specific (reduced)` = sapply(`Method specific (reduced)`, function(x) str_c("",sort(unique(unlist(str_extract_all(x,extract_pattern)))), collapse = " "))) %>%
    mutate(`Method specific (reduced)` = str_replace(`Method specific (reduced)`,"^$","None")) %>%
    mutate(`Method specific (reduced)` = factor(`Method specific (reduced)`)) %>%
    return(.)
}

fun_extract_reduced_speech_specific <- function(data){
  extract_pattern = '(Licht|Roboter|Du)'
  data %>% 
    mutate(`Speech specific (reduced)` = `Speech specific`) %>%
    mutate(`Speech specific (reduced)` = str_replace_all(`Speech specific (reduced)`,"(Beleuchtung|Lampe)","Licht")) %>%
    mutate(`Speech specific (reduced)` = str_replace_all(`Speech specific (reduced)`,"(du)","Du")) %>%
    mutate(`Speech specific (reduced)` = sapply(`Speech specific (reduced)`, function(x) str_c("",sort(first(unlist(str_extract_all(x,extract_pattern)))), collapse = " "))) %>%
    mutate(`Speech specific (reduced)` = str_replace(`Speech specific (reduced)`,"^$","None")) %>%
    mutate(`Speech specific (reduced)` = factor(`Speech specific (reduced)`)) %>%
    return(.)
}

fun_extract_expression_reduced <- function(data){
  data %>% 
    mutate(`Expression (reduced)` = `Expression (facial, gestural, verbal)`) %>%
    mutate(`Expression (reduced)` = str_replace_all(`Expression (reduced)`,"(curious|joyful|surprised)","positive")) %>%
    mutate(`Expression (reduced)` = str_replace_all(`Expression (reduced)`,"(baffled|disappointed)","negative")) %>%
    mutate(`Expression (reduced)` = factor(`Expression (reduced)`)) %>%
    return(.)
}

fun_extract_speech_type_reduced <- function(data){
  data %>% 
    mutate(`Speech type of sentence (reduced)` = `Speech type of sentence`) %>%
    mutate(`Speech type of sentence (reduced)` = str_replace_all(`Speech type of sentence (reduced)`,"^Statement.*","Statement")) %>%
    mutate(`Speech type of sentence (reduced)` = str_replace_all(`Speech type of sentence (reduced)`,"^Question.*","Question")) %>%
    mutate(`Speech type of sentence (reduced)` = str_replace_all(`Speech type of sentence (reduced)`,"^Command.*","Command")) %>%
    mutate(`Speech type of sentence (reduced)` = factor(`Speech type of sentence (reduced)`)) %>%
    return(.)
}

fun_extract_speech_type_sentence <- function(data){
  data %>% 
    mutate(`Speech phrasing` = `Speech type of sentence`) %>%
    mutate(`Speech phrasing` = str_replace_all(`Speech phrasing`,".*[(]words[)]","Words")) %>%
    mutate(`Speech phrasing` = str_replace_all(`Speech phrasing`,".*[(]sentence[)]","Sentence")) %>%
    mutate(`Speech phrasing` = factor(`Speech phrasing`)) %>%
    return(.)
}

# read data
data_raw <- fun_load_data()
fun_assert_equals_print(ncol(data_raw),length(c(tiers_annotated, tiers_deduced, tiers_metainfo,tiers_ignore)))
data <- select(data_raw, -tiers_ignore)
fun_assert_equals_print(ncol(data),length(c(tiers_annotated, tiers_deduced, tiers_metainfo)))
data <- data[,c(tiers_annotated,tiers_deduced,tiers_metainfo)]
# filter broken/corrected entries
print(sprintf('%d raw entries',dim(data)[1]))
data <- fun_filter_time(data)
print(sprintf('%d time filtered',dim(data)[1]))
data <- fun_filter_addressee_na(data)
print(sprintf('%d addressee filtered',dim(data)[1]))
data <- fun_filter_wizard_error(data)
print(sprintf('%d wizard error filtered',dim(data)[1]))
data <- fun_filter_corrected(data)
print(sprintf('%d corrected annotation filtered',dim(data)[1]))
data <- fun_fix_annotations(data)



data_both_annotators = fun_filter_only_both_annotators(data)
#inter_rater_agreement = kappa2(fun_create_inter_rater_table(data_both_annotators, agreement_tiers ), 'unweighted')
inter_rater_agreement = kappa2(fun_create_inter_rater_table(data_both_annotators, tiers_irr ), 'unweighted')
print(sprintf('Inter rater agreement: %f',inter_rater_agreement$value))
for (tier in tiers_irr){
  print(sprintf('Inter rater agreement on tier %s: %f', tier, kappa2(fun_create_inter_rater_table(data_both_annotators, c(tier)),'unweighted')$value))
}

# remove repeated task solutions and multiple annotations
data <- fun_filter_remove_paired_data(data)
print(sprintf('%d redundant annotation filtered',dim(data)[1]))

# create factors where applicable
data <- fun_make_factors(data)

# process addresseed
data <- fun_reduce_addressees(data)
data <- fun_sort_addressee_factors(data)

# shorten addressee names
data <- fun_shorten_addressee_names(data)

# make reduced specifics
data <- fun_extract_reduced_speech_specific(data)
data <- fun_extract_reduced_method_specific(data)
data <- fun_extract_expression_reduced(data)
data <- fun_extract_speech_type_reduced(data)
data <- fun_extract_speech_type_sentence(data)

# remove remaining na's
data$`Speech specific`[is.na(data$`Speech specific`)] <- '---'
data$`Expression specific`[is.na(data$`Expression specific`)] <- '---'

# calculate how often addressee and attention are equal
data <- data %>% mutate(`Addressee equals focus` = `Addressee final` == `Focus of attention`)
addressee_equals = fun_summarise_boolean_vector(data$`Addressee equals focus`)


# write out information for latex
addressees <- fun_create_addressees_table(data)
# write meta information 
info <- list()
info[['max.time.seconds']] <- max_time_seconds
info[['sum.entries']]<- nrow(data)
info[['irr.value']] <- round(inter_rater_agreement$value,2)
info[['irr.trials']] <- length(unique(data_both_annotators$Vp_num))
info[['irr.quality']] <- fun_inter_rater_classification(inter_rater_agreement$value)
info[['addressee.most.frequent.prop']] = round(fct_count(data$`Addressee final`) %>% 
  mutate(proc=n/sum(n)) %>% arrange(desc(proc)) %>% 
  mutate(cum=cumsum(proc)) %>% .[[4,'cum']]*100,2)
info[['attention.most.frequent.prop']] = round(fct_count(data$`Focus of attention`) %>% 
  mutate(proc=n/sum(n)) %>% arrange(desc(proc)) %>% 
  mutate(cum=cumsum(proc)) %>% .[[4,'cum']]*100,2)
info[['addressee.equals.attention.p']] =  round(addressee_equals$p*100,2)
info[['addressee.equals.attention.task.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Wizard task`)$p.value,3)
info[['addressee.equals.attention.order.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Order`)$p.value,3)
info[['addressee.equals.attention.condition.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Condition`)$p.value,3)
info[['addressee.equals.attention.method.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Method`)$p.value,3)
info[['addressee.equals.attention.wizardaddressee.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Wizard addressee`)$p.value,3)
info[['addressee.equals.attention.foa-reduced.pval']] =  round(fun_calc_grouped_bool_count_test(data, group=`Focus of attention (reduced)`)$p.value,3)
info[['chi.square.replicates']] =  chi.square.replicates

#info <- fun_add_significance_to_info(info, fun_calc_significance_all(data, variable=`Addressee final (reduced)`),'addressee.pval.')


# check importance for addressee final (reduced)
vars_indep = c(
  #"Addressee final",
  "Addressee final (reduced)",
  "Focus of attention (reduced)",
  "Addressee equals focus",
  "Expression (reduced)",
  "Method",
  "Method specific (reduced)",
  "Speech form of address",
  "Speech politeness",
  "Speech type of sentence (reduced)",
  "Speech phrasing",
  "Speech specific (reduced)",
  "Wizard addressee",
  "Wizard task",
  "Condition",
  "Order",
  "Vp_num"
  #"Focus of attention",
  )
vars_indep_short = fun_shorten_variable_names(vars_indep)
cramer_data <- data %>% droplevels()
#cramer_data <- data %>% filter(`Method` != "speech") %>% droplevels()
#cramer_data <- data %>% filter(`Addressee equals focus` == FALSE) %>% droplevels()
### parallel

parallelStartSocket(use_cores, logging=TRUE)
parallelExport("vars_indep","cramer_data","cramerV","select","chi.square.replicates")
cramer = parallelSapply(vars_indep, 
                function (x) {
                  sapply(vars_indep, 
                         function (y) {
                           if(x==y) {
                             return(1)
                           } else {
                             print(paste("...calculating cramerV: ",x,y))
                             set.seed(1)
                             return(cramerV(table(select(cramer_data,x, y)), bias.correct=TRUE, digits=3, simulate.p.value = TRUE, B=chi.square.replicates))
                           }
                         }
                         )
                }) %>% 
  as_tibble(rownames="Names") %>%
  mutate(Names=vars_indep_short)
parallelStop()
names(cramer) <- c("Names", vars_indep_short)
#cramer_sort = (arrange(cramer,Ar))$Names
cramer_sort = cramer$Names
set.seed(NULL)
# plotting is done in the -plot script

# chi square
vars_dep = vars_indep
vars_dep_short = fun_shorten_variable_names(vars_dep)
chisq_steps = c("[0,0.001)", "[0.001,0.01)", "[0.01,0.05)", "[0.05,0.1)", "[0.1,1]")
chisq_levels =  c("\\(< 0.001\\)", "\\(< 0.01\\)", "\\(< 0.05\\)", "\\(< 0.1\\)", "\\( \\leq 1\\)")
parallelStartSocket(use_cores, logging=TRUE)
parallelExport("vars_dep", "%>%", "select", "cramer_data", "chi.square.replicates", "wafflecut", "chisq_steps", "chisq_levels")
chisq <- parallelSapply(vars_dep, function (x) { 
  sapply(vars_dep, 
         function (y) { 
           if(x==y) {
             return(0)
             } else { 
               print(paste("...calculating pvalue: ",x,y))
               set.seed(1)
               return(tryCatch(
                 chisq.test(table(select(cramer_data,x, y)), simulate.p.value = TRUE, B=chi.square.replicates)$p.value,
                 warning=function(w) return (NaN)))
             }
           }
  ) %>% wafflecut(., chisq_steps,chisq_levels)
} ) %>% as_tibble() %>% mutate(Names = vars_dep_short) %>% select("Names",eval(vars_dep)) 
names(chisq) <- c("Names", vars_dep_short)
parallelStop()
set.seed(NULL)

chisq_sort = cramer_sort

# plotting is done in the -plot script

# prepare data for evaluation
vars_eval = c(
  "Addressee final (reduced) Code",
  "Focus of attention (reduced) Code",
  
  "Method",
  "Addressee equals focus",
  
  "Speech form of address",
  "Speech politeness",
  "Speech type of sentence (reduced)",
  "Speech phrasing",
  
  "Speech specific (reduced)",
  "Method specific (reduced)",
  "Expression (reduced)",
  
  "Wizard addressee",
  "Wizard task",
  "Vp_num",
  "Condition",
  "Order"
)
data_eval = data %>% select(vars_eval)
colnames(data_eval) <- fun_shorten_variable_names(colnames(data_eval))
data_eval$Aef <- factor(data_eval$Aef)

# evaluate bayesian models
fun_get_predicition_bn <- function(bn, data, test_data = data, input=c()){
  fitted <- bn.fit(bn, data = data.frame(data), method='bayes')
  return(predict(fitted,data=data.frame(test_data),node="Ar",method="bayes-lw", from=input)==test_data$Ar)
}

fun_get_l1o_cv_result_bn <- function(bn, data, ...) {
  plot(bn)
  return(lapply(1:nrow(data), function (row) {
    data_train <- data[-row,]
    data_test  <- data[row,]
    prediction <- fun_get_predicition_bn(bn, data=data_train, test_data=data_test, ...)
    return(tibble(result=prediction))
  })  %>% 
    bind_rows(.) %>%
    groupwiseMean(result ~ 1, data=., conf=0.95)
  )
}

set.seed(1) # need this to be reproducible
bn_auto = hc(data.frame(data_eval), restart=1000, perturb=1000)
bn_auto = model2network("[M][Aef][Er][Str|M][Msr|M][Sph|Str][Ssr|Str][Ar|Sph][Sf|Sph][Sp|Sph][Fr|Ar:Aef][Aw|Ar][O|Sf][T|Fr][Pid|O][C|Pid]")
bn_auto_o = model2network("[Aef][Er][O][Sf|O][Pid|O][Sp|Sf][C|Pid][M|Sp][Str|Sp][Ar|Str][Msr|M][Fr|Ar:Aef][Ssr|Ar][Aw|Ar][T|Fr]")
bn_baseline = model2network("[Aef][Aw][Er][Pid][C][O][Sf][Sp][M][Str][Sph][Ar][Msr][Fr|Ar][Ssr][T]")
#bn_manual_better = model2network("[Aef|Pid][Aw|Ar][Er|Pid:Ar][Pid][C][O][Sf|Sp][Sp|Ar:M][M|Ar:O:C][Str|Sp][Ar|C:T:Pid:O][Msr|M:Pid][Fr|Ar:Aef][Ssr|Ar][T]")
bn_manual = model2network("[Aef|Pid][Aw|Ar][Er|Pid][Pid][C][O][Sf|Sp][Sp|Ar:M][M|Ar:O][Str|Sp][Sph|Sp][Ar|C:T:Pid][Msr|M:Pid][Fr|Ar:Aef][Ssr|Ar][T]")

input_all = c("Fr", "M", "Aef", "Sf", "Sp", "Str", "Sph", "Ssr", "Msr", "Er", "Aw", "T", "Pid", "C", "O")
input_observable = c("Fr", "M", "Sf", "Sp", "Str", "Sph", "Ssr", "Msr", "Er", "Pid")
input_speech = c("Sf", "Sp", "Str", "Sph", "Ssr")
input_visual = c("Fr", "M", "Msr", "Pid", "Er")

eval_networks = list(Baseline=bn_baseline,Manual=bn_manual,Auto=bn_auto)
eval_input_sets = list(All=input_all, Observable=input_observable,Visual=input_visual, Speech=input_speech)
bn_result <- 
  lapply(names(eval_input_sets), function (inp_name) {
  lapply(names(eval_networks), function (bn_name) {
    print(sprintf("evaluating network %s with inputs %s", bn_name, inp_name))
    a <- fun_get_l1o_cv_result_bn(eval_networks[[bn_name]],data_eval,input=eval_input_sets[[inp_name]])
    return(tibble(network=bn_name, data=inp_name, conf.level=a$Conf.level, mean=a$Mean, lower=a$Trad.lower, upper=a$Trad.upper))
  })
}) %>% unlist(recursive=FALSE) %>% bind_rows()

set.seed(NULL)  


# evaluate random forest
vars_drop_all = c()
vars_drop_observable = setdiff(fun_shorten_variable_names(vars_eval), c("Ar", input_observable))
vars_drop_speech = setdiff(fun_shorten_variable_names(vars_eval), c("Ar", input_speech))
vars_drop_visual = setdiff(fun_shorten_variable_names(vars_eval), c("Ar", input_visual))
eval_drop_sets = list(All=vars_drop_all, Observable=vars_drop_observable, Visual=vars_drop_visual, Speech=vars_drop_speech)
## settings for drafting
rf.conf.tune.it = 100
rf.conf.tune.resample = 2
## parameters to optimize
parallelStartSocket(use_cores, logging=TRUE)
start_time <- now()
set.seed(1) # need this to be reproducible
rf_result <- lapply(names(eval_drop_sets), function (drop_name) {
  resample_result <- NULL
  if(draft | draft_rf) {
      resample_desc <- makeResampleDesc("CV", iters=5)
      model = makeLearner("classif.randomForest")
      ml_task <- makeClassifTask(data = data.frame(data_eval), target = "Ar")
      if(!is.null(eval_drop_sets[[drop_name]])) {
        ml_task  <- dropFeatures(task = ml_task,features = eval_drop_sets[[drop_name]])
      }
      resample_result <- resample(model, ml_task, resampling=resample_desc, show.info = TRUE)
  } else if (load_rf){
    resample_result <- readRDS(file = sprintf("study-addressee-rf-%s.rds", drop_name))
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
  a <- resample_result$pred$data %>% filter(set=="test") %>% mutate(result=response==truth) %>% groupwiseMean(result ~ 1, data=., conf=0.95)
  return(tibble(data=drop_name, conf.level=a$Conf.level, mean=a$Mean, lower=a$Trad.lower, upper=a$Trad.upper, all=list(resample_result)))
}) %>% bind_rows()
set.seed(NULL)  
end_time <- now()
parallelStop()
print("Random forest evaluation done in:")
print(end_time-start_time)

# plot crossevaluation results
cv_result <- bind_rows(bn_result, rf_result %>% mutate(network="RF  "))
cv_result$network[cv_result$network=="Auto"] <- "BA  "
cv_result$network[cv_result$network=="Baseline"] <- "BF  "
cv_result$network[cv_result$network=="Manual"] <- "BM  "
cv_result <- cv_result %>%
  mutate(data=factor(data,levels=c("Speech", "Visual", "Observable", "All"))) %>%
  mutate(network=factor(network,levels=c("BF  ", "BM  ", "BA  ", "RF  "))) %>%
  arrange(data)
# plotting is done in the -plot script

#write to files
fun_write_all_out <- function(){
  fun_write_csv_addressees(addressees)
  fun_write_data_filtered(data)
}
if(write_out) { fun_write_all_out() }
if(!draft) {
  cramer_nondraft = cramer
  chisq_nondraft = chisq
  cv_result_nondraft = cv_result
  # save workspace
  if(draft_rf){
    save.image(file="study-addressee-draftrf.RData")
  } else {
    save.image(file="study-addressee-workspace.RData")
  }
}
if (FALSE) {
 load("study-addressee-workspace.RData")
}
