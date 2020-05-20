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
library(rlang)
library(tidyverse)
library(bnlearn)
library(Rgraphviz)
library(parallel)
library(parallelMap)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)

input_file_gridsearch = "group_match_grid_search.tsv.gz"
input_file_ffm = "group_match_statistics.tsv.gz"
input_file_faces = "group_face_detections.tsv.gz"
input_roles = "role_annotations.tsv.gz"

# meta switch
draft = FALSE
use_cores = detectCores(all.tests = FALSE, logical = TRUE)

if (Sys.getenv("CALCULATE_NONDRAFT") == "ON"){
  draft = FALSE
}
if (Sys.getenv("USE_CORES") != ""){
  use_cores = as.integer(Sys.getenv("USE_CORES"))
}

fun_assert_equals_print <- function(x,y, msg="Unexpected amount of entries. got: %s != %s expected.", pre=''){
  assertthat::assert_that(x==y,msg=sprintf(paste(pre,msg), x, y))
}

fun_load_data_grid_search <- function() {
  ## read the data
  result <- read_tsv(input_file_gridsearch,
           col_names = c(
             "x1",
             "algorithm", 
             "mdl", 
             "stride", 
             "observation.type", 
             "agent", 
             "group.precision",
             "group.recall",
             "group.f1",
             "group.markedness",
             "group.informedness",
             "t2.precision",
             "t2.recall",
             "t2.f1",
             "t2.markedness",
             "t2.informedness",
             "t1.precision",
             "t1.recall",
             "t1.f1",
             "t1.markedness",
             "t1.informedness",
             "x2"
             ),
           col_types = cols(
             .default = col_double(),
             x1 = col_factor(), 
             algorithm = col_factor(), 
             mdl = col_factor(), 
             stride = col_factor(), 
             observation.type = col_factor(), 
             agent = col_factor()
            )) %>% 
    select(-c(x1,x2))
  # the agents are encoded in the column names so this needs to be fixed too
  bind_rows(
    result %>% 
      select(algorithm,mdl,stride,observation.type,agent,contains("group")) %>% 
      mutate(measure="group") %>% 
      rename_all(function(x) {str_remove(x,'group.')}),
    result %>%
      select(algorithm,mdl,stride,observation.type,agent,contains("t2")) %>% 
      mutate(measure="t2") %>% 
      rename_all(function(x) {str_remove(x,'t2.')}),
    result %>%
      select(algorithm,mdl,stride,observation.type,agent,contains("t1")) %>% 
      mutate(measure="t1") %>% 
      rename_all(function(x) {str_remove(x,'t1.')})
  ) %>% return()
}

fun_load_data_ffm <- function() {
  ## read the data
  result <- read_tsv(input_file_ffm, comment="ingroup-test",
           col_names = c(
             "x1",
             "time",
             "algorithm", 
             "mdl", 
             "stride", 
             "observation.type", 
             "agent", 
             "gt.group.size",
             "cl.group.size",
             "mising.persons",
             "tp",
             "fp",
             "tn",
             "fn",
             "cp",
             "cn",
             "pp",
             "pn",
             "precision",
             "recall",
             "f1",
             "markedness",
             "informedness",
             "fpr",
             "gt.group.center.x",
             "gt.group.center.y",
             "gt.group.distance.cost",
             "gt.agent.distance.cost",
             "gt.group.visibility.cost",
             "gt.agent.visibility.cost",
             "cl.group.center.x",
             "cl.group.center.y",
             "cl.group.distance.cost",
             "cl.agent.distance.cost",
             "cl.group.visibility.cost",
             "cl.agent.visibility.cost",
             "x2"
             ),
           col_types = cols(
             .default = col_double(),
             x1 = col_factor(),
             algorithm = col_factor(), 
             mdl = col_factor(), 
             stride = col_factor(), 
             observation.type = col_factor(), 
             agent = col_factor(), 
             gt.group.size = col_integer(),
             cl.group.size = col_integer(),
             mising.persons = col_integer(),
             tp = col_integer(),
             fp = col_integer(),
             tn = col_integer(),
             fn = col_integer(),
             cp = col_integer(),
             cn = col_integer(),
             pp = col_integer(),
             pn = col_integer(),
             x2 = col_logical()
            )) %>% 
    select(-c(x1,x2)) %>%
    return()
}

fun_load_data_faces <- function(reference){
  # need to adapt the times to the same time frame as the ffm data
  #
  # the first parameter is the time that was substracted from the 
  # original person tracking data to create the annotations from 
  # person tracking.
  tide_person_track_delta = 1510666601266
  # the second parameter is the time that gets substracted from the 
  # person annotations to get them into the 'Home' timeframe.
  home_frame_delta = -194225
  changes <- 
    read_tsv(input_file_faces, comment="ingroup-test",
             col_types = cols(
               timestamp = col_double(),
               entrance.faces.angle.h = col_double(),
               entrance.faces.angle.v = col_double(),
               entrance.faces.angle.w = col_double(),
               assistance.faces.angle.h = col_double(),
               assistance.faces.angle.v = col_double(),
               assistance.faces.angle.w = col_double(),
               entrance.speaking = col_character(),
               assistance.speaking = col_character(),
               .default = col_integer())) %>% 
    mutate(
      timestamp = timestamp-tide_person_track_delta-home_frame_delta,
      assistance.speaking = assistance.speaking=='speaking_start',
      assistance.speaking = replace_na(assistance.speaking,FALSE),
      entrance.speaking = entrance.speaking=='speaking_start',
      entrance.speaking = replace_na(entrance.speaking,FALSE))
  changes[is.na(changes)] <- NaN
  # the data contains changes but a we need regular observations according to reference
  times <- reference %>% select(time) %>% unique() %>% arrange()
  filled <- full_join(times ,changes, by=c("time"="timestamp")) %>%
    arrange(time) %>%
    tidyr::fill(everything(), .direction=c('down'))
  filtered <- semi_join(filled,times, by="time")
  # the agents are encoded in the column names so this needs to be fixed too
  bind_rows(
    filtered %>% 
      select(time,contains("assistance")) %>% 
      mutate(agent="flobi_assistance") %>% 
      rename_all(function(x) {str_remove(x,'assistance.')}),
    filtered %>% 
      select(time,contains("entrance")) %>% 
      mutate(agent="flobi_entrance") %>% 
      rename_all(function(x) {str_remove(x,'entrance.')})
  ) %>% mutate(agent=factor(agent,levels=levels(reference$agent))) %>%
    arrange(time) %>%
  return()
}

fun_load_data_roles <- function(reference){
  result <- read_tsv(input_roles,
                     col_types = cols(
                       flobi_entrance.role.agent = col_character(),
                       flobi_entrance.role.Viktor = col_character(),
                       flobi_entrance.role.P1 = col_character(),
                       flobi_entrance.role.P2 = col_character(),
                       flobi_entrance.role.P3 = col_character(),
                       flobi_entrance.role.P4 = col_character(),
                       flobi_entrance.role.P5 = col_character(),
                       flobi_entrance.role.P6 = col_character(),
                       flobi_entrance.role.P7 = col_character(),
                       flobi_entrance.role.P8 = col_character(),
                       flobi_entrance.role.P9 = col_character(),
                       flobi_entrance.role.P10 = col_character(),
                       flobi_assistance.role.agent = col_character(),
                       flobi_assistance.role.Viktor = col_character(),
                       flobi_assistance.role.P1 = col_character(),
                       flobi_assistance.role.P2 = col_character(),
                       flobi_assistance.role.P3 = col_character(),
                       flobi_assistance.role.P4 = col_character(),
                       flobi_assistance.role.P5 = col_character(),
                       flobi_assistance.role.P6 = col_character(),
                       flobi_assistance.role.P7 = col_character(),
                       flobi_assistance.role.P8 = col_character(),
                       flobi_assistance.role.P9 = col_character(),
                       flobi_assistance.role.P10 = col_character(),
                        .default = col_double()
                       ))
  bind_rows(
    result %>% 
      select(time,starts_with("position"),starts_with("flobi_entrance.role")) %>%
      rename_all(function (x) {str_remove(x,'^(position|flobi_entrance)[.]')}) %>% 
      mutate(observation.type="annotated", agent="flobi_entrance"),
    result %>% 
      select(time,starts_with("position"),starts_with("flobi_assistance.role")) %>% 
      rename_all(function (x) {str_remove(x,'^(position|flobi_assistance)[.]')}) %>%
      mutate(observation.type="annotated", agent="flobi_assistance"),
    result %>% 
      select(time,starts_with("mposition"),starts_with("flobi_entrance.role")) %>% 
      rename_all(function (x) {str_remove(x,'^(mposition|flobi_entrance)[.]')}) %>%
      mutate(observation.type="detected", agent="flobi_entrance"),
    result %>% 
      select(time,starts_with("mposition"),starts_with("flobi_assistance.role")) %>% 
      rename_all(function (x) {str_remove(x,'^(mposition|flobi_assistance)[.]')}) %>%
      mutate(observation.type="detected", agent="flobi_assistance")
  ) %>% 
    select(time, agent, observation.type, everything()) %>% 
    select(-contains("role.flobi_")) %>% 
    mutate_at(vars(matches('role')),function (x) {
      parse_factor(replace_na(x,'non-member'), levels=c("speaker","addressee","member","non-member"))
      }) %>% 
    mutate(
      agent=factor(agent,levels=levels(reference$agent)),
      observation.type=factor(observation.type,levels=levels(reference$observation.type))
           ) %>%
    return()
}

fun_create_measurements <- function(tp,fp,tn,fn){
  result <- list(
    "tp" = tp,
    "fp" = fp,
    "tn" = tn,
    "fn" = fn,
    "cp" = tp + fn,
    "cn" = tn + fp,
    "pp" = tp + fp,
    "pn" = tn + fn
  )
  result$precision <- tp / result$pp
  result$recall <- tp / result$cp
  result$f1 <- 2 * ((result$precision*result$recall)/(result$precision+result$recall))
  result$fpr <- fp / result$cn
  result$markedness <- tp / result$pp - fn / result$pn
  result$informedness <- tp / result$cp - fp / result$cn
  return(result)
}

fun_create_cm_and_measurements <- function(gt, cl){
  return(fun_create_measurements(
    sum(gt==TRUE & cl==TRUE),
    sum(gt==FALSE & cl==TRUE),
    sum(gt==FALSE & cl==FALSE),
    sum(gt==TRUE & cl==FALSE)
  ))
}

fun_tolerant_match_evaluation <- function(tpr,tnr,gt.in.group,cl.in.group,t){
  over_t = (tpr > t) & (tnr > t)
  tp = sum(gt.in.group & cl.in.group & over_t)
  fp = sum(!gt.in.group & cl.in.group)
  tn = sum(!gt.in.group & !cl.in.group)
  fn = sum(gt.in.group & !cl.in.group) + sum(gt.in.group & cl.in.group & !over_t)
  return(fun_create_measurements(tp,fp,tn,fn))
}

fun_ingroup_evaluation <- function(gt.in.group,cl.in.group){
  tp = 0;
  fp = 0;
  tn = 0;
  fn = 0;
  for (i in seq(1,length(gt.in.group))){
    if(!cl.in.group[i]) {
      if(gt.in.group[i]){
        fn = fn + 1; # falsely detected as not in group
      } else {
        tn = tn + 1; # correctly detected as not in group
      }
    } else {
      if (gt.in.group[i]){
        tp = tp + 1; # correctly detected as in group
      } else {
        fp = fp + 1; # falsely detected as in group
      }
    }
  }
  return(fun_create_measurements(tp,fp,tn,fn))
}

fun_calculate_ffm_result <- function(data_filtered, config, t, ts){
  vals <- fun_tolerant_match_evaluation(data_filtered$recall,1-data_filtered$fpr,data_filtered$gt.in.group,data_filtered$cl.in.group, t)
  vals$config <- config
  vals$mdl <- data_filtered$mdl[1]
  vals$stride <- data_filtered$stride[1]
  vals$algorithm <- data_filtered$algorithm[1]
  vals$agent <- data_filtered$agent[1]
  vals$observation.type <- data_filtered$observation.type[1]
  vals$t <- t
  vals$ts <- ts
  return(vals) 
}

fun_auc <- function(x,y){
  sorted <- tibble(x=x,y=y) %>% filter(!is.na(x) & !is.na(y)) %>% arrange(x)
  if(nrow(sorted)<2){
    return(0.)
  }
  area = 0.
  for(i in seq(2,nrow(sorted))){
    x1 <- sorted[[i-1,1]]
    x2 <- sorted[[i,1]]
    y1 <- sorted[[i-1,2]]
    y2 <- sorted[[i,2]]
    area <- area + ((x2-x1)*y1 + (x2-x1)*y2)/2
  }
  result <- area*1/(sorted[[nrow(sorted),1]]-sorted[[1,1]])
  return(result)
}

# read grid search data
data_ffm_grid <- fun_load_data_grid_search() %>%
  mutate(measure=recode_factor(measure,group="Group", t2="T2/3",t1="T1")) %>%
  mutate(algorithm=recode_factor(algorithm,gco="Gco", grow="Grow", shrink="Shrink", none="None", one="One")) %>%
  mutate(agent=recode_factor(agent,flobi_assistance="Flobi Assistance", flobi_entrance="Flobi Entrance")) %>%
  mutate(observation.type=recode_factor(observation.type,annotated="Annotated", detected="Detected")) %>%
  filter(algorithm %in% c("Gco","Grow","Shrink"))

# read and fix data
data <- fun_load_data_ffm()

study_episodes = list(
  "pre" = 194162,
  "demo_hallway" = 356162,
  "demo_living" = 737162,
  "demo_kitchen" = 1457162,
  "open" = 3583162
)

data <- data %>%
  filter(time > study_episodes$pre) %>%
  filter(time < study_episodes$open) %>%
  mutate(open.interaction = time > study_episodes$demo_kitchen,
         gt.in.group = gt.group.size > 1,
         cl.in.group = cl.group.size > 1,
         config=paste(mdl,stride,algorithm,observation.type,agent, sep="_"))

data_ffm_gco <- data %>% filter(algorithm=="gco",mdl==4500,stride==50, observation.type=="detected")

# ffm evaluation
max_group_size = max(data$gt.group.size)
ffm_results <- lapply(unique((data %>% filter(algorithm %in% c("gco","grow","shrink","none","one")))$config), function (c) { 
  data_filtered <- data %>% filter(config==c)
  print(paste("calculating group match",c))
  bind_rows(
    lapply(seq(2,max_group_size+1), function(p) {
      t = (p-1)/p
      ts <- sprintf("%d/%d",p-1,p) 
      return(fun_calculate_ffm_result(data_filtered,c,t,ts))
    }),
    fun_calculate_ffm_result(data_filtered,c,0.,"0"),
    fun_calculate_ffm_result(data_filtered,c,0.05,"1/20"),
    fun_calculate_ffm_result(data_filtered,c,0.9999,"1")
  ) %>% return()
  }) %>% 
  bind_rows() %>% 
  rename(Precision=precision,Recall=recall,F1=f1,Markedness=markedness,
         Informedness=informedness,Threshold=t,Input=observation.type,
         Agent=agent,Mdl=mdl,Stride=stride,Algorithm=algorithm) %>%
  mutate(Agent=recode(Agent, flobi_assistance="Flobi Assistance", flobi_entrance="Flobi Entrance")) %>%
  mutate(Algorithm=recode(Algorithm, gco="Gco", grow="Grow", shrink="Shrink", none="None", one="One" )) %>%
  mutate(Input=recode(Input, annotated="Annotated", detected="Detected" )) %>%
  mutate(ts=factor(ts, levels = c("0","1/20","1/2","2/3","3/4","4/5","5/6","6/7","7/8","8/9","9/10","10/11","11/12", "1")))

## h2/h3
eval_in_group_sample_size = 5000
fun_eval_in_group_feature <- function(gt, feature, comp=`>=` ,max_steps=eval_in_group_sample_size){
  steps <- feature %>% sort() %>% unique()
  if(length(steps) > max_steps){
    steps <- steps[round(seq(1,max_steps)*length(steps)/max_steps)]
  }
  lapply(steps, function(x) {
    m <- fun_create_cm_and_measurements(gt,comp(feature,x))
    m$t <- x
    return(m)
  }) %>% 
    bind_rows() %>%
    return()
}

data_faces <- fun_load_data_faces(data_ffm_gco)
data_in_group <- inner_join(data_ffm_gco,data_faces, by=c('time','agent')) %>%
  select(time,agent,open.interaction,gt.in.group,cl.in.group,contains('cost'),faces.angle.w,faces.size) %>%
  mutate(feature_angle = replace_na(faces.angle.w,90),
         feature_size = replace_na(faces.size,0),
         feature_cost = (int(!cl.in.group) * 10006065) + cl.agent.distance.cost + cl.agent.visibility.cost,
         agent=recode(agent, flobi_assistance="Flobi Assistance", flobi_entrance="Flobi Entrance")
         )

results_in_group <- lapply(unique(data_in_group$agent), function(a) {
  agent_data <- data_in_group %>% filter(agent==a)
  print(paste("calculate in group match for agent:",a,"feature: angle"))
  angle <- fun_eval_in_group_feature(agent_data$gt.in.group,agent_data$feature_angle,`<=`) %>% mutate(agent=a, feature='Gaze')
  print(paste("calculate in group match for agent:",a,"feature: size"))
  size <- fun_eval_in_group_feature(agent_data$gt.in.group,agent_data$feature_size,`>=`) %>% mutate(agent=a, feature='Face')
  print(paste("calculate in group match for agent:",a,"feature: cost"))
  cost <- fun_eval_in_group_feature(agent_data$gt.in.group,agent_data$feature_cost,`<=`) %>% mutate(agent=a, feature='Gco-Agent')
  return(bind_rows(angle,size,cost))
} %>% return()
) %>% bind_rows()

# person movements in apartment
data_roles <- fun_load_data_roles(data_ffm_gco)

persons = c("flobi_entrance", "flobi_assistance","Viktor", "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")
data_roles_long <- lapply(persons, function(p) {
  data_roles %>% 
    select(time,agent,observation.type,role.agent,starts_with(paste(p,'.',sep='')),ends_with(p)) %>%
    rename_all(function (x) {str_remove(x,paste('[.]?', p, '[.]?', sep = ''))}) %>% 
    mutate(person=p)
}) %>% 
  bind_rows() %>%
  select(time, agent, observation.type, role.agent, person, role, x, y, r)

fun_export_movement_image <- function() {
  plot_person_movement <- 
      data_roles_long %>% 
      filter(agent=="flobi_entrance") %>%
      #filter(seq(1,length(time)) %% 50 == 0) %>% 
      mutate(Person=recode(person, 
                           flobi_assistance="Flobi Assistance",
                           flobi_entrance="Flobi Entrance",
                           Viktor="Presenter",
                           P1="Participant-A",
                           P2="Participant-B",
                           P3="Participant-C",
                           P4="Participant-D",
                           P5="Participant-E",
                           P6="Participant-F",
                           P7="Participant-G",
                           P8="Participant-H",
                           P9="Participant-I",
                           P10="Participant-J"),
             Input=recode(observation.type,annotated="Annotated",detected="Detected")) %>%
    ggplot() + 
    geom_point(aes(x=x,y=y,color=Person), na.rm = TRUE, alpha = 1/20) + 
    coord_fixed(xlim = c(0,500), ylim = c(-200,600)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) + 
    facet_grid(cols=vars(Input)) + 
    scale_color_manual(values=colorRampPalette(RColorBrewer::brewer.pal(5,"Set1"))(length(unique(data_roles_long$person)))) +
    theme_void() +
    theme(legend.position = "none",
          strip.text.x = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA)
          )
  ggsave("../figures/ffm-movements.tiff", units="in", width=5, height=6, dpi=600, compression = 'lzw')
}

fun_plot_density <- function(var=NA, data=data_roles_full){
  if(is.na(var)){
    print(names(data))
  } else {
    x = data[, var]
    x = x[[1,]]
    if(typeof(x) %in% c("logical","integer")) { 
      #print(paste("skipping",var))
      x <- as.double(x)
    }
      hist(x, prob = TRUE, xlab = var, ylab = "", main = "", col = "ivory")
      lines(density(x), lwd = 2, col = "tomato")
      curve(dnorm(x,mean = mean(x), sd = sd(x)), from = min(x), to = max(x), add = TRUE, lwd = 2, col = "steelblue")
  }
}

# write info
info <- list()
info["num_frames"] <- nrow(data_ffm_gco)
info["num_frames_agent"] <- nrow(data_ffm_gco %>% filter(agent=='flobi_entrance'))
info["prev_entrance"] <- round(sum(data_ffm_gco$gt.in.group & data_ffm_gco$agent=='flobi_entrance')/sum(data_ffm_gco$agent=='flobi_entrance'),2)
info["prev_assistance"] <- round(sum(data_ffm_gco$gt.in.group & data_ffm_gco$agent=='flobi_assistance')/sum(data_ffm_gco$agent=='flobi_assistance'),2)
info["eval_ingroup_sample_size"] <- eval_in_group_sample_size

if(!draft) {
  # save workspace
  save.image(file="study-group-workspace.RData")
}
if (FALSE) {
 load("study-group-workspace.RData")
}
