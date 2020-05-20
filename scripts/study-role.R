## required libraries
library(ggplot2)
library(ggrepel)
library(tikzDevice)
library(rlang)
library(tidyverse)
library(Rgraphviz)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)

load("study-role-data-full.RData")
load("study-role-eval-performance-data.RData")

e <- new.env()
load(file="study-group-workspace.RData", envir = e)
for(var in names(e)[startsWith(names(e),"fun_")]){
  .GlobalEnv[[var]] <- e[[var]]
}
rm(e)

# meta switch
draft = TRUE

if (Sys.getenv("CALCULATE_NONDRAFT") == "ON"){
  draft = FALSE
}

source("../scripts/tikz-export-config.R")

model_performance <- model_performance %>% filter(observations==101668)

fun_fix_time_and_agent <- function(data){
  assertthat::assert_that(nrow(data)==101668)
  lapply(unique(data$set), function(s) {
    seto <- data_roles_full %>% filter(second == s)
    setr <- data %>% filter(set == s)
    assertthat::are_equal(seto$role.agent, setr$role.agent)
    setr %>% mutate(time = seto$time, agent=seto$agent) %>% return()
  }) %>% 
    bind_rows() %>% 
    select(time,agent,role.agent,role.predicted,starts_with("role.predicted"),everything()) %>%
    return()
}

if(FALSE){
  model_performance %>% 
    arrange(desc(seed.mean.f1.lower),model,class) %>% 
    filter(observations==101668) %>% 
    filter(is.na(model.seed) | model.seed=="1") %>% 
    select(model, class, f1, markedness, informedness, starts_with("seed"), starts_with("model")) %>% 
    View()
}

#if(!is.na(model_performance_sets)){
model_performance_sets %>% 
  filter(model.type %in% c("keras","lstm","lstmconv","rule")) %>% 
  ggplot() + 
  geom_point(aes(x=recall, y=precision, shape=model.type, color=model.epochs), na.rm = TRUE) + 
  facet_grid(cols = vars(class), rows = vars(set))

model_performance_sets %>% 
  filter(set==4, model.type %in% c("keras","lstm","lstmconv","rule")) %>% 
  ggplot() + 
  geom_point(aes(x=recall, y=precision, shape=model.type, color=model.epochs), na.rm=TRUE) + 
  facet_wrap(facets = vars(class))
#}

data_tile <- model_performance %>% 
  select(model, model.base, model.seed, class, starts_with('predicted.')) %>%
  mutate(sum = predicted.speaker+predicted.addressee+predicted.member+`predicted.non-member`) %>%
  gather(predicted, "amount", "predicted.speaker", "predicted.addressee", "predicted.member", "predicted.non-member") %>% 
  mutate(
    predicted = str_remove(predicted, 'predicted[.]'),
    class = factor(class, levels = c('speaker', 'addressee', 'member', 'non-member')),
    predicted = factor(predicted, levels = levels(class))
  ) %>% # collect different seeds
  group_by(model.base, class, predicted) %>%
  summarize(sum=sum(sum), amount=sum(amount), seeds=length(unique(model.seed))) %>%
  ungroup() %>%
  mutate(model=model.base) %>%
  select(-model.base)
  

data_tile <- data_tile %>% filter(model %in% c(
  "rule",
  "bnmrulec",
  "keras_full_dropout_0.5_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0",
  "lstmdist_rule_dropout_0.5_hist_15_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0"
)) %>% 
  mutate(
    class = recode_factor(class, speaker = "Speaker", addressee = "Addressee", member = "Side-Participant", `non-member` = "Non-Participant"),
    predicted = recode_factor(predicted, speaker = "Speaker", addressee = "Addressee", member = "Side-Participant", `non-member` = "Non-Participant"),
    model = factor(model)
  ) %>% 
  mutate(
    thesismodel = recode_factor(model, 
                              `rule`="rule",
                              `bnmrulec`="BnM",
                              `keras_full_dropout_0.5_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0`="D.F.128.1",
                              `lstmdist_rule_dropout_0.5_hist_15_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0`="L.R.128.1"
                              ),
    beamermodel = recode_factor(model, 
                              `rule`="Rule",
                              `bnmrulec`="Bayes",
                              `keras_full_dropout_0.5_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0`="Dense-Low-Level",
                              `lstmdist_rule_dropout_0.5_hist_15_epochs_50_units_128_lr_0.0001_hl_1_leaky_FALSE_vsplit_0`="Lstm-Rule"
                              ),
    model = thesismodel,
  )    

(
plot_cm_models <- data_tile %>% 
  mutate(Amount = amount/sum, Predicted = predicted) %>% 
  mutate(value = sprintf("%.2f",Amount)) %>% 
  mutate(Number = sprintf("%.0f", amount/seeds)) %>% 
  mutate(Role = class,
         goodbad = ifelse(Role == Predicted,"good","bad"),
         ) %>% 
  ggplot() +
  geom_tile(aes(x=Predicted,y=Role,alpha=Amount,fill=goodbad)) + 
  facet_wrap(facets = vars(thesismodel)) + 
  scale_fill_manual(values = c(good = RColorBrewer::brewer.pal(name = "Set1", n = 9)[3], bad = RColorBrewer::brewer.pal(name = "Set1", n = 9)[1])) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        legend.position = "None") + 
  geom_text(aes(x=Predicted, y=Role, label = value), color = "black", size = 4) + 
  scale_y_discrete(limits = rev(levels(data_tile$class)))
)

(
plot_cm_models_beamer <- data_tile %>% 
  mutate(Amount = amount/sum, Predicted = predicted) %>% 
  mutate(value = sprintf("%.2f",Amount)) %>% 
  mutate(Number = sprintf("%.0f", amount/seeds)) %>% 
  mutate(Role = class,
         goodbad = ifelse(Role == Predicted,"good","bad"),
         ) %>% 
  ggplot() +
  geom_tile(aes(x=Predicted,y=Role,alpha=Amount,fill=goodbad)) + 
  facet_wrap(facets = vars(beamermodel)) + 
  scale_fill_manual(values = c(good = RColorBrewer::brewer.pal(name = "Set1", n = 9)[3], bad = RColorBrewer::brewer.pal(name = "Set1", n = 9)[1])) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        legend.position = "None") + 
  geom_text(aes(x=Predicted, y=Role, label = value), color = "black", size = 4) + 
  scale_y_discrete(limits = rev(levels(data_tile$class)))
)

(
plot_cm_models_simple <- data_tile %>% 
  filter(beamermodel %in% c("Rule","Bayes")) %>%
  mutate(model = fct_drop(beamermodel)) %>%
  mutate(Amount = amount/sum, Predicted = predicted) %>% 
  mutate(value = sprintf("%.2f",Amount)) %>% 
  mutate(Number = sprintf("%.0f", amount/seeds)) %>% 
  mutate(Role = class,
         goodbad = ifelse(Role == Predicted,"good","bad"),
         ) %>% 
  ggplot() +
  geom_tile(aes(x=Predicted,y=Role,alpha=Amount,fill=goodbad)) + 
  facet_wrap(facets = vars(model)) + 
  scale_fill_manual(values = c(good = RColorBrewer::brewer.pal(name = "Set1", n = 9)[3], bad = RColorBrewer::brewer.pal(name = "Set1", n = 9)[1])) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        legend.position = "None") + 
  geom_text(aes(x=Predicted, y=Role, label = value), color = "black", size = 4) + 
  scale_y_discrete(limits = rev(levels(data_tile$class))) +
  coord_fixed()
)

(
plot_cm_models_counts <- data_tile %>% 
  mutate(Amount = amount/sum, Predicted = predicted) %>% 
  mutate(value = sprintf("%.2f",Amount)) %>% 
  mutate(Number = sprintf("%.0f", amount/seeds)) %>% 
  mutate(Role = class,
         goodbad = ifelse(Role == Predicted,"good","bad"),
         ) %>% 
  ggplot() +
  geom_tile(aes(x=Predicted,y=Role,alpha=Amount,fill=goodbad)) + 
  facet_wrap(facets = vars(beamermodel)) + 
  scale_fill_manual(values = c(good = RColorBrewer::brewer.pal(name = "Set1", n = 9)[3], bad = RColorBrewer::brewer.pal(name = "Set1", n = 9)[1])) +
  theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        legend.position = "None") + 
  geom_text(aes(x=Predicted, y=Role, label = Number), color = "black", size = 4) + 
  scale_y_discrete(limits = rev(levels(data_tile$class)))
)

data_tile %>% 
  filter(model=="BnM") %>%
  group_by(class) %>% 
  summarize(Sum=sum(amount)) %>% 
  mutate(Class=class, Role="Role") %>%
  ggplot() + 
  geom_bar(aes(x=Role, y=Sum, fill=Class), stat = 'identity', position = 'stack')


model_performance %>%
  filter(model.type %in% c("bnmrulec", "rule", "keras", "lstm", "lstma", "lstmconv", "lstmdist")) %>%
  mutate(model.data = ifelse(is.na(model.data) | model.data == "resample", "rule", model.data)) %>%
  mutate(model.epochs = ifelse(is.na(model.epochs), "1", model.epochs)) %>%
  mutate(model.nh = paste(model.units,model.hl)) %>% 
  select(model, class, precision, recall, starts_with("model")) %>% unique() %>%
  ggplot(aes(x=recall, y=precision)) + 
  geom_point(aes(color=model.data, shape=model.type), na.rm=TRUE) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) +
  guides(fill=guide_legend()) +
  theme(legend.position = "bottom") + 
  facet_grid(rows=vars(model.nh), cols=vars(class)) +
  scale_colour_brewer(palette="Set1")


# rule based evaluation
models_eval_rule <- c(
  "rule",
  "bnmrulec",
  "bnmrulec_resample_undersample2",
  "bnauto_rule",
  "bnauto_rule_resample_undersample2"
)
model_performance_rule <- model_performance %>% filter(model %in% models_eval_rule) %>%
  select(-tp, -fp, -tn, -fn, -cp, -cn, -pp, -pn, -fpr, -starts_with("model."), -observations, -sets) %>% 
  gather(measure, value, precision, recall, f1, markedness, informedness, accuracy,
         mean.f1, mean.precision, mean.recall, mean.markedness, mean.informedness) %>% 
  mutate(model = recode_factor(model, 
                               rule = "\\(Rule\\)", 
                               bnmrulec = "\\(BnM\\)", 
                               bnmrulec_resample_undersample2 = "\\(BnM_u\\)", 
                               bnauto_rule = "\\(BnA\\)", 
                               bnauto_rule_resample_undersample2 = "\\(BnA_u\\)"
  ))

(
  plot_measures_rule <- model_performance_rule %>% 
    filter(measure %in% c("f1","markedness","informedness")) %>%
    mutate(measure = factor(measure, levels = c("f1","markedness","informedness")),
           measure = recode_factor(measure, f1 = "F1", markedness = "Markedness", informedness = "Informedness"),
           class = factor(class, levels = c("speaker", "addressee", "member", "non-member")),
           class = recode_factor(class, speaker = "S", addressee = "A", member = "P", `non-member` = "N")) %>%
    ggplot() +
    geom_bar(aes(x=class, y=value, fill=model), stat = "identity", position = position_dodge(), na.rm = TRUE) + 
    facet_wrap(facets = vars(measure)) + 
    scale_fill_brewer(palette="Set1") + 
    scale_y_continuous(limits = c(0,1), name = "Value", breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.05)) + 
    scale_x_discrete(name = "Role") +
    labs(fill = "Model") +
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) + 
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.spacing.y = unit(0.0, 'cm'),
          legend.margin=margin(2,2,2,2),
          legend.box.margin=margin(-10,0,0,0),
          legend.key.size = unit(1,"line")
    )
)

(
  plot_f1_rule <- model_performance %>% 
    filter(model %in% models_eval_rule) %>% 
    filter(!is.na(mean.f1)) %>%
    select(model, mean.f1) %>% 
    mutate(model = recode_factor(model, 
                                 rule = "\\(Rule\\)", 
                                 bnmrulec = "\\(BnM\\)", 
                                 bnmrulec_resample_undersample2 = "\\(BnM_u\\)", 
                                 bnauto_rule = "\\(BnA\\)", 
                                 bnauto_rule_resample_undersample2 = "\\(BnA_u\\)"
    )) %>% unique() %>%
    ggplot() + 
    geom_bar(aes(x=model, y=mean.f1, fill=model), stat = "identity") +
    geom_text(aes(x=model,y=mean.f1, label=format(mean.f1, digits = 2), vjust = 1.5)) +
    labs(fill = "Model") +
    scale_fill_brewer(palette="Set1") + 
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.05), name="\\(F1_\\mu\\)") + 
    scale_x_discrete(name = "Model") +
    theme(legend.position = "none")
)

(
  plot_accuracy_rule <- model_performance %>% 
    filter(model %in% models_eval_rule) %>% 
    select(model, accuracy) %>% 
    mutate(Accuracy = accuracy) %>%
    mutate(model = recode_factor(model, 
                                 rule = "\\(Rule\\)", 
                                 bnmrulec = "\\(BnM\\)", 
                                 bnmrulec_resample_undersample2 = "\\(BnM_u\\)", 
                                 bnauto_rule = "\\(BnA\\)", 
                                 bnauto_rule_resample_undersample2 = "\\(BnA_u\\)"
    )) %>% unique() %>%
    ggplot() + 
    geom_bar(aes(x=model, y=Accuracy, fill=model), stat = "identity") +
    geom_text(aes(x=model,y=Accuracy, label=format(Accuracy, digits = 2), vjust=1.5)) +
    labs(fill = "Model") +
    scale_fill_brewer(palette="Set1") + 
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.05)) + 
    scale_x_discrete(name = "Model") +
    theme(legend.position = "none")
)

# time based evaluation
model_performance_time <- model_performance %>% 
  filter(observations==101668, seed.seeds > 1) %>% 
  filter(seed.seeds >= 5) %>% 
  select(-tp, -fp, -tn, -fn, -cp, -cn, -pp, -pn, -observations, -precision, -recall, -fpr, 
         -markedness, -informedness, -starts_with("mean"), -starts_with("accuracy")) %>% 
  mutate(modelconf = factor(paste(model.type,model.data,model.units,model.hl,sep=".")),
         modelconf = recode_factor(modelconf,
                                   keras.rule.128.1    = "D.R.128.1",
                                   keras.rule.128.4    = "D.R.128.4",
                                   keras.rule.512.1    = "D.R.512.1",
                                   keras.rule.512.4    = "D.R.512.4",
                                   keras.ruleraw.128.1 = "D.W.128.1",
                                   keras.ruleraw.128.4 = "D.W.128.4",
                                   keras.ruleraw.512.1 = "D.W.512.1",
                                   keras.ruleraw.512.4 = "D.W.512.4",
                                   keras.full.128.1    = "D.F.128.1",
                                   keras.full.128.4    = "D.F.128.4",
                                   keras.full.512.1    = "D.F.512.1",
                                   keras.full.512.4    = "D.F.512.4",
                                   lstm.rule.128.1     = "O.R.128.1",
                                   lstm.rule.128.4     = "O.R.128.4",
                                   lstm.rule.512.1     = "O.R.512.1",
                                   lstm.rule.512.4     = "O.R.512.4",
                                   lstm.ruleraw.128.1  = "O.W.128.1",
                                   lstm.ruleraw.128.4  = "O.W.128.4",
                                   lstm.ruleraw.512.1  = "O.W.512.1",
                                   lstm.ruleraw.512.4  = "O.W.512.4",
                                   lstm.full.128.1     = "O.F.128.1",
                                   lstm.full.128.4     = "O.F.128.4",
                                   lstm.full.512.1     = "O.F.512.1",
                                   lstm.full.512.4     = "O.F.512.4",
                                   lstmdist.rule.128.1     = "L.R.128.1",
                                   lstmdist.rule.128.4     = "L.R.128.4",
                                   lstmdist.rule.512.1     = "L.R.512.1",
                                   lstmdist.rule.512.4     = "L.R.512.4",
                                   lstmdist.ruleraw.128.1  = "L.W.128.1",
                                   lstmdist.ruleraw.128.4  = "L.W.128.4",
                                   lstmdist.ruleraw.512.1  = "L.W.512.1",
                                   lstmdist.ruleraw.512.4  = "L.W.512.4",
                                   lstmdist.full.128.1     = "L.F.128.1",
                                   lstmdist.full.128.4     = "L.F.128.4",
                                   lstmdist.full.512.1     = "L.F.512.1",
                                   lstmdist.full.512.4     = "L.F.512.4"
         ),
         seeds = factor(seed.seeds),
         accuracy.epoch = factor(paste("\\(Acc\\) @ ",format(parse_integer(model.epochs)),sep="")),
         f1.epoch = factor(paste("\\(F1_\\mu\\) @ ",format(parse_integer(model.epochs),sep=""))),
         f1s.epoch = factor(paste("S @ ",format(parse_integer(model.epochs),sep=""))),
         f1a.epoch = factor(paste("A @ ",format(parse_integer(model.epochs),sep=""))),
         f1m.epoch = factor(paste("P @ ",format(parse_integer(model.epochs),sep=""))),
         f1n.epoch = factor(paste("N @ ",format(parse_integer(model.epochs),sep="")))
  )
         

mycolors <- RColorBrewer::brewer.pal(name = "Set1", n = 9)
rule.accuracy <- (model_performance %>% filter(model=="rule"))$accuracy[1]
rule.f1 <- (model_performance %>% filter(model=="rule"))$mean.f1[1]
bnm.accuracy <- (model_performance %>% filter(model=="bnmrulec"))$accuracy[1]
bnm.f1 <- (model_performance %>% filter(model=="bnmrulec"))$mean.f1[1]
fun_plot_acc <- function(data, plot, errorbar.width=1, name.colorlab="Measure @ Epochs",color.scale=NA){
  pdata <- data %>% 
    mutate(model.hist = ifelse(is.na(model.hist),1,model.hist)) %>%
    select(starts_with("model"), starts_with("seed"), modelconf, seeds, accuracy.epoch, f1.epoch) %>% 
    select(-model.seed) %>% 
    unique() %>%
    arrange(accuracy.epoch,f1.epoch)
  scalecolor <- colorRampPalette(mycolors)(length(unique(pdata$model.epochs))*2)
  if(!anyNA(color.scale)){
    scalecolor <- color.scale
  }
  plot <- plot + 
    scale_x_discrete(name="Model Configuration") +
    geom_hline(yintercept = bnm.accuracy, linetype="dashed", color=mycolors[1]) + 
    geom_hline(yintercept = rule.accuracy, linetype="dotted", color=mycolors[1]) + 
    geom_hline(yintercept = bnm.f1, linetype="dashed", color=mycolors[2]) + 
    geom_hline(yintercept = rule.f1, linetype="dotted", color=mycolors[2]) + 
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=seed.mean.accuracy.lower, ymax=seed.mean.accuracy.upper, color=accuracy.epoch, linetype=seeds), size=1, width=errorbar.width, position="dodge", na.rm = TRUE) +
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=seed.mean.f1.lower, ymax=seed.mean.f1.upper, color=f1.epoch, linetype=seeds), size=1, width=errorbar.width, position="dodge", na.rm = TRUE) +
    scale_color_manual(values=scalecolor) +
    labs(color = name.colorlab, linetype = "\\(\\#\\) Seeds") + 
    guides(color=guide_legend(nrow=2,byrow=TRUE), linetype=FALSE) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.0, 'cm'),
      legend.margin=margin(2,2,2,2),
      legend.box.margin=margin(-10,0,0,0),
      legend.key.size = unit(1,"line")
    )
  return(plot)
}

# full plot
(
  plot_nn_acc_f1 <- fun_plot_acc(
    data=model_performance_time %>% filter(model.epochs %in% c("5", "20", "50")), 
    plot=ggplot() + 
      geom_vline(xintercept = seq(0,24,4)+0.5, linetype = "dashed", color = "gray") + 
      geom_text(data = tibble(names=rep(c("\\(rule\\)","\\(rule_{raw}\\)","\\(full\\)"),2), x= seq(2.5,24,4), y = 0.7), mapping = aes(x=x, y=y, label=names), color=mycolors[4]) +
      geom_line(data = tibble(x=c(7.5,8.5), y = c(rule.accuracy-0.025,rule.accuracy)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(16.5,15.5), y = c(rule.accuracy-0.025,bnm.accuracy)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(8.5,9.5), y = c(rule.f1+0.03,rule.f1)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(4.5,5), y = c(rule.f1+0.07,bnm.f1)), mapping = aes(x=x, y=y)) +
      geom_label(data = 
                   tribble(
                     ~names,                            ~x,  ~y,
                     "\\(BnM\\) model \\(F1_\\mu\\)",   4.5, bnm.f1+0.065,
                     "\\(Rule\\) model \\(F1_\\mu\\)",  8.5, bnm.f1+0.025,
                     "\\(Rule\\) model accuracy",       6.5, rule.accuracy-0.025,
                     "\\(BnM\\) model accuracy",       16.5, rule.accuracy-0.025), 
                 mapping = aes(x=x, y=y, label=names), fill="white")  
  ) +
    scale_y_continuous(limits=c(0.5,0.9), name="Value", breaks = seq(0,1,0.1), expand = c(0,0))
)

# plot simpler plot
(
  plot_nn_acc_f1_less <- fun_plot_acc(
    data=model_performance_time %>% filter(
      model.epochs == "50",
      model.units == "128",
      model.hl == "1"
      ) %>% 
      mutate(accuracy.epoch = "\\(Accuracy\\)",
             f1.epoch = "\\(F1_\\mu\\)"), 
    errorbar.width = 0.2,
    name.colorlab = "Measure",
    color.scale = c(mycolors[1],mycolors[2]),
    plot=ggplot() +
      geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray") + 
      geom_text(data = tibble(names=c("\\(dense\\)","\\(lstm\\)"), x = c(2,5), y = 0.7), mapping = aes(x=x, y=y, label=names), color="black")
  ) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.45,0.55)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color = mycolors[2], size=1) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.7,0.78)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color = mycolors[1], size=1) +
    #geom_label(data = 
    #             tribble(
    #               ~names,            ~x,  ~y,
    #               "\\(Accuracy\\)",   3.5, 0.7,
    #               "\\(F1_\\mu\\)",    3.5, 0.45),
    #           mapping = aes(x=x, y=y, label=names), fill="white") +
    guides(color=guide_legend(nrow=1,byrow=TRUE), linetype=FALSE) +
    scale_y_continuous(limits=c(0.5,0.9), name="Value", breaks = seq(0,1,0.1), expand = c(0,0)) + 
    theme(legend.position = "bottom")
)
# plot defence plot
(
  plot_nn_acc_f1_defence <- fun_plot_acc(
    data=model_performance_time %>% filter(model.epochs == "50"), 
    plot=ggplot() + 
      geom_vline(xintercept = seq(0,24,4)+0.5, linetype = "dashed", color = "gray") + 
      geom_text(data = tibble(names=rep(c("\\(rule\\)","\\(rule_{raw}\\)","\\(full\\)"),2), x= seq(2.5,24,4), y = 0.7), mapping = aes(x=x, y=y, label=names), color=mycolors[4]) +
      geom_line(data = tibble(x=c(7.5,8.5), y = c(rule.accuracy-0.025,rule.accuracy)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(16.5,15.5), y = c(rule.accuracy-0.025,bnm.accuracy)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(8.5,9.5), y = c(rule.f1+0.03,rule.f1)), mapping = aes(x=x, y=y)) +
      geom_line(data = tibble(x=c(4.5,5), y = c(rule.f1+0.07,bnm.f1)), mapping = aes(x=x, y=y)) +
      geom_label(data = 
                   tribble(
                     ~names,                            ~x,  ~y,
                     "\\(Bayes\\) model \\(F1_\\mu\\)",   4.5, bnm.f1+0.065,
                     "\\(Rule\\) model \\(F1_\\mu\\)",  8.5, bnm.f1+0.025,
                     "\\(Rule\\) model accuracy",       6.5, rule.accuracy-0.025,
                     "\\(Bayes\\) model accuracy",       16.5, rule.accuracy-0.025), 
                 mapping = aes(x=x, y=y, label=names), fill="white")  
  ) +
    scale_y_continuous(limits=c(0.5,0.9), name="Value", breaks = seq(0,1,0.1), expand = c(0,0))
)

rule.f1.class <- model_performance %>% filter(model=="rule") %>% select(class,f1) %>% arrange(class)
bnm.f1.class <- model_performance %>% filter(model=="bnmrulec") %>% select(class,f1) %>% arrange(class)
fun_plot_f1 <- function(data, plot, annotate.ranges=TRUE, errorbar.size=1, errorbar.width=1, color.scale=NA, name.colorlab="Class @ Epochs"){
  pdata <- data %>% 
    mutate(model.hist = ifelse(is.na(model.hist),1,model.hist)) %>%
    select(starts_with("model"), starts_with("seed"), modelconf, seeds, accuracy.epoch, f1s.epoch, f1a.epoch, f1m.epoch, f1n.epoch) %>% 
    select(-model.seed) %>% 
    unique() %>%
    arrange(f1s.epoch, f1a.epoch, f1m.epoch, f1n.epoch)
  scalecolor <- c(
    colorRampPalette(c(mycolors[1],mycolors[8]))(length(unique(pdata$model.epochs))),
    colorRampPalette(c(mycolors[3],mycolors[2]))(length(unique(pdata$model.epochs))),
    colorRampPalette(c(mycolors[4],mycolors[7]))(length(unique(pdata$model.epochs))),
    colorRampPalette(c(mycolors[6],mycolors[5]))(length(unique(pdata$model.epochs)))) # each class is ramped between two colors
  if(!anyNA(color.scale)){
    scalecolor <- color.scale
  }
  plot <- plot + 
    scale_x_discrete(name="Model Configuration") +
    scale_y_continuous(limits=c(0,1), name="F1", breaks = seq(0,1,0.1), expand = c(0,0))
  if(annotate.ranges){
    plot <- plot + 
      annotate("rect", xmin = 0.5, xmax = length(unique(pdata$modelconf))+0.5, ymin = c(
        pdata$seed.mean.f1.addressee.lower %>% min(na.rm = TRUE),
        pdata$seed.mean.f1.speaker.lower %>% min(na.rm = TRUE),
        pdata$seed.mean.f1.member.lower %>% min(na.rm = TRUE),
        pdata$`seed.mean.f1.non-member.lower` %>% min(na.rm = TRUE)
      ), ymax = c(
        pdata$seed.mean.f1.addressee.upper %>% max(na.rm = TRUE),
        pdata$seed.mean.f1.speaker.upper %>% max(na.rm = TRUE),
        pdata$seed.mean.f1.member.upper %>% max(na.rm = TRUE),
        pdata$`seed.mean.f1.non-member.upper` %>% max(na.rm = TRUE)
      ),
      alpha = 0.3, fill = c(mycolors[1],mycolors[6],mycolors[4],mycolors[3]))
  }
  plot <- plot + 
    geom_hline(yintercept = rule.f1.class[[4,2]], linetype="solid", color="black") + 
    geom_hline(yintercept = rule.f1.class[[3,2]], linetype="solid", color=mycolors[2]) + 
    geom_hline(yintercept = rule.f1.class[[1,2]], linetype="solid", color=mycolors[1]) + 
    geom_hline(yintercept = rule.f1.class[[2,2]], linetype="solid", color=mycolors[4]) + 
    geom_hline(yintercept = bnm.f1.class[[4,2]], linetype="dashed", color="black") + 
    geom_hline(yintercept = bnm.f1.class[[3,2]], linetype="dashed", color=mycolors[2]) + 
    geom_hline(yintercept = bnm.f1.class[[1,2]], linetype="dashed", color=mycolors[1]) + 
    geom_hline(yintercept = bnm.f1.class[[2,2]], linetype="dashed", color=mycolors[4]) + 
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=seed.mean.f1.speaker.lower, ymax=seed.mean.f1.speaker.upper, color=f1s.epoch, linetype=seeds), size=errorbar.size, width=errorbar.width, position="dodge", na.rm = TRUE) +
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=seed.mean.f1.addressee.lower, ymax=seed.mean.f1.addressee.upper, color=f1a.epoch, linetype=seeds), size=errorbar.size, width=errorbar.width, position="dodge", na.rm = TRUE) +
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=seed.mean.f1.member.lower, ymax=seed.mean.f1.member.upper, color=f1m.epoch, linetype=seeds), size=errorbar.size, width=errorbar.width, position="dodge", na.rm = TRUE) +
    geom_errorbar(data=pdata, aes(x=modelconf, ymin=`seed.mean.f1.non-member.lower`, ymax=`seed.mean.f1.non-member.upper`, color=f1n.epoch, linetype=seeds), size=errorbar.size, width=errorbar.width, position="dodge", na.rm = TRUE) +
    scale_color_manual(values=scalecolor) + 
    labs(color = name.colorlab, linetype = "\\(\\#\\) Seeds") + 
    guides(color=guide_legend(nrow=4,byrow=TRUE),linetype=FALSE) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.spacing.y = unit(0.0, 'cm'),
      legend.margin=margin(2,2,2,2),
      legend.box.margin=margin(-10,0,0,0),
      legend.key.size = unit(1,"line")
    )
}

(
  plot_nn_f1_class <- fun_plot_f1(
    data = model_performance_time %>% filter(model.epochs %in% c("5","20","50")),
    plot = ggplot()) + 
    geom_vline(xintercept = seq(0,24,4)+0.5, linetype = "dashed", color = "gray") + 
    geom_text(data = tibble(names=rep(c("\\(rule\\)","\\(rule_{raw}\\)","\\(full\\)"),2), x= seq(2.5,24,4), y = 0.8), mapping = aes(x=x, y=y, label=names), color=mycolors[4]) +
    geom_line(data = tibble(x=c(21,20.5), y = c(0.875,rule.f1.class[[3,2]])), mapping = aes(x=x, y=y), linetype="solid") +
    geom_line(data = tibble(x=c(21.5,22), y = c(0.875,bnm.f1.class[[3,2]])),  mapping = aes(x=x, y=y), linetype="dashed") +
    geom_line(data = tibble(x=c(4.5,4.5), y = c(0.725,rule.f1.class[[2,2]])), mapping = aes(x=x, y=y), linetype="solid") +
    geom_line(data = tibble(x=c(5.5,6), y = c(0.725,bnm.f1.class[[2,2]])),    mapping = aes(x=x, y=y), linetype="dashed") +
    geom_line(data = tibble(x=c(10,9.5), y = c(0.55,rule.f1.class[[4,2]])),   mapping = aes(x=x, y=y), linetype="solid") +
    geom_line(data = tibble(x=c(11,11.3), y = c(0.55,bnm.f1.class[[4,2]])),   mapping = aes(x=x, y=y), linetype="dashed") +
    geom_line(data = tibble(x=c(6.5,5.5), y = c(mean(c(bnm.f1.class[[1,2]],rule.f1.class[[1,2]])),rule.f1.class[[1,2]])), mapping = aes(x=x, y=y)) +
    geom_line(data = tibble(x=c(7.5,6.5), y = c(mean(c(bnm.f1.class[[1,2]],rule.f1.class[[1,2]])),bnm.f1.class[[1,2]])), mapping = aes(x=x, y=y), linetype="dashed") +
    geom_label(data = 
                 tribble(
                   ~names,              ~x,     ~y,
                   "Non-Participant",  20.5, 0.875,
                   "Side-Participant",  4.5, 0.725,
                   "Speaker",          10.5, 0.55,
                   "Addressee",         6.5, mean(c(bnm.f1.class[[1,2]],rule.f1.class[[1,2]]))), 
               mapping = aes(x=x, y=y, label=names), fill="white")
)
  
(
  plot_nn_f1_class_less <- fun_plot_f1(
    data = model_performance_time %>% 
      filter(
        model.epochs == "50",
        model.units == "128",
        model.hl == "1") %>% 
      mutate(
        f1s.epoch = "\\(Speaker\\)",
        f1a.epoch = "\\(Addressee\\)",
        f1m.epoch = "\\(Side-Participant\\)",
        f1n.epoch = "\\(Non-Participant\\)"
      ),
    annotate.ranges = FALSE,
    errorbar.width = 0.2,
    name.colorlab = "Class",
    color.scale = c(mycolors[1],mycolors[2],mycolors[4],"black"),
    plot = ggplot() +
      geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray") + 
      geom_text(data = tibble(names=c("\\(dense\\)","\\(lstm\\)"), x = c(2,5), y = 0.8), mapping = aes(x=x, y=y, label=names), color="black")
  ) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.875,0.925)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color=mycolors[2], size=1) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.725,0.675)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color=mycolors[4], size=1) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.575,0.475)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color="black", size=1) +
    #geom_line(data = tibble(x=c(3.5,3.5), y = c(0.1,0.275)), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc")), color=mycolors[1], size=1) +
    #geom_label(data = 
    #             tribble(
    #               ~names,            ~x,  ~y,
    #               "\\(Non-Participant\\)",   3.5, 0.875,
    #               "\\(Side-Participant\\)",  3.5, 0.725,
    #               "\\(Speaker\\)",           3.5, 0.575,
    #               "\\(Addressee\\)",         3.5, 0.1),
    #           mapping = aes(x=x, y=y, label=names), fill="white") +
    guides(color=guide_legend(nrow=1,byrow=TRUE), linetype=FALSE) + 
    theme(legend.position = "bottom")
)
   
  
show_models_long <- c("D.F.128.1", "L.F.512.1")
data <- model_performance_time %>% filter(modelconf %in% show_models_long, model.epochs != "5") %>% mutate()
(
  plot_nn_f1_class_long <- data %>% 
    filter(modelconf %in% show_models_long) %>% 
    select(starts_with("model"), starts_with("seed"), modelconf, seeds, accuracy.epoch, f1s.epoch, f1a.epoch, f1m.epoch, f1n.epoch) %>% 
    select(-model.seed) %>% 
    unique() %>%
    arrange(f1s.epoch, f1a.epoch, f1m.epoch, f1n.epoch) %>%
    ggplot() + 
    scale_x_discrete(name="Model Configuration") +
    scale_y_continuous(limits=c(0,1), name="F1", breaks = seq(0,1,0.1), expand = c(0,0)) + 
 geom_hline(yintercept = rule.f1.class[[4,2]], linetype="solid", color="black") + 
    geom_hline(yintercept = rule.f1.class[[3,2]], linetype="solid", color=mycolors[2]) + 
    geom_hline(yintercept = rule.f1.class[[1,2]], linetype="solid", color=mycolors[1]) + 
    geom_hline(yintercept = rule.f1.class[[2,2]], linetype="solid", color=mycolors[4]) + 
    geom_hline(yintercept = bnm.f1.class[[4,2]], linetype="dashed", color="black") + 
    geom_hline(yintercept = bnm.f1.class[[3,2]], linetype="dashed", color=mycolors[2]) + 
    geom_hline(yintercept = bnm.f1.class[[1,2]], linetype="dashed", color=mycolors[1]) + 
    geom_hline(yintercept = bnm.f1.class[[2,2]], linetype="dashed", color=mycolors[4]) + 
    geom_vline(xintercept = 1.5, linetype = "dashed", color = "gray") + 
    geom_errorbar(aes(x=modelconf, ymin=seed.mean.f1.speaker.lower, ymax=seed.mean.f1.speaker.upper, color=f1s.epoch, linetype=seeds), size=1, position="dodge", na.rm = TRUE) +
    geom_errorbar(aes(x=modelconf, ymin=seed.mean.f1.addressee.lower, ymax=seed.mean.f1.addressee.upper, color=f1a.epoch, linetype=seeds), size=1, position="dodge", na.rm = TRUE) +
    geom_errorbar(aes(x=modelconf, ymin=seed.mean.f1.member.lower, ymax=seed.mean.f1.member.upper, color=f1m.epoch, linetype=seeds), size=1, position="dodge", na.rm = TRUE) +
    geom_errorbar(aes(x=modelconf, ymin=`seed.mean.f1.non-member.lower`, ymax=`seed.mean.f1.non-member.upper`, color=f1n.epoch, linetype=seeds), size=1, position="dodge", na.rm = TRUE) +
    geom_text(data = tibble(names=show_models_long, x= c(1,2), y = 0.8), mapping = aes(x=x, y=y, label=names), color=mycolors[4]) +
    scale_color_manual(values=c(
      colorRampPalette(c(mycolors[1],mycolors[1]))(length(unique(data$model.epochs))),
      colorRampPalette(c(mycolors[2],mycolors[2]))(length(unique(data$model.epochs))),
      colorRampPalette(c(mycolors[4],mycolors[4]))(length(unique(data$model.epochs))),
      colorRampPalette(c("black","black"))(length(unique(data$model.epochs))))
    ) + # each class is ramped between two colors
    #geom_line(data = tibble(x=c(0.575,1.425), y = 0.1), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.01, "npc"), ends = "both"), size=1) +
    #geom_line(data = tibble(x=c(1.575,2.425), y = 0.1), mapping = aes(x=x, y=y), arrow = arrow(length = unit(0.03, "npc"), ends = "both"), size=1) +
    geom_text(data = 
                 tribble(
                   ~names,       ~x,  ~y,
                   "10 Epochs",   0.675, 0.05,
                   "500 Epochs",  1.325, 0.05,
                   "10 Epochs",   1.675, 0.05,
                   "500 Epochs",  2.325, 0.05),
               mapping = aes(x=x, y=y, label=names)) +
    labs(color = "Class @ Epochs", linetype = "\\(\\#\\) Seeds") + 
    guides(color=guide_legend(nrow=4,byrow=TRUE),linetype=FALSE) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "None",
      legend.box = "vertical",
      legend.spacing.y = unit(0.0, 'cm'),
      legend.margin=margin(2,2,2,2),
      legend.box.margin=margin(-10,0,0,0),
      legend.key.size = unit(1,"line")
    )
)

#library(RColorBrewer)
#display.brewer.all()
#display.brewer.pal(name = "Set1",n = 9)

## save informations for document
info <- list()
info["obs"] <- data_roles_full %>% nrow()
info["obs.s"] <- data_roles_full %>% filter(role.agent == "speaker") %>% nrow()
info["obs.a"] <- data_roles_full %>% filter(role.agent == "addressee") %>% nrow()
info["obs.m"] <- data_roles_full %>% filter(role.agent == "member") %>% nrow()
info["obs.n"] <- data_roles_full %>% filter(role.agent == "non-member") %>% nrow()
info["obs.sp"] <- format(info$obs.s / info$obs * 100, digits=2, nsmall = 2)
info["obs.ap"] <- format(info$obs.a / info$obs * 100, digits=2, nsmall = 2)
info["obs.mp"] <- format(info$obs.m / info$obs * 100, digits=2, nsmall = 2)
info["obs.np"] <- format(info$obs.n / info$obs * 100, digits=2, nsmall = 2)
info["max.acc.model"] <- (model_performance_time %>% filter(seed.mean.accuracy==max(seed.mean.accuracy)) %>% select(model.base))[[1,1]]
info["max.acc.acc"] <- format((model_performance_time %>% filter(seed.mean.accuracy==max(seed.mean.accuracy)) %>% select(seed.mean.accuracy))[[1,1]], digits = 2, nsmall = 2)


#write plots to files
fun_write_all_out <- function(){
  fun_write_info(info, filename="data.info_role.ini")
  fun_write_plot_tex(plot_measures_rule, 'role-rule-measures.tex', hfac=0.8*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_accuracy_rule, 'role-rule-accuracy.tex', hfac=0.8*plot_height_factor_golden_ratio, vfac=1*(5.9/11))
  fun_write_plot_tex(plot_f1_rule, 'role-rule-f1.tex', hfac=0.8*plot_height_factor_golden_ratio, vfac=1*(4.9/11))
  fun_write_plot_tex(plot_nn_acc_f1, 'role-nn-acc-f1.tex', hfac=2.0*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_nn_f1_class, 'role-nn-f1-class.tex', hfac=2.0*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_nn_acc_f1_less, 'role-nn-acc-f1-less.tex', hfac=1.5*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_nn_f1_class_less, 'role-nn-f1-class-less.tex', hfac=2.5*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_nn_f1_class_long, 'role-nn-f1-class-long.tex', hfac=2.4*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_cm_models, 'role-cm.tex', hfac=1*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_nn_acc_f1_defence, 'role-nn-acc-f1-beamer.tex', hfac=1.5*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_cm_models_beamer, 'role-cm-beamer.tex', hfac=1*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_cm_models_simple, 'role-cm-simple-beamer.tex', hfac=1*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_cm_models_counts, 'role-cm-counts-beamer.tex', hfac=1*plot_height_factor_golden_ratio)
}
if(!draft) {
  fun_write_all_out() 
}
if (FALSE) {
  load("study-role-workspace.RData")
}
