library(ggplot2)
library(tidyverse)
library(reshape2)
library(tikzDevice)

setwd("~/sync/projects/publications/diss/data")
load("study-meka-workspace.RData")

write_out = FALSE
if (Sys.getenv("WRITE_OUT") == "ON"){
  write_out = TRUE
}

source("../scripts/tikz-export-config.R")


# create plots
(
  plot_da_counts <- 
    data %>% 
    mutate(an.addressed=recode(as.numeric(an.addressed), `1`="Robot", `0`="Other")) %>%
    group_by(h.dialogact, an.addressed) %>% count() %>%
    mutate(hline = 10-as.numeric(h.dialogact=="Light Off")*5) %>%
    ggplot(., aes(x=h.dialogact,y=n,fill=an.addressed)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymax=hline, ymin=hline), width=plot_height_factor_golden_ratio) + 
    scale_fill_brewer(palette='Set1') +
    theme(
      legend.position = "right",
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
    ) +
    labs(x='Dialogue Acts', y='Summed recognitions') + 
    guides(fill=guide_legend("Addressee")) + 
    coord_flip()
)

fun_plot_performance <- function(data, classifier, xlab) {
  dor_factor <- 1/(data %>% filter(cl==classifier))[[1,"dor_factor"]]
  data %>% 
    filter(cl==classifier, !is.na(value)) %>%
    ggplot(aes(x=base, y=value, fill=base)) +
    facet_grid(cols=vars(value_type)) +
    geom_bar(stat='identity') +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, na.rm = TRUE) +
    guides(fill = guide_legend(title = NULL)) + 
    labs(x=xlab, y=NULL) +
    scale_fill_brewer(palette='Set1') +
    scale_y_continuous(sec.axis = sec_axis(~.*dor_factor)) +
    theme(
      legend.position = "bottom", 
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(),
      legend.text = element_text(margin = margin(r = 10, unit = "pt"))
    )
}

( plot_cl_performance_mouth <- fun_plot_performance(cl_performance,"mouthmovement","Speaker classification") )
( plot_cl_performance_gaze <- fun_plot_performance(cl_performance,"mutualgaze","Mutual gaze recognition") )
( plot_cl_performance_mad <- fun_plot_performance(cl_performance,"mutualgaze-addressee","Addressee from mutual gaze") )

fun_calc_curve_and_plot <- function(
  model_data, 
  base_data, 
  xdim, ydim, 
  xlabname, 
  ylabname,
  auc_x = 0.88,
  auc_y = seq(from=0.1, to=0.7, length.out = 5),
  plot_as_points = FALSE,
  ggbase = ggplot()
  ) {
  xdim_q <- enquo(xdim)
  ydim_q <- enquo(ydim)
  result <- model_data %>% 
    filter(!is.na(!!xdim_q), !is.na(!!ydim_q)) %>% 
    mutate(xdim = !!xdim_q,
           ydim = !!ydim_q,
           model=factor(model,c("All","Both+Self","Both","Gaze","Mouth")))
  if (!plot_as_points) {
    result <- result %>% group_by(model, data, xdim) %>% 
    summarise(ydim=mean(ydim)) %>% 
    ungroup()
  }
  auc <- result %>%
    group_by(data,model) %>% 
    summarise(val = fun_calc_auc(xdim,ydim)) %>% 
    mutate(model=factor(model,c("All","Both+Self","Both","Gaze","Mouth")),
           auc_string = sprintf("%.2f",round(val,2), sep="")) %>% 
    arrange(desc(model)) %>% 
    mutate(x=auc_x, y=auc_y)
  plot <- ggbase +
    geom_label(data=auc, aes(x=x, y=y,label=auc_string, color = model), fontface="bold")
  if (plot_as_points) {
    plot = plot + geom_point(data=result, aes(x=xdim, y=ydim, color=model))
  } else {
    plot = plot + geom_line(data=result, aes(x=xdim, y=ydim, color=model), size=1)
  }
  plot = plot + 
    geom_point(data=base_result, aes(x=!!xdim_q, y=!!ydim_q, shape=model)) +
    coord_fixed(ratio = 1, expand = TRUE, clip = "on") +
    labs(x=xlabname, y=ylabname, color="BN:", shape="Study:") +
    facet_grid(rows="data") +
    guides(color = guide_legend(order=1),
           shape = guide_legend(order=2)) +
    theme(
      panel.spacing.x=unit(1, "lines"), 
      legend.position = "bottom", 
      legend.box = "vertical",
      legend.box.spacing = unit(0.1, "lines"),
      legend.spacing.y=unit(0, "lines")
      ) +
    scale_y_continuous(minor_breaks = seq(0 , 1, 0.05), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(minor_breaks = seq(0 , 1, 0.05), breaks = seq(0, 1, 0.2)) +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1") +
    scale_shape_manual(values=c(15,17,5,3))
    return(list("result"=result, "auc"=auc, "plot"=plot))
}
roc_orig <- fun_calc_curve_and_plot(result, base_data, 
                               `fpr`, `tpr`, 
                               "False Positive Rate (False Alarms)", "True Positive Rate (Recall)", 
                               ggbase = ggplot() + 
                                 annotate("rect", xmin = 0.45, xmax = 1, ymin = 0.0225, ymax = 0.775, alpha = 0.3) + 
                                 annotate("text", x = 0.575, y = 0.4, label = "\\footnotesize{AUC}")
                               )

(roc_orig$plot)
roc_pr <- fun_calc_curve_and_plot(result, base_data, 
                               `recall`, `precision`, 
                               "Recall", "Precision", 
                               auc_x = rev(c(0.2, 0.5, 0.8, 0.35, 0.65)),
                               auc_y = rev(c(0.6, 0.6, 0.6, 0.15, 0.15)),
                               ggbase = ggplot() + 
                                 geom_hline(yintercept=info$prev.addressed,linetype="dashed") +
                                 annotate("rect", xmin = 0.075, xmax = 0.925, ymin = 0.075, ymax = 0.675, alpha = 0.3) + 
                                 annotate("text", x = 0.5, y = 0.375, label = "\\footnotesize{AUC}")
                               )
(roc_pr$plot)

auc_table <- bind_rows(
  roc_orig$auc %>% mutate(measurement = "roc"),
  roc_pr$auc %>% mutate(measurement = "pr"))

for (i in 1:nrow(auc_table)) {
  x <- auc_table[i,]
  name <- sprintf("model.auc%s.%s.%s",x[["measurement"]],x[["data"]],x[["model"]])
  value <- x[["auc_string"]]
  info[[name]] <- value
}

#write plots to files
fun_write_all_out <- function(){
  fun_write_info(info, filename="data.info_meka.ini")
  fun_write_plot_tex(plot_da_counts, 'meka-da-countsplot.tex', hfac=0.7*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_cl_performance_mouth, 'meka-perf-mouth.tex', hfac=0.7*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_cl_performance_gaze,  'meka-perf-gaze.tex', hfac=0.7*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_cl_performance_mad,   'meka-perf-mad.tex', hfac=0.7*plot_height_factor_golden_ratio)
  fun_write_plot_tex(roc_orig$plot,             'meka-roc-models.tex', hfac=1.1668*plot_height_factor_golden_ratio)
  fun_write_plot_tex(roc_pr$plot,               'meka-rocpr-models.tex', hfac=1.1668*plot_height_factor_golden_ratio)
}

if(write_out) { 
  fun_write_all_out()
}
