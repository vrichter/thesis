library(ggplot2)
library(tidyverse)
library(reshape2)
library(tikzDevice)

setwd("~/sync/projects/publications/diss/data")
load("study-group-workspace.RData")

write_out = FALSE
if (Sys.getenv("WRITE_OUT") == "ON"){
  write_out = TRUE
}

source("../scripts/tikz-export-config.R")


# create plots
(
  plot_grid_im <- 
    data_ffm_grid %>% 
    filter(informedness >= 0, markedness >= 0) %>%
    ggplot() +
    geom_point(aes(x=informedness, y=markedness, color=algorithm, shape=agent), alpha=5/10) +
    facet_grid(row=vars(measure), cols=vars(observation.type)) + 
    scale_color_brewer(palette="Set1") +
    scale_shape_manual(values=c(3, 1)) +
    labs(x='Informedness', y="Markedness", color="Algorithm", shape="Agent")
)

(
  plot_grid_pr <- 
    data_ffm_grid %>% 
    ggplot() +
    geom_point(aes(x=recall, y=precision, color=algorithm, shape=agent), alpha=5/10) +
    facet_grid(cols=vars(measure), rows=vars(observation.type)) + 
    scale_color_brewer(palette="Set1") +
    scale_shape_manual(values=c(3, 1)) +
    labs(x='True Positive Rate (Recall)', y="Positive Predictive Value (Precision)", color="Algorithm", shape="Agent") + 
    coord_fixed(xlim=c(0,1),ylim=c(0,1)) + 
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.spacing.y = unit(0.0, 'cm'),
          legend.text = element_text(margin = margin(r = 10, unit = "pt")),
          legend.margin=margin(2,2,2,2),
          legend.box.margin=margin(-10,0,0,0),
          axis.text.x = element_text(angle = 60))
)

# plot flobi in group
(
  plot_ingroup_counts <- data_ffm_gco %>%
    mutate(agent=recode(agent, flobi_assistance="Flobi Assistance", flobi_entrance="Flobi Entrance")) %>%
    ggplot() + 
    geom_bar(aes(gt.group.size)) + 
    facet_wrap(vars(agent)) + 
    geom_text(stat='count', aes(x=gt.group.size, label=..count..), hjust=-0.2, angle=60) + 
    coord_cartesian(xlim = c(1, 12), ylim = c(0, 65000))  + 
    scale_x_discrete(name="Group Size", limits=seq(1,12,1)) + 
    xlab("Group Size") + 
    ylab("Times Observed Overall")
)

(
  plot_ffm_curves <-
    ffm_results %>% 
    filter(Threshold >= 0.5) %>% 
    mutate(Config = paste(Algorithm,Mdl,Stride,sep="-")) %>%
    filter(Config %in% 
             c("Gco-4500-50", 
               "Gco-6000-60", 
               "Grow-6000-50",
               "Shrink-4500-50"
             )
    ) %>%
    mutate(Detector=Config) %>% 
    gather(qm,"Value","Precision","Recall","F1","Markedness","Informedness") %>%
    mutate(qm=recode_factor(qm,Precision="Precision", Recall="Recall",F1="F1", Markedness="Markedness", Informedness="Informedness")) %>%
    ggplot(aes(x=Threshold, y=Value)) +
    geom_line(aes(color=Detector, linetype=Input),size=1) +
    facet_grid(rows=vars(Agent),cols=vars(qm)) + 
    scale_y_continuous(minor_breaks = seq(0 , 1, 0.05), breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(minor_breaks = seq(0 , 1, 0.05), breaks = seq(0.5, 1.0, 0.1)) + 
    coord_fixed(xlim = c(0.5, 1.0), ylim = c(0., 1.)) +
    scale_color_brewer(palette="Set1") +
    xlab("Tolerance Threshold") + 
    labs(color = "Detector:", linetype = "Input:") +
    guides(col = guide_legend(nrow=2)) +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.spacing.y = unit(0.0, 'cm'),
          legend.text = element_text(margin = margin(r = 10, unit = "pt")),
          legend.margin=margin(2,2,2,2),
          legend.box.margin=margin(-10,0,0,0),
          axis.text.x = element_text(angle = 60))
)

fun_plot_with_auc <- function(
  evaluation_data, 
  xdim, ydim, 
  xlabname, 
  ylabname,
  auc_x = 0.88,
  auc_y = seq(from=0.1, to=0.7, length.out = 5),
  ggbase = ggplot()
) 
{
  xdim_q <- enquo(xdim)
  ydim_q <- enquo(ydim)
  auc <- results_in_group %>%
    group_by(agent,feature) %>%
    summarize(auc=fun_auc(!!xdim_q,!!ydim_q)) %>%
    ungroup() %>%
    mutate(x=auc_x, y=auc_y, auc_string=sprintf("%.2f",round(auc,2)))
  ggbase +
    geom_line(data=evaluation_data, aes(x=!!xdim_q,y=!!ydim_q, color=feature, linetype=agent), size=1) +
    scale_color_brewer(palette="Set1") +
    scale_fill_grey(start=0.2, end=0.9) +
    labs(x=xlabname, y=ylabname, color="Feature", linetype="Agent", fill="Agent AUC") + 
    geom_label(data=auc, aes(x=x, y=y, label=auc_string, color=feature, fill=agent), fontface="bold")
}

(
  plot_in_group_roc <- fun_plot_with_auc(results_in_group, 
                                         fpr, recall, 
                                         "False Positive Rate (False Alarms)", "True Positive Rate (Recall)",
                                         rep(c(0.25,0.75),each=3),rev(c(rep(seq(0.25,0.5,0.125),2))),
                                         ggbase = ggplot() + 
                                           annotate("rect", xmin = 0.15, xmax = 0.85, ymin = 0.15, ymax = 0.6, alpha = 0.3) + 
                                           annotate("text", x = 0.5, y = 0.375, label = "\\footnotesize{AUC}")
  )
)

(
  plot_in_group_pr <- fun_plot_with_auc(results_in_group, 
                                        recall, precision, 
                                        "True Positive Rate (Recall)", "Positive Predictive Value (Precision)",
                                        rep(c(0.175,0.575),each=3),rev(c(rep(seq(0.25,0.5,0.125),2))),
                                        ggbase = ggplot() + 
                                          annotate("rect", xmin = 0.075, xmax = 0.675, ymin = 0.15, ymax = 0.6, alpha = 0.3) + 
                                          annotate("text", x = 0.375, y = 0.375, label = "\\footnotesize{AUC}")
  )
)

#write plots to files
fun_write_all_out <- function(){
  fun_write_info(info, filename="data.info_group.ini")
  fun_write_plot_tex(plot_ingroup_counts, 'group-ingroup-counts.tex', hfac=0.75*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_ffm_curves, 'group-ffm-evaluation.tex', hfac=1.5*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_in_group_roc, 'group-ingroup-roc.tex', hfac=1.*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_in_group_pr, 'group-ingroup-pr.tex', hfac=1.*plot_height_factor_golden_ratio)
}
if(write_out) { 
  fun_write_all_out()
}
