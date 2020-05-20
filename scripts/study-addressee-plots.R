library(ggplot2)
library(tidyverse)
library(reshape2)
library(tikzDevice)

setwd("~/sync/projects/publications/diss/data")
load("study-addressee-workspace.RData")

write_out = FALSE
if (Sys.getenv("WRITE_OUT") == "ON"){
  write_out = TRUE
}

source("../scripts/tikz-export-config.R")


# create plots
(
  plot_cramer_all <- cramer %>%
    melt(id=c("Names")) %>%
    ggplot() +
    geom_tile(aes(x=Names, y=variable, fill=value)) +
    scale_fill_gradient2(low=color_low, mid=color_mid, high=color_high, na.value=color_na, midpoint=0.5) +
    scale_x_discrete(limits=cramer_sort) +
    scale_y_discrete(limits=cramer_sort) +
    labs(x=NULL, y=NULL) +
    guides(fill = guide_legend(title = "Cram\\'er's \\~V", reverse=TRUE)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_fixed()
)

(
  plot_chisq_all <- chisq %>%
    melt(id=c("Names")) %>% mutate(value=factor(value,levels=chisq_levels, ordered=TRUE)) %>%
    ggplot() +
    geom_tile(aes(x=Names, y=variable, fill=value)) +
    scale_x_discrete(limits=chisq_sort) +
    scale_y_discrete(limits=chisq_sort) +
    labs(x=NULL, y=NULL) +
    guides(fill = guide_legend(title = "Sign. level", reverse=TRUE)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_fixed() + scale_fill_brewer(palette="RdBu", direction = -1, na.value='black')
)

(
  plot_adr_foa <- ggplot(data = melt(filter(addressees, Addressee != "Ap"), id.vars=c('Addressee','Reduced'))) +
    geom_bar(mapping = aes(x=reorder(Addressee, -value), y=value, fill=reorder(Addressee,-value), color=Reduced),
             stat='identity', size=1.0) +
    guides(fill=FALSE, color=FALSE) +
    facet_grid(rows = vars(variable)) +
    labs(x='Entity', y='Count') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_manual(values=colorRampPalette(RColorBrewer::brewer.pal(5,"Set1"))(nrow(addressees))) +
    scale_color_manual( values = c( 'TRUE'="black", '-'="white" ), guide = FALSE ) # use - instead of false to not set the color
)

(
  plot_adr_foa_reduced <- ggplot(data = melt(filter(addressees, Reduced == TRUE), id.vars=c('Addressee','Reduced'))) +
    geom_bar(mapping = aes(x=reorder(Addressee, -value), y=value, fill=(variable)),
             stat='identity', size=1.0, position=position_dodge2()) +
    labs(x='Entity', y='Count', color='', fill='') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_manual( values = c( 'TRUE'="black", '-'="white" ), guide = FALSE ) # use - instead of false to not set the color
)

(
  plot_adr_by_task <- data %>%
    group_by(`Wizard task`) %>%
    count(`Addressee final (reduced) Code`) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    geom_bar(mapping = aes(x=`Wizard task`, y=freq, fill=`Addressee final (reduced) Code`), stat='identity') +
    labs(x='Wizard task', y='Addressed') +
    guides(fill = guide_legend(title = "Entity")) +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "none")
)

(
  plot_adr_equals_by_task <- data %>%
    group_by(`Wizard task`) %>%
    count(`Addressee equals focus`) %>%
    mutate(all = sum(n)) %>%
    filter(`Addressee equals focus` == TRUE) %>%
    mutate(pval = prop.test(n,all)$p.value,
           lower = prop.test(n,all)$conf.int[1],
           upper=prop.test(n,all)$conf.int[2],
           estimate=prop.test(n,all)$estimate) %>%
    ggplot(aes(x=`Wizard task`, y=estimate, fill=`Wizard task`)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    labs(x='Wizard task', y='\\(P(match)\\)') +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "none")
)

(
  plot_adr_equals_by_vpnum <- data %>%
    group_by(`Vp_num`) %>%
    count(`Addressee equals focus`) %>%
    mutate(freq = n / sum(n)) %>%
    ggplot() +
    scale_y_continuous(breaks = c(0,0.5,1.0)) +
    scale_x_discrete(breaks = c(27,38,52)) +
    geom_bar(mapping = aes(x=Vp_num, y=freq, fill=`Addressee equals focus`), stat='identity') +
    geom_hline(yintercept=addressee_equals$p, linetype="dotted", color = "white", size=2) +
    labs(x='Participant', y='Match') +
    guides(fill = guide_legend(title = "Equals")) +
    scale_fill_brewer(palette='Set1',label=c('no','yes')) +
    theme(legend.position="none")
)

(
  plot_equality_from_foa <- data %>%
    group_by(`Focus of attention (reduced) Code`) %>%
    count(`Addressee equals focus`) %>%
    mutate(all = sum(n)) %>%
    filter(`Addressee equals focus` == TRUE) %>%
    mutate(pval = prop.test(n,all)$p.value,
           lower = prop.test(n,all)$conf.int[1],
           upper=prop.test(n,all)$conf.int[2],
           estimate=prop.test(n,all)$estimate) %>%
    ggplot(aes(x=`Focus of attention (reduced) Code`, y=estimate, fill=`Focus of attention (reduced) Code`)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    labs(x='Focus of attention', y='\\(P(match)\\)') +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "none")
)

(
  plot_cv <- cv_result %>%
    ggplot(aes(x=network, y=mean, fill=network)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    facet_grid(cols=vars(cv_result$data)) +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(margin = margin(r = 10, unit = "pt"))
    ) +
    labs(x=NULL, y='Accuracy') +
    guides(fill=guide_legend(title = "Classifier", title.theme = element_text(margin = margin(r=10, unit= "pt"))))
)

cv_result_short <- cv_result %>%
    mutate(network = recode_factor(network,`BF  `="Attention", `BM  `="Manual")) %>%
    mutate(data = recode_factor(data,Speech="Speech", Visual="Visual", Observable="Speech + Visual")) %>%
    filter(network %in% c("Attention", "Manual")) %>%
    mutate(network = droplevels(network), data = droplevels(data))
(
  plot_cv_short <- cv_result_short %>% 
    ggplot(aes(x=network, y=mean, fill=network)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    facet_grid(cols=vars(cv_result_short$data)) +
    scale_fill_brewer(palette='Set1') +
    theme(legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(margin = margin(r = 10, unit = "pt"))
    ) +
    labs(x=NULL, y='Accuracy with 95\\% confidence intervals') +
    guides(fill=guide_legend(title = "Model", title.theme = element_text(margin = margin(r=10, unit= "pt"))))
)

(
plot_cv_values <- cv_result %>%
    group_by(data) %>% 
    mutate(highlight=lower>min(upper)) %>% 
    ungroup() %>%
    mutate(
    data = fct_rev(data),
    bf = ifelse(highlight,"",""),
    value=sprintf("\\(%s{%.2f \\pm %.2f}\\)",bf, mean, upper-lower)
  ) %>% 
    ggplot() + 
    geom_tile(aes(x=network, y=data), color="black", alpha=0.0) + 
    geom_text(aes(x=network, y=data, label = value)) + 
    scale_color_brewer(palette = 'Set1') +
    labs(x="Classifier", y="Variable Set") + 
    theme(panel.background = element_blank())
)

plot_cv_values <- cv_result %>%
    group_by(data) %>% 
    mutate(highlight=lower>min(upper)) %>% 
    ungroup() %>%
    mutate(
    bf = ifelse(highlight,"",""),
    value=sprintf("\\(%s{%.2f \\pm %.2f}\\)",bf, mean, upper-lower)
  ) %>%
  select(network, data, value) %>%
  spread(data, value)

#write to files
fun_write_all_out <- function(){
  fun_write_info(info, filename="data.info_study-addressing-apartment.ini")
  fun_write_plot_tex(plot_adr_foa, 'countsplot1.tex', hfac=plot_height_factor_golden_ratio, vfac=1)
  fun_write_plot_tex(plot_adr_foa_reduced, 'countsplot2.tex', hfac=0.6*plot_height_factor_golden_ratio, vfac=1)
  fun_write_plot_tex(plot_adr_equals_by_task, 'adr_equals_task.tex', vfac=0.55, hfac=0.55*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_adr_equals_by_vpnum, 'adr_equals_vpnum.tex', vfac=0.49, hfac=0.48*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_equality_from_foa, 'equality_from_foa.tex', vfac=0.49, hfac=0.60*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_adr_by_task, 'adr_by_task.tex', vfac=0.49, hfac=0.60*plot_height_factor_golden_ratio)
  fun_write_plot_tex(plot_cramer_all, 'cramer_all.tex', hfac=0.75, vfac=1)
  fun_write_plot_tex(plot_chisq_all, 'chisq_all.tex', hfac=0.75, vfac=1)
  fun_write_plot_tex(plot_cv, 'cv.tex', hfac=0.8*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_cramer_all, 'cramer_all-beamer.tex', hfac=0.75, vfac=1)
  fun_write_plot_beamer_tex(plot_chisq_all, 'chisq_all-beamer.tex', hfac=0.75, vfac=1)
  fun_write_plot_beamer_tex(plot_adr_by_task, 'adr_by_task-beamer.tex', vfac=0.49, hfac=0.60*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_equality_from_foa, 'equality_from_foa-beamer.tex', vfac=0.49, hfac=0.60*plot_height_factor_golden_ratio)
  fun_write_plot_beamer_tex(plot_cv_short, 'cv-beamer.tex', hfac=2.*plot_height_factor_golden_ratio)
  write_csv(plot_cv_values, 'cv-values.csv')
}

if(write_out) { 
  fun_write_all_out()
}
