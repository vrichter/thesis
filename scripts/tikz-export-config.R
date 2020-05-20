# configure everything that is required to export plots in the right form
tikz_document_declaration <- system("cd .. > /dev/null; latexpand --makeatletter thesis.tex | sed '/begin{document}/Q'; cd - > /dev/null", intern = TRUE, ignore.stderr = TRUE)
tikz_document_declaration_beamer <- system("cd .. > /dev/null; latexpand --makeatletter defence.tex | sed '/begin{document}/Q'; cd - > /dev/null", intern = TRUE, ignore.stderr = TRUE)

# document info for rendering
latex_textwidth_in = 4.649232
latex_textwidth_beamer_in = 398.3386/72.27
plot_height_factor_golden_ratio = 0.618

# theming
theme_default = theme_get()
my_theme = 
theme_set(theme_default + 
            theme(text = element_text(family = "Times", size=11, colour="black"),
                  axis.text = element_text(family = "Times", size=11, colour="black")
            )
          )
color_low  = "#377eb8"
color_mid  = "#f7f7f7"
color_high = "#e41a1c"
color_very_low = "#4daf4a"
color_na   ='black'
color_defence_bg = '#fafafa'

fun_tikz_postprocessing <- function(filename, fix_date=TRUE, fix_image_inclusion=TRUE){
  processed <- readLines(filename)
  if (fix_date) {
    processed <- gsub( "(% Created by tikzDevice version 0.[0-9]+)( on.*)", "\\1", processed)
  }
  if (fix_image_inclusion) {
    processed <- gsub( "\\pgfimage", "\\includegraphics", processed)
  }
  cat(processed, file=filename, sep="\n")
}

fun_write_plot_tex <- function(plot, filename, vfac = 1.0, hfac = 1.0, ...) {
  tikz(filename, width = latex_textwidth_in*vfac, height = latex_textwidth_in*hfac, documentDeclaration = tikz_document_declaration, ...)
  print(plot)
  dev.off()
  fun_tikz_postprocessing(filename)
}

fun_write_plot_beamer_tex <- function(plot, filename, vfac = 1.0, hfac = 1.0, bgcolor = color_defence_bg, ...) {
  tikz(filename, width = latex_textwidth_beamer_in*vfac, height = latex_textwidth_beamer_in*hfac, documentDeclaration = tikz_document_declaration_beamer, ...)
  print(plot + theme(plot.background = element_rect(fill = bgcolor)))
  dev.off()
  fun_tikz_postprocessing(filename)
}

fun_write_info <- function(data, filename, ... ){
  con <- file(filename, "w")
  for (name in names(data)) {
    entry <- sprintf('%s = %s\n',name,data[[name]])
    cat(entry,file=con)
  }
  close(con)
}