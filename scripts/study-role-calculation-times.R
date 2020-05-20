library(ggplot2)
library(ggrepel)
library(tikzDevice)
library(rlang)
library(tidyverse)
library(Rgraphviz)
datadir = "~/sync/projects/publications/diss/data"
setwd(datadir)

data <- read_tsv("ann_calculation_time.csv", 
                 col_names = c("id","type","runtime","status","exit","enddate"), 
                 col_types = cols(
                   .default = col_character(),
                   id = col_integer(),
                   type = col_factor(),
                   runtime = col_character(),
                   status = col_factor(),
                   exit = col_integer(),
                   enddate = col_datetime(format = "%Y-%m-%d %H%M")
                 ))
times <- str_match(data$runtime, pattern="([0-9]+)?-?([0-9][0-9]):([0-9][0-9]):([0-9][0-9])")[,2:5]
data <- data %>% mutate(
  runtime = (ifelse(is.na(times[,1]),0,parse_integer(times[,1])))*24 + 
    parse_integer(times[,2]) +
    parse_integer(times[,3])/60 +
    parse_integer(times[,4])/3600
  )

chapters <- c("introduction","related-work","study-addressee","study-meka","study-corpus","study-group","study-role","conclusion")
writing_data <- read_tsv("writing.tsv", 
                 col_names = c("date","build","words","chapter"),
                 col_types = cols(
                   .default = col_character(),
                   date = col_datetime(format="%Y-%m-%d %H:%M:%S %z"),
                   build = col_integer(),
                   words = col_integer(),
                   chapter = col_character()
                 )) %>% 
  mutate(chapter = str_remove(chapter,"^chapter-") %>% str_remove("[.]tex")) %>%
  filter(chapter %in% chapters) %>%
  mutate(chapter = factor(chapter, levels = rev(chapters)))

writing_data %>% ggplot() + geom_area(aes(x=date,y=words, fill=chapter))

                                      