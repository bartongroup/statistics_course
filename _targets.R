library(targets)
library(tarchetypes)
source("R/setup.R")
source("R/general.R")

packages <- c("biomaRt", "viridis", "cowplot", "ggridges", "ggbeeswarm", "ggforce", "lemon", "dendextend", "gganimate", "ggalt", "gsl", "edgeR", "limma","glue", "furrr", "kolmim", "pwr", "Rfast", "qvalue", "tidyverse")
tar_option_set(packages = packages, format = "qs")
options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# for interactive session only
if(interactive()) sapply(packages, library, character.only=TRUE)

files_R <- Sys.glob("targets/*/*.R")
sr_ <- sapply(files_R, source)

sesinfo <- list(
  tar_target(session_info, sessionInfo())
)


c(
  sesinfo,
  lecture_00(),
  lecture_01(),
  lecture_02(),
  lecture_03(),
  lecture_04(),
  lecture_05(),
  lecture_06(),
  lecture_07(),
  lecture_08(),
  lecture_09(),
  lecture_10(),
  lecture_11(),
  lecture_12(),
  lecture_13(),
  lecture_14()
)

