library(targets)
library(tarchetypes)
library(future)
library(future.callr)
plan(callr)
source("R/setup.R")
source("R/general.R")

packages <- c("biomaRt", "viridis", "cowplot", "ggridges", "ggbeeswarm", "ggforce", "dendextend", "edgeR", "limma","glue", "tidyverse")
tar_option_set(packages = packages, format = "qs")
options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# for interactive session only
# sapply(packages, library, character.only=TRUE)

TOP_DIR <- getwd()

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
  lecture_03()
)

