library(targets)
library(tarchetypes)
source("R/setup.R")
source("R/general.R")

options(tidyverse.quiet = TRUE, dplyr.summarise.inform = FALSE)

# attach R packages
required_packages <- read.delim("packages.txt", header = FALSE, col.names = "name")$name

# tar options
tar_option_set(
  packages = required_packages,
  format = "qs",
  trust_object_timestamps = TRUE
)

# Create dirs if necessary
for (d in c("fig")) if (!dir.exists(d)) dir.create(d)

# load all functions from .R files
files_R <- Sys.glob("targets/*/*.R")
sr_ <- sapply(files_R, source)

# load all functions from .R files
sesinfo <- list(
  tar_target(session_info, sessionInfo())
)

# Add session info
sesinfo <- list(
  tar_force(session_info, sessionInfo(), force = TRUE)
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

