fill.colour <- "lightgoldenrod2"
fill.colour.mid <- "lightgoldenrod3"
fill.colour.dark <- "lightgoldenrod4"
bar.outline <- "grey40"
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

british.palette <- c(
  English = rgb(255, 90, 90, max=255),
  Scottish = rgb(110, 110, 255, max=255),
  Welsh = rgb(255, 214, 0, max=255),
  N.Irish = rgb(20, 185, 90, max=255)
)


gs <- function(prefix, pl, width, height, dpi=300) {
  nm <- deparse(substitute(pl)) %>% 
    str_remove("^fig_")
  file <- file.path(prefix, glue("{nm}.png"))
  ggsave(file, pl, device="png", width=width, height=height, dpi=dpi)
}