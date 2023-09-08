fill.colour <- "lightgoldenrod2"
fill.colour.mid <- "lightgoldenrod3"
fill.colour.dark <- "lightgoldenrod4"
bar.outline <- "grey40"
ef_palette <- c("lightgoldenrod3", "#009E73")
okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#F0E442", "#0072B2", "#D55E00")

british.palette <- c(
  English = rgb(255, 90, 90, maxColorValue = 255),
  Scottish = rgb(110, 110, 255, maxColorValue = 255),
  Welsh = rgb(255, 214, 0, maxColorValue = 255),
  N.Irish = rgb(20, 185, 90, maxColorValue = 255)
)

theme_clean <- 
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank())

theme_dist <- ggplot2::theme_classic() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.line.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

theme_d <- ggplot2::theme(
  axis.title = ggplot2::element_blank(),
  axis.ticks = ggplot2::element_blank(),
  axis.text = ggplot2::element_blank(),
  axis.line.y = ggplot2::element_blank(),
  panel.border = ggplot2::element_blank()
)


gs <- function(pl, prefix, width, height, nm = NULL, dpi = 300, dev = "png") {
  if(!dir.exists(prefix)) dir.create(prefix, recursive = TRUE)
  
  if(is.null(nm)) nm <- deparse(substitute(pl)) |> str_remove("^fig_") |> str_remove("^.+\\$")
  file <- file.path(prefix, str_glue("{nm}.{dev}"))
  ggsave(file, pl, device = dev, width=width, height=height, dpi=dpi)
}


gpl <- function(pl, prefix, width, height, nm = NULL) {
  if(!dir.exists(prefix)) dir.create(prefix, recursive = TRUE)
  
  if(is.null(nm)) nm <- deparse(substitute(pl)) |> str_remove("^fig_") |> str_remove("^.+\\$")
  file <- file.path(prefix, str_glue("{nm}.pdf"))
  print(file)
  pdf(file, width = width, height = height)
  plot(pl)
  dev.off()
}


ans <- function(pl, prefix, width, height, nm = NULL, dpi = 300, nframes = 1000, fps = 30) {
  if(!dir.exists(prefix)) dir.create(prefix, recursive = TRUE)
  
  if(is.null(nm)) nm <- deparse(substitute(pl)) |> str_remove("^fig_") |> str_remove("^.+\\$")
  file <- file.path(prefix, glue("{nm}.gif"))
  anim <- animate(pl, nframes = nframes, fps = fps, width = width, height = height, units = "in", res = dpi,
                  renderer = gifski_renderer(loop = FALSE))
  anim_save(file, animation = anim)
}
