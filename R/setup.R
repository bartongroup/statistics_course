library(ggplot2)
library(gganimate)
library(knitr)
library(kableExtra)
library(gridExtra)
library(cowplot)
library(ggridges)
suppressMessages(library(ggridges))
suppressMessages(library(viridis))
suppressMessages(library(dplyr))
library(parallel)

theme_dist <- theme_classic()

fill.colour <- "lightgoldenrod2"
fill.colour.mid <- "lightgoldenrod3"
fill.colour.dark <- "lightgoldenrod4"
bar.outline <- "grey40"
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

british.palette <- c(
  English = rgb(255, 90, 90, max=255),
  Scottish = rgb(110, 110, 255, max=255),
  Welsh = rgb(255, 214, 0, max=255),
  N.Irish = rgb(20, 185, 90, max=255)
)

myKable <- function(df, row.names=FALSE, col.names=NA, digits=2, bootstrap="condensed") {
  kable(df, format="html", row.names=row.names, col.names=col.names, digits=digits) %>% kable_styling(bootstrap_options=bootstrap, full_width=FALSE, position="left", font_size=12)
}

geom_outline <- function(d, breaks, ...) {
  bins <- breaks[2] - breaks[1]
  st <- data.frame(
    x = breaks[-1] - bins,
    y = as.numeric(table(cut(d, breaks))) / (bins * length(d))
  )
  geom_step(data=st, aes(x, y), ...)
}

plotOneDist <- function(d, name, title, limits, fun, ..., bins=100, f.bins=100, maxy=NA) {
  brks <- seq(limits[1], limits[2], length.out = bins)
  g <- ggplot(data.frame(d=d), aes(x=d, y=..density..)) +
    geom_histogram(breaks=brks, fill=fill.colour.mid) +
    #geom_outline(d, brks, colour="grey50") +
    labs(x=name, y="Density", title=title) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, maxy))
  if(!is.null(fun)) {
    x <- seq(limits[1], limits[2], length.out = f.bins)
    y <- fun(x, ...)
    g <- g + geom_line(data=data.frame(x=x, y=y), aes(x, y), size=0.5)
  }
  g
}
