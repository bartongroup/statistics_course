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
  m <- length(d)
  x <- breaks[-1] - bins
  y <- as.numeric(table(cut(d, breaks, right=TRUE)))
  y <- y / (bins * sum(y))
  
  n <- length(x)
  st <- data.frame(
    x = c(x, x[n] + bins),
    y = c(y, y[n])
  )
  geom_step(data=st, aes(x, y), ...)
}

plotOneDist <- function(d, name, title, limits, fun=NULL, ..., bins=100, f.bins=100,
                        maxy=NA, alpha.line=1, with.outline=FALSE, outline.colour="grey50") {
  brks <- seq(limits[1], limits[2], length.out = bins)
  g <- ggplot(data.frame(d=d), aes(x=d, y=..density..)) +
    geom_histogram(breaks=brks, fill=fill.colour.mid) +
    labs(x=name, y="Density", title=title) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, maxy))
  if(with.outline) g <- g + geom_outline(d, brks, colour=outline.colour)
  if(!is.null(fun)) {
    x <- seq(limits[1], limits[2], length.out = f.bins)
    y <- fun(x, ...)
    g <- g + geom_line(data=data.frame(x=x, y=y), aes(x, y), size=0.5, alpha=alpha.line)
  }
  g
}

plotDistributionCut <- function(x, cut=NULL, side="upper", brks=seq(-1, 1, 0.01), xlab="", fill=fill.colour.mid) {
  df <- data.frame(x = x)
  
  dst <- ggplot(df, aes(x=x, y=..density..)) +
    theme_classic() +
    scale_x_continuous(expand=c(0,0)) +
    geom_histogram(breaks=brks, fill=fill) +
    labs(x=xlab, y="Normalized frequency")
  maxh <- max(ggplot_build(dst)$data[[1]]$density)  # max density in histogram
  dst <- dst + scale_y_continuous(expand=c(0,0), limits=c(0, maxh*1.03))
  
  gout <- geom_outline(x, brks, size=0.3)
  
  if(!is.null(cut)) {
    cut <- brks[which.min(abs(brks - cut))]
    cut.lo <- cut
    cut.up <- cut
    if(side == "both") cut.lo <- -cut.lo
    df.cut.lower <- df[df$x <= cut.lo,, drop=FALSE]
    df.cut.upper <- df[df$x >= cut.up,, drop=FALSE]
    norm.fac.lower <- nrow(df.cut.lower) / nrow(df)
    norm.fac.upper <- nrow(df.cut.upper) / nrow(df)
    g.upper <- geom_histogram(data=df.cut.upper, breaks=brks, fill=fill.colour.dark, aes(y=..density.. * norm.fac.upper))
    g.lower <- geom_histogram(data=df.cut.lower, breaks=brks, fill=fill.colour.dark, aes(y=..density.. * norm.fac.lower))
    if(side == "upper") {
      return(dst + g.upper + gout)
    } else if (side == "lower") {
      return(dst + g.lower + gout)
    } else if(side == "both") {
      return(dst + g.upper + g.lower + gout)
    }
  } else {
    return(dst + gout)
  }
}

se <- function(x) {
  sd(x, na.rm=TRUE) / sqrt(length(x))
}
