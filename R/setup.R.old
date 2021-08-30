suppressPackageStartupMessages({

library(tidyverse)
library(gganimate)
library(ggbeeswarm)
library(lemon)
library(knitr)
library(kableExtra)
library(gridExtra)
library(cowplot)
library(ggridges)
library(reshape2)
library(ggridges)
library(viridis)
library(parallel)
  
})

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

geom_outline <- function(d, breaks, ..., y.value="density") {
  bins <- breaks[2] - breaks[1]
  m <- length(d)
  x <- breaks[-1] - bins
  y <- as.numeric(table(cut(d, breaks, right=TRUE)))
  if(y.value == "density") {
    y <- y / (bins * sum(y))
  }
  
  n <- length(x)
  st <- data.frame(
    x = c(x, x[n] + bins),
    y = c(y, y[n])
  )
  geom_step(data=st, aes(x, y), ...)
}

plotOneDist <- function(d, name, title, limits, fun=NULL, ..., bins=100, f.bins=100,
                        maxy=NA, alpha.line=1, with.outline=FALSE, outline.colour="grey50",
                        fill=fill.colour.mid, brks.y=waiver()) {
  brks <- seq(limits[1], limits[2], length.out = bins)
  g <- ggplot(data.frame(d=d), aes(x=d, y=..density..)) +
    theme_classic() +
    geom_histogram(breaks=brks, fill=fill) +
    labs(x=name, y="Density", title=title) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, maxy), breaks=brks.y)
  if(with.outline) g <- g + geom_outline(d, brks, colour=outline.colour)
  if(!is.null(fun)) {
    x <- seq(limits[1], limits[2], length.out = f.bins)
    y <- fun(x, ...)
    g <- g + geom_line(data=data.frame(x=x, y=y), aes(x, y), size=0.5, alpha=alpha.line)
  }
  g
}

plotDistributionCut <- function(x, cut=NULL, locut=NULL, side="upper", brks=seq(-1, 1, 0.01), 
                                xlab="", fill=fill.colour.mid, fill.cut=fill.colour.dark,
                                x.brks=waiver(), outline.colour="black") {
  df <- data.frame(x = x)
  
  dst <- ggplot(df, aes(x=x, y=..density..)) +
    theme_classic() +
    scale_x_continuous(expand=c(0,0), breaks=x.brks) +
    geom_histogram(breaks=brks, fill=fill) +
    labs(x=xlab, y="Normalized frequency")
  maxh <- max(ggplot_build(dst)$data[[1]]$density)  # max density in histogram
  dst <- dst + scale_y_continuous(expand=c(0,0), limits=c(0, maxh*1.03))
  
  gout <- geom_outline(x, brks, size=0.3, colour=outline.colour)
  
  if(!is.null(cut)) {
    cut <- brks[which.min(abs(brks - cut))]
    cut.lo <- cut
    cut.up <- cut
    if(side == "both") cut.lo <- -cut.lo
    if(side == "two") cut.lo <- brks[which.min(abs(brks - locut))]
    df.cut.lower <- df[df$x <= cut.lo,, drop=FALSE]
    df.cut.upper <- df[df$x >= cut.up,, drop=FALSE]
    norm.fac.lower <- nrow(df.cut.lower) / nrow(df)
    norm.fac.upper <- nrow(df.cut.upper) / nrow(df)
    g.upper <- geom_histogram(data=df.cut.upper, breaks=brks, fill=fill.cut, aes(y=..density.. * norm.fac.upper))
    g.lower <- geom_histogram(data=df.cut.lower, breaks=brks, fill=fill.cut, aes(y=..density.. * norm.fac.lower))
    if(side == "upper") {
      return(dst + g.upper + gout)
    } else if (side == "lower") {
      return(dst + g.lower + gout)
    } else {
      return(dst + g.upper + g.lower + gout)
    }
  } else {
    return(dst + gout)
  }
}


plotFun <- function(FUN, ..., x.grid, cut.lo=NULL, cut.up=NULL, name="") {
  x <- x.grid
  x.lo <- x[1]
  x.up <- x[length(x)]
  y <- FUN(x, ...)
  dff <- data.frame(
    x = c(x.lo, x, x.up),
    y = c(0, y, 0)
  )
  
  g <- ggplot(dff, aes(x=x, y=y)) +
    theme_dist +
    geom_polygon(colour="black", fill=fill.colour) +
    labs(x=name, y="Density") +
    scale_x_continuous(limits=c(x.lo, x.up), expand=c(0,0)) +
    scale_y_continuous(limits=c(0, max(dff$y)*1.03), expand=c(0,0))
  
  if(!is.null(cut.lo)) {
    xx <- x[x <= cut.lo]
    df.lo <- data.frame(
      x = c(x.lo, xx, cut.lo),
      y = c(0, FUN(xx, ...), 0)
    )
    g <- g + geom_polygon(data=df.lo, aes(x, y), colour="black", fill=fill.colour.dark)
  }
  
  if(!is.null(cut.up)) {
    xx <- x[x >= cut.up]
    df.up <- data.frame(
      x = c(cut.up, xx, x.up),
      y = c(0, FUN(xx, ...), 0)
    )
    g <- g + geom_polygon(data=df.up, aes(x, y), colour="black", fill=fill.colour.dark)
  }
  g
}
se <- function(x) {
  sd(x, na.rm=TRUE) / sqrt(length(x))
}


plotMiceBox <- function(d, what="Mass", ylab="Mass (g)", limits=NULL, cex=4, size=2, m.width=0.3,
                        with.means=TRUE, with.boxes=FALSE, with.shape=FALSE, palette=british.palette) {
  g <- ggplot() +
    theme_classic() +
    theme(legend.position = "none") +
    scale_fill_manual(values=palette) +
    labs(x="", y=ylab)
  
  if(!is.null(limits)) g <- g + scale_y_continuous(limits=limits, expand=c(0,0))

  if(with.boxes) {
    g <- g + geom_boxplot(data=d, aes_string(x="Country", y=what, fill="Country"), alpha=0.4, colour="grey60", outlier.shape = NA)
  }
  if(with.shape) {
    g <- g + 
      geom_beeswarm(data=d, aes_string(x="Country", y=what, fill="Country", shape="Country"), cex=cex, size=size, priority = "density") +
      scale_shape_manual(values=21:24)
  } else {
    g <- g + geom_beeswarm(data=d, aes_string(x="Country", y=what, fill="Country"), shape=21, cex=cex, size=size, priority = "density")
  }
  if(with.means) {
    m <- d %>% group_by(Country) %>% summarise(M = mean(!!sym(what))) %>% mutate(i=as.integer(as.factor(Country)))
    #g <- g + geom_boxplot(data=m, aes(x=Country, middle=M, ymin=M, lower=M, upper=M, ymax=M), stat="identity", width=0.7, lwd=0.6, fatten=0, colour="grey40")
    g <- g + geom_segment(data=m, aes(x=i-m.width, xend=i+m.width, y=M, yend=M), size=0.9, colour="grey30", lineend="round")
  }
  g
}


theme_no_y <- theme(
  legend.position = "none",
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  axis.title.y = element_blank()
)

