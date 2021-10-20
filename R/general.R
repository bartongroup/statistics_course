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

biomart_gene_download <- function(mart) {
  getBM(attributes = c(
    "chromosome_name",
    "start_position",
    "end_position",
    "strand",
    "gene_biotype",
    "ensembl_gene_id",
    "external_gene_name",
    "description"
  ), mart=mart) %>% 
    as_tibble() %>% 
    rename(
      chr = chromosome_name,
      start = start_position,
      end = end_position,
      gene_id = ensembl_gene_id,
      gene_name = external_gene_name
    )
}



plot_discrete_distribution_cut <- function(lo, up, FUN, ..., dcut=NULL) {
  dist <- tibble(
    x = lo:up,
    P = FUN(x, ...)
  )
  g <- ggplot(dist, aes(x, P)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_col(fill=fill.colour, colour="black", width=0.7, size=0.3) +
    scale_x_continuous(breaks=0:max(dist$x)) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.03))) +
    labs(x = "k", y = "P(X = k)")
  if(!is.null(dcut)) {
    dist_cut <- dist %>% filter(x >= dcut[1] & x <= dcut[2])
    g <- g + geom_col(data=dist_cut, fill=fill.colour.dark, colour="black", width=0.7, size=0.3)
  }
  g
}

geom_cut <- function(lo, up, FUN, ..., n=100, colour="black", fill=fill.colour.dark) {
  x <- lo + (0:(n - 1)) * (up - lo) / (n - 1)
  d <- data.frame(
    x = c(lo, x, up),
    y = c(0, FUN(x, ...), 0)
  )
  geom_polygon(data=d, aes(x, y), colour=colour, fill=fill)
}

plot_continuous_distributon_cut <- function(lo, up, FUN, ..., dcut=NULL, n=100) {
 
  g <- ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_cut(lo, up, FUN, ..., n=n, fill=fill.colour) +
    labs(x="x", y="f(x)") +
    scale_x_continuous(limits=c(lo, up), expand=c(0,0)) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.03)))
  if(!is.null(dcut)) {
    g <- g + geom_cut(dcut[1], dcut[2], FUN, ...)
  }
  g
}

plot_one_dist <- function(v, name, title, limits, fun=NULL, ..., bins=100, f.bins=100,
                        maxy=NA, alpha.line=1, with.outline=FALSE, outline.colour="grey50",
                        fill=fill.colour.mid, brks.x=waiver(), brks.y=waiver(), with.mean=FALSE, with.sd=FALSE) {
  brks <- seq(limits[1], limits[2], length.out = bins)
  d <- tibble(x = v)
  g <- ggplot(d, aes(x=x, y=..density..)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_histogram(breaks=brks, fill=fill) +
    labs(x=name, y="Density", title=title) +
    scale_x_continuous(expand=c(0,0), breaks=brks.x) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.03)), limits=c(0, NA), breaks=brks.y)
  if(with.outline) g <- g + geom_outline(v, brks, colour=outline.colour)
  if(!is.null(fun)) {
    x <- seq(limits[1], limits[2], length.out = f.bins)
    y <- fun(x, ...)
    g <- g + geom_line(data=data.frame(x=x, y=y), aes(x, y), size=0.5, alpha=alpha.line)
  }
  if(with.mean | with.sd) g <- g + geom_vline(xintercept = mean(v))
  if(with.sd) g <- g + geom_vline(xintercept = c(mean(v) - sd(v), mean(v) + sd(v)), linetype="dotted")
  g
}


plot_fun <- function(FUN, ..., x.grid, cut.lo=NULL, cut.up=NULL, name="") {
  x <- x.grid
  x.lo <- x[1]
  x.up <- x[length(x)]
  y <- FUN(x, ...)
  dff <- data.frame(
    x = c(x.lo, x, x.up),
    y = c(0, y, 0)
  )
  
  g <- ggplot(dff, aes(x=x, y=y)) +
    theme_classic() +
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

