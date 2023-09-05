geom_outline <- function(d, breaks, ..., y.value = "density") {
  bins <- breaks[2] - breaks[1]
  m <- length(d)
  x <- breaks[-1] - bins
  y <- as.numeric(table(cut(d, breaks, right = TRUE)))
  if(y.value == "density") {
    y <- y / (bins * sum(y))
  }
  
  n <- length(x)
  st <- data.frame(
    x = c(x, x[n] + bins),
    y = c(y, y[n])
  )
  geom_step(data = st, aes(x, y), ...)
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
  ), mart = mart) |> 
    as_tibble() |> 
    rename(
      chr = chromosome_name,
      start = start_position,
      end = end_position,
      gene_id = ensembl_gene_id,
      gene_name = external_gene_name
    )
}



plot_discrete_distribution_cut <- function(lo, up, FUN, ..., dcut = NULL) {
  dist <- tibble(
    x = lo:up,
    P = FUN(x, ...)
  )
  g <- ggplot(dist, aes(x, P)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_col(fill = fill.colour, colour = "black", width = 0.7, size = 0.3) +
    scale_x_continuous(breaks = 0:max(dist$x)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x = "k", y = "P(X = k)")
  if(!is.null(dcut)) {
    dist_cut <- dist |> filter(x >= dcut[1] & x <= dcut[2])
    g <- g + geom_col(data = dist_cut, fill = fill.colour.dark, colour = "black", width = 0.7, size = 0.3)
  }
  g
}

geom_cut <- function(lo, up, FUN, ..., n = 100, colour = "black", fill = fill.colour.dark) {
  x <- lo + (0:(n - 1)) * (up - lo) / (n - 1)
  d <- data.frame(
    x = c(lo, x, up),
    y = c(0, FUN(x, ...), 0)
  )
  geom_polygon(data = d, aes(x, y), colour = colour, fill = fill)
}

plot_continuous_distributon_cut <- function(lo, up, FUN, ..., dcut = NULL, n = 100) {
 
  g <- ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_cut(lo, up, FUN, ..., n = n, fill = fill.colour) +
    labs(x = "x", y = "f(x)") +
    scale_x_continuous(limits = c(lo, up), expand = c(0,0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)))
  if(!is.null(dcut)) {
    g <- g + geom_cut(dcut[1], dcut[2], FUN, ...)
  }
  g
}

plot_one_dist <- function(v, name, title, limits, fun = NULL, ..., bins = 100, f.bins = 100,
                        maxy = NA, alpha.line = 1, with.outline = FALSE, outline.colour = "grey30",
                        fill = fill.colour.mid, brks.x = waiver(), brks.y = waiver(), with.mean = FALSE, with.sd = FALSE) {
  brks <- seq(limits[1], limits[2], length.out = bins)
  d <- tibble(x = v)
  g <- ggplot(d, aes(x = x, y = after_stat(density))) +
    theme_dist +
    geom_histogram(breaks = brks, fill = fill) +
    labs(x = name, y = "Density", title = title) +
    scale_x_continuous(expand = c(0,0), breaks = brks.x) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits = c(0, NA), breaks = brks.y)
  if(with.outline) g <- g + geom_outline(v, brks, colour = outline.colour)
  if(!is.null(fun)) {
    x <- seq(limits[1], limits[2], length.out = f.bins)
    y <- fun(x, ...)
    g <- g + geom_line(data = data.frame(x = x, y = y), aes(x, y), linewidth = 0.5, alpha = alpha.line)
  }
  if(with.mean | with.sd) g <- g + geom_vline(xintercept = mean(v))
  if(with.sd) g <- g + geom_vline(xintercept = c(mean(v) - sd(v), mean(v) + sd(v)), linetype = "dotted")
  g
}


plot_fun <- function(FUN, ..., x.grid, cut.lo = NULL, cut.up = NULL, name = "") {
  x <- x.grid
  x.lo <- x[1]
  x.up <- x[length(x)]
  y <- FUN(x, ...)
  dff <- data.frame(
    x = c(x.lo, x, x.up),
    y = c(0, y, 0)
  )
  
  g <- ggplot(dff, aes(x = x, y = y)) +
    theme_classic() +
    geom_polygon(colour = "black", fill = fill.colour) +
    labs(x = name, y = "Density") +
    scale_x_continuous(limits = c(x.lo, x.up), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, max(dff$y)*1.03), expand = c(0,0))
  
  if(!is.null(cut.lo)) {
    xx <- x[x <= cut.lo]
    df.lo <- data.frame(
      x = c(x.lo, xx, cut.lo),
      y = c(0, FUN(xx, ...), 0)
    )
    g <- g + geom_polygon(data = df.lo, aes(x, y), colour = "black", fill = fill.colour.dark)
  }
  
  if(!is.null(cut.up)) {
    xx <- x[x >= cut.up]
    df.up <- data.frame(
      x = c(cut.up, xx, x.up),
      y = c(0, FUN(xx, ...), 0)
    )
    g <- g + geom_polygon(data = df.up, aes(x, y), colour = "black", fill = fill.colour.dark)
  }
  g
}


se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}


plot_distribution_cut <- function(x, cut = NULL, locut = NULL, side = "upper", brks = seq(-1, 1, 0.01), 
                                xlab = "", fill = fill.colour.mid, fill.cut = fill.colour.dark,
                                x.brks = waiver(), outline.colour = "black") {
  df <- data.frame(x = x)
  
  dst <- ggplot(df, aes(x = x, y = after_stat(density))) +
    theme_dist +
    scale_x_continuous(expand = c(0,0), breaks = x.brks) +
    geom_histogram(breaks = brks, fill = fill) +
    labs(x = xlab, y = "Normalized frequency")
  maxh <- max(ggplot_build(dst)$data[[1]]$density)  # max density in histogram
  dst <- dst + scale_y_continuous(expand = c(0,0), limits = c(0, maxh*1.03))
  
  gout <- geom_outline(x, brks, size = 0.3, colour = outline.colour)
  
  if(!is.null(cut)) {
    cut <- brks[which.min(abs(brks - cut))]
    cut.lo <- cut
    cut.up <- cut
    if(side == "both") cut.lo <- -cut.lo
    if(side == "two") cut.lo <- brks[which.min(abs(brks - locut))]
    df.cut.lower <- df[df$x <= cut.lo,, drop = FALSE]
    df.cut.upper <- df[df$x >= cut.up,, drop = FALSE]
    norm.fac.lower <- nrow(df.cut.lower) / nrow(df)
    norm.fac.upper <- nrow(df.cut.upper) / nrow(df)
    g.upper <- geom_histogram(data = df.cut.upper, breaks = brks, fill = fill.cut, aes(y = after_stat(density) * norm.fac.upper))
    g.lower <- geom_histogram(data = df.cut.lower, breaks = brks, fill = fill.cut, aes(y = after_stat(density) * norm.fac.lower))
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


# generate tree plots of t-distribution
# t - basic plot
# t.one - one tail
# t.two - two tails
t_cut <- function(t.obs, dof) {
  xx <- seq(-5, 5, 0.01)
  yy <- dt(xx, dof)
  dft <- tibble(
    x = c(-5, xx, 5),
    y = c(0, yy, 0)
  )
  xx <- seq(t.obs, 5, 0.01)
  df <- tibble(
    x = c(t.obs, xx, max(xx), t.obs),
    y = c(0, dt(xx, dof), 0, 0)
  )
  df1 <- df |> mutate(x = -x)

  g1 <- ggplot(df, aes(x = x, y = after_stat(density))) +
    theme_dist +
    #geom_line(data = dft, aes(x, y), colour = "blue") +
    geom_polygon(data = dft, aes(x, y), colour = "black", fill = fill.colour) +
    labs(x = "t", y = "Density") +
    scale_x_continuous(limits = c(-5, 5), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, max(dft$y)*1.03), expand = c(0,0))
  g2 <- g1 + geom_polygon(data = df, aes(x, y), colour = "black", fill = fill.colour.dark)
  g3 <- g2 + geom_polygon(data = df1, aes(x, y), colour = "black", fill = fill.colour.dark)
  list(t = g1, t.one = g2, t.two = g3)
}


plot_mice_box <- function(d, what = "Mass", ylab = "Mass (g)", limits = NULL, cex = 1, size = 2, m.width = 0.3,
                        with.means = TRUE, with.boxes = FALSE, with.shape = FALSE, palette = british.palette) {
  d <- d |> 
    mutate(val = get(what))
  
  g <- ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    scale_fill_manual(values = palette) +
    labs(x = NULL, y = ylab)
  
  if(!is.null(limits)) g <- g + scale_y_continuous(limits = limits, expand = c(0,0))
  
  if(with.boxes) {
    g <- g + geom_boxplot(data = d, aes(x = Country, y = val, fill = Country), alpha = 0.4,
                          colour = "grey60", outlier.shape = NA)
  }
  if(with.shape) {
    g <- g + 
      geom_beeswarm(data = d, aes(x = Country, y = val, fill = Country, shape = Country),
                    cex = cex, size = size, priority = "density") +
      scale_shape_manual(values = 21:24)
  } else {
    g <- g + geom_beeswarm(data = d, aes(x = Country, y = val, fill = Country), shape = 21,
                           cex = cex, size = size, priority = "density")
  }
  if(with.means) {
    m <- d |>
      group_by(Country) |>
      summarise(M = mean(val)) |>
      mutate(i = as.integer(as.factor(Country)))
    g <- g + geom_segment(data = m, aes(x = i - m.width, xend = i + m.width, y = M, yend = M),
                          linewidth = 0.9, colour = "grey30", lineend = "round")
  }
  g
}


get_means <- function(mice, width, what = "Mass") {
  m <- mice |> 
    mutate(val = get(what)) |> 
    mutate(grand_mean = mean(val)) |> 
    mutate(group = as.integer(Country)) |> 
    group_by(group) |> 
    mutate(
      N = n(),
      n = seq_along(val),
      x = group + width*(n - N/2 - 1/2) / (N - 1),
      Mean = mean(val)
    ) |> 
    ungroup()
  M <- m |> 
    select(Country, Mean, x = group) |> 
    distinct()
  list(mice = m, means = M)
}

plot_variance <- function(mice, within = TRUE, width = 0.6, what = "Mass", ylab = "Body mass (g)") {
  m <- get_means(mice, width, what)
  gm <- m$mice$grand_mean[1]
  g <- ggplot(m$mice) + theme_clean
  if(within) {
    g <- g + geom_segment(aes(x = x, xend = x, y = Mean, yend = !!sym(what)), colour = "grey70")
  } else {
    g <- g +
      geom_line(data = data.frame(x = c(1-width/2,4+width/2), y = c(gm, gm)), aes(x,y), linetype = "dotted") +
      geom_segment(aes(x = x, xend = x, y = grand_mean, yend = Mean), colour = "grey70")
  }
  g <- g +
    geom_segment(data = m$means, aes(x = x-width/2, xend = x+width/2, y = Mean, yend = Mean, colour = Country)) +
    geom_point(aes(x = x, y = !!sym(what), fill = Country), colour = "grey30", shape = 21, size = 1.8) +
    theme(legend.position = "none", axis.ticks.x = element_blank()) +
    scale_x_continuous(breaks = c(1,2,3,4), labels = COUNTRIES) +
    labs(x = NULL, y = ylab) +
    scale_colour_manual(values = british.palette) +
    scale_fill_manual(values = british.palette)
  g
}
