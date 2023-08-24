make_param_nonparam <- function() {
  Ctrl <- c(16.1, 18.2, 21.6, 26.0)
  Treat <- c(16.5, 16.6, 19.2, 21.0, 21.8, 25.6, 26.8)
  dat <- tibble(
    Condition = c(rep("Ctrl", length(Ctrl)), rep("Treat", length(Treat))),
    Value = c(Ctrl, Treat),
    Rank = rank(Value)
  )
}



plot_one_thing <- function(dat, what, with.error=FALSE, width=0) {
  g <- ggplot(dat) +
    theme_clean +
    theme(legend.position = "none") +
    labs(x="", y=what)
  if(with.error) {
    m <- dat |> group_by(Condition) |> summarise(M = mean(!!sym(what)), SE = sd(!!sym(what)) / sqrt(n()))
    g <- g + 
      geom_errorbar(data=m, aes(x=Condition, ymin=M-SE, ymax=M+SE, colour=Condition), width=0.2) +
      geom_point(data=m, aes(x=Condition, y=M, colour=Condition), shape=15, size=3) +
      scale_colour_manual(values=okabe_ito_palette)
  }
  g <- g + geom_jitter(aes_string(x="Condition", y=what), width=width, height=0)
  g
}

plot_param_nonparam <- function(dat) {
  g1 <- plot_one_thing(dat, "Value", with.error = TRUE, width=0)
  g2 <- plot_one_thing(dat, "Rank") + scale_y_continuous(breaks=1:nrow(dat))
  plot_grid(g1, g2, ncol=2)
}

countU <- function(x, y) {
  P <- lapply(x, function(xi) {
    length(which(xi > y))
  })
  unlist(P)
}

prepare_Udata <- function(x, y, conditions) {
  ux <- countU(x, y)
  uy <- countU(y, x)
  
  dat <- tibble(
    Condition = c(rep(conditions[1], length(x)), rep(conditions[2], length(y))),
    Lifespan = c(x, y),
    U = c(ux, uy)
  )
  dat$Rank <- rank(dat$Lifespan)
  dat$Condition <- factor(dat$Condition, levels=conditions)
  dat$text.adj <- -1
  dat[dat$Condition == conditions[1], "text.adj"] <- 2
  
  cnnct <- setNames(expand.grid(x, y), conditions)
  cnnct$Colour <- cnnct[, conditions[1]] > cnnct[, conditions[2]]
  
  list(dat=dat, cnnct=cnnct)
}

make_lifespan_data <- function() {
  wt <- c(0, 7, 56, 112, 464, 537, 575)
  ko <- c(402, 434, 474, 510, 600, 627)
  
  prepare_Udata(wt, ko, c("WT", "KO"))
}


plotU <- function(lifespan, sel=NULL, only.points=FALSE) {
  dat <- lifespan$dat
  cnnct <- lifespan$cnnct
  
  if(!is.null(sel)) {
    dat.sel <- dat |> 
      filter(Condition=="WT" & Lifespan==sel)
    cnnct.sel <- filter(cnnct, WT==sel)
  } else {
    dat.sel <- dat
    cnnct.sel <- cnnct
  }
  dat$Condition <- factor(dat$Condition, levels=c("WT", "KO"))
  g <- ggplot(dat, aes(Condition, Lifespan)) +
    theme_clean +
    theme(legend.position = "none") +
    labs(x="", y="Lifespan (day)") +
    scale_colour_manual(values=okabe_ito_palette) +
    geom_point()
  if(!only.points) {
    g <- g + geom_segment(data=cnnct.sel, aes(x="KO", xend="WT", y=KO, yend=WT, colour=Colour)) +
      geom_point() +
      geom_text(data=dat.sel, aes(x=Condition, y=Lifespan, label=U, hjust=text.adj))
  }
  g
}

plot_mann_u <- function(lifespan) {
  list(
    lifespan_u_0 = plotU(lifespan, only.points = TRUE),
    lifespan_u_1 = plotU(lifespan, 537),
    lifespan_u = plotU(lifespan)
  )
}

make_u_diff <- function(lifespan) {
  wt <- lifespan$dat |> filter(Condition == "WT") |> pull(Lifespan)
  ko <- lifespan$dat |> filter(Condition == "KO") |> pull(Lifespan)
  map_dfr(seq(-700, 240, 1), function(dm) {
    x <- wt - dm
    y <- ko
    ux <- sum(countU(x, y))
    uy <- sum(countU(y, x))
    u <- min(ux, uy)
    tibble(
      dm = dm,
      ux = ux,
      uy = uy,
      u = u
    )
  })
}


simple_dat_plot <- function(dat, dm) {
  dat[dat$Condition == "WT", "Lifespan"] <- dat[dat$Condition == "WT", "Lifespan"] - dm
  ggplot(dat, aes(x=Condition, y=Lifespan)) +
    theme_clean +
    theme(
      legend.position = "none",
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(fill=NA)
    ) +
    geom_point() +
    labs(x=NULL, y=NULL)
}


plot_u_range <- function(udiff, lifespan) {
  g <- ggplot(udiff) +
    theme_clean +
    geom_line(aes(dm, ux), colour="blue", size=0.3) +
    geom_line(aes(dm, uy), colour="darkgreen", size=0.3) +
    labs(x="Difference of the means", y="U")
  gu <- g + geom_line(aes(dm, u), colour="black", size=1)
  
  g1 <- simple_dat_plot(lifespan$dat, -800)
  g2 <- simple_dat_plot(lifespan$dat, -350)
  g3 <- simple_dat_plot(lifespan$dat, 300)
  
  list(
    U_example = g,
    U_example_withu = gu,
    U_example_1 = g1,
    U_example_2 = g2,
    U_example_3 = g3
  )
}

generate_udist <- function(seed=253, nx, ny, nsim=100000) {
  set.seed(seed)
  map_dbl(1:nsim, function(i) {
    x <- rnorm(nx)
    y <- rnorm(ny)
    ux <- sum(countU(x, y))
    uy <- sum(countU(y, x))
    u <- min(ux, uy)
  })
}

plot_udist <- function(U, nx, ny, u.cut=10, xmax=45,  ymax=0.12) {
  Utab <- table(U) / length(U)
  udat <- data.frame(
    U = as.numeric(names(Utab)),
    freq = as.numeric(Utab)
  )
  g1 <- ggplot(udat, aes(U, freq)) +
    theme_clean +
    geom_segment(aes(x=U, xend=U, y=0, yend=freq), colour="grey60") +
    geom_point() +
    scale_y_continuous(limits=c(0, ymax), expand=c(0,0)) +
    labs(x="U", y="Normalized frequency")
  
  if(xmax <= 12) {
    g1 <- g1 + scale_x_continuous(limits=c(0, xmax), breaks=0:12)
  } else {
    g1 <- g1 + scale_x_continuous(limits=c(0, xmax))
  }
  
  M <- nx * ny / 2
  S <- sqrt(nx * ny * (nx + ny + 1) / 12)
  
  x <- seq(0, 50, 0.1)
  gs <- data.frame(
    x = x,
    y = 2*dnorm(x, mean=M, sd=S)
  )
  g2 <- g1 +
    geom_line(data=gs, aes(x, y), colour="red")
  g2
  
  xc <- seq(0, u.cut, 0.1)
  gc <- data.frame(
    x = c(xc, u.cut, 0),
    y = c(2*dnorm(xc, M, S), 0, 0)
  )
  g3 <- g2 +
    geom_polygon(data=gc, aes(x, y), colour="red", fill=fill.colour.dark)
  g3
  
  list(udist=g1, udist_norm=g2, udist_cut=g3)
}

plot_udist_small_example <- function() {
  udat33 <- prepare_Udata(c(450, 500, 520), c(530, 550, 600), c("WT", "KO"))
  plotU(udat33)
}

plot_lifespan_param_rank <- function(lifespan) {
  g1 <- plot_one_thing(lifespan$dat, what="Lifespan", with.error = TRUE)
  g2 <- plot_one_thing(lifespan$dat, what="Rank") + scale_y_continuous(breaks=1:13)
  
  plot_grid(g1, g2, ncol=2)
}


read_scores <- function() {
  s1 <- read_tsv("data/velos1_peptides_info.txt", n_max = 100)
  s2 <- read_tsv("data/velos3_peptides_info.txt", n_max = 100)
  full_join(s1, s2, by="Sequence") |> 
    select(Sequence, S1 = Score.x, S2 = Score.y) |> 
    pivot_longer(-Sequence)
}


plot_mw_scores <- function(scores) {
  g1 <- ggplot(scores, aes(x=value, y=name, fill=name)) +
    theme_classic() +
    geom_density_ridges(stat="binline", bins=20, scale=0.8) +
    scale_fill_manual(values=okabe_ito_palette) +
    labs(x="Score", y=NULL) +
    theme(legend.position = "none") 
  g2 <- ggplot(scores, aes(x=name, y=value)) +
    theme_classic() +
    geom_boxplot(aes(fill=name), outlier.shape = NA) +
    #geom_jitter(width=0.1, height=0, size=0.5) +
    geom_beeswarm(cex=1, size=1, priority="density") +
    scale_fill_manual(values=okabe_ito_palette) +
    theme(legend.position = "none") +
    labs(x="", y="Score")
  
  plot_grid(g2, g1, nrow=1, scale=0.8)
}

make_apgar_scores <- function() {
  control <- c(8, 7, 6, 2, 5, 8, 7, 3)
  new <- c(9, 8, 7, 8, 10, 9, 6)
  
  d <- tibble(
    sample = c(rep("Old", length(control)), rep("New", length(new))),
    score = c(control, new)
  ) |> 
    mutate(sample = fct_relevel(sample, "Old"))
}

plot_apgar_scores <- function(d) {
  ggplot(d, aes(x=sample, y=score, fill=sample)) +
    theme_clean +
    theme(
      legend.position = "none"
    ) +
    geom_beeswarm(cex=0.6, size=2, shape=21) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_y_continuous(breaks=0:10, limits=c(0,10.5), expand=c(0,0)) +
    labs(x=NULL, y="Score")
}



make_before_after <- function() {
  tibble(
    Before = c(21.4, 20.2, 23.5, 17.5, 18.6, 17.0, 18.9, 19.2),
    After = c(22.6, 20.9, 23.8, 18.0, 18.4, 17.9, 19.3, 19.1),
    sgn = sign(Before - After) |> factor(levels = c(-1, 1))
  )
}


plot_paired <- function(d, simple=FALSE) {
  m <- d |> 
    pivot_longer(c(Before, After)) |> 
    mutate(name = as_factor(name) |> fct_relevel("Before"))

  laby <- ifelse(simple, "", "Body mass (g)")
  g <- ggplot(m, aes(name, value)) +
    theme_clean +
    labs(x=NULL, y=laby) +
    geom_point() +
    geom_segment(data=d, aes(x="Before", xend="After", y=Before, yend=After, colour=sgn)) +
    geom_point() +
    theme(legend.position = "none") +
    scale_color_manual(breaks=c(-1,1), values=okabe_ito_palette, drop=FALSE)
  if(simple) {
    g <- g +
      scale_y_continuous(expand=c(0,2)) +
      scale_x_discrete(expand=c(0,0.3)) +
      theme(
        legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(fill=NA)
      )
  }
  g
}


wilcox_table <- function(d) {
  d |> 
    mutate(
      delta = abs(Before - After),
      r = rank(delta),
      s = sign(After - Before),
      sr = r * s
    ) |> 
    arrange(r)
}


wilcoxW <- function(d) {
  f <- abs(d$Before - d$After)
  r <- rank(f)
  s <- sign(d$After - d$Before)
  sum(s * r)
}


make_w_diff <- function(d) {
  m0 <- mean(d$After) - mean(d$Before)
  map_dfr(seq(-1, 1, 0.01), function(dm) {
    d0 <- d |> 
      mutate(After = After - m0 + dm)
    W <- wilcoxW(d0)
    data.frame(
      dm = dm,
      W = W
    )
  })
}

plot_w_range <- function(d) {
  wdat <- make_w_diff(d)
  m0 <- mean(d$After) - mean(d$Before)
  
  g <- ggplot(wdat, aes(dm, W)) +
    geom_vline(xintercept = 0, colour="grey60") +
    geom_hline(yintercept = 0, colour="grey60") +
    geom_line(colour="blue") +
    theme_clean +
    scale_y_continuous(breaks=c(-30,-20,-10,0,10,20,30)) +
    labs(x="Difference between the means", y="W")

  g1 <- plot_paired(d |> mutate(After = After - m0 - 1), simple=TRUE)
  g2 <- plot_paired(d |> mutate(After = After - m0) , simple=TRUE)
  g3 <- plot_paired(d |> mutate(After = After - m0 + 1), simple=TRUE)

  list(
    W_plot = g,
    W_plot_1 = g1,
    W_plot_2 = g2,
    W_plot_3 = g3
  )
}


generate_wdist <- function(seed=123, nx, nsim=100000) {
  set.seed(seed)
  map_dbl(1:nsim, function(i) {
    tibble(
      Before = rnorm(nx),
      After = rnorm(nx)
    ) |> 
    wilcoxW()
  })
}


plot_wdist <- function(W, n, w.cut=30, xmax=50,  ymax=0.06) {
  Wtab <- table(W) / length(W)
  wdat <- data.frame(
    W = as.numeric(names(Wtab)),
    freq = as.numeric(Wtab)
  )
  g1 <- ggplot(wdat, aes(W, freq)) +
    theme_clean +
    geom_segment(aes(x=W, xend=W, y=0, yend=freq), colour="grey60") +
    geom_point() +
    scale_x_continuous(limits=c(-xmax, xmax), breaks=c(-40, -30,-20,-10,0,10,20,30, 40)) +
    scale_y_continuous(limits=c(0, ymax), expand=c(0,0)) +
    labs(x="W", y="Normalized frequency")
  
  M <- 0
  S <- sqrt(n * (n + 1) * (2*n + 1) / 6)
  
  x <- seq(-50, 50, 0.1)
  gs <- data.frame(
    x = x,
    y = 2*dnorm(x, mean=M, sd=S)
  )
  g2 <- g1 +
    geom_line(data=gs, aes(x, y), colour="red")
  g2
  
  xc1 <- seq(-50, -w.cut, 0.1)
  gc1 <- data.frame(
    x = c(xc1, -w.cut, -50),
    y = c(2*dnorm(xc1, M, S), 0, 0)
  )
  
  xc2 <- seq(w.cut, 50, 0.1)
  gc2 <- data.frame(
    x = c(w.cut, xc2, 50),
    y = c(0, 2*dnorm(xc2, M, S), 0)
  )
  g3 <- g2 +
    geom_polygon(data=gc1, aes(x, y), colour="red", fill=fill.colour.dark) +
    geom_polygon(data=gc2, aes(x, y), colour="red", fill=fill.colour.dark)
  g3
  
  list(w_dist=g1, w_dist_norm=g2, w_dist_norm_cut=g3)
}


read_mice_lifespan <- function(file) {
  read_tsv(file) |> 
    mutate(
      Rank = rank(Lifespan, ties.method="average"),
      Country = factor(Country, levels=COUNTRIES)
    )
} 


plot_rank <- function(mice) {
  m <- mice |>
    group_by(Rank) |>
    mutate(idx = row_number(Rank))
  labsi <- c(1, seq(5,40,5))
  labs <- rep("", 40)
  labs[labsi] <- labsi
  ggplot(m, aes(x=Rank, y=idx, fill=Country, shape=Country)) +
    theme_classic() +
    geom_point(size=3) +
    theme(
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    ) +
    scale_fill_manual(values=british.palette) +
    scale_x_continuous(breaks=1:40, labels=labs, limits=c(0.5, max(mice$Rank)+0.5), expand=c(0,0)) +
    scale_shape_manual(values=21:24) +
    labs(x = "Rank", y="")
}

reduce_mice <- function(mice4) {
  mice4 |> 
    filter(Country %in% c("English", "Scottish")) |> 
    mutate(Rank = rank(Lifespan, ties.method="random"))
}

plot_value_rank <- function(mice4) {
  mice2 <- reduce_mice(mice4)
  g1 <- plot_mice_box(mice2, cex=3.2, size=2, what="Lifespan", ylab="Lifespan (days)", with.shape=TRUE)
  g2 <- plot_mice_box(mice2, size=2, what="Rank", ylab="Rank", with.shape=TRUE) +
    scale_y_continuous(expand=c(0,0), limits=c(0,nrow(mice2)+1), breaks=1:nrow(mice2))
  plot_grid(g1, g2, ncol=2) 
  
}

plot_anova_kruskal <- function(mice4) {
  labsi <- c(1, seq(5,40,5))
  labs <- rep("", 40)
  labs[labsi] <- labsi
  
  g1 <- plot_mice_box(mice4, what="Lifespan", ylab="Lifespan (days)", cex=2, with.shape=TRUE)
  g2 <- plot_mice_box(mice4, what="Rank", ylab="Rank", cex=1.5, with.shape=TRUE) +
    scale_y_continuous(breaks=1:40, labels=labs)
  
  plot_grid(g1, g2, ncol=2)
}


generate_hdist <- function(seed=153, n=c(12, 9, 8, 5), nsim=1000) {
  set.seed(seed)
  map_dbl(1:nsim, function(i) {
    d <- map_dfr(1:length(n), function(k) {
      tibble(
        Country = k,
        Lifespan = rnorm(n[k])
      )
    })
    kt <- kruskal.test(Lifespan ~ Country, data=d)
    as.numeric(kt$statistic)
  })
}

plot_hdist_cut <- function(hd, mice) {
  kt.chi2 <- as.numeric(kruskal.test(Lifespan ~ Country, data=mice)$statistic)
  xx <- seq(kt.chi2, 20, 0.1)
  df <- tibble(
    x = c(kt.chi2, xx, 20),
    y = c(0, dchisq(xx, 3), 0)
  )
  
  xx <- seq(0,20, 0.02)
  mod <- tibble(x=xx, y=dchisq(xx, 3))
  
  hdat <- tibble(H=hd)
  g <- ggplot(hdat, aes(x=H, y=..density..)) +
    theme_dist +
    geom_histogram(bins=100, fill=fill.colour.mid) +
    labs(x=expression(H), y="Density") +
    scale_x_continuous(limits=c(0, 20), expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    geom_line(data=mod, aes(x, y)) +
    geom_polygon(data=df, aes(x, y), colour="blue", fill=fill.colour.dark)
  g
}


QKS <- function(lambda, eps1=0.0001, eps2=1e-12) {
  a2 <- -2 * lambda^2
  fac <- 2
  probks <- 0
  termbf <- 0
  for(j in 1:1000) {
    term <- fac * exp(a2 * j^2)
    probks <- probks + term
    if(abs(term) < eps1 * termbf || abs(term) < eps2 * probks) {
      return(probks)
    }
    fac <- -1 * fac
    termbf <- abs(term)
  }
  return(1)
}

kolmogorov_dist <- function(x, n1, n2) {
  sn <- sqrt(n1 * n2 / (n1 + n2))
  #x <- seq(0, 1, delta)
  delta <- x[2] - x[1]
  y <- sapply(x, function(d) {
    1 - QKS((sn + 0.12 + 0.11/sn) * d)
  })
  n <- length(y)
  f <- (y[2:n] - y[1:(n-1)]) / delta
  x <- (x[2:n] + x[1:(n-1)]) / 2
  
  data.frame(
    x = x,
    f = f
  )
}



plot_cumsum <- function(dat, vars, what="Lifespan", xlab="Lifespan (days)", xmargin=0.1, ymargin=0.05, palette=british.palette) {
  mn <- min(dat[, what])
  mx <- max(dat[, what])
  delta <- mx - mn
  mn <- mn - xmargin * delta
  mx <- mx + xmargin * delta
  
  dat <- dat |>
    mutate(value = !!sym(what)) |>
    filter(Country %in% vars) |>
    group_by(Country) |>
    arrange(value) |> 
    mutate(idx = row_number(Country), c=1) |> 
    mutate(cumdist = cumsum(c) / n()) |> 
    mutate(Group = as.integer(Country))  |> 
    mutate(y = -ymargin * Group) |>
    as.data.frame()
  dat2 <- dat |> 
    add_row(Country=vars, value=mn, cumdist=0) |>
    add_row(Country=vars, value=mx, cumdist=1) |>
    arrange(value, cumdist)
  
  g <- ggplot(dat2, aes(x=value, y=cumdist, colour=Country)) +
    theme_clean +
    geom_hline(yintercept = 0, colour="grey70") +
    geom_step() +
    scale_colour_manual(values=palette) +
    geom_point(data=dat, aes(x=value, y=y, colour=Country), shape=3, size=2) +
    scale_y_continuous(breaks=seq(0, 1, 0.2)) +
    labs(x=xlab, y="Cumulative distribution") +
    theme(legend.position = "none")
  g
}


plot_ks_2 <- function(mice4, countries) {
  mice2 <- mice4 |> 
    filter(Country %in% countries)
  m <- mice2 |>
    group_split(Country) |> 
    map(function(w) pull(w, Lifespan))

  test <- ks.test(m[[1]], m[[2]])
  
  g1 <- plot_mice_box(mice2, what="Lifespan", ylab="Lifespan (days)", cex=2, size=1.8)
  g2 <- plot_cumsum(mice2, vars=countries)
  g <- plot_grid(g1, g2, ncol=2, rel_widths = c(1, 1.4))
  list(
    plot = g,
    test = test
  )
} 


generate_mice_lifespan <- function(nx, ny, what="Lifespan", Mx=500, My=500, Sx=50, Sy=50, countries=c("English", "Scottish")) {
  x <- rnorm(nx, Mx, Sx)
  y <- rnorm(ny, My, Sy)
  d <- tibble(
    v = c(x, y),
    Country = c(rep(countries[1], nx), rep(countries[2], ny))
  )
  colnames(d)[1] <- what
  d$Country <- factor(d$Country, levels=countries)
  d
}


generate_ddist <- function(seed=23, nx, ny, nsim=100000) {
  set.seed(seed)
  map_dbl(1:nsim, function(i) {
    m <- generate_mice_lifespan(nx, ny)
    kt <- ks.test(m[m$Country=="English", ]$Lifespan, m[m$Country=="Scottish", ]$Lifespan)
    as.numeric(kt$statistic)
  })
}

dks <- function(x, n) {
  p <- sapply(x, function(d) {
    pkolmim(d, n)
  })
  delta <- x[2] - x[1]
  l <- length(x)
  f <- (p[2:l] - p[1:(l-1)]) / delta
  xf <- (x[2:l] + x[1:(l-1)]) / 2
  data.frame(
    x = xf,
    f = f
  )
}


plot_Ddist <- function(D, nx, ny, d.cut=0.42, ymax=5) {
  Dtab <- table(D) / length(D)
  ddat <- data.frame(
    D = as.numeric(names(Dtab)),
    freq = as.numeric(Dtab)
  )
  delta <- min(ddat$D[2:nrow(ddat)] - ddat$D[1:(nrow(ddat)-1)])
  ddat$freq <- ddat$freq / delta
  
  g1 <- ggplot(ddat, aes(D, freq)) +
    theme_dist +
    geom_segment(aes(x=D, xend=D, y=0, yend=freq), colour="grey60") +
    geom_point() +
    scale_x_continuous(limits=c(0, 1)) +
    scale_y_continuous(limits=c(0, ymax), expand=c(0,0)) +
    labs(x="D", y="Normalized frequency")
  
  ne <- nx * ny / (nx + ny)
  
  #gs <- dks(x, ne)
  gs <- kolmogorov_dist(seq(0, 1, 0.001), nx, ny)
  
  g2 <- g1 +
    geom_line(data=gs, aes(x, f), colour="red")
  g2
  
  ff <- kolmogorov_dist(seq(d.cut, 1, 0.001), nx, ny)
  gc <- data.frame(
    x = c(d.cut, ff$x, 1),
    y = c(0, ff$f, 0)
  )
  g3 <- g2 +
    geom_polygon(data=gc, aes(x, y), colour="red", fill=fill.colour.dark)
  g3
  
  list(ks_dist=g1, ks_dist_d=g2, ks_dist_d_cut=g3)
}

plot_ks_loc_shape <- function(seed1=747, seed2=700) {
  set.seed(seed1)
  countries <- c("CTRL", "TREAT")
  ls.gen <- generate_mice_lifespan(12, 12, Mx=100, My=110, Sx=10, Sy=10, what="Value", countries=countries)
  test1 <- ks.test(ls.gen[ls.gen$Country==countries[1], ]$Value, ls.gen[ls.gen$Country==countries[2], ]$Value) 
  g1 <- plot_mice_box(ls.gen, what="Value", ylab="Value", palette=okabe_ito_palette, cex=2.8, size=1.8)
  g2 <- plot_cumsum(ls.gen, vars=countries, what="Value", xlab="Value", palette = okabe_ito_palette)
  pl1 <- plot_grid(g1, g2, ncol=2, rel_widths = c(1, 1.4))
  
  set.seed(seed2)
  ls.gen2 <- generate_mice_lifespan(16, 16, what="Value", Mx=100, My=100, Sx=2, Sy=10, countries=countries)
  test2 <- ks.test(ls.gen2[ls.gen2$Country==countries[1], ]$Value, ls.gen2[ls.gen2$Country==countries[2], ]$Value)
  g1 <- plot_mice_box(ls.gen2, what="Value", ylab="Value", palette=okabe_ito_palette, cex=2.8, size=1.8)
  g2 <- plot_cumsum(ls.gen2, vars=countries, what="Value", xlab="Value", palette = okabe_ito_palette)
  pl2 <- plot_grid(g1, g2, ncol=2, rel_widths = c(1, 1.4))
  
  list(
    ks_location = pl1,
    ks_shape = pl2,
    test_location = test1,
    test_shape = test2
  )
  
}

