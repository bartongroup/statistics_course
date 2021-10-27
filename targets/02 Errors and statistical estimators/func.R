

plot_mini_normal <- function(n = 10000) {
  brks <- seq(-4, 4, 0.1)
  d <- rnorm(n)
  tibble(
    d = d
  ) %>% 
  ggplot(aes(x=d, y=..density..)) +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    geom_histogram(breaks=brks, fill=fill.colour) +
    labs(x="", y="") +
    geom_outline(d, brks, size=0.3)
}


plot_counting_2d <- function(seed=124, n=100, nd=5) {
  set.seed(seed)
  d <- tibble(
    x = runif(n),
    y = runif(n)
  )
  brks <- seq(0, 1, length.out = nd + 1)
  delta <- brks[2] - brks[1]

  # count in boxes
  dc <- d %>% 
    mutate(
      px = cut(x, breaks = brks, labels=brks[1:nd]),
      py = cut(y, breaks = brks, labels=brks[1:nd])
    ) %>% 
    group_by(px, py) %>% 
    tally() %>% 
    mutate_at(vars(px, py), ~as.numeric(as.character(.x)))
  
  g1 <- ggplot() +
    theme_classic() +
    theme(
      axis.title = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.border = element_rect(fill=NA)
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
    geom_point(data=d, aes(x=x, y=y))
  
  g2 <- g1 +
    geom_vline(xintercept = brks, colour = "grey70") +
    geom_hline(yintercept = brks, colour = "grey70")
  
  g3 <- g2 +
    geom_text(data = dc, aes(x = px, y=py + delta, label=n, hjust=-0.8, vjust=1.5), colour="royalblue2")
  
  list(
    poiss2d = g1,
    poiss2d_grid = g2,
    poiss2d_grid_count = g3
  )
}



plot_counting_error <- function(seed=2002, n=10000, mu=11, lims=c(0, 23)) {
  set.seed(seed)
  
  d <- rpois(n, mu)
  
  dm <- tibble(
    M = mean(d),
    S = sd(d)
  )
  
  df <- tibble(
    i = seq_along(d),
    cnt = d,
    err = sqrt(d)
  )
  
  dist <- df %>% 
    group_by(cnt) %>% 
    tally() %>% 
    mutate(y = n / sum(n))
  
  # top panel
  g1 <- ggplot(df[1:16, ]) +
    theme_classic() +
    coord_flip() +
    geom_errorbar(aes(x=i, ymin=cnt-err, ymax=cnt+err), width=0.2) +
    geom_point(aes(i, cnt)) +
    labs(y=NULL, x="Plate no.") +
    scale_x_continuous(breaks=c(1:16)) +
    scale_y_continuous(limits=lims) +
    geom_hline(yintercept = 11, colour="darkolivegreen", linetype="dotted")
  #theme(
  #  plot.margin = margin(l=3, r=2, b=1, t=1)
  #) 
  
  # bottom panel
  yerr <- max(dist$y) + 0.03
  g2 <- ggplot(dist) +
    theme_classic() +
    geom_segment(aes(x=cnt, xend=cnt, y=y, yend=0), colour="grey70") +
    geom_point(aes(cnt, y), size=2) +
    labs(x="Colony count", y="Normalized frequency") +
    scale_x_continuous(limits=lims) +
    scale_y_continuous(expand = expansion(mult=c(0,0.03)), breaks=c(0, 0.05, 0.1)) +
    geom_errorbarh(data=dm, aes(xmin=M-S, xmax=M+S, y=yerr), height=0.01, colour="darkolivegreen") +
    geom_point(data=dm, aes(x=M, y=yerr), colour="darkolivegreen")
  
  plot_grid(g1, g2, ncol=1, align = "v", rel_heights = c(2.5, 1))
}


murder_plot <- function(d) {
  ggplot(d) +
    theme_clean +
    geom_errorbar(aes(x=City, ymin=Rate-Error, ymax=Rate+Error), width=0.2) +
    geom_point(aes(x=City, y=Rate), size=2) +
    labs(x=NULL, y="Murder rate per 100,000") +
    scale_y_continuous(expand=c(0,0), limits=c(0, 6))
}


murder_plots <- function() {
  cities <- c("Dundee", "Glasgow", "Aberdeen", "Edinburgh")
  dat <- tibble(
    City = cities,
    Murders = c(6, 19, 2, 2),
    Rate = c(4.1, 3.2, 0.88, 0.41)
  ) %>% 
    mutate(
      Error = Rate * sqrt(Murders) / Murders,
      City = factor(City, levels=cities)
    )

  list(
    murder_1 = murder_plot(dat[1:2, ]),
    murder_2 = murder_plot(dat)
  )
}


plot_population_and_sample <- function(seed=14, mu=20, sigma=5, n=30) {
  set.seed(seed)
  v <- rnorm(n, mu, sigma)
  M <- mean(v)
  S <- sd(v)
  
  df <- tibble(x=rep(1, n), y=v)
  ggplot(df) +
    theme_clean +
    theme(
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    geom_beeswarm(aes(x, y), size=1.7, shape=21, cex=3, colour="darkolivegreen", fill="goldenrod1") +
    geom_errorbar(aes(x=0.85, ymin=M-S, ymax=M+S), width=0.1, colour="darkolivegreen") +
    geom_point(aes(x=0.85, y=M), colour="darkolivegreen") +
    geom_errorbar(aes(x=0, ymin=mu-sigma, ymax=mu+sigma), width=0.1) +
    geom_point(aes(x=0, y=mu)) +
    labs(x=NULL, y="Body mass (g)") +
    scale_x_continuous(limits=c(-0.4, 1.4))
}

plot_median <- function(seed=112, n=10) {
  set.seed(seed)
  x1 <- sort(rnorm(n, 20, 5))
  x2 <- x1
  x2[n] <- 60
  x2[n-1] <- 55
  d <- tibble(
    type = c(rep("Symmetric", n), rep("Outliers", n)),
    value = c(x1, x2)
  ) %>% 
    mutate(
      type = fct_relevel(type, "Symmetric")
    )

  dm <- d %>% group_by(type) %>%
    summarise(
      mean=mean(value),
      median=median(value)
    ) %>%
    mutate(i=as.integer(type))
  
  shift <- 0.3
  ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    geom_beeswarm(data=d, aes(x=type, y=value), shape=21, cex=3, size=2, fill="white") +
    geom_segment(data=dm, aes(x=i-shift, xend=i+shift, y=mean, yend=mean), size=0.9, colour="royalblue3", lineend="round") +
    geom_segment(data=dm, aes(x=i-shift, xend=i+shift, y=median, yend=median), size=0.9, colour="seagreen4", lineend="round") +
    scale_fill_manual(values=okabe_ito_palette) +
    labs(x=NULL, y=NULL)
}

plot_standard_deviation <- function(seed=1002, mu=29, sigma=5, n=12) {
  set.seed(seed)
  
  v <- rnorm(n, mu, sigma)
  M <- mean(v)
  S <- sd(v)
  
  df <- tibble(x=seq_along(v), y=v)
  
  ggplot(df, aes(x=x, y=y)) +
    theme_clean +
    geom_segment(aes(xend=x, yend=M), colour="grey70") +
    geom_point(aes(x, y), size=1.7, shape=21, colour="black", fill="darkgoldenrod1") +
    geom_hline(yintercept = M, linetype="dotted", colour="grey50") +
    labs(x="Data point", y="Body mass (g)") +
    scale_x_continuous(breaks=1:n)
}


generate_sampling_distributions <- function(seed=1, ns=c(5, 30), n_pop=100000, n_samp=100000, mu=20, sigma=5) {
  set.seed(seed)
  
  pop <- rnorm(n_pop, mu, sigma)
  map(ns, function(n) {
    samples <- t(replicate(n_samp, sample(pop, n)))
    sample_means <- rowMeans(samples)
    list(
      samples = samples,
      sample_means = sample_means
    )
  }) %>% 
    set_names(ns) %>% 
    c(list(population = pop))
}



plot_pop_sammean <- function(sm, n=5) {
  g1 <- plot_one_dist(sm$population, "Mouse weight (g)", "Population", c(0, 40), with.outline = TRUE)
  g2 <- plot_one_dist(sm[[as.character(n)]]$sample_means, "Sample mean weight (g)", "Sample means", c(0, 40), with.outline = TRUE)
  plot_grid(g1, g2, ncol=2, scale=0.9)
}




plot_sampling_mean <- function(sam, lims=c(0, 40), nsub=8, seed=123, cex=2, size=1, bins=100, with.errors=FALSE) {
  set.seed(seed)
  
  sam_mean <- rowMeans(sam)
  MM <- mean(sam_mean)
  SS <- sd(sam_mean)
  
  n <- nrow(sam)
  ns <- ncol(sam)
  m <- sam[sample(1:n, nsub), ] %>% 
    as_tibble(.name_repair = "unique") %>% 
    set_names(as.character(1:ns)) %>% 
    add_column(i = 1:nsub) %>% 
    pivot_longer(-i, names_to = "n")

  mm <- m %>% 
    group_by(i) %>% 
    summarise(M = mean(value), S = sd(value), SE = S / sqrt(n()))
    
  s2 <- m %>% filter(i == 2) %>% pull(value) %>% sort()
  s2e <- sd(s2) /sqrt(length(s2))
  print(round(s2, 1))
  print(signif(s2e,2))
  
  lmar <- 5
  rmar <- 6
  
  # top panel
  shift <- 0.2
  err.col <- ifelse(with.errors, "grey50", "black")
  g1 <- ggplot(m) +
    theme_classic() +
    coord_flip() +
    geom_beeswarm(aes(x=i, y=value), cex=cex, size=size, shape=21, colour="grey50", fill="darkgoldenrod1") +
    labs(y="Mouse weight (g)", x="Sample no.") +
    scale_x_continuous(breaks=c(1:nsub)) +
    scale_y_continuous(limits=lims, expand=c(0,0)) +
    geom_hline(yintercept = MM, colour="darkolivegreen", linetype="dotted") +
    theme(
      plot.margin = margin(b=-2, l=lmar, r=rmar)
    )
  if(with.errors) {
    g1 <- g1 +
      geom_errorbar(data=mm, aes(x=i+shift, ymin=M-S, ymax=M+S), width=0.2, colour=err.col) +
      geom_errorbar(data=mm, aes(x=i+shift, ymin=M-SE, ymax=M+SE), width=0.2, colour="black") +
      geom_point(data=mm, aes(i+shift, M), shape=15, size=1.5)
  } else {
    g1 <- g1 +
      geom_segment(data=mm, aes(x=i-shift, xend=i+shift, y=M, yend=M))
  }
  
  
  # bottom panel
  brks <- seq(lims[1], lims[2], length.out = bins)
  g2 <- ggplot(data.frame(d=sam_mean), aes(x=d, y=..density..)) +
    theme_dist +
    geom_histogram(breaks=brks, fill=fill.colour.mid) +
    labs(x="Mean sample weight (g)", y="Density", title=NULL) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    geom_outline(sam_mean, brks, colour="grey50") +
    theme(
      plot.margin = margin(t=-2, l=lmar, r=rmar)
    ) 
  
  # mid plot
  g3 <- ggplot(data.frame(M=MM, S=SS)) +
    theme_nothing() +
    theme(
      plot.margin = margin(t=-1, b=-5, l=lmar, r=rmar)
    ) +
    #geom_errorbarh(aes(xmin=M-S, xmax=M+S, y=0), width=0.1) +
    geom_segment(aes(x=M-S, xend=M+S, y=0, yend=0), colour="darkolivegreen") +
    geom_point(aes(x=M, y=0), colour="darkolivegreen") +
    scale_y_continuous(expand=c(0,0), limits=c(-0.5, 0.5)) +
    scale_x_continuous(expand=c(0,0), limits=lims)
  
  
  g <- plot_grid(g1, g3, g2, ncol=1, align = "v", rel_heights = c(3, 0.3, 1)) 
  g
}


plot_sampling_mean_comparison <- function(smp) {
  g1 <- plot_sampling_mean(smp$`5`$samples, seed=1007, bins=150, with.errors = TRUE)
  g2 <- plot_sampling_mean(smp$`30`$samples, seed=13, cex=1, size=0.5, bins=300, with.errors = TRUE)
  plot_grid(g1, g2, nrow=1, align="h")
}




plot_correlation_examples <- function(seed=233, n=30) {
  set.seed(seed)
  x <- runif(n)
  
  d1 <- data.frame(x=x, y=x + 0.1 * rnorm(n))
  d2 <- data.frame(x=x, y=runif(n))
  d3 <- data.frame(x=x, y=1-x + 0.1 * rnorm(n))
  
  plotR <- function(d) {
    r <- round(cor(d$x, d$y),2)
    ggplot(d, aes(x, y)) +
      theme_classic() +
      theme(
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.border = element_rect(fill=NA)
      ) +
      geom_point() +
      scale_x_continuous(limits=c(0, 1)) +
      scale_y_continuous(limits=c(0, 1)) +
      annotate("text", x=0.14, y=0.99, label=paste0("r = ", r))
  }
  
  g1 <- plotR(d1)
  g2 <- plotR(d2)
  g3 <- plotR(d3)
  
  plot_grid(g1, g2, g3, ncol=3)
}
