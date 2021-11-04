count_ci_approx <- function(n, conf_level=0.95) {
  Z <- qnorm((1 + conf_level) / 2)
  list(
    n = n,
    lo = ifelse(n == 0, 0, n - Z * sqrt(n) + (Z^2 - 1) / 3),
    up = n + Z * sqrt(n + 1) + (Z^2 + 2) / 3
  )
}

count_ci <- function(n, conf_level=0.95) {
  p <- poisson.test(n, conf.level = conf_level)
  c(
    n = n,
    lo = p$conf.int[1],
    up = p$conf.int[2]
  )
}


plot_pois_shift <- function(mu, n, cut="lo") {
  xx <- 0:30
  d <- tibble(
    x = xx,
    y = dpois(xx, mu)
  )
  g <- ggplot(d, aes(x, y)) +
    theme_dist +
    geom_col(fill=fill.colour, colour="grey50", width=0.7) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$y) * 1.03)) +
    scale_x_continuous(expand=c(0,0), limits=c(-0.5, 20.5), breaks=0:20) +
    labs(x=NULL)
  if(cut == "lo") {
    dsel <- d[d$x <= n, ]
  } else {
    dsel <- d[d$x >= n, ]
  }
  g <- g + geom_col(data=dsel, fill=fill.colour.dark, colour="grey50", width=0.7)
}

plot_poisson_ci_shift <- function(n = 5) {
  ci <- count_ci(n)
  g1 <- plot_pois_shift(ci[["lo"]], n, cut="up")
  g2 <- plot_pois_shift(ci[["up"]], n, cut="lo")
  
  plot_grid(g1, g2, ncol=1, align="v")
}


plot_count_ci <- function(n, breaks=NULL, no.lab.y=FALSE) {
  if(is.null(breaks)) breaks <- waiver()
  d <- map_dfr(n, count_ci) %>% 
    mutate(
      lo_se = n - sqrt(n),
      up_se = n + sqrt(n),
      x = seq_along(n)
    )

  laby <- ifelse(no.lab.y, "", "Count")
  g <- ggplot(d) +
    theme_clean +
    theme(
      axis.ticks.x = element_blank()
      #axis.text.x = element_blank()
    ) +
    geom_errorbar(aes(x=x, ymin=lo, ymax=up), width=0.15, colour="black") +
    geom_errorbar(aes(x=x, ymin=lo_se, ymax=up_se), width=0.15, colour="grey60") +
    geom_point(aes(x, n), size=2) +
    labs(x=NULL, y=laby) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits=c(0, NA), breaks=breaks) +
    scale_x_continuous(breaks=d$x, labels=d$n)
}


plot_count_ci_example <- function() {
  g1 <- plot_count_ci(0:5, breaks=0:12)
  g2 <- plot_count_ci(c(10, 30, 50, 100), breaks=seq(0, 200, 10), no.lab.y = TRUE)
  plot_grid(g1, g2, ncol=, align="h")
}


generate_cor <- function(n, r, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  X <- MASS::mvrnorm(n=n, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
  colnames(X) <- c("x", "y")
  as_tibble(X)
}


sampling_distribution_cor <- function(dat_cor, seed=123, nsim=100000, n=30) {
  set.seed(seed)
  R <- double(nsim)
  for(i in 1:nsim) {
    d <- sample_n(dat_cor, n)
    R[i] <- cor(d$x, d$y)
  }
  tibble(
    r = R,
    Z = 0.5 * log((1 + r) / (1 - r))
  )
}


plot_cor_sample <- function(dat_cor, n=30, lim=2.5, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  d <- sample_n(dat_cor, n)
  r <- cor(d$x, d$y)
  g <- ggplot(d, aes(x, y)) +
    theme_nothing() +
    theme(
      panel.border = element_rect(colour="black", fill=NA),
      plot.margin = margin(0,0,0,0)
    ) +
    xlim(-lim, lim) + ylim(-lim, lim) +
    geom_point() +
    annotate("text", x=-1.9, y=1.9, label=round(r, 2), colour="darkgoldenrod4", size=5)
  g
}

plot_9_cor <- function(dat_cor, seed = 2237) {
  set.seed(seed)
  map(1:9, ~plot_cor_sample(dat_cor)) %>% 
    plot_grid(plotlist=., ncol=3)  
}

plot_cor_sampling <- function(sam_cor) {
  cor_cut <- quantile(sam_cor$r, c(0.025, 0.975))
  z_cut <- quantile(sam_cor$Z, c(0.025, 0.975))
  list(
    cor_sampling_r = plot_distribution_cut(sam_cor$r, cut=cor_cut[2], locut=cor_cut[1], side="two", brks=seq(0, 1, 0.01), x.brks=seq(0, 1, 0.2)),
    cor_sampling_z = plot_distribution_cut(sam_cor$Z, cut=z_cut[2], locut=z_cut[1], side="two", brks=seq(0, 1.8, 0.018))               
  )
}

# Pearson's r density function
dpears <- function(r, ro, n) {
  f <- hyperg_2F1(0.5, 0.5, (2*n - 1) /2, (ro*r + 1) / 2)
  num <- (n - 2) * gamma(n - 1) * (1 - ro^2)^((n - 1) / 2) * (1 - r^2) ^ ((n - 4) / 2)
  den <- sqrt(2 * pi) * gamma(n - 0.5) * (1 - ro*r)^(n - 1.5)
  num * f / den
}


plot_95_ci_r <- function(n = 30, r = 0.7) {
  Z <- 0.5 * log((1+r) / (1-r))
  sigma <- 1 / sqrt(n - 3)
  Z95 <- qnorm(0.975)
  
  Z.lo <- Z - Z95 * sigma
  Z.up <- Z + Z95 * sigma
  
  r.lo <- (exp(2*Z.lo) - 1) / (exp(2*Z.lo) + 1)
  r.up <- (exp(2*Z.up) - 1) / (exp(2*Z.up) + 1)

  g1 <- plot_fun(dnorm, Z, sigma, x.grid=seq(0, 1.8, 0.01), cut.lo=Z.lo, cut.up=Z.up) +
    geom_segment(aes(x=Z, xend=Z, y=0, yend=dnorm(Z, Z, sigma)))

  g2 <- plot_fun(dpears, 0.7, 30, x.grid=seq(0, 1, 0.01), cut.lo=r.lo, cut.up=r.up) +
    geom_segment(aes(x=r, xend=r, y=0, yend=dpears(r, 0.7, 30))) +
    scale_x_continuous(expand=c(0,0), limits=c(0,1), breaks=seq(0,1,0.2))
  list(
    correlation_dist = g1,
    fisher_dist = g2
  )
}


plot_cor_6_30 <- function(seed=143, r=0.7) {
  set.seed(seed)
  d6 <- generate_cor(6, r)
  d30 <- generate_cor(30, r)
  cor.test(d6$x, d6$y)
  cor.test(d30$x, d30$y)
  
  plotCor <- function(d) {
    ggplot(d) +
      theme_clean +
      geom_point(aes(x, y), shape=21, fill=fill.colour) +
      xlim(-2, 2) + ylim(-2, 2)
  }
  
  g1 <- plotCor(d6)
  g2 <- plotCor(d30)
  plot_grid(g1, g2, nrow=1)
}


sampling_dist_proportion <- function(p = 0.13, ns = c(998, 50, 10), nsim=100000) {
  map_dfr(ns, function(k) {
    tibble(
      s = rbinom(nsim, k, p)
    ) %>%
      group_by(s) %>% 
      tally() %>% 
      mutate(
        prop = s / k,
        prob = n / sum(n),
        n = k
      )
  })
}

one_prop_plot <- function(d, nn, point.size=1, xmax=0.6, xlab=NULL) {
  d %>% 
    filter(n == nn) %>% 
    ggplot() +
    theme_clean +
    geom_segment(aes(x=prop, xend=prop, y=0, yend=prob), colour="grey70") +
    geom_point(aes(prop, prob), size=point.size) +
    labs(x=xlab, y="Probablity") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(limits=c(0, xmax), breaks=seq(0, 1, 0.1))
}



plot_samp_propotion <- function() {
  S <- sampling_dist_proportion()
  
  g1 <- one_prop_plot(S, 998, point.size = 0.8)
  g2 <- one_prop_plot(S, 50, point.size = 1)
  g3 <- one_prop_plot(S, 10, point.size = 1.2, xlab="Proportion")
  plot_grid(g1, g2, g3, ncol=1)
  
}

# not used anymore
wald_proportion <- function(n, S, alpha=0.95) {
  Z <- qnorm(1 - (1 - alpha) / 2)
  Sp <- S + Z^2/2
  np <- n + Z^2
  pp <- Sp / np
  SE <- sqrt(pp * (1 - pp) / np)
  W <- Z * SE
  v <- c(pp - W, pp + W)
  v[v < 0] <- 0
  v[v > 1] <- 1
  d <- tibble(
    prop_lo = v[1],
    prop_up = v[2]
  )
  names(d) <- paste0(names(d), "_", n)
  d
}


plot_mice_survival <- function() {
  sur <- tibble(
    lengths = c(3, 3, 1, 5, 3, 6),
    values = c(10, 8, 6, 2, 1, 0)
  )
  P <- inverse.rle(sur) / 10
  
  d <- map_dfr(c(10, 100), function(N) {
    tibble(
      day = seq_along(P) - 1,
      prop = P,
      n = N,
      s = N * P
    )
  }) %>% 
    # need to differentiate rows for nest
    mutate(id = row_number()) %>% 
    nest(data = c(s, n)) %>%
    mutate(
      tst = map(data, ~prop.test(.x$s, .x$n)),
      tidied = map(tst, broom::tidy),
    ) %>%
    unnest(c(data, tidied)) %>% 
    select(day, prop, s, n, conf.low, conf.high) %>% 
    pivot_wider(id_cols = c(day, prop), names_from = n, values_from = c(conf.low, conf.high))
  

  g0 <- ggplot(d) +
    theme_clean +
    scale_x_continuous(expand=c(0,0), limits=c(0,20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x = "Time (day)", y = "Survival proportion")
  
  g1 <- g0 + geom_step(aes(x=day, y=prop), size=1.1)
  
  g2 <- g1 +
    geom_ribbon(aes(x=day, ymin=conf.low_10, ymax=conf.high_10), stat="stepribbon", fill=fill.colour) +
    geom_step(aes(x=day, y=prop), size=1.1) 
  
  g3 <- g2 +
    geom_ribbon(aes(x=day, ymin=conf.low_100, ymax=conf.high_100), stat="stepribbon", fill=fill.colour.dark) +
    geom_step(aes(x=day, y=prop), size=1.1)
  
  list(
    survival = g1,
    survival_ci = g3
  )
}


plot_bootstrap_example <- function(seed = 124, x = c(19.4, 18.2, 11.5, 17.2, 25.7, 19.2, 21.5, 16.7, 15.6, 27.7, 14.3, 16.3), nsim=100000) {
  set.seed(seed)
  n <- length(x)
  
  m_boot <- map_dbl(1:nsim, ~mean(sample(x, n, replace = TRUE)))
  lims <- quantile(m_boot, c(0.025, 0.975))
  plot_distribution_cut(m_boot, cut=lims[2], locut=lims[1], side="two", brks=seq(13, 24, 0.05), x.brks = seq(10, 30, 2))
}
