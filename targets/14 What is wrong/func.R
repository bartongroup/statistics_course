generate_two_dist <- function(seed=111, n1=97000, n2=3000, M1=20, M2=30, S=5) {
  set.seed(seed)
  tibble(
    hyp = c(rep(TRUE, n1), rep(FALSE, n2)),
    value = c(rnorm(n1, M1, S), rnorm(n2, M2, S))
  )
}

p_sampling <- function(d, null, n=5, mu=20) {
  x <- d %>% filter(hyp == null) %>% pull(value)
  if(length(x) == 0) return(numeric(0))
  nsim <- length(x)
  map_dbl(1:nsim, function(i) {
    t.test(sample(x, n), mu = mu)$p.value
  })
}

sampling_two_dist <- function(d, seed=9999, n=5, mu=20) {
  set.seed(seed)
  p1 <- p_sampling(d, TRUE, n=n, mu=mu)
  p2 <- p_sampling(d, FALSE, n=n, mu=mu)
  tibble(
    hyp = c(rep(TRUE, length(p1)), rep(FALSE, length(p2))),
    p = c(p1, p2)
  )
}


generate_effect_size <- function(seed=3736, nsim=100000, M1=20, M2=25, n=3, S=5) {
  set.seed(seed)
  pop <- rnorm(nsim, M2, S)
  map_dfr(1:nsim, function(i) {
    x <- sample(pop, n)
    M <- mean(x)
    p <- t.test(x, mu = M1)$p.value
    c(M=M, p=p)
  })
}


read_strains <- function(file) {
  read_tsv("data/all_update.txt", col_names = FALSE) %>% 
    select(11, 30) %>% 
    set_names("distance", "strain") %>% 
    filter(strain %in% c("70.6kb", "71kb")) %>% 
    mutate(distance = distance / 1000)
}


plot_mix_dist <- function(d, breaks, alpha=0.8) {
  d0 <- d %>% filter(hyp)
  d1 <- d %>% filter(!hyp)
  ggplot() +
    theme_dist +
    labs(x="Body mass (g)", y=NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    geom_histogram(aes(x=value), data=d0, fill=ef_palette[1], breaks=breaks, alpha=alpha) +
    geom_outline(d0$value, breaks, y.value="count", colour="grey40", alpha=alpha)+
    geom_histogram(aes(x=value), data=d1, fill=ef_palette[2], breaks=breaks, alpha=alpha) +
    geom_outline(d1$value, breaks, y.value="count", colour="grey40", alpha=alpha)
}

plot_mix_p <- function(p, x.lim=c(0, 1), bins=0.01, alpha=0.8, ymax=5, x.breaks=seq(0,1,0.2)) {
  brks <- seq(0, 1, bins)
  
  ggplot() +
    theme_clean +
    theme(
      legend.position = "none",
      plot.margin = margin(l=10, r=10, t=6)
    ) +
    labs(x="P-value", y=NULL) +
    scale_y_continuous(expand=c(0,0), limits=c(0, ymax)) +
    scale_fill_manual(values = ef_palette[2:1]) +
    scale_x_continuous(breaks=x.breaks, expand=c(0,0)) +
    coord_cartesian(xlim=x.lim) +
    geom_histogram(data=p, aes(x=p, y=..count../(sum(..count..) * bins), fill=hyp), breaks=brks, position="stack") +
    geom_outline(p$p, brks, colour="grey40", alpha=alpha)
}



plot_effect_sizes <- function(eff) {
  eff$sig <- eff$p < 0.05
  mean(eff[eff$sig, "M"])
  
  brks <- seq(10, 40, 0.2)
  g1 <- ggplot(eff) +
    theme_dist +
    geom_histogram(aes(x=M, fill=sig), breaks=brks) +
    scale_x_continuous(expand=c(0,0)) +
    scale_fill_manual(values=ef_palette) +
    geom_outline(eff$M, brks, y.value = "count", colour="grey30", size=0.3) +
    labs(x="Sample mean (g)") +
    geom_vline(xintercept = 20) +
    geom_vline(xintercept = 25, linetype="dotted") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)))

  
  g2 <- plot_distribution_cut(eff$p, cut=0.05, brks=seq(0,1,0.01), fill=ef_palette[2], fill.cut=ef_palette[1], outline.colour="grey30", xlab="P-value", x.brks = seq(0, 1, 0.2)) 
  
  list(
    effect_size_M = g1,
    effect_size_p = g2
  )
}


plot_strain_dist <- function(d) {
  m <- d %>%
    group_by(strain) %>%
    summarise(M = mean(distance), SE = sd(distance) / sqrt(n()))
  
  t.test(distance ~ strain, data=d)
  
  g1 <- ggplot(m) +
    theme_clean +
    geom_errorbar(aes(x=strain, ymin=M-SE, ymax=M+SE), width=0.1) +
    geom_col(aes(x=strain, y=M), fill="black", width=0.5) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(m$M + m$SE)*1.03)) +
    labs(x=NULL, y = ~ "Distance ("*mu*m*")")
  
  g2 <- ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    scale_colour_manual(values=okabe_ito_palette) +
    geom_beeswarm(data=d, aes(x=strain, y=distance), cex=1, size=0.15, priority = "density") +
    labs(x=NULL, y = ~ "Distance ("*mu*m*")") +
    geom_boxplot(data=m, aes(x=strain, middle=M, ymin=M, lower=M, upper=M, ymax=M), stat="identity", width=0.7, lwd=0.6, fatten=0, colour="darkgoldenrod3")
  
  list(
    large_sample_shitplot = g1,
    large_sample = g2
  )
}

