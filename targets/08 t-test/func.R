srnorm <- function(seed, ...) {
  set.seed(seed)
  rnorm(...)
}

znorm <- function(seed, n, SE, M0, Z, eps=1e-3) {
  set.seed(seed)
  S <- SE * sqrt(n)
  M <- M0 + Z * SE
  repeat {
    x <- rnorm(n, mean=M, sd=S)
    m <- mean(x)
    se <- sd(x) / sqrt(length(x))
    if(abs(abs(m - M0) - abs(Z * se)) < eps) break
  }
  x
}


plot_one_sample <- function(x, m, with.point=FALSE, limits=NULL, point.colour="blue", error.type="SE") {
  df <- tibble(x = x)
  maxx <- ifelse(with.point, 3, 2)
  if(is.null(limits)) {
    limits <- c(0, max(x) * 1.03)
  }
  g <- ggplot(df, aes(x=1, y=x)) +
    theme_clean +
    #geom_jitter(width=0.15, height=0) +
    geom_beeswarm(cex=0.35, size=1.2, priority = "density") +
    labs(y="Body mass (g)") +
    scale_x_continuous(limits=c(0, maxx)) +
    scale_y_continuous(expand=c(0, 0), limits=limits) +
    theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(), panel.border = element_rect(colour="grey50", fill=NA)) +
    geom_hline(yintercept = m, colour="blue", linetype="dotted")
  if(with.point) {
    M <- mean(x)
    SE <- sd(x) / sqrt(length(x))
    tc <- qt(0.975, df = length(x) - 1)
    E <- ifelse(error.type=="SE", SE, tc*SE)
    pf <- data.frame(x=2, y=M, lo=M-E, up=M+E)
    g <- g + 
      geom_point(data=pf, aes(x, y), colour=point.colour) +
      geom_errorbar(data=pf, aes(x=x, ymin=lo, ymax=up), width=0.2, colour=point.colour)
  }
  g
}


plot_t_animation <- function() {
  x <- seq(-5, 5, 0.1)
  tf <- map_dfr(1:30, function(dof) {
    tibble(
      x = x,
      y = dt(x, dof),
      dof = dof
    )
  })
  tf1 <- tf |> rename(dummy = dof)
  ef <- tibble(
    x = x,
    y = dnorm(x)
  )
  
  g <- ggplot(tf, aes(x=x, y=y)) +
    theme_clean +
    geom_line(data=tf1, aes(x=x, y=y, group=dummy), colour="grey80", linewidth=0.5) +
    geom_line(colour="red", linewidth=2) +
    geom_line(data=ef, aes(x, y), colour="black", linewidth=1) +
    theme(
      axis.text = element_text(size=24),
      axis.ticks.length = unit(3, "mm"),
      text = element_text(size=24),
      plot.title = element_text(size=26)
    ) +
    transition_manual(dof) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(tf$y) * 1.03)) +
    scale_x_continuous(breaks=seq(-4,4,2), limits=c(-5,5), expand=c(0,0)) +
    labs(title="d.o.f. = {current_frame}", x="t", y="f(t)")
  
  animate(g, fps = 20, renderer = gifski_renderer(loop = TRUE), width=600, height=500)
}


generate_null_distributions <- function(seed=4826, M=20, S=5, n=5, nsim=1e6) {
  set.seed(seed)
  X <- matrix(rnorm(n * nsim, M, S), ncol=n)
  sN <- sqrt(n)
  
  list(
    M = M,
    S = S,
    n = n,
    X = X,
    m = apply(X, 1, mean),
    z = apply(X, 1, function(x) (mean(x) - M) / (S / sN)),
    t = apply(X, 1, function(x) (mean(x) - M) / (sd(x) / sN))
  )
}


plot_t_nulls <- function(d) {
  g1 <- plot_one_dist(as.vector(d$X), "Body mass (g)", "Original distribution",c(0, 40), dnorm, mean=d$M, sd=d$S)
  g2 <- plot_one_dist(d$z, "Z", "Distribution of Z", c(-5, 5), dnorm, mean=0, sd=1)
  g3 <- plot_one_dist(d$m, "Sample mean (g)", "Distribution of M", c(0, 40), dnorm, mean=d$M, sd=d$S/sqrt(d$n))
  
  gt <- tibble(
    x = seq(-5, 5, length.out = 100),
    y = dt(x, d$n-1)
  )
  g4 <- plot_one_dist(d$t, "t", "Distribution of t", c(-5, 5), dnorm, mean=0, sd=1) +
    geom_line(data=gt, aes(x, y), size=0.5, colour="blue") 
  
  plot_grid(g1, g2, g3, g4, ncol=2, scale = 0.93)
}


plot_t_with_cuts <- function(smpl) {
  M <- mean(smpl)
  n <- length(smpl)
  SE <- sd(smpl) / sqrt(n)
  t.obs <- (M - 20) / SE
  dof <- length(smpl) - 1
  
  list(
    t_dist = plot_fun(dt, df=dof, x.grid=seq(-5,5,0.01), name="t"),
    t_dist_cut1 = plot_fun(dt, df=dof, x.grid=seq(-5,5,0.01), cut.up=t.obs, name="t"),
    t_dist_cut2 = plot_fun(dt, df=dof, x.grid=seq(-5,5,0.01), cut.lo=-t.obs, cut.up=t.obs, name="t")
  )
}



generate_normality_distributions <- function(seed=4826, M=20, S=5, n=5, nsim=1e6) {
  set.seed(seed)
  
  # normal
  X1 <- matrix(rnorm(n * nsim, M, S), ncol=n)
  
  # bimodal
  X2 <- c(rnorm(n * nsim/2, M - 5, S/2), rnorm(n * nsim/2, M + 5, S/2))
  X2 <- matrix(sample(X2), ncol=n) # mix both distributions
  M2 <- mean(X2)

  # asymmetric
  X3 <- c(rnorm(n * nsim/4, M - 5, S/2), rnorm(n * 3 * nsim/4, M + 5, S/2))
  X3 <- matrix(sample(X3), ncol=n) # mix both distributions
  M3 <- mean(X3)

  sN <- sqrt(n)
  
  list(
    M = M,
    S = S,
    n = n,
    X1 = X1,
    X2 = X2,
    X3 = X3,
    t1 = apply(X1, 1, function(x) (mean(x) - M) / (sd(x) / sN)),
    t2 = apply(X2, 1, function(x) (mean(x) - M2) / (sd(x) / sN)),
    t3 = apply(X3, 1, function(x) (mean(x) - M3) / (sd(x) / sN))
  )
}


plot_normality_t <- function(d) {
  t.lim <- c(-5, 5)
  
  g1 <- plot_one_dist(as.vector(d$X1), "Body mass (g)", "", c(0, 40), dnorm, mean=d$M, sd=d$S, maxy=0.085)
  g2 <- plot_one_dist(d$t1, "t", "", t.lim, dt, df=4)
  
  g3 <- plot_one_dist(as.vector(d$X2), "Body mass (g)", "", c(0, 40), dnorm, mean=d$M, sd=d$S, maxy=0.085)
  g4 <- plot_one_dist(d$t2, "t", "", t.lim, dt, df=4, maxy=0.45)
  
  g5 <- plot_one_dist(as.vector(d$X3), "Body mass (g)", "", c(0, 40), dnorm, mean=d$M, sd=d$S, maxy=0.125)
  g6 <- plot_one_dist(d$t3, "t", "", t.lim, dt, df=4, maxy=0.45)
  
  g <- plot_grid(g1, g3, g5, g2, g4, g6, ncol=3, scale=0.95)
  
}


generate_p5 <- function(seed = 222) {
  set.seed(seed)
  done <- FALSE
  while(!done) {
    x5 <- rnorm(5, 25, 5)
    p <- t.test(x5, mu=20)$p.value
    done <- abs(p - 0.05) < 1e-5
  }
  x5
}


plot_ci_vs_test <- function(smp) {
  plot_one_sample(smp, 20, limits=c(min(smp)-2, max(smp)+2), with.point=TRUE, error.type="CI") 
}

make_DMT <- function(X1, X2) {
  n1 <- ncol(X1)
  n2 <- ncol(X2)
  
  mean1 <- apply(X1, 1, mean)
  mean2 <- apply(X2, 1, mean)
  sd1 <- apply(X1, 1, sd)
  sd2 <- apply(X2, 1, sd)
  sd12 <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  se.eq <- sd12 * sqrt(1/n1 + 1/n2)
  nu.eq <- n1 + n2 - 2
  
  se1.sq <- sd1^2 / n1
  se2.sq <- sd2^2 / n2
  se.ne <- sqrt(se1.sq + se2.sq)
  nu.ne <- (se1.sq + se2.sq)^2 / ( se1.sq^2 / (n1 - 1) +  se2.sq^2 / (n2 - 1) )
  
  tibble(
    n1 = n1,
    n2 = n2,
    dM = mean1 - mean2,
    sd12 = sd12,
    se.eq = se.eq,
    se1.sq = se1.sq,
    se2.sq = se2.sq,
    se.ne = se.ne,
    nu.eq = nu.eq,
    nu.ne = nu.ne,
    t.eq = (mean1 - mean2) / se.eq,
    t.ne = (mean1 - mean2) / se.ne
  )
}


generate_two_sample_t_test <- function(seed=22, M=20, S=5, N1=12, N2=9, nsim=1e6) {
  set.seed(seed)
  X1 <- matrix(rnorm(N1 * nsim, M, S), ncol=N1)
  X2 <- matrix(rnorm(N2 * nsim, M, S), ncol=N2)
  
  make_DMT(X1, X2) 
}


generate_t_test_variance <- function(seed=22, M=20, S1=5, S2=2.5, N1=12, N2=9, nsim=1e6) {
  set.seed(seed)
  X1 <- matrix(rnorm(N1 * nsim, M, S1), ncol=N1)
  X2 <- matrix(rnorm(N2 * nsim, M, S2), ncol=N2)
  
  make_DMT(X1, X2)
}


plot_t2_null <- function(md) {
  list(
    two_sample_tdist = plot_one_dist(md$t.eq, "t", "", c(-4, 4), NULL),
    two_sample_tdist_t = plot_one_dist(md$t.eq, "t", "", c(-4, 4), dt, md$n1 + md$n2 - 2)
  )
}


mice_properties <- function(mice) {
  English <- mice |> filter(Country=="English") |> pull(Mass)
  Scottish <- mice |> filter(Country=="Scottish") |> pull(Mass)
  mice |> group_by(Country) |> summarise(M=mean(Mass), SD=sd(Mass), VAR=var(Mass))
  make_DMT(
    matrix(English, nrow=1), 
    matrix(Scottish, nrow=1)
  )
}


plot_t2_var <- function(mice) {
  mp <- mice_properties(mice)
  
  list(
    t_eq = plot_fun(dt, df=mp$nu.eq, x.grid=seq(-5,5,0.01), cut.up = -mp$t.eq, name="t"),
    t_ne = plot_fun(dt, df=mp$nu.ne, x.grid=seq(-5,5,0.01), cut.up = -mp$t.ne, name="t")
  )
}


plot_t2_var_dist <- function(mv) {
  g1 <- plot_one_dist(mv$t.eq, "t", expression(Equal~variance), c(-4, 4), dt, mv$n1 + mv$n2 - 2)
  g2 <- plot_one_dist(mv$t.ne, "t", expression(Unequal~variance), c(-4, 4), dt, mv$n1 + mv$n2 - 2)
  plot_grid(g1, g2, ncol=1)
}

generate_effect_p <- function(n, p, M1=20, S=5, eps=1e-4, max.iter=10000) {
  t <- qt(1 - p, 2 * n - 2)
  SE <- S / sqrt(n)
  dM <- t * SE
  M2 <- M1 + dM
  
  i <- 1
  repeat {
    x1 <- rnorm(n, M1, S)
    x2 <- rnorm(n, M2, S)
    test <- t.test(x1, x2)
    delta <- abs(p - test$p.value)
    i <- i + 1
    #cat(paste(i, delta, test$p.value, "\n"))
    if(delta < eps || i > max.iter) break
  }
  list(x1=x1, x2=x2, p=test$p.value, fc=mean(x2) / mean(x1), dM=mean(x2) - mean(x1))
}


mk_mice <- function(dat) {
  tibble(
    Country = c(rep("English", length(dat$x1)), rep("Scottish", length(dat$x2))),
    Mass = c(dat$x1, dat$x2)
  )
}

plot_pval_effect <- function(seed=777) {
  set.seed(seed)
  dat1 <- generate_effect_p(8, 0.02)
  dat2 <- generate_effect_p(100, 0.02)
  
  print(dat1$dM)
  print(dat2$dM)

  lms <- c(0, 40)
  
  g1 <- plot_mice_box(mk_mice(dat1), cex=1.2, size=1, limits=lms)
  g2 <- plot_mice_box(mk_mice(dat2), cex=1.2, size=1, limits=lms)
  
  plot_grid(g1, g2, ncol=2)
}



gen_sd <- function(n, M, S, eps=1e-3) {
  repeat{
    x <- rnorm(n, 0, S)
    if(abs(sd(x) - S) < eps) break
  }
  x - mean(x) + M
}


generate_overlap_ci <- function(seed = 225, n=8, S=5, M=20) {
  set.seed(seed)
  x1 <- gen_sd(n, M, S)
  x <- gen_sd(n, M, S)
  tc <- qt(0.975, n - 1)
  CI <- tc * S / sqrt(n)
  x2 <- x + 2 * CI
  
  # find delta M to give p = 0.05
  err <- function(dm) (t.test(x1, x+dm)$p.value - 0.05)^2
  opt <- optim(5, err, method = "Brent", lower=0, upper=10)
  x3 <- x + opt$par
  
  list(
    x1 = x1,
    x2 = x2,
    x3 = x3
  )
}

plotCI <- function(x1, x2) {
  n <- length(x1)
  m1 <- mean(x1)
  m2 <- mean(x2)
  tc <- qt(0.975, n - 1)
  ci1 <- tc * sd(x1) / sqrt(n)
  ci2 <- tc * sd(x2) / sqrt(n)
  
  d <- data.frame(
    x = c(rep("A", n), rep("B", n)),
    y = c(x1, x2)
  )
  dm <- data.frame(
    x = c("A", "B"),
    y = c(m1, m2),
    s = c(ci1, ci2)
  )
  dm$i <- c(1.4, 1.6)
  
  p <- t.test(x1, x2, var.equal = TRUE)$p.value
  p <- sprintf("%.2g", p)
  
  g <- ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    geom_beeswarm(data=d, aes(x=x, y=y, fill=x), shape=21, cex=1.3) +
    geom_errorbar(data=dm, aes(x=i, ymin=y-s, ymax=y+s), width=0.1, colour="grey50") +
    #geom_errorbar(data=dm, aes(x=i, ymin=y-s/sqrt(2), ymax=y+s/sqrt(2)), width=0.1) +
    geom_point(data=dm, (aes(x=i, y=y, fill=x)), shape=22, size=2) +
    labs(x=NULL, y="Mass (g)", title=paste0("p = ", p)) +
    scale_fill_manual(values=okabe_ito_palette)
  g
}



plot_ci_conf <- function() {
  d <- generate_overlap_ci()
  
  g1 <- plotCI(d$x1, d$x2)
  g2 <- plotCI(d$x1, d$x3)
  
  plot_grid(g1, g2, nrow=1)
}


make_paired_t_data <- function() {
  tibble(
    Before = c(21.4, 20.2, 23.5, 17.5, 18.6, 17.0, 18.9, 19.2),
    After = c(22.6, 20.9, 23.8, 18.0, 18.4, 17.9, 19.3, 19.1)
  )
}


plot_paired_test <- function(pd) {
  x <- pd |> 
    mutate(sign = as_factor(sign(Before - After)))
  
  m <- pd |> 
    mutate(id = row_number()) |> 
    pivot_longer(-id) |> 
    mutate(name = name |> as_factor() |> fct_relevel("Before"))
  
  
  g <- ggplot(m, aes(name, value)) +
    theme_clean +
    labs(x="", y="Body mass (g)") +
    theme(legend.position = "none") +
    scale_color_manual(values=okabe_ito_palette)
  
  list( 
    ttest_unpaired = g + geom_point(),
    ttest_paired = g + geom_point() + geom_segment(data=x, aes(x="Before", xend="After", y=Before, yend=After, colour=sign)) + geom_point() 
  )
}


generate_f_null <- function(seed=1966, M=20, S=5, N1=12, N2=9, nsim=1000000) {
  set.seed(seed)
  X1 <- matrix(rnorm(N1 * nsim, M, S), ncol=N1)
  X2 <- matrix(rnorm(N2 * nsim, M, S), ncol=N2)
  
  sd1 <- apply(X1, 1, sd)
  sd2 <- apply(X2, 1, sd)
  F <- sd1^2 / sd2^2
}


plot_f_tests <- function(d, N1 = 12, N2 = 9) {
  list(
    ftest_distribution = plot_one_dist(d, "F", "", c(0, 6), NULL),
    ftest_distribution_line = plot_one_dist(d, "F", "", c(0, 6), df, df1=N1-1, df2=N2-1)
  )
}


plot_f_dist <- function(mice) {
  ms <- mice |> 
    group_by(Country) |> 
    summarise(S = sd(Mass), n = n())
  m <- map(1:nrow(ms), ~slice(ms, .x)) |> set_names(ms$Country)
  
  F_obs <- m$English$S^2 / m$Scottish$S^2
  df1 = m$English$n - 1
  df2 <- m$Scottish$n - 1
  
  
  plot_fun(df, df1 = df1, df2 = df2, cut.up = F_obs, x.grid = seq(0, 6, 0.01), name = "F")
}
