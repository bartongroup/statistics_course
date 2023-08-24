generate_95_ci <- function(seed, n, nsim, M, S) {
  set.seed(seed)
  Z <- qnorm(0.975)
  
  while(TRUE) {
    d <- map_dfr(1:nsim, function(i) {
      x <- rnorm(n, M, S)
      ci <- Z * sd(x) / sqrt(n)
      m <- mean(x)
      tibble(i = i, m = m, lo = m - ci, up = m + ci, nc = abs(m - M) / ci)
    })
    if(length(which(d$nc > 1)) == 5) break()
  }
  d
}


plot_95_ci <- function(seed=1001, n=30, nsim=100, M=20, S=5) {
  ci95 <- generate_95_ci(seed, n, nsim, M, S)
  ggplot(ci95) + 
    theme_clean +
    geom_errorbar(aes(x=i, ymin=lo, ymax=up), width=0.3, colour="grey70") +
    geom_errorbar(data=subset(ci95, nc>1), aes(x=i, ymin=lo, ymax=up), width=0.6, colour="black") +
    geom_point(aes(x=i, y=m), size=0.8) +
    labs(x="Experiment", y="Body mass (g)") +
    geom_hline(yintercept = M, colour="red", linetype="dotted")
  
}


generate_4_sampling_distributions <- function(seed=987, n=5, nsim=100000, mu=20, sigma=5) {
  set.seed(seed)
  map_dfr(1:nsim, function(i) {
    x <- rnorm(n, mu, sigma)
    M <- mean(x)
    S <- sd(x)
    SE <- S / sqrt(n)
    # c is much faster than tibble
    c(
      Mean = M,
      Median = median(x),
      `Standard deviation` = S,
      IQR = IQR(x),
      `Median deviation` = mad(x),
      t = (M - mu) / SE
    )
  })
}


plot_4_sampling_distributions <- function(d) {
  lms <- list(
    Mean = c(10, 30),
    Median = c(10, 30),
    `Standard deviation` = c(0, 12),
    IQR = c(0, 17),
    t = c(-6, 6)
  )
  
  gs <- map(colnames(d)[1:4], function(nm) {
    plot_one_dist(d[[nm]], nm, "", limits=lms[[nm]], with.outline = TRUE) +
      labs(y=NULL) +
      theme(
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank()
      )
  })
  plot_grid(plotlist = gs, ncol=2, scale=0.95)
}


plot_dist_ci <- function(x, brks, p = 0.05) {
  q.up <- quantile(x, 1 - 0.5*p)
  q.lo <- quantile(x, 0.5*p)
  plot_distribution_cut(x, cut=q.up, locut=q.lo, side = "two", brks=brks, fill=fill.colour, xlab="")
}


plot_err <- function(seed, n, p.size=1, cex=1) {
  set.seed(seed)
  xx <- rnorm(n)
  M <- mean(xx)
  S <- sd(xx)
  SE <- S / sqrt(n)
  CI <- SE * qt(0.975, n - 1)
  errv <- c(S, SE, CI)
  
  d <- tibble(x=rep(1, n), y=xx)
  d.err <- tibble(
    x = 2:4,
    y = M,
    ymin = M - errv,
    ymax= M + errv
  )
  ggplot() +
    theme_dist +
    theme(
      axis.line.y = element_line(arrow=arrow(type="closed", angle=20, length=unit(3, "mm"))),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      plot.margin = margin(l=6, b=5)
    ) +
    geom_beeswarm(data=d, aes(x, y), cex=cex, size=p.size, shape=21, fill=fill.colour, colour="grey40") +
    geom_errorbar(data=d.err, aes(x=x, ymin=ymin, ymax=ymax), width=0.2) +
    geom_point(data=d.err, aes(x, y), size=2, shape=21, fill="white", colour="black") +
    scale_x_continuous(limits=c(0.5, 4.5), breaks=1:4, labels=c("Sample", "SD", "SE", "95%CI"))  +
    labs(x=NULL, y="Measured value")
}


plot_se_ci_comparison <- function() {
  set.seed(1112)
  g1 <- plot_err(1112, 8)
  g2 <- plot_err(321, 100, cex=1.5) + labs(y="") + theme(plot.margin = margin(l=20))
  plot_grid(g1, g2, ncol=2, align="h", rel_widths = c(1, 1.1))
}


c_plot <- function(x, y, ylab, yintercept, ylim=NULL, ...) {
  g <- ggplot(data=data.frame(x=x, y=y)) +
    theme_clean +
    theme(plot.margin = margin(l=5, t=15, r=5)) +
    geom_hline(yintercept = yintercept, colour="red", linetype="dotted") +
    geom_line(aes(x, y), colour="grey70") +
    geom_point(aes(x, y)) +
    scale_x_continuous(expand=c(0,0), limits=c(0.01, max(x)+1), breaks=c(2, seq(5,max(x), 5))) +
    labs(x="Sample size", y=ylab)
  if(!is.null(ylim)) {
    g <- g + scale_y_continuous(expand=c(0,0), limits=ylim, ...)
  }
  g
}

plot_se_ci_limits <- function() {
  xx <- 2:30
  g1 <- c_plot(x=xx, y=qt(0.975, xx-1), expression(t[c]), qnorm(0.975), ylim=c(0,13), breaks=seq(2,14,2))
  g2 <- c_plot(x=xx, y=1 - 2*pt(-1, xx-1), "Confidence of SE", 1 - 2*pnorm(-1))
  plot_grid(g1, g2, ncol=1, align="v")
}


example_ci <- function() {
  d <- tibble(
    Day = c(rep(1, 3), rep(2, 5)),
    Value = c(0.89, 0.92, 0.90, 0.55, 0.76, 0.61, 0.83, 0.75)
  )
  sm <- d |>
    group_by(Day) |>
    summarise(m = mean(Value), ci = qt(0.975, n() - 1) * sd(Value) / sqrt(n())) |>
    mutate(lo = m - ci, up = m + ci)
  
  g1 <- ggplot() +
    theme_clean +
    geom_point(data=d, aes(Day, Value), shape=21, size=2, fill=fill.colour) +
    scale_x_continuous(limits=c(0.5,2.5), breaks=c(1,2)) +
    ylim(0.5, 1)
  
  g2 <- g1 +
    geom_errorbar(data=sm, aes(x=Day-0.1, ymin=lo, ymax=up), width=0.1) +
    geom_point(data=sm, aes(Day-0.1, m), size=2)
  
  list(
    ex1_no_ci = g1,
    ex1_with_ci = g2
  )
}


plot_binomial_median_ci <- function(n = 8) {
  x <- 0:n
  d <- data.frame(
    x = x,
    y = dbinom(x, size=n, prob=0.5)
  )
  
  ci <- 1 - 2 * pbinom(0:floor(n/2), size=n, prob=0.5)
  ci
  
  ggplot(d) +
    theme_clean +
    geom_segment(aes(x=x, xend=x, y=0, yend=y), colour="grey70") +
    geom_point(aes(x, y)) +
    geom_text(aes(x=x, y=y, label=round(y,3)), nudge_y = 0.04, size=3) +
    labs(x="", y="Probability") +
    scale_x_continuous(breaks=0:n, labels=d$x) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.2)), limits=c(0, NA))
  
}

