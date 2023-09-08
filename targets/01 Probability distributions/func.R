plot_2dice_binomial <- function() {
  dist <- data.frame(
    x = 2:12,
    n = c(1:6, 5:1)
  )
  dist$y <- dist$n / sum(dist$n)
  
  g <- ggplot(dist, aes(x, y)) +
    theme_clean +
    geom_segment(aes(x=x, y=y, xend=x, yend=0), colour="grey") +
    geom_point(shape=21, fill="white", colour="black") +
    geom_text(aes(label=n), nudge_y=0.01) +
    scale_x_continuous(breaks=2:12) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(dist$y)*1.1)) +
    labs(x = "Outcome", y = "Probability")
}


plot_normal_sigmas <- function(M, S) {
  x <- seq(0, 20, 0.1)
  y <- dnorm(x, M, S)
  dn <- data.frame(x=x, y=y)
  
  lns1 <- data.frame(
    x = c(M - S, M, M + S),
    y = c(0.3, 0.28, 0.3)
  )
  
  lns2 <- data.frame(
    x = c(M - 3*S, M - 2*S, M + 2*S, M + 3*S),
    y = c(0.4, 0.35, 0.35, 0.4)
  )
  
  g1 <- ggplot() +
    theme_clean +
    geom_segment(data=lns1, aes(x=x, y=y, xend=x, yend=0), colour="grey") +
    geom_line(data=dn, aes(x, y), colour="black") +
    labs(x="x", y="f(x)") +
    scale_x_continuous(limits=c(4, 16), expand=c(0,0)) +
    scale_y_continuous(limits=c(0, 0.4), expand=c(0,0))
  
  
  g2 <- g1 + 
    geom_segment(data=lns2, aes(x=x, y=y, xend=x, yend=0), colour="grey") +
    geom_cut(M - 3*S, M + 3*S, dnorm, M, S, fill=fill.colour) +
    geom_cut(M - 2*S, M + 2*S, dnorm, M, S, fill=fill.colour.mid) +
    geom_cut(M - 1*S, M + 1*S, dnorm, M, S, fill=fill.colour.dark)
  
  list(normal_sigmas_1=g1, normal_sigmas_2=g2)
}


plot_baseball_normal <- function(baseball) {
  baseball <- baseball |> 
    mutate(height = 2.54 * `Height(inches)`)
  dst <-  baseball |> 
    group_by(height) |> 
    tally() 
  
  M <- mean(baseball$height)
  S <- sd(baseball$height)
  print(M)
  print(S)
  
  mdl <- tibble(
    x = seq(100, 250, 0.1),
    y = dnorm(x, M, S) * sum(dst$n) * 2.54
  )
  
  ggplot(dst, aes(height, n)) +
    theme_clean +
    geom_segment(aes(xend=height, yend=0), colour="grey60") +
    geom_point(shape=21, fill="white", size=2) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.03)), limits=c(0, NA)) +
    geom_line(data=mdl, aes(x, y), colour="red") +
    labs(x = "Height (cm)", y = "Frequency") +
    scale_x_continuous(limits=c(165, 215), expand=c(0,0))
}


plot_lognormals <- function(d) {
  lognormal_lin <- plot_one_dist(d$value/1e6, expression(Intensity~x10^6), "Linear scale", c(0, 10), with.outline=TRUE, outline.colour="grey30", maxy=1.7, brks.x=c(0,2,4,6,8,10))
  lognormal_lin_1 <- plot_one_dist(d$value/1e6, expression(Intensity~x10^6), "Linear scale", c(0, 10), with.outline=TRUE, outline.colour="grey30", maxy=1.7, brks.x=c(0,2,4,6,8,10), with.sd=TRUE) + scale_x_continuous(limits=c(0,10), expand=c(0,0), breaks=c(0,2,4,6,8,10))
  lognormal_log <- plot_one_dist(log10(d$value), expression(log[10]~Intensity), "Logarithmic scale", c(3.5, 8.5), with.outline=TRUE, outline.colour="grey30", max=0.65)
  lognormal_log_1 <- plot_one_dist(log10(d$value), expression(log[10]~Intensity), "Logarithmic scale", c(3.5, 8.5), with.outline=TRUE, outline.colour="grey30", max=0.65, with.sd=TRUE)
  
  list(
    lognormal_lin = lognormal_lin,
    lognormal_lin_1 = lognormal_lin_1,
    lognormal_log = lognormal_log,
    lognormal_log_1 = lognormal_log_1
  )
}


plot_xy <- function(d, log.scale=FALSE) {
  if(log.scale) {
    xlab <- expression(log[10]~I[1])
    ylab <- expression(log[10]~I[2])
    d <- d |> mutate(x = log10(x), y = log10(y))
  } else {
    xlab <- expression(I[1])
    ylab <- expression(I[2])
  }
  
  ggplot(d, aes(x=x, y=y)) +
    theme_clean +
    geom_point(size = 0.4, alpha = 0.4) +
    labs(x=xlab, y=ylab)
}

plot_replicates_loglin <- function(d, reps) {
  d <- d |> 
    select(all_of(reps)) |> 
    set_names(c("x", "y"))
  list(
    lin = plot_xy(d, log.scale = FALSE),
    log = plot_xy(d, log.scale = TRUE)
  )
}

plot_poisson_plates_dist <- function(seed=226, n=20, m=7) {
  set.seed(seed)

  d <- tibble(
    plate = 1:n,
    cnt = rpois(n, m)
  ) 
  
  g1 <- ggplot(d, aes(plate, cnt)) +
    theme_classic() +
    #geom_segment(aes(x=plate, xend=plate, y=cnt, yend=0), colour="grey80") +
    geom_point(shape=22, fill=fill.colour) +
    scale_x_continuous(breaks=c(1,5,10,15,20)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits=c(0, NA), breaks=2*1:10) +
    labs(x="Plate", y="Count")

  x <- 0:16
  d <- data.frame(
    x = x,
    y = dpois(x, m)
  )
  g2 <- ggplot(d, aes(x, y)) +
    theme_classic() +
    geom_col(fill=fill.colour, colour="black", width=0.8) +
    scale_x_continuous(breaks=0:16) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "k", y = "P(X = k)")
  
  list(
    poisson_plates = g1,
    poisson_dist = g2
  )
  
}


plot_poisson_dist_examples <- function(mus = c(0.3, 1, 4, 10)) {
  gs <- map(mus, function(mu) {
    xp <- 0:20
    p <- tibble(
      x = xp,
      y = dpois(xp, mu)
    )
    xn <- seq(0, 20, 0.01)
    d <- tibble(
      x = xn,
      y = dnorm(xn, mu, sqrt(mu))
    )
    
    ggplot() +
      theme_clean +
      geom_segment(data=p, aes(x=x, xend=x, y=y, yend=0), colour="grey80") +
      geom_point(data=p, aes(x, y), shape=21, fill="white") +
      geom_line(data=d, aes(x, y), colour="red") +
      #stat_function(fun=dnorm, args=list(mean=mu, sd=sqrt(mu)), colour="red") +
      scale_x_continuous(breaks=c(0,5,10,15,20)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.07)), limits = c(0, NA)) +
      labs(x="k", y="")
  })
  plot_grid(plotlist = gs, ncol = 1, align = "v")
}


plot_horse_kicks <- function() {
  h <- c(144, 91, 32, 11, 2, 0)
  x <- 0:5
  mu <- sum(h * x) / sum(h)
  d <- tibble(
    x = x,
    h = h,
    s = sqrt(h),
    p = dpois(x, mu) * sum(h)
  )
  
  g <- ggplot(d) +
    theme_clean +
    geom_col(aes(x, p), fill=fill.colour, width=0.7, colour="grey30") +
    geom_point(aes(x, h), size=2) +
    geom_errorbar(aes(x=x, ymin=h-s, ymax=h+s), width=0.1) +
    scale_x_continuous(breaks=0:5) +
    scale_y_continuous(expand=c(0,0), limits=c(0,160)) +
    labs(x="Deaths per corps-year", y="Count")
}



gen_binom <- function(size, prob) {
  tibble(
    x = 0:size,
    y = dbinom(x, size=size, prob=prob)
  )
}


plot_binoms <- function() {
  g1 <- gen_binom(8, 0.5) |> 
    ggplot() +
    theme_clean +
    geom_col(aes(x, y), fill=fill.colour, width=0.7, colour="grey30") +
    scale_x_continuous(breaks=0:8) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x="Number of successes", y="Probability")
  
  
  g2 <- gen_binom(100, 0.5) |> 
    ggplot() +
    theme_clean +
    geom_segment(aes(x=x, xend=x, y=y, yend=0), colour="grey70") +
    geom_point(aes(x=x, y=y), size=2) +
    stat_function(fun=dnorm, args=list(mean=50, sd=sqrt(25)), colour="red") +
    scale_x_continuous(limits=c(35, 65)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x="Number of successes", y="Probability")
  
  mu <- 100 * 0.01
  g3 <- gen_binom(100, 0.01) |> 
    mutate(p = dpois(x, mu)) |> 
    ggplot() +
    theme_clean +
    geom_col(aes(x, p), fill=fill.colour, width=0.7, colour="grey30") +
    geom_segment(aes(x=x, xend=x, y=y, yend=0), colour="grey70") +
    geom_point(aes(x, y), size=2) +
    scale_x_continuous(breaks=0:10, limits=c(-0.5,10)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,0.4)) +
    labs(x="Number of successes", y="Probability")

  list(
    binomial_example = g1,
    binomial_example_normal = g2,
    binomial_example_poisson = g3
  )
}


plot_r_dists <- function() {
  g1 <- plot_fun(dnorm, x.grid=seq(-4, 4, 0.01), cut.up = 1.7) +
    theme_d
  
  g2 <- plot_fun(dnorm, x.grid=seq(-4, 4, 0.01), cut.lo = -qnorm(0.975), cut.up = qnorm(0.975)) +
    theme_d

  d <- tibble(
    k = 0:8,
    p = dbinom(k, 8, 0.5)
  )
  
  g3 <- ggplot(d, aes(x=k, y=p)) +
    theme_classic() +
    theme_d +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    geom_col(fill=fill.colour, colour="black", width=0.6, linewidth=0.2) +
    labs(x=NULL, y=NULL) +
    geom_col(data=d[7:9, ], fill=fill.colour.dark, colour="black", width=0.6, linewidth=0.2)

  g4 <- plot_fun(dt, 4, x.grid=seq(-6, 6, 0.01), cut.up = qt(0.975, 4)) +
    theme_d
  
  g5 <- plot_fun(dt, 4, x.grid=seq(-6, 6, 0.01), cut.lo = -qt(0.975, 4), cut.up = qt(0.975, 4)) +
    theme_d
  
  
  list(
    dnorm = g1,
    dnorm2 = g2,
    dbinom = g3,
    dt = g4,
    dt2 = g5
  )
}



sim_rand <- function(prand) {
  n_sim <- length(prand)
  max_n <- max(prand)
  dst <- rep(0, max_n + 1)
  
  map(1:n_sim, function(i) {
    rnd <- prand[i]
    dst[rnd + 1] <<- dst[rnd + 1] + 1
    tibble(
      i = i,
      k = 0:max_n,
      cumul = dst
    )
  }) |> 
    list_rbind()
}

plot_rand <- function(p, x_lab, y_lab, title) {
  p |>
    ggplot(aes(x = k, y = cumul)) +
    theme_classic() +
    geom_col(fill = fill.colour, colour = "black", width = 0.8) +
    labs(x = x_lab, y = y_lab, title = paste(title, "{frame_time}")) +
    transition_time(i) +
    ease_aes("linear") +
    scale_x_continuous(breaks = 0:max(p$cumul)) +
    scale_y_continuous(limits = c(0, 1.03 * max(p$cumul)), expand = c(0, 0))
}

anim_poisson <- function(mu, n_sim = 1000, seed = 42) {
  set.seed(seed)
  
  rpois(n_sim, mu) |> 
    sim_rand() |>
    plot_rand(x_lab = "k", y_lab = "Frequency", title = "Plate")
}


anim_coins <- function(n, n_sim = 1000, seed = 42) {
  set.seed(seed)
  
  rbinom(n_sim, n, 0.5) |> 
    sim_rand() |>
    plot_rand(x_lab = "Number of heads", y_lab = "Frequency", title = "Toss")
}
