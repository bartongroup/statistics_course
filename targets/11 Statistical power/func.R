cohens_d <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  M1 <- mean(x1)
  M2 <- mean(x2)
  S1 <- sd(x1)
  S2 <- sd(x2)
  S12 <- sqrt(((n1 - 1) * S1^2 + (n2 - 1) * S2^2) / (n1 + n2 - 2))
  d <- abs(M2 - M1) / S12
}

generate_cohens_d <- function(M, S, n, d, eps = 1e-4, epsM = 0.1, epsS = 0.1, restrict.M2 = FALSE) {
  M1 <- M
  M2 <- M1 + d * S
  repeat {
    repeat {
      x1 <- rnorm(n, M1, S)
      if(abs(mean(x1) - M1) < epsM && abs(sd(x1) - S) < epsS) break
    }
    if(restrict.M2) {
      repeat {
        x2 <- rnorm(n, M2, S)
        if(abs(mean(x2) - M2) < epsM) break
      }
    } else {
      x2 <- rnorm(n, M2, S)
    }
    dr <- cohens_d(x1, x2)
    if(abs(d - dr) < eps) break
  }
  tibble(
    Country = c(rep("English", n), rep("Scottish", n)),
    Mass = c(x1, x2)
  )
}


generate_fold_change <- function(M, S, n, FC, epsM = 0.1, epsS = 0.1) {
  M1 <- M
  M2 <- M1 * FC
  repeat {
    x1 <- rnorm(n, M1, S)
    if(abs(mean(x1) - M1) < epsM) break
  }
  repeat {
    x2 <- rnorm(n, M2, S)
    if(abs(mean(x2) - M2) < epsM) break
  }
  tibble(
    Country = c(rep("English", n), rep("Scottish", n)),
    Mass = c(x1, x2)
  )
}


plot_cohens_d_example <- function(seed = 123456) {
  set.seed(seed)
  generate_cohens_d(20, 5, 12, 1.1) |> 
    plot_mice_box(with.boxes = FALSE, with.means = TRUE, cex = 3)
}


make_cohen_sizes <- function(seed = 999, n = 10, M = 20, S = 5) {
  set.seed(seed)
  cohens.sizes <- tibble(
    size = c("Very small", "Small", "Medium", "Large", "Very large", "Huge"),
    d = c(0.01, 0.2, 0.5, 0.8, 1.2, 2)
  )
  map_dfr(1:nrow(cohens.sizes), function(i) {
    r <- cohens.sizes[i, ]
    generate_cohens_d(M, S, n, r$d) |> 
      mutate(Size = r$size)
  }) |> 
    mutate(Size = factor(Size, levels = cohens.sizes$size))
}


plot_cohens_sizes <- function(cs) {
  gs <- cs |> 
    group_split(Size) |> 
    map(function(d) {
      s <- first(d$Size)
      lm <- ifelse(s == "Very small", 0, -3)
      plot_mice_box(d, cex = 1.5, size = 1.5, with.boxes = FALSE, with.means = TRUE) + 
        labs(y = NULL, title = s) +
        ylim(min(cs$Mass), max(cs$Mass)) +
        theme(
          legend.position = "none",
          plot.margin = margin(l = lm, t = 0, b = 0, r = 0),
          panel.spacing = unit(0, "cm"),
          panel.border = element_rect(fill = NA),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5)
        )
    })
  plot_grid(plotlist = gs, nrow = 1)
}

fold_change_fig <- function(M, S, n, FC = 2) {
  fc <- generate_fold_change(M, S, n, FC)
  d <- cohens_d(filter(fc, Country == "English")$Mass, filter(fc, Country == "Scottish")$Mass) |> 
    signif(2)
  plot_mice_box(fc, cex = 2.5, size = 1.5, with.boxes = FALSE, with.means = TRUE) +
    ylim(0,50) +
    annotate("text", x = 0.6, y = 50, hjust = 0, label = glue::glue("d = {d}")) 
}

sample_size_fig <- function(n, d, M, S) {
  dat <- generate_cohens_d(M, S, n, d)
  tst <- t.test(Mass ~ Country, data = dat)
  t <- abs(tst$statistic) |> as.numeric() |> signif(2)
  p <- tst$p.value |> as.numeric() |> signif(1) |> format(scientific = FALSE)
  plot_mice_box(dat, size = 1.1, cex = 1.5, with.boxes = FALSE, with.means = TRUE) +
    ylim(0,50) +
    annotate("text", x = 0.6, y = 50, hjust = 0, label = glue::glue("n = {n}")) +
    annotate("text", x = 0.6, y = 46, hjust = 0, label = glue::glue("t = {t}")) +
    annotate("text", x = 0.6, y = 42, hjust = 0, label = glue::glue("p = {p}"))
}

plot_cohens_fold_change <- function(seed = 23, n = 10, M = 20) {
  set.seed(seed)
  g1 <- fold_change_fig(M, 15, n)
  g2 <- fold_change_fig(M, 5, n)
  g3 <- fold_change_fig(M, 1, n)
  plot_grid(g1, g2, g3, ncol = 3)
}

plot_cohens_sample_size <- function(seed = 77, S = 5, M = 20) {
  set.seed(seed)
  g1 <- sample_size_fig(5, 0.8, M, S)
  g2 <- sample_size_fig(20, 0.8, M, S)
  g3 <- sample_size_fig(50, 0.8, M, S)
  plot_grid(g1, g2, g3, ncol = 3)
}


aov_results <- function(dat) {
  fit <- aov(Mass ~ Country, data = dat)
  res <- summary(fit)
  MS.w <- res[[1]]$`Mean Sq`[2]
  MS.b <- res[[1]]$`Mean Sq`[1]
  p <- res[[1]]$`Pr(>F)`[1]
  F <- res[[1]]$`F value`[1]
  list(MS.w = MS.w, MS.b = MS.b, F = F, p = p)
}

generate_ANOVA <- function(f = 1, M = c(20, 20, 20, 30), S = 5, n = 8, seed = 42, eps = 1e-3) {
  set.seed(seed)
  found <- FALSE
  while(!found) {
    d <- map_dfr(1:4, function(i) {
      tibble(
        Country = COUNTRIES[i],
        Mass = rnorm(n, M[i], S)
      )
    }) |> 
      mutate(Country = factor(Country, levels = COUNTRIES))
    av <- aov_results(d)
    f.obs <- sqrt((av$F - 1) / n)
    found <- abs(f - f.obs) < eps
  }
  d
}


plot_anova_f <- function() {
  d1 <- generate_ANOVA(M = c(20, 20, 20, 30), seed = 67)
  d2 <- generate_ANOVA(M = c(15, 25, 15, 25), seed = 34)
  
  d <- rbind(d1, d2)
  y1 <- min(d$Mass)
  y2 <- max(d$Mass)
  
  g1 <- plot_mice_box(d1, size = 1.5, cex = 1.5, with.boxes = FALSE, with.means = TRUE) + ylim(y1, y2)
  g2 <- plot_mice_box(d2, size = 1.5, cex = 1.5, with.boxes = FALSE, with.means = TRUE) + ylim(y1, y2)
  
  plot_grid(g1, g2, nrow = 1, scale = 0.9)
}


generate_t <- function(M1 = 20, M2 = 20, S = 5, n = 5, nsim = 100000) {
  map_dbl(1:nsim, function(i) {
    x1 <- rnorm(n, M1, S)
    x2 <- rnorm(n, M2, S)
    test <- t.test(x1, x2, var.equal = TRUE)
    t <- test$statistic[['t']]
  })
}

generate_t_H0_H1 <- function(seed = 331, n = 5) {
  set.seed(seed)
  list(
    H0 = generate_t(20, 20, n = n),
    H1 = generate_t(30, 20, n = n)
  )
}


generate_a <- function(M = c(20, 20, 20, 20), S = 5, n = 8, nsim = 100000) {
  map_dbl(1:nsim, function(i) {
    d <- map_dfr(1:4, function(i) {
      tibble(
        Country = COUNTRIES[i],
        Mass = rnorm(n, M[i], S)
      )
    })
    av <- aov_results(d)
    F <- av$F 
  })
}

generate_a_H0_H1 <- function(seed = 331, n = 5) {
  set.seed(seed)
  list(
    H0 = generate_a(c(20, 20, 20, 20), n = n),
    H1 = generate_a(c(20, 20, 25, 25), n = n)
  )
}



plot_t_h0_h1 <- function(ts, n = 5) {
  t0 <- qt(0.975, 2 * n - 2)
  
  g1 <- plot_distribution_cut(ts$H0, cut = NULL, brks = seq(-5, 10, 0.1), fill = fill.colour)
  g1.cut <- plot_distribution_cut(ts$H0, cut = t0, side = "both", brks = seq(-5, 10, 0.1), fill = fill.colour) +
    geom_vline(xintercept = -t0) +
    geom_vline(xintercept = t0)

  g2 <- plot_distribution_cut(ts$H1, cut = NULL, brks = seq(-5, 10, 0.1), fill = fill.colour)
  g2.cut <- plot_distribution_cut(ts$H1, cut = t0, side = "lower", brks = seq(-5, 10, 0.1), fill = fill.colour) +
    geom_vline(xintercept = -t0) +
    geom_vline(xintercept = t0)
  
  list(
    t_pow = plot_grid(g1, g2, ncol = 1, scale = 0.95),
    t_pow_cut = plot_grid(g1.cut, g2.cut, ncol = 1, scale = 0.95)
  )
}


plot_anova_h0_h1 <- function(as) {
  f0 <- quantile(as$H0, 0.95)
  
  g1.cut <- plot_distribution_cut(as$H0, cut = f0, side = "upper", brks = seq(0, 15, 0.1), fill = fill.colour) +
    scale_x_continuous(expand = c(0,0), limits = c(0,15)) +
    geom_vline(xintercept = f0)
  
  g2.cut <- plot_distribution_cut(as$H1, cut = f0, side = "lower", brks = seq(0, 15, 0.1), fill = fill.colour) +
    scale_x_continuous(expand = c(0,0), limits = c(0,15)) +
    geom_vline(xintercept = f0)

  plot_grid(g1.cut, g2.cut, ncol = 1, scale = 0.95)

}


# non-centrality parameter for populations
ncp <- function(dM, S, n) {
  d <- dM / S   # effect size for populations
  ncp <- d * sqrt(n / 2)
}


plot_two_t <- function(M1, M2, S = 5, n = 5) {
  tcut <- qt(0.975, 2 * n - 2)
  np <- ncp(M2 - M1, S, n)
  x <- seq(-5, 10, 0.01)
  t0 <- dt(x, 2*n - 2)
  t1 <- dt(x, 2*n-2, ncp = np)
  dat <- tibble(
    x = x,
    t0 = t0,
    t1 = t1
  )
  
  x.cut <- seq(tcut, 10, 0.01)
  t1.cut <- dt(x.cut, 2*n-2, ncp = np)
  dat.cut <- tibble(
    x = c(tcut, x.cut, 10),
    t1 = c(0, t1.cut, 0)
  )
  
  mx <- max(dat$t0) * 1.02
  
  ggplot() +
    theme_classic() +
    geom_polygon(data = dat.cut, aes(x, t1), fill = fill.colour.mid) +
    geom_line(data = dat, aes(x, t1)) +
    geom_line(data = dat, aes(x, t0)) +
    geom_vline(xintercept = tcut) +
    geom_vline(xintercept = -tcut) +
    scale_y_continuous(expand = c(0,0), limits = c(0, mx)) +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(x = NULL)
}


power_t_test <- function(dM, S, n) {
  map_dfr(dM, function(dm) {
    np <- ncp(dm, S, n)
    tcut <- qt(0.975, 2 * n - 2)
    p <- 1 - pt(tcut, 2*n - 2, np)
    d <- dm / S
    tibble(
      d = d,
      power = p
    )
  })
}


plot_power_t_test <- function(M, S, n) {
  gs <- map(M, function(m) {
    plot_two_t(20, m, S = S, n = n)
  })
  plot_grid(plotlist = gs, ncol = 1)
}

t_power_curve <- function(ns = seq(2,10,1), deltas = seq(-10, 10, 0.02)) {
  expand_grid(n = ns, delta= deltas) |> 
    rowwise() |> 
    mutate(power = power.t.test(n = n, delta = delta)$power) |> 
    mutate(n  = factor(n, levels = ns))
}

t_size_curve <- function(ns = seq(2,50,1), powers = c(0.6, 0.8, 0.95)) {
  expand_grid(n = ns, power = powers) |> 
    rowwise() |> 
    mutate(delta = power.t.test(n = n, power = power, sd = 1, alternative = "two.sided")$delta) |> 
    mutate(power = factor(power, levels = powers))
}

anova_power_curve <- function(ns = seq(2,10,1), fs = seq(0, 5, 0.01), k = 4) {
  expand_grid(n = ns, delta = fs) |> 
    rowwise() |> 
    mutate(power = pwr.anova.test(n = n, k = k, f = delta)$power) |> 
    mutate(n  = factor(n, levels = ns))
}

anova_size_curve <- function(ns = seq(2,50,1), powers = c(0.6, 0.8, 0.95), k = 4) {
  expand_grid(n = ns, power = powers) |> 
    rowwise() |> 
    mutate(delta = pwr.anova.test(n = n, k = k, power = power)$f) |> 
    mutate(power = factor(power, levels = powers))
}


plot_power_curves <- function(pow, xlim = c(-6, 6), with.hline = TRUE, x.breaks = waiver()) {
  g <- ggplot(pow, aes(delta, power, group = n)) +
    theme_clean +
    geom_line(aes(colour = n), linewidth = 0.7) +
    labs(x = "Effect size", y = "Power") +
    scale_x_continuous(limits = xlim, expand = c(0,0), breaks = x.breaks) +
    scale_colour_manual(values = okabe_ito_palette) +
    scale_y_continuous(breaks = seq(0,1,0.2), expand = c(0,0), limits = c(0, 1.05)) +
    theme(legend.position = "none")
  if(with.hline) g <- g + geom_hline(yintercept = 0.8, linetype = "dashed")
  g
}

plot_power_size <- function(pow) {
  g <- ggplot(pow, aes(n, delta, group = power)) +
    theme_clean +
    geom_line(aes(colour = power)) +
    labs(x = "Sample size", y = "Effect size") +
    scale_colour_manual(values = okabe_ito_palette) +
    theme(legend.position = "none") +
    scale_x_log10(breaks = c(2,5,10,50))
  g
}


plot_t_power_curves <- function() {
  pow5 <- t_power_curve(n = 5)
  pow <- t_power_curve(n = c(2, 3, 5, 10, 30))
  spow <- t_size_curve(powers = c(0.6, 0.8, 0.95))

  g1 <- plot_power_curves(pow5, with.hline = FALSE, xlim = c(-6,6), x.breaks = seq(-6,6,2))
  g2 <- plot_power_curves(pow, xlim = c(-6,6), x.breaks = seq(-6,6,2))
  g3 <- plot_power_size(spow)
  
  list(
    t_power_curve_5 = g1,
    t_power_curves = g2,
    t_size_curves = g3
  )
}



plot_anova_power_curves <- function() {
  pow <- anova_power_curve(ns = c(2, 3, 5, 10, 30))
  spow <- anova_size_curve(powers = c(0.6, 0.8, 0.95))

  g1 <- plot_power_curves(pow, xlim = c(0,3), x.breaks = 0:3)
  g2 <- plot_power_size(spow)
  
  list(
    anova_power_curves = g1,
    anova_size_curves = g2
  )
}


generate_worked_example <- function(seed = 1002) {
  set.seed(seed)
  
  n <- 6
  name <- c("WT", "KO1", "KO2", "KO3", "KO4")
  M <- c(400, 400, 360, 200, 410)
  S <- c(150, 150, 100, 140, 100)
  map_dfr(1:length(name), function(i) {
    repeat {
      x <- rnorm(n, M[i], S[i])
      if(all(x > 0)) break
    }
    tibble(
      Group = name[i],
      Volume = x
    )
  }) |> 
    mutate(Group = factor(Group, levels = name))
}


plot_tumour_example <- function(tumour) {
  sdt <- tumour |>
    group_by(Group) |>
    summarise(M = mean(Volume), sd = sd(Volume), n = n()) |>
    mutate(sde = sd / sqrt(2*(n-1)))
  
  g1 <- ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    #geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    scale_x_discrete() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 700)) +
    geom_vline(xintercept = 1:5, colour = "grey90", linewidth = 0.3) +
    geom_boxplot(data = sdt, aes(x = Group, middle = M, ymin = M, lower = M, upper = M, ymax = M), stat = "identity", width = 0.7, lwd = 0.6, fatten = 0, colour = "grey40") +
    scale_fill_manual(values = okabe_ito_palette) +
    geom_beeswarm(data = tumour, aes(x = Group, y = Volume, fill = Group), shape = 21, cex = 2.5, size = 1.5) +
    #geom_jitter(width = 0.15, height = 0) +
    labs(x = "", y = expression(Volume~(mm^3)))

  g2 <- ggplot(sdt, aes(Group, sd)) +
    theme_clean +
    geom_point() +
    geom_errorbar(aes(x = Group, ymin = sd-sde, ymax = sd+sde), width = 0.2) +
    geom_hline(yintercept = c(75, 160), linetype = "dotted") +
    labs(x = "", y = expression(Standard~deviation~(mm^3)))
  
  pow <- t_power_curve(n = c(2, 3, 4, 5, 6, 10))
  g3 <- plot_power_curves(pow, xlim = c(0, 6)) + geom_vline(xintercept = c(1.25, 2.7), linetype = "dotted")

  
  tum.aov = aov(Volume ~ Group, data = tumour)
  # Extract F value
  F = summary(tum.aov)[[1]]$F[1]
  # Effect size: Cohen's f
  f = sqrt((F - 1)/6)
  
  pow <- anova_power_curve(n = c(2, 3, 4, 5, 6, 10), k = 5)
  g4 <- plot_power_curves(pow, xlim = c(0,2)) + geom_vline(xintercept = f, linetype = "dotted")

  list(
    tumour_data = g1,
    tumour_sd = g2,
    tumour_power = g3,
    tumour_anova = g4
  )
  
}
