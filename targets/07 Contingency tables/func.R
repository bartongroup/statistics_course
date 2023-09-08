make_pipetting_data <- function() {
  tibble(
    O = c(39, 21, 23, 30, 28),
    E = mean(O),
    S = sqrt(E),
    lo = O - S,
    up = O + S,
    chi =  (O - E) / S,
    plate = seq_along(O),
  )
}


plotPip <- function(x, title = "") {
  df <- data.frame(x = seq_along(x), y = x)
  ggplot(df, aes(x, y)) +
    theme_clean +
    #geom_col(fill = fill.colour.mid, colour = bar.outline, width = 0.8) +
    geom_segment(aes(xend = x, yend = 0), colour = "grey70") +
    geom_point(shape = 21, fill = fill.colour.mid, size = 2) +
    labs(x = "Plate", y = "Colony count", title = title) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df$y * 1.05))) +
    xlim(0.5,5.5) +
    geom_hline(yintercept = mean(df$y), colour = "red", linetype = "dashed", linewidth = 0.3)
}

plot_pip_uni_nonuni <- function() {
list(
  pipetting_uniform = plotPip(c(38, 32, 41, 35, 37), title = "Uniform"),
  pipetting_nonuniform = plotPip(c(64, 41, 12, 28, 19), title = "Non-uniform")
)  
}


plot_pipetting <- function(d) {
  g1 <- ggplot(d, aes(x = plate, y = O)) +
    theme_clean +
    theme(
      plot.margin = margin(b = 0, l = 6, t = 3, r = 5),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    geom_hline(yintercept = mean(d$O), colour = "red", linetype = "dashed", linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
    geom_point(shape = 21, size = 2, fill = "white") +
    scale_x_continuous(expand = c(0,0), limits = c(0.7, 5.3)) +
    scale_y_continuous(limits = c(13,47)) +
    labs(x = NULL, y = "Count")
  
  g2 <- ggplot(d, aes(x = plate, y = (O - E) / S)) +
    theme_clean +
    theme(
      plot.margin = margin(t = -2.5, l = 6, r = 5)
    ) +
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = 0.3) +
    geom_segment(aes(xend = plate, yend = 0), colour = "grey70") +
    geom_point(shape = 21, size = 2, fill = "white") +
    scale_x_continuous(expand = c(0,0), limits = c(0.7, 5.3)) +
    scale_y_continuous(expand = c(0,0), limits = c(-2.7, 2.7), breaks = c(-2,0,2)) +
    labs(x = "Plate", y = "Residual")
  
  plot_grid(g1, g2, align = "v", ncol = 1, rel_heights = c(2, 1))
}


nunif <- function(nbin, ntot) {
  n <- floor(runif(ntot, min = 1, max = nbin+1-1e-16))
  as.integer(table(n))
}

make_pipetting_batch <- function(nbin, ntot, nrow) {
  ex <- rep(ntot/nbin, nbin)
  d <- matrix(nrow = nrow, ncol = nbin)
  colnames(d) <- paste0("bin_", 1:nbin)
  for(i in 1:nrow) {
    r <- nunif(nbin, ntot)
    d[i, ] <- (r - ex) / sqrt(ex)
  }
  as_tibble(d)
} 



generate_pipetting <- function(pd, nsim = 10000, nbatch = 10) {
  nbin <- nrow(pd)
  ntot <- sum(pd$O)
  map_dfr(1:nbatch, function(i) {
    make_pipetting_batch(nbin, ntot, nsim)
  }) |> 
    mutate(id = row_number()) |> 
    pivot_longer(-id, names_to = "bin", values_to = "chi")
}


plot_pipetting_sim <- function(ps) {
  pexp <- tibble(
    x = seq(-4, 4, 0.05),
    y = dnorm(x)
  )
  
  d <- ps |> 
    group_by(bin, chi) |>
    tally()
  binsize <- d[2, ]$chi - d[1, ]$chi
    
  d |> 
    mutate(dens = n / (sum(n) * binsize)) |> 
  ggplot(aes(x = chi, y = dens)) +
    theme_dist +
    geom_col(fill = fill.colour.mid, colour = fill.colour.mid) +
    facet_grid(. ~ bin) +
    geom_line(data = pexp, aes(x, y), colour = "blue") +
    labs(x = expression(chi), y = "Density") +
    theme(strip.background = element_blank(), strip.text = element_blank()) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(limits = c(-3, 3)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.46))
}


plot_chisq_dist <- function(dofs = c(3,4,5,8,10)) {
  dc <- map_dfr(dofs, function(dof) {
    tibble(
      dof = dof,
      x = seq(0, 30, 0.1),
      y = dchisq(x, dof)  
    )
  }) |> 
    mutate(dof = as_factor(dof))
  
  ggplot(dc, aes(x = x, y = y, group = dof, colour = dof)) +
    theme_clean +
    geom_line() +
    scale_colour_manual(values = okabe_ito_palette) +
    labs(x = expression(chi^2), y = "Density") +
    scale_x_continuous(limits = c(0, 20), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
}

plot_pipetting_chisq_dist <- function(ps) {
  df <- ps |> 
    group_by(id) |> 
    summarise(chisq = sum(chi^2))
  dof <- ps$bin |> unique() |> length() - 1
  
  dchi2 <- tibble(
    x = seq(0, 30, 0.1),
    y = dchisq(x, dof)
  )
  
  brks <- seq(0, 20, 0.5)
  g1 <- ggplot(df, aes(x = chisq, y = after_stat(density))) +
    theme_dist +
    geom_histogram(breaks = brks, fill = fill.colour.mid) +
    labs(x = expression(chi^2), y = "Density") +
    scale_x_continuous(limits = c(0, 20), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  g2 <- g1 + geom_line(data = dchi2, aes(x, y), colour = "blue")
  list(g1 = g1, g2 = g2)
}


plot_chisq_cut <- function(pd) {
  chi2pip <- chisq.test(pd$O)$statistic
  plot_fun(dchisq, df = nrow(pd)-1, x.grid = seq(0, 20, 0.1), cut.up = chi2pip, name = expression(chi^2))
}


make_geissler_data <- function() {
  geissler <- tibble(
    girls = 0:12,
    boys = 12:0,
    count = c(7, 45, 181, 478, 829, 1112, 1343, 1033, 670, 286, 104, 24, 3)
  )
}



geisslerPlot <- function(geissler, with.binomial = FALSE) {
  tot <- sum(geissler$count)
  n <- max(geissler$girls)
  p <- sum(geissler$girls * geissler$count) / (n * tot)
  geissler <- geissler |> 
    mutate(
      binom = dbinom(0:n, n, p) * tot,
      chi = (count - binom) / sqrt(binom)
    )
  
  g <- ggplot()

  # binomial has to go at the bottom
  if(with.binomial) {
    g <- g + geom_col(data = geissler, aes(x = girls, y = binom), fill = fill.colour.mid, colour = bar.outline, width = 0.8)
  }
  
  # main plot
  g <- g +
    theme_clean +
    theme(axis.text = element_text(size = 12)) +
    geom_segment(data = geissler, aes(x = girls, xend = girls, y = 0, yend = count), colour = bar.outline) +
    geom_point(data = geissler, aes(x = girls, y = count)) +
    labs(x = "Girls", y = "Count") +
    scale_x_continuous(breaks = 0:n, labels = as.character(0:n), limits = c(-0.1, 12.1)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(geissler$count) * 1.05), breaks = c(500,1000))
  
  # residual plot
  if(with.binomial) {
    g <- g + 
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(l = 3, r = 2, b = 0)
      )
    g1 <- ggplot(geissler) +
      theme_clean +
      theme(axis.text = element_text(size = 12)) +
      geom_segment(aes(x = girls, xend = girls, y = 0, yend = chi), colour = bar.outline) +
      geom_point(data = geissler, aes(x = girls, y = chi)) +
      labs(x = "Girls", y = expression(chi)) +
      scale_x_continuous(breaks = 0:n, labels = as.character(0:n), limits = c(-0.1, 12.1)) +
      scale_y_continuous(limits = c(-7,7), breaks = c(-5,0,5)) +
      theme(plot.margin = margin(t = -2.5)) +
      geom_hline(yintercept = 0, colour = "grey60")
    return(plot_grid(g, g1, ncol = 1, align = "v", rel_heights = c(3, 1)))
  } else {
    return(g)
  }
}

plot_geissler <- function(geissler) {
  list(
    geissler = geisslerPlot(geissler),
    geissler_biom = geisslerPlot(geissler, with.binomial = TRUE)
  ) 
}


plot_binomial_example <- function() {
  df <- tibble(
    x = 0:8,
    y = dbinom(x, size = 8, prob = 0.5)
  )
  
  ggplot(df, aes(x, y)) +
    theme_dist +
    geom_col(fill = fill.colour.mid, colour = bar.outline, width = 0.8) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(df$y) * 1.05)) +
    labs(x = "Number of heads", y = "Probability")
}


make_flow_cytometry_data <- function() {
  X <- matrix(c(50, 61, 78, 43, 172, 175, 162, 178, 55, 45, 47, 59), ncol = 4, byrow = TRUE)
  colnames(X) <- c("WT", "KO1", "KO2", "KO3")
  rownames(X) <- c("G1", "S", "G2")
  X
}


plot_flow_cytometry <- function(fc) {
  d <- fc |> 
    as_tibble(rownames = "cell_cycle") |> 
    pivot_longer(-cell_cycle, names_to = "condition", values_to ="count") |> 
    mutate(
      condition = as_factor(condition),
      cell_cycle = as_factor(cell_cycle)
    ) |> 
    group_by(condition) |> 
    mutate(n = sum(count)) |> 
    ungroup() |> 
    nest(data = c(count, n)) |> 
    mutate(
      fit = map(data, ~prop.test(.x$count, .x$n, conf.level = 0.95)),
      tid = map(fit, broom::tidy)
    ) |> 
    unnest(tid)

  txt <- tibble(
    condition = rep("WT", 3),
    cell_cycle = c("G1", "S", "G2"),
    estimate = rep(0.03, 3)
  ) |> 
    mutate(cell_cycle = as_factor(cell_cycle))
  
  ggplot(d, aes(x = condition, y = estimate, group = cell_cycle)) +
    theme_clean +
    geom_col(aes(fill = cell_cycle), colour = bar.outline, position = "dodge") +
    geom_errorbar(aes(x = condition, ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.9)) +
    scale_fill_manual(values = okabe_ito_palette) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x = "Condition", y = "Proportion", fill = "Cell cycle") +
    theme(legend.position = "none") +
    geom_text(data = txt, aes(label = cell_cycle), position = position_dodge(0.9), size = 3.5)
}
