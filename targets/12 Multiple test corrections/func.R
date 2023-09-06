plot_fwer <- function(max_k = 30, alpha = 0.05) {
  k <- 1:max_k
  dat <- tibble(
    k = k,
    pm = 1 - (1 - alpha)^k,
    pmc = 1 - (1 - alpha/k)^k
  )
  
  g0 <- ggplot(dat) +
    theme_clean +
    geom_point(aes(k, pm), fill = "red", colour = "black", shape = 21, size = 2) +
    labs(x = "m", y = "FWER") +
    geom_hline(yintercept = alpha) +
    scale_x_continuous(expand = c(0,0), limits = c(0, 31), breaks = c(1,5,10,15,20,25,30)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.8), breaks = seq(0,1,0.2))
  
  p20 <- 1 - (1 - alpha)^20
  seg <- tibble(
    x = c(0, 20, 20),
    y = c(p20, p20, 0)
  )

  g1 <- g0 +
    geom_point(aes(k, pmc), fill = "blue", colour = "black", shape = 21, size = 2)
  
  list(
    fwer = g0,
    fwer_bonferroni = g1
  )
}

generate_two_data <- function(seed = 110, N1 = 970, M1 = 20, N2 = 30, M2 = 40, n = 5, S = 5) {
  set.seed(110)
  h0 <- rnorm(n * N1, M1, S)
  h1 <- rnorm(n * N2, M2, S)
  tibble(
    samp = rep(seq(1, N1 + N2), each = n),
    value = c(h0, h1),
    hypothesis = c(rep(0, length(h0)), rep(1, length(h1))) |> factor(levels = c(1, 0))
  )
}


two_data_tests <- function(td, mu = 20, n_tests = 1000, seed = 321) {
  set.seed(seed)
  N <- max(td$samp)
  map_dfr(1:n_tests, function(i) {
    sel <- td |> filter(samp == sample(1:N, 1))
    x <- sel$value
    hyp <- sel$hypothesis[1]
    tt <- t.test(x, mu = mu)
    tibble(hypothesis = hyp, p_value = tt$p.value)
  })
}

read_two_p <- function(file) {
  read_tsv(file) |>
    mutate(hypothesis = factor(hypothesis, levels = c(1,0))) |> 
    mutate(padj = p.adjust(p_value, method = "BH"))
}


plot_two_dist <- function(td) {
  brks <- seq(0, 60, 1)
  ggplot(td, aes(x = value)) +
    theme_dist +
    geom_histogram(aes(fill = hypothesis), breaks = brks, position = "identity") +
    scale_fill_manual(values = c(fill.colour.dark, fill.colour.mid)) +
    labs(x = "Body mass (g)", y = "Frequency") +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0))
}

plot_two_dist_p <- function(pd) {
  brks <- seq(0, 1, 0.025)
  ggplot(pd, aes(x = p_value)) +
    theme_dist +
    geom_histogram(aes(fill = hypothesis), breaks = brks, position = "stack") +
    scale_fill_manual(values = c(fill.colour.dark, fill.colour.mid)) +
    labs(x = "p-value", y = "Frequency") +
    theme(legend.position = "none") +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)) +
    geom_vline(xintercept = 0.05, linetype = "dotted")
}


p_table <- function(dat, alpha = 0.05, what = "p_value") {
  dat <- dat |> 
    mutate(p = get(what))
  FP <- sum(dat$hypothesis == 0 & dat$p <= alpha)
  TP <- sum(dat$hypothesis == 1 & dat$p <= alpha)
  TN <- sum(dat$hypothesis == 0 & dat$p > alpha)
  FN <- sum(dat$hypothesis == 1 & dat$p > alpha)
  m0 = FP+TN
  m1 = FN+TP
  list(FP = FP, TP = TP, TN = TN, FN = FN, P = FP+TP, N = TN+FN, m0 = m0, m1 = m1, tot = m0+m1, FPR = FP/m0, FNR = FN/m1, FDR = FP/(FP+TP))
}


make_small_p_data <- function() {
  m <- 5
  k <- 1:m
  alpha <- 0.05
  sp <- tibble(
    k = k,
    p = c(0.003, 0.005, 0.012, 0.038, 0.058),
    alpha = alpha,
    alpha_b = alpha / m,
    alpha_hb = alpha / (m - k + 1),
    alpha_bh = k * alpha / m
  )
}

make_large_p_data <- function(pd, alpha = 0.05) {
  m <- nrow(pd)
  pd |> 
    arrange(p_value) |> 
    mutate(
      k = seq_along(p_value),
      alpha = alpha,
      alpha_b = alpha / m,
      alpha_hb = alpha / (m - k + 1),
      alpha_bh = k * alpha / m
    ) |> 
    mutate(padj = p.adjust(p_value, "BH")
)
}


plot_small_data <- function(sp, alpha = 0.05) {
  m <- nrow(sp)
  sp1 <- rbind(sp, c(6, NA, alpha, alpha, alpha, alpha))
  
  g <- ggplot(sp) +
    theme_clean +
    geom_point(aes(x = k, y = p), shape = 21, fill = "red", colour = "black", size = 2) +
    labs(x = "k", y = "p-value") +
    scale_x_continuous(expand = c(0,0), limits = c(0.5, 5.5)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, 0.06))
  
  g <- g +
    geom_hline(yintercept = alpha, colour = "darkgreen", linetype = "dotted") +
    geom_hline(yintercept = alpha/m, colour = "darkgreen", linetype = "dotted") +
    annotate("text", x = 0.7, y = 0.053, label = "Uncorrected", colour = "darkgreen", size = 3.2, hjust = 0) +
    annotate("text", x = 5.35, y = 0.007, label = "Bonferroni", colour = "darkgreen", size = 3.2, hjust = 1)
  g1 <- g +
    geom_step(data = sp1, aes(x = k-0.5, y = alpha_hb), colour = "blue") +
    annotate("text", x = 3.56, y = 0.022, label = "Holm-Bonferroni", colour = "blue", size = 3.2, hjust = 0)
  
  g2 <- g1 +
    geom_step(data = sp1, aes(x = k-0.5, y = alpha_bh), colour = "red") +
    annotate("text", x = 3.44, y = 0.033, label = "Benjamini-Hochberg", colour = "red", size = 3.2, hjust = 1)
  
  list(
    holm_bonferroni = g1,
    benjamini_hochberg = g2
  )
}


plot_large_data <- function(lp, alpha = 0.05) {
  m <- nrow(lp)
  hb_limit <- lp |> filter(p_value < alpha_hb) |> pull(k) |> max()
  bh_limit <-lp |> filter(p_value < alpha_bh) |> pull(k) |> max()
  
  g <- ggplot(lp) +
    theme_classic() +
    geom_point(aes(x = k, y = log10(p_value)), shape = 21, fill = "grey", colour = "black", size = 2) +
    labs(x = "k", y = "log p-value") +
    scale_x_continuous(expand = c(0,0), limits = c(0.5, 30.5), breaks = c(1,5,10,15,20,25,30)) +
    scale_y_continuous(expand = c(0,0), limits = c(-6, -2))
  
  g1 <- g +
    geom_point(data = lp[1:hb_limit,], aes(x = k, y = log10(p_value)), shape = 21, fill = "red", colour = "black", size = 2) +
    geom_step(aes(x = k-0.5, y = log10(alpha_hb)), colour = "blue", linetype = "dashed", alpha = 0.5) +
    annotate("text", x = 30, y = -4.45, label = "Holm-Bonferroni", colour = "blue", size = 3.2, hjust = 1) +
    annotate("text", x = 30, y = -4.15, label = "Bonferroni", colour = "darkgreen", size = 3.2, hjust = 1) +
    geom_hline(yintercept = log10(alpha/m), colour = "darkgreen", linetype = "solid") +
    geom_vline(xintercept = hb_limit, colour = "red", linetype = "dashed")
  
  g2 <- g1 +
    geom_step(aes(x = k-0.5, y = log10(alpha_bh)), colour = "red", linetype = "solid", alpha = 1) +
    annotate("text", x = 14, y = -3, label = "Benjamini-Hochberg", colour = "red", size = 3.2, hjust = 1) +
    geom_vline(xintercept = bh_limit, colour = "red", linetype = "dashed") +
    geom_point(data = lp[1:bh_limit,], aes(x = k, y = log10(p_value)), shape = 21, fill = "red", colour = "black", size = 2)
  
  g3 <- g +
    geom_point(data = lp[1:bh_limit,], aes(x = k, y = log10(p_value)), shape = 21, fill = "red", colour = "black", size = 2) +
    geom_point(aes(x = k, y = log10(padj)), shape = 21, fill = "grey", colour = "black", size = 2) +
    geom_point(data = lp[1:bh_limit,], aes(x = k, y = log10(padj)), shape = 21, fill = "red", colour = "black", size = 2) +geom_step(aes(x = k-0.5, y = log10(alpha_bh)), colour = "red", linetype = "solid", alpha = 1) +
    #annotate("text", x = 14, y = -3, label = "Benjamini-Hochberg", colour = "red", size = 3.2, hjust = 1) +
    #annotate("text", x = 3, y = -1.1, label = "0.05", colour = "red", size = 3.2, hjust = 0) +
    scale_y_continuous(expand = c(0,0), limits = c(-6,0)) +
    geom_hline(yintercept = log10(0.05), colour = "red")
  
  list(
    large_HB = g1,
    large_BH = g2,
    large_adjusted = g3
  )
}

# old version (slow)
test_matrix <- function(x, mu) {
  sapply(1:nrow(x), function(i) {
    t.test(x[i, ], mu = mu)$p.value
  })
}

# this is really fast (about 200 times faster!)
test_matrix_fast <- function(x, mu) {
  tst <- Rfast::ttest(t(x), mu)
  tst[, 2]
}

generate_test_data <- function(m0, m1, n = 5, M1 = 20, M2 = 40, S = 5) {
  H0 <- matrix(rnorm(n * m0, M1, S), ncol = n)
  H1 <- matrix(rnorm(n * m1, M2, S), ncol = n)
  p <- test_matrix_fast(rbind(H0, H1), M1)
  tibble(
    hypothesis = c(rep(0, m0), rep(1, m1)) |> factor(levels = c(1, 0)),
    p_value = p
  )
}


generate_fdr_distribution <- function(seed = 17, nboot = 10000) {
  set.seed(seed)
  map_dbl(1:nboot, function(i) {
    pd <- generate_test_data(970, 30) |> 
      mutate(padj = p.adjust(p_value, method = "BH"))
    p_table(pd, alpha = 0.05, what = "padj")$FDR
  })
}


plot_fdr_distribution <- function(fdr) {
  d <- tibble(fdr = fdr) |> drop_na()
  brks <- seq(-0.01, 0.3, 0.004999)
  ggplot(d) +
    theme_dist +
    geom_histogram(aes(x = fdr, y = after_stat(density)), fill = fill.colour.mid, breaks = brks) +
    scale_x_continuous() +
    scale_y_continuous(expand = c(0,0), limits = c(0, 80)) +
    geom_outline(fdr, brks) +
    labs(x = "FDR")
}

plot_simple_p_dist <- function(dat, ymax = 5, plim = 0.5, bin.size = 0.02) {
  brks <- seq(0, 1, bin.size)
  n <- nrow(dat)
  x <- brks[-1] - bin.size
  y <- as.numeric(table(cut(dat$p_value, brks, right = TRUE))) / (bin.size * length(dat$p_value))
  y0 <- mean(y[x > plim])
  ggplot(dat) +
    theme_clean +
    geom_histogram(aes(x = p_value, y = (after_stat(count)) / (n * bin.size), fill = hypothesis, colour = hypothesis), breaks = brks) +
    scale_fill_manual(values = c(fill.colour.dark, fill.colour.mid), drop = FALSE) +
    scale_colour_manual(values = c(fill.colour.dark, fill.colour.mid), drop = FALSE) +
    scale_y_continuous(expand = c(0,0), limits = c(0,ymax)) +
    scale_x_continuous(expand = c(0,0), limits = c(0,1), breaks = seq(0,1,0.2)) +
    labs(x = "p-value", y = "density") +
    geom_outline(dat$p_value, brks) +
    geom_hline(yintercept = y0) +
    theme(legend.position = "none")
}


plot_p_distributions <- function(seed = 225) {
  set.seed(seed)
  pnull <- generate_test_data(10000, 0)
  palt <- generate_test_data(8000, 2000, M2 = 25)
  pbad <- tibble(
    p_value = c(runif(6000), 1 - runif(4000)^1.6),
    hypothesis = factor(rep("0", 10000), levels = c("1", "0"))
  )
  palt_one <- palt |> 
    mutate(hypothesis = factor(rep("0", 10000), levels = c("1", "0")))
  
  # nasty, adding TP and FP colours
  p_cut <- 0.1
  bin_size <- 0.02
  brks <- seq(0, p_cut, bin_size)
  palt_1 <- palt |> filter(p_value < p_cut)
  n <- nrow(palt)
  x <- seq(0, 1, bin_size)[-1] - bin_size
  y <- as.numeric(table(cut(palt$p_value, seq(0, 1, bin_size), right = TRUE))) / (bin_size * length(palt$p_value))
  y0 <- mean(y[x > 0.5])
  p_fp <- tibble(
    x = c(0, p_cut, p_cut, 0, 0),
    y = c(0, 0, y0, y0, 0)
  )
  p_dist_alt_p <- plot_simple_p_dist(palt, 3.5) +
    geom_histogram(
      data = palt_1,
      aes(x = p_value, y = after_stat(count) / (n * bin_size)),
      breaks = brks, fill = "deepskyblue2", colour = "deepskyblue2"
    ) +
    geom_outline(palt$p_value, seq(0, 1, bin_size)) +
    geom_polygon(
      data = p_fp,
      aes(x = x, y = y),
      fill = "brown1",
      colour = "black"
    )

  list(
    p_dist_null = plot_simple_p_dist(pnull, 2),
    p_dist_alt = plot_simple_p_dist(palt, 3.5),
    p_dist_bad = plot_simple_p_dist(pbad, 3.5),
    p_dist_alt_one = plot_simple_p_dist(palt_one, 3.5),
    p_dist_alt_p = p_dist_alt_p
  )  
}


plot_pq <- function(dat, xlim = c(0,1), ylim = NULL, x.brks = seq(0,1,0.2), y.brks = seq(0,1,0.2)) {
  dat <- dat |> 
    mutate(q_value = qvalue(p_value)$qv)

  if(is.null(ylim)) {
    mx <- dat |> 
      filter(p_value <= xlim[2]) |> 
      pull(q_value) |> 
      max()
    ylim <- c(0, 1.02 * mx)
  }
  
  ggplot(dat, aes(x = p_value, y = q_value)) +
    theme_clean +
    theme(plot.margin = margin(l = 5, r = 16, t = 10)) +
    geom_line(linewidth = 0.5, colour = "red") +
    scale_x_continuous(expand = c(0,0), limits = xlim, breaks = x.brks) +
    scale_y_continuous(expand = c(0,0), limits = ylim, breaks = y.brks)
}

plot_storey <- function(pd) {
  g0 <- plot_pq(pd, ylim = c(0,1)) +
    labs(x = "P-value limit", y = "False discovery rate") +
    geom_line(aes(x = p_value, y = padj), colour = "blue") +
    geom_hline(yintercept = qvalue(pd$p_value)$pi0, colour = "red", linetype = "dashed")
  
  g1 <- plot_pq(pd, ylim = c(0,1)) +
    labs(x = "P-value limit", y = "Point FDR")
  g2 <- plot_pq(pd, c(0, 0.004), x.brks = seq(0,0.01, 0.002), y.brks = seq(0,0.1,0.05)) +
    labs(x = NULL, y = NULL)
  
  qobj <- qvalue(pd$p_value)
  pl <- hist(qobj)
  
  list(
    pq_bh = g0,
    pq = g1,
    pq_small = g2,
    pq_hist = pl
  )
  
}
