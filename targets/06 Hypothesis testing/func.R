# not used?
generate_mouse_eng_sco <- function(seed = 666) {
  set.seed(seed)
  repeat{
    eng <- round(rnorm(12, 19, 5), 1)
    if(abs(mean(eng) - 19) < 1e-3 & abs(sd(eng) - 5) < 1 & abs(median(eng) - 19) < 0.1) break
  }
  repeat{
    sco <- round(rnorm(9, 24, 5), 1)
    if(abs(mean(sco) - 24) < 1e-3 & abs(sd(sco) - 5) < 1 & abs(median(sco) - 24) < 0.1) break
  }
  save(eng, sco, file = "uk_mice.RData")
}


generate_mouse_delta <- function(seed = 13) {
  set.seed(seed)
    map_dbl(1:1000000, function(i) {
      eng <- rnorm(12, 20, 5)
      sco <- rnorm(9, 20, 5)
      mean(sco) - mean(eng)
    })
}



plot_mouse_dist <- function(d) {
  brks <- seq(-10, 10, 0.1)
  
  ggplot(d, aes(x = m, y = ..density..)) +
    theme_dist +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    geom_histogram(breaks = brks, fill = fill.colour) +
    labs(x = "Body mass difference (g)", y = "Normalized frequency") +
    geom_outline(mouse, brks, size = 0.3)
}


generate_hypergeometric <- function() {
}


plot_hyper <- function() {
  white_drawn <- 0:5
  hyper <- tibble(
    k = factor(white_drawn, levels = white_drawn),
    p = dhyper(white_drawn, 13, 10, 5)
  )
  brks <- as.character(white_drawn)

  hyp <- ggplot(hyper, aes(x = k, y = p)) +
    theme_clean +
    scale_x_discrete(breaks = brks, labels = brks) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    geom_col(fill = fill.colour, colour = "black") +
    labs(x = "Number of white balls drawn", y = "Probability")
  hyp4 <- hyp +
    geom_col(data = hyper[5:6, ], fill = fill.colour.dark, colour = "black")
  hyp1 <- hyp +
    geom_col(data = hyper[c(1:2, 5:6), ], fill = fill.colour.dark, colour = "black")

  list(
    hyper = hyp,
    hyper4 = hyp4,
    hyper1 = hyp1
  )
  
}



plot_tea <- function() {
  tea.first <- 0:4
  tea <- tibble(
    k = factor(tea.first, levels = tea.first),
    p = dhyper(tea.first, 4, 4, 4)
  )
  
  brks <- as.character(tea.first)
  ggplot(tea, aes(x = k, y = p)) +
    theme_clean +
    scale_x_discrete(breaks = brks, labels = brks) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    geom_col(fill = fill.colour, colour = "black") +
    labs(x = "Number of correct answers", y = "Probability") +
    geom_col(data = tea[4:5, ], fill = fill.colour.dark, colour = "black")
}


plot_fisher <- function(d) {
  ggplot(d, aes(x = col, y = cnt, group = row, fill = row)) +
    theme_clean +
    #theme(panel.grid = element_blank()) +
    geom_col(position = position_dodge(), colour = "black") +
    scale_fill_manual(values = c(fill.colour, fill.colour.dark)) +
    scale_y_continuous(expand = c(0,0), limits = c(0, max(d$cnt * 1.03))) +
    labs(x = NULL, y = "Count", fill = "Outcome")
} 


plot_tea_fisher <- function() {
  d1 <- tibble(
    col = c("Tea first", "Milk first", "Tea first", "Milk first") |> as_factor(),
    row = c("Says 'tea'", "Says 'tea'", "Says 'milk'", "Says 'milk'") |> as_factor(),
    cnt = c(4, 5, 2, 1) 
  )
  g1 <- plot_fisher(d1) 
  
  d2 <- tibble(
    col = c("Tea first", "Milk first", "Tea first", "Milk first") |> as_factor(),
    row = c("Says 'tea'", "Says 'tea'", "Says 'milk'", "Says 'milk'") |> as_factor(),
    cnt = c(5, 2, 0, 5) 
  )
  g2 <- plot_fisher(d2) 
  
  list(
    fisher_bristol_1 = g1,
    fisher_bristol_2 = g2
  )
  
}
