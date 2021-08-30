plot_2dice_binomial <- function() {
  dist <- data.frame(
    x = 2:12,
    n = c(1:6, 5:1)
  )
  dist$y <- dist$n / sum(dist$n)
  
  g <- ggplot(dist, aes(x, y)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
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
    theme_bw() +
    theme(panel.grid = element_blank()) +
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
  
  list(g1=g1, g2=g2)
}