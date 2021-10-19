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
  
  list(normal_sigmas_1=g1, normal_sigmas_2=g2)
}


plot_baseball_normal <- function(baseball) {
  baseball <- baseball %>% 
    mutate(height = 2.54 * `Height(inches)`)
  dst <-  baseball %>% 
    group_by(height) %>% 
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
    theme_bw() +
    theme(panel.grid = element_blank()) +
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
    d <- d %>% mutate(x = log10(x), y = log10(y))
  } else {
    xlab <- expression(I[1])
    ylab <- expression(I[2])
  }
  
  ggplot(d, aes(x=x, y=y)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_point(size = 0.4, alpha = 0.4) +
    labs(x=xlab, y=ylab)
}

plot_replicates_loglin <- function(d, reps) {
  d <- d %>% 
    select(all_of(reps)) %>% 
    set_names(c("x", "y"))
  list(
    lin = plot_xy(d, log.scale = FALSE),
    log = plot_xy(d, log.scale = TRUE)
  )
}