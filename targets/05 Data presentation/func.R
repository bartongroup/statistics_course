gp_model <- function(x, A=1, tau=16) {
  y <- A * exp(-x / tau)
}

good_plot <- function(seed=344) {
  set.seed(seed)
  x <- c(0.5, 1, 2, 4, 8, 12, 16, 24, 48)
  m <- gp_model(x)
  
  d <- map_dfr(m, function(M) {
    s <- rnorm(12, M, 0.15)
    c(mean=mean(s), se=sd(s) / sqrt(length(s)))
  }) %>% 
    mutate(
      x = x,
      ww = 1 / se^2,
      w = ww / sum(ww)
    ) 
  
  fit <- nls(mean ~ A * exp(-x / tau), data=d, weights=d$w, start=c(A=1, tau=10))
  cf <- summary(fit)$coefficients[,1:2]
  cf
  cf[, 2] * qnorm(0.975)
  
  dm <- tibble(
    x = seq(0, 50, 0.1),
    y = cf[1, 1] * exp(-x / cf[2, 1])
  )
  
  
  ggplot(d) +
    theme_clean +
    geom_line(data=dm, aes(x, y), colour="darkolivegreen4") +
    geom_errorbar(aes(x=x, ymin=mean-se, ymax=mean+se), width=0.2) +
    geom_point(aes(x=x, y=mean)) +
    labs(x= "Time (h)", y="Relative abundance") +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$mean + d$se) * 1.03))
}


show_your_data <- function() {
  set.seed(101)
  n1 <- 30
  n2 <- 35
  d <- tibble(
    type = c(rep("WT", n1), rep("KO", n2)),
    value = c(rnorm(n1, 20, 5), rnorm(n2, 25, 7))
  ) %>% 
    mutate(type = fct_relevel(type, "WT"))

  dm <- d %>%
    group_by(type) %>%
    summarise(M = mean(value), SE = sd(value) / sqrt(n()))
  
  g0 <- ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    scale_fill_manual(values=okabe_ito_palette) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(d$value) * 1.03)) +
    labs(x=NULL, y=NULL)
  
  g1 <- g0 +
    geom_errorbar(data=dm, aes(x=type, ymin=M-SE, ymax=M+SE), width=0.3, colour="black") +
    geom_col(data=dm, aes(x=type, y=M)) +
    labs(y="Mass (g)")
  
  g2 <- g0 +
    geom_boxplot(data=d, aes(x=type, y=value, fill=type))
  
  g3 <- g0 +
    geom_jitter(data=d, aes(x=type, y=value, fill=type), shape=21, width=0.2, height=0)
  
  
  g4 <- g0 +
    geom_beeswarm(data=d, aes(x=type, y=value, fill=type), shape=21, cex=2.5, priority = "density") 

  plot_grid(g1, g2, g3, g4, ncol=4, align = "v")
}


emptyBox <- function(ann="") {
  ggplot() +
    theme_classic() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.background = element_rect(fill=NA, colour="black"),
      plot.margin = margin(t=1, b=0, l=5, r=5)
    ) +
    labs(x=NULL, y=NULL, title=NULL) +
    scale_x_continuous(expand=c(0.2,0)) +
    scale_y_continuous(expand=c(0.2,0)) +
    annotate("text", Inf, -Inf, label=ann, hjust=1, vjust=-0.4)
}

lines_and_symbols <- function() {
  
  # symbol shape
  x <- 1:5
  d1 <- data.frame(x=x, y=x^2)
  gt <- emptyBox("bad") + geom_point(data=d1, aes(x, y), shape=8, size=2)
  gb <- emptyBox("good") + geom_point(data=d1, aes(x, y), shape=16, size=1.2)
  g1 <- plot_grid(gt, gb, ncol=1)
  
  # two groups symbol
  x <- 1:20
  d2 <- data.frame(
    x = x,
    y1 = x^0.5,
    y2 = 0.3 + x^0.51
  )
  gt <- emptyBox("bad") +
    geom_point(data=d2, aes(x, y1), shape=21, fill="black") +
    geom_point(data=d2, aes(x, y2), shape=21, fill="black")
  gb <- emptyBox("good") + 
    geom_point(data=d2, aes(x, y1), shape=21, fill="white") +
    geom_point(data=d2, aes(x, y2), shape=21, fill="black")
  g2 <- plot_grid(gt, gb, ncol=1)
  
  # large symbols
  x <- 1:200
  y <- x + rnorm(length(x), 0, 10)
  d3 <- data.frame(x=x, y=y)
  gt <- emptyBox("bad") + geom_point(data=d3, aes(x, y), shape=0, size=2)
  gb <- emptyBox("good") + geom_point(data=d3, aes(x, y), shape=16, size=0.4)
  g3 <- plot_grid(gt, gb, ncol=1)
  
  # symbols vs lines
  x <- 1:100
  y1 <- log(x) + rnorm(length(x), 0, 0.05)
  y2 <- 1.03 * log(x) * cos(0.04*x/(2*pi)) + rnorm(length(x), 0, 0.05)
  d4 <- data.frame(x=x, y1=y1, y2=y2)
  gt <- emptyBox("not great") + 
    geom_point(data=d4, aes(x, y1), shape=16, size=0.8, colour="red") +
    geom_point(data=d4, aes(x, y2), shape=16, size=0.8, colour="green")
  gb <- emptyBox("better") + 
    geom_line(data=d4, aes(x, y1), colour=okabe_ito_palette[1], alpha=0.8) +
    geom_line(data=d4, aes(x, y2), colour=okabe_ito_palette[2], alpha=0.8)
  g4 <- plot_grid(gt, gb, ncol=1)
  
  # spline
  x <- 1:5
  y1 <- c(1, 5, 10, 9, 6)
  y2 <- c(2, 4, 7, 5, 3.5)
  d5 <- data.frame(x=x, y1=y1, y2=y2)
  
  s1 <- spline(x, y1, n=100)
  s2 <- spline(x, y2, n=100)
  d5s <- data.frame(x=s1$x, y1=s1$y, y2=s2$y)
  
  gt <- emptyBox("bad") +
    geom_line(data=d5s, aes(x, y1), colour="grey40") +
    geom_line(data=d5s, aes(x, y2), colour="black") +
    geom_point(data=d5, aes(x, y1), shape=22, colour="grey40", fill="white") +
    geom_point(data=d5, aes(x, y2), shape=22, colour="black", fill="black")
  gb <- emptyBox("good") + 
    geom_pointpath(data=d5, aes(x, y1), shape=22, colour="grey40", fill="white", linecolour="grey40") +
    geom_pointpath(data=d5, aes(x, y2), shape=22, colour="black", fill="black")
  g5 <- plot_grid(gt, gb, ncol=1)
  
  
  plot_grid(g1, g2, g3, g4, g5, nrow=1)
}



plot_colour_blind <- function(seed=666) {
  set.seed(seed)
  d1 <- tibble(x = runif(12, min=3, max=9)) %>% mutate(y = 20 + 0.4 * x + rnorm(12, 0, 0.4), strain="S1")
  d2 <- tibble(x = runif(8, min=5, max=11)) %>% mutate(y = 21 + 0.45 * x + rnorm(8, 0, 0.5), strain="S2")
  d3 <- tibble(x = runif(9, min=7, max=10)) %>% mutate(y = 18 + 0.3 * x + rnorm(9, 0, 0.5), strain="S3")
  
  d <- bind_rows(d1, d2, d3)
  
  g1 <- ggplot(d, aes(x=x, y=y, colour=strain)) +
    theme_clean +
    geom_point() +
    labs(x = "Daily calorific input (kcal)", y = "Body mass (g)")
  
  g2 <- g1 + scale_colour_manual(values = okabe_ito_palette)
  
  list(colour_blind_1 = g1, colour_blind_2 = g2)
  
}



plot_loglin_1 <- function(dat, seed=45) {
  set.seed(seed)
  
  d <- dat %>% 
    select(c(2, 3)) %>% 
    set_names("x", "y") %>% 
    filter(x > 0 & y > 0) 

  mx <- max(d) * 1.03
  mx <- 25000
  p.size <- 0.5
  
  g1 <- ggplot(d) +
    theme_classic() +
    theme(plot.margin = margin(r=15)) +
    geom_point(aes(x, y), size=p.size) +
    scale_x_continuous(expand=c(0,0), limits=c(0, mx)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, mx)) +
    labs(x="Sample 1", y="Sample 2")
  g2 <- ggplot(d) +
    theme(plot.margin = margin(r=15)) +
    theme_classic() +
    geom_point(aes(log10(x), log10(y)), size=p.size) +
    labs(x=expression(log[10]~Sample~1), y=expression(log[10]~Sample~2))
  
  g3 <- plot_one_dist(d$x, "Expression", "", c(0, 25000), with.outline = TRUE) + theme_classic() +   theme(plot.margin = margin(r=15)) 
  
  g4 <- plot_one_dist(log10(d$x), expression(log[10]~expression), "", c(-0.1, 5.5), with.outline = TRUE) + theme_classic()
  
  g <- plot_grid(g1, g3, g2, g4, ncol=2, align="hv")
  g
}



plot_loglin_2 <- function(seed=77) {
  d <- tibble(
    x = 1:6 / 6,
    y1 = c(1.3, 1.5, 2.5, 3.0, 3.6, 4.1),
    s1 = 0.2
  ) %>% 
    mutate(
      y2 = 10^y1,
      s2 = log(10) * y2 * s1
    )

  g0 <- ggplot(d) +
    theme_clean +
    scale_x_continuous(expand=c(0,0), limits=c(0,1.1), breaks=seq(0,1,0.2)) +
    labs(x="Time (d)")
  
  g1 <- g0 +
    geom_errorbar(aes(x=x, ymin=y2-s2, ymax=y2+s2), width=0.03) +
    geom_point(aes(x=x, y=y2)) +
    scale_y_continuous(expand=c(0,0), limits=c(0,20000)) +
    labs(y="Abundance")
  
  g2 <- g0 +
    geom_errorbar(aes(x=x, ymin=y1-s1, ymax=y1+s1), width=0.03) +
    geom_point(aes(x=x, y=y1)) +
    scale_y_continuous() +
    labs(y=expression(log[10]~abundance))
  
  set.seed(seed)
  n1 <- 50
  n2 <- 100
  d <- tibble(
    type = c(rep("WT", n1), rep("KO", n2)),
    log.value = c(rnorm(n1, 2.1, 0.3), rnorm(n2, 1.1, 0.2))
  ) %>% 
    mutate(
      type = fct_relevel(type, "WT"),
      value = as.integer(10^log.value),
      log.value = log10(value)
    )

  g0 <- ggplot(d) +
    theme_clean +
    theme(legend.position = "none") +
    labs(x=NULL, y="Cell count") +
    theme(
      axis.title.y = element_text(margin = margin(t = 0, r = -10, b = 0, l = 0))
    ) +
    scale_fill_manual(values=okabe_ito_palette)
  
  g3 <- g0 +
    geom_beeswarm(aes(x=type, y=value, fill=type), shape=21, colour="grey40", cex=1.2, size=0.7) +
    #geom_boxplot(aes(x=type, y=value), fill=NA, colour="grey70", width=0.6, outlier.shape=NA) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$value)*1.03))
  
  g4 <- g0 +
    geom_beeswarm(aes(x=type, y=value, fill=type), shape=21, colour="grey40", cex=2.2) +
    #geom_boxplot(aes(x=type, y=value), fill=NA, colour="grey70", width=0.6, outlier.shape=NA) +
    scale_y_log10(breaks=c(2, 5, 10, 20, 50, 100, 200, 500)) +
    annotation_logticks(sides="l") +
    theme(axis.ticks.y = element_blank())
  
  
  plot_grid(g1, g3, g2, g4, ncol=2, align="hv")
  
}


plot_errorbars <- function(seed=1410) {
  set.seed(seed)
  n1 <- 8
  d1 <- data.frame(
    x = 1:n1,
    y = n1 - 1:n1 + rnorm(n1, 0, 1.5),
    s = rnorm(n1, 0.8, 0.1)
  )
  
  g1 <- emptyBox("") +
    geom_errorbar(data=d1, aes(x=x, ymin=y-s, ymax=y+s), width=0.2) +
    geom_point(data=d1, aes(x=x, y=y))
  
  n2 <- 5
  d2 <- data.frame(
    x = 1:n2,
    y = 1:n2 + rnorm(n2, 0, 1.5),
    sy = rnorm(n2, 0.4, 0.1),
    sx = 0.3
  )
  
  g2 <- emptyBox("") +
    geom_errorbar(data=d2, aes(x=x, ymin=y-sy, ymax=y+sy), width=0.12) +
    geom_errorbarh(data=d2, aes(y=y, xmin=x-sx, xmax=x+sx), height=0.08) +
    geom_point(data=d2, aes(x=x, y=y), shape=22, fill="white", size=1.5)
  
  n3 <- 30
  d3 <- data.frame(
    x = 1:n3,
    y = sin(1.3 * 1:n3/n3) + rnorm(n3, 0, 0.03),
    s = 0.06
  )
  g3 <- emptyBox("") +
    geom_errorbar(data=d3, aes(x=x, ymin=y-s, ymax=y+s), width=0, colour="grey70") +
    geom_point(data=d3, aes(x=x, y=y), size=1.5)
  
  
  n4 <- 5
  d4 <- data.frame(
    x = 1:n4,
    y = rnorm(n4, 5, 1),
    s = rnorm(n4, 1, 0.1)
  )
  g4 <- emptyBox("") +
    geom_col(data=d4, aes(x=x, y=y), fill=fill.colour, colour="black") +
    geom_errorbar(data=d4, aes(x=x, ymin=y-s, ymax=y+s), width=0.2) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d4$y + d4$s) * 1.2))
  
  plot_grid(g1, g2, g3, g4, nrow=1)
}


plot_boxplot <- function(n = 250) {
  d <- data.frame(
    x = 1,
    y = rnorm(n)
  )
  
  g <- ggplot(d) +
    theme_nothing() +
    geom_boxplot(aes(x,y), fill=fill.colour)
}


plot_boxplot_examples <- function(fastq, seed=666) {
  set.seed(seed)
  n <- c(50, 30, 40, 20)
  d <- data.frame(
    type = c(rep("WT", n[1]), rep("KO1", n[2]), rep("KO2", n[3]), rep("KO3", n[4])),
    value = c(rnorm(n[1], 20, 5), rnorm(n[2], 25, 3), rnorm(n[3], 30, 4), rnorm(n[4], 18, 7))
  )
  dm <- data.frame(
    x = "WT",
    y = median(d[d$type == "WT", "value"])
  )
  
  g1 <- ggplot() +
    theme_clean +
    theme(legend.position = "none", plot.margin = margin(r=30)) +
    geom_boxplot(data=dm, aes(x=x, middle=y, ymin=y, lower=y, upper=y, ymax=y), stat="identity", width=0.7, lwd=0.6, fatten=0) +
    geom_beeswarm(data=d[d$type == "WT", ], aes(type, value), size=0.8, cex=2.5, shape=21, fill=fill.colour) +
    geom_boxplot(data=d[d$type != "WT", ], aes(type, value, fill=type)) +
    scale_fill_manual(values=okabe_ito_palette) +
    labs(x=NULL, y="Value")
  
  
  g2 <- ggplot(fastq) +
    theme_clean +
    theme(axis.title.y.right = element_blank(), axis.title.x.top = element_blank()) +
    geom_boxplot(aes(x=Base, group=Base, middle=Median, ymin=`10thPercentile`, lower=LowerQuartile, upper=UpperQuartile, ymax=`90thPercentile`), stat="identity", fill=fill.colour, width=0.7, fatten=0.5, lwd=0.3) +
    scale_y_continuous(expand=c(0,0), limits=c(0,41.5), sec.axis = dup_axis()) +
    scale_x_continuous(breaks=c(1, seq(5,50,5)), sec.axis = dup_axis()) +
    labs(x="Position in read (bp)", y="Sequence quality")
  
  g <- plot_grid(g1, g2, nrow=1, rel_widths = c(1,2.2), align="h")
}


plot_barplot_1 <- function() {
  dat <- tibble(
    n = c(101, 98, 112, 100, 104, 89, 99, 100),
    s = c(86, 23, 67, 55, 95, 12, 34, 34),
    cond = rep(c("WT", "KO1", "KO2", "KO3"), 2),
    treat = c(rep("DMSO", 4), rep("DD342", 4))
  ) %>%
    mutate(p = s / n, cond = relevel(factor(cond), "WT"), treat = relevel(factor(treat), "DMSO")) %>% 
    nest(data = c(n ,s)) %>% 
    mutate(
      tst = map(data, ~prop.test(.x$s, .x$n)),
      tid = map(tst, broom::tidy)
    ) %>% 
    unnest(tid) %>% 
    select(cond, p, treat, conf.low, conf.high)
  
  
  ggplot(dat, aes(x=cond, y=p, fill=treat)) +
    theme_clean +
    theme(legend.position = "bottom") +
    geom_bar(position=position_dodge(0.8), stat="identity", colour="black", width=0.8) +
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2, position=position_dodge(0.8)) +
    scale_fill_manual(values=c(fill.colour, fill.colour.dark), name="Treatment") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x=NULL, y="Proportion of cells")
}


plot_barplot_2 <- function(seed=1) {
  set.seed(seed)
  x <- c(runif(500), abs(rnorm(100, 0, 0.04)))
  
  brks <- seq(0, 1, 0.1)
  ggplot(data.frame(x=x), aes(x=x)) +
    theme_clean +
    theme(axis.title.y.right = element_text(margin=margin(l=8))) +
    theme(plot.margin = margin(l=10, r=12, t=10)) +
    geom_histogram(breaks=brks, fill=fill.colour, colour="black") +
    labs(x="P-value", y="Count") +
    scale_x_continuous(expand=c(0,0), breaks=seq(0,1,0.2)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 150), sec.axis = sec_axis(~./60, "Density"))
}



bar1 <- function(d, lims=c(0, 900), ylab="Count") {
  ggplot(d, aes(x, y)) +
    theme_clean +
    geom_col(fill=fill.colour, colour="black") +
    coord_cartesian(ylim=lims, xlim=c(0.3,2.8), expand=FALSE) +
    labs(x=NULL, y=ylab)
}

bad_bar_plots <- function() {
  d <- tibble(
    x = c("Control", "Treatment"),
    y = c(400, 800)
  )
  
  pl1 <- bar1(d, lims=c(0, 900))
  pl2 <- bar1(d, lims=c(350, 850))
  g1 <- plot_grid(pl1, pl2, nrow=1, align="h")
  
  d <- d %>% mutate(y = log10(y))
  
  pl1 <- bar1(d, lims=c(0, 3), ylab=expression(log[10]~count))
  pl2 <- bar1(d, lims=c(2.2, 3), ylab=expression(log[10]~count))
  g2 <- plot_grid(pl1, pl2, nrow=1, align="h")

  list(
    badbard_baseline = g1,
    badbar_log = g2
  )
}



bar_problems <- function() {
  set.seed(1383)
  n <- 20
  M <- c(1.4, 1.35, 1.25, 1.25, 1.23, 1.25)
  d <- map_dfr(1:length(M), function(i) {
    tibble(sample=i, speed=rnorm(n, M[i], 0.15))
  })
  dm <- d %>% group_by(sample) %>% summarise(m=mean(speed), se=sd(speed) / sqrt(n()))
  
  g0 <- ggplot() +
    theme_clean +
    scale_x_continuous(breaks=seq(1:length(M))) +
    labs(x="Sample", y = ~ "Speed ("*mu*m/min*")")
  
  g1 <- g0 +
    geom_col(data=dm, aes(x=sample, y=m), fill=fill.colour, colour="black") + 
    geom_errorbar(data=dm, aes(x=sample, ymin=m-se, ymax=m+se), width=0.2) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(dm$m+dm$se)*1.03))
  
  g2 <- g0 +
    geom_point(data=dm, aes(x=sample, y=m), colour="black") + 
    geom_errorbar(data=dm, aes(x=sample, ymin=m-se, ymax=m+se), width=0.2)

  d$samplef <- as.factor(d$sample)
  g3 <- g0 +
    theme(legend.position = "none") +
    geom_boxplot(data=dm, aes(x=sample, group=sample, middle=m, ymin=m, lower=m, upper=m, ymax=m), stat="identity", width=0.5, lwd=0.5, fatten=0) +
    geom_beeswarm(data=d, aes(x=sample, y=speed, fill=samplef), shape=21, colour="grey50", cex=0.4, size=0.8) +
    scale_fill_manual(values=okabe_ito_palette)
  
  
  set.seed(557)
  t <- seq(0, 16, 2)
  n <- length(t)
  y1 <-  40 - 2 * t + rnorm(n, 0, 2)
  s1 <-  rnorm(n, 2, 0.3)
  y2 <-  3 + t + rnorm(n, 0, 1)
  s2 <-  rnorm(n, 2, 0.3)
  
  d <- tibble(
    time = c(t, t),
    type = c(rep("A", n), rep("B", n)),
    y = c(y1, y2),
    s = c(s1, s2)
  )
  
  g00 <- ggplot(d, aes(x=time, y=y)) +
    theme_clean +
    theme(legend.position = "none") +
    labs(x="Time (h)", y = ~ "Distance ("*mu*m*")") +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(d$y + d$s) * 1.03)) 
  
  g4 <- g00 +
    geom_errorbar(aes(ymin=y-s, ymax=y+s, group=type), width=.5, position=position_dodge(1.5)) +
    geom_bar(aes(fill=type), position=position_dodge(1.5), stat="identity", colour="black", width=1.5) +
    scale_fill_manual(values=c("black", "white"), name="Treatment")
  g5 <- g00 +
    geom_errorbar(aes(ymin=y-s, ymax=y+s, group=type), width=.5) +
    geom_pointpath(aes(fill=type, group=type), shape=21, colour="black", linecolour="grey70") +
    scale_fill_manual(values=c("black", "white"), name="Treatment")
  
  list(
    bar_problem_1 = g1,
    bar_problem_2 = g2,
    bar_problem_3 = g3,
    bar_problem_4 = g4,
    bar_problem_5 = g5
  )
}



dynamite_plots <- function() {
  d <- tibble(
    type = c("WT", "T1", "T2"),
    value = c(100, 87, 59),
    lo = c(NA, 61, 48),
    up = c(NA, 98, 69)
  ) %>% 
    mutate(type = fct_relevel(type, "WT"))
  
  
  g <- ggplot(d, aes(x=type, y=value, ymin=lo, ymax=up)) +
    theme_clean +
    labs(x=NULL, y="Percentage") +
    scale_y_continuous(expand=c(0,0), limits=c(0, 105))
  
  g1 <- g +
    geom_errorbar(width=.5) +
    geom_col(fill="black")
  
  g2 <- g +
    geom_col(fill="grey60") +
    geom_errorbar(width=.5)
  
  g3 <- ggplot(d[2:3, ], aes(x=type, y=value/100, ymin=lo/100, ymax=up/100)) +
    theme_classic() +
    labs(x=NULL, y="WT fraction") +
    geom_hline(yintercept = 1, colour="grey60") +
    geom_errorbar(width=.2) +
    geom_point()
  
  list(
    dynamite_1 = g1,
    dynamite_2 = g2,
    dynamite_3 = g3
  )
  
}
