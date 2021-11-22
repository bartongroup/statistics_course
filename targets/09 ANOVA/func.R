COUNTRIES <- c("English", "Scottish", "Welsh", "N.Irish")
COLOURS <- c("White", "Black")

aovResults <- function(dat) {
  fit <- aov(Mass ~ Country, data=dat)
  res <- summary(fit)
  MS.w <- res[[1]]$`Mean Sq`[2]
  MS.b <- res[[1]]$`Mean Sq`[1]
  p <- res[[1]]$`Pr(>F)`[1]
  F <- res[[1]]$`F value`[1]
  list(MS.w=MS.w, MS.b=MS.b, F=F, p=p)
}

aov2Results <- function(dat) {
  mice.lm <- lm(Mass ~ Country + Colour + Country*Colour, dat)
  aov <- anova(mice.lm)
  p.col <- aov[5]$`Pr(>F)`[1]
  p.row <- aov[5]$`Pr(>F)`[2]
  p.int <- aov[5]$`Pr(>F)`[3]
  list(p.col=p.col, p.row=p.row, p.int=p.int)
}


makeVar <- function(M, S=5, n=20) {
  M %>%
    set_names(COUNTRIES) %>%
    map_dfr(rnorm, n=n, sd=S) %>%
    pivot_longer(everything(), names_to = "Country", values_to = "Mass") %>% 
    mutate(Country = factor(Country, levels = COUNTRIES)) %>% 
    mutate(x = as.integer(Country))
}


plotWB <- function(d, maxy=40) {
  aov <- aovResults(d)
  dm <- d %>% group_by(Country) %>% summarise(M=mean(Mass), SD=sd(Mass)) %>% mutate(x=as.numeric(Country))
  vb <- data.frame(x=5, M=20, SD=sqrt(aov$MS.b))
  ggplot() +
    theme_clean +
    theme(legend.position = "none") +
    geom_beeswarm(data=d, aes(x=x, y=Mass, fill=Country), colour="grey70", cex=1.3, size=0.9, shape=21) +
    scale_fill_manual(values=british.palette) +
    scale_y_continuous(expand=c(0,0), limits=c(0, maxy)) +
    geom_errorbar(data=dm, aes(x=x, ymin=M-SD, ymax=M+SD), width=0.4) +
    geom_point(data=dm, aes(x=x, y=M), size=2, shape=21, fill="white") +
    geom_errorbar(data=vb, aes(x=x, ymin=M-SD, ymax=M+SD), width=0.4, colour="grey50") +
    labs(x=NULL, y="Body mass (g)") +
    scale_x_continuous(breaks=1:4, labels=COUNTRIES)
}

plot_var_within_between <- function(seed = 23143) {
  set.seed(seed)
  
  v.same <- makeVar(c(20, 20, 20, 20))
  v.diff <- makeVar(c(20, 28, 20, 20))
  
  g1 <- plotWB(v.same)
  g2 <- plotWB(v.diff)
  plot_grid(g1, g2, nrow=1)
}


getMeans <- function(mice, value="Mass", width=0.6) {
  width <- 0.6
  M <- tapply(mice[[value]], mice$Country, mean)
  L <- tapply(mice$Country, mice$Country, length)
  mice$grand.mean <- mean(mice[[value]])
  mice$Mean <- M[mice$Country]
  mice$N <- L[mice$Country]
  mice$group <- as.integer(mice$Country)
  mice$n <- unlist(tapply(mice$Country, mice$Country, seq_along))
  mice$x <- as.integer(mice$Country) + width*(mice$n - mice$N/2 - 1/2) / (mice$N - 1)
  means <- data.frame(x=as.numeric(seq_along(M)), mean=M, country=names(M))
  list(mice=mice, means=means)  
}


get_means <- function(mice, width, what="Mass") {
  m <- mice %>% 
    mutate(val = get(what)) %>% 
    mutate(grand_mean = mean(val)) %>% 
    mutate(group = as.integer(Country)) %>% 
    group_by(group) %>% 
    mutate(
      N = n(),
      n = seq_along(val),
      x = group + width*(n - N/2 - 1/2) / (N - 1),
      Mean = mean(val)
    ) %>% 
    ungroup()
  M <- m %>% 
    select(Country, Mean, x = group) %>% 
    distinct()
  list(mice = m, means = M)
}


plot_variance <- function(mice, within=TRUE, width=0.6) {
  m <- get_means(mice, width)
  gm <- m$mice$grand_mean[1]
  g <- ggplot(m$mice) + theme_clean
  if(within) {
    g <- g + geom_segment(aes(x=x, xend=x, y=Mean, yend=Mass), colour="grey70")
  } else {
    g <- g +
      geom_line(data=data.frame(x=c(1-width/2,4+width/2), y=c(gm, gm)), aes(x,y), linetype="dotted") +
      geom_segment(aes(x=x, xend=x, y=grand_mean, yend=Mean), colour="grey70")
  }
  g <- g +
    geom_segment(data=m$means, aes(x=x-width/2, xend=x+width/2, y=Mean, yend=Mean, colour=Country)) +
    geom_point(aes(x=x, y=Mass, fill=Country), colour="grey30", shape=21, size=1.8) +
    theme(legend.position = "none", axis.ticks.x = element_blank()) +
    scale_x_continuous(breaks=c(1,2,3,4), labels=COUNTRIES) +
    labs(x="", y="Body mass (g)") +
    scale_colour_manual(values=british.palette) +
    scale_fill_manual(values=british.palette)
  g
}

generate_F_mice <- function(mice, seed=33, nsim=100000) {
  set.seed(seed)
  n <- nrow(mice)
  map_dbl(1:nsim, function(i) {
    m <- mice %>% 
      mutate(Mass = rnorm(n, 20, 5))
    res <- aovResults(m)
    res$F
  })
}

generate_no_effect_anova <- function(mice, seed=8) {
  set.seed(seed)
  n <- nrow(mice)
  repeat {
    m <- mice %>% 
      mutate(Mass = rnorm(n, 20, 5))
    res <- aovResults(m)
    if(res$F < 1.1 && res$F > 0.9 && res$p > 0.1) break
  }
  list(dat = m, res = res)
}


plot_anova_2 <- function(mice, ylim=c(0,40), text.size=10) {
  grand.mean <- mean(mice$Mass)
  mice.gr <- mice %>% group_by(Country, Colour) %>% summarise(M = mean(Mass))
  
  g1 <- ggplot(mice, aes(x=1, y=Mass)) +
    theme_classic() +
    facet_grid(Colour ~ Country, switch="both") +
    geom_hline(yintercept = grand.mean, linetype = "dotted", size=0.3) +
    #geom_boxplot(aes(fill=Country), alpha=0.5, outlier.shape = NA, width=0.7) +
    scale_fill_manual(values=british.palette) +
    geom_beeswarm(aes(fill=Country), cex=3, size=1.5, shape=21) +
    labs(x="", y="") +
    ylim(ylim) +
    xlim(0, 2) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(size=text.size),
      panel.spacing = unit(0, "cm"),
      panel.border = element_rect(fill=NA),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank()
    )
  
  g2 <- ggplot(mice.gr, aes(x=Country, y=M, group=Colour, colour=Colour)) +
    theme_classic() +
    geom_line(aes(group=Colour)) +
    geom_point(aes(fill=Colour), size=2, shape=22) +
    labs(x="", y="") +
    ylim(ylim) +
    geom_hline(yintercept = grand.mean, linetype = "dotted", size=0.3) +
    scale_colour_manual(values=c("grey60", "black")) +
    scale_fill_manual(values=c("white", "black")) +
    scale_x_discrete(position = "top") +
    theme(
      panel.border = element_rect(colour="black", linetype="solid", fill=NA),
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      panel.spacing = unit(0, "cm"),
      axis.line = element_blank(),
      axis.text = element_text(size=text.size),
      plot.margin = unit(c(-0.5,0,0,0),"cm")
    )
  
  g3 <- ggplot(mice.gr, aes(x=Colour, y=M, group=Country, colour=Country)) +
    theme_classic() +
    geom_line(aes(group=Country)) +
    geom_point(aes(colour=Country, fill=Country), size=2, shape=22) +
    labs(x="", y="") +
    ylim(ylim) +
    geom_hline(yintercept = grand.mean, linetype = "dotted", size=0.3) +
    scale_colour_manual(values=british.palette) +
    scale_fill_manual(values=british.palette) +
    scale_x_discrete(position = "bottom") +
    theme(
      panel.border = element_rect(colour="black", linetype="solid", fill=NA),
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      panel.spacing = unit(0, "cm"),
      axis.line = element_blank(),
      axis.text = element_text(size=text.size),
      plot.margin = unit(c(0,0,0.6,0),"cm")
    )
  
  left_col <- g1
  right_col <- plot_grid(g2, g3, ncol=1, rel_heights = c(1, 1.33))
  
  plot_grid(left_col, right_col, ncol=2, rel_widths = c(1, 0.6))
}


rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

generate_mices_2way <- function(A = matrix(c(0, 0, 0, 0), nrow=1),
                                B = matrix(c(0, 0), ncol=1), 
                                G = rbind(c(0,0,0,0), c(0,0,0,0)),
                                countries = c("English", "Scottish", "Welsh", "N.Irish"),
                                colours = c("White", "Black"),
                                m=20, s=5, n=10) {
  
  nr <- nrow(G)
  nc <- ncol(G)
  stopifnot(ncol(A) == nc && nrow(B) == nr)
  
  AA <- rep.row(A, nr)
  BB <- rep.col(B, nc)
  
  map_dfr(1:n, function(i) {
    X <- m + AA + BB + G + rnorm(8, 0, s)
    rownames(X) <- colours
    colnames(X) <- countries
    X %>%
      as_tibble(rownames = "Colour") %>% 
      pivot_longer(-Colour, names_to = "Country", values_to = "Mass")
  }) %>% 
    mutate(Colour = factor(Colour, levels = COLOURS), Country = factor(Country, levels = COUNTRIES))
}


plot_anova_2_4nulls <- function() {
  # all three true
  set.seed(1001)
  X1 <- generate_mices_2way()
  g1 <- plot_anova_2(X1)
  r1 <- aov2Results(X1)
  
  # columns not equal
  set.seed(2000)
  X2 <- generate_mices_2way(A=matrix(c(0, 10, -10, 0), nrow=1))
  g2 <- plot_anova_2(X2)
  r2 <- aov2Results(X2)
  
  # rows not equal
  set.seed(3001)
  X3 <- generate_mices_2way(B=matrix(c(10, -10), ncol=1))
  g3 <- plot_anova_2(X3)
  r3 <- aov2Results(X3)
  
  # interaction
  set.seed(4003)
  X4 <- generate_mices_2way(G=rbind(c(-10, 10, 0, 0), c(10, -10, 0, 0)))
  g4 <- plot_anova_2(X4)
  r4 <- aov2Results(X4)
  
  list(
    anova2_true = g1,
    anova2_cols = g2,
    anova2_rows = g3,
    anova2_inter = g4,
    
    res_true = r1,
    res_cols = r2,
    res_rows = r3,
    res_inter = r4
  )
  
}



simple_anova_plot <- function(dat, title="") {
  colnames(dat) <- c("Drug", "Placebo")
  rownames(dat) <- c("Male", "Female")
  dat %>% 
    as_tibble(rownames = "Gender") %>% 
    pivot_longer(-Gender, names_to = "Treatment", values_to = "Score") %>% 
  ggplot(aes(x=Gender, y=Score, group=Treatment)) +
    theme_clean +
    geom_line(aes(colour=Treatment)) +
    geom_point(aes(shape=Treatment, colour=Treatment)) +
    scale_colour_manual(values=okabe_ito_palette) +
    ylim(c(0,30)) +
    theme(legend.position = "none") +
    labs(title=title)
}

plot_drug_anova <- function() {
  dat1 <- rbind(c(20, 5), c(20, 5))
  g1 <- simple_anova_plot(dat1, "Drug effect")

  dat2 <- rbind(c(15, 5), c(25, 15))
  g2 <- simple_anova_plot(dat2, "Drug and gender effect")

  dat3 <- rbind(c(25, 5), c(10, 5))
  g3 <- simple_anova_plot(dat3, "Interaction effect")
  
  plot_grid(g1, g2, g3, ncol=3)
}


plot_time_course <- function(tc) {
  tc <- as_tibble(tc) %>% 
    group_by(Treatment, Course) %>% 
    mutate(nMass = Mass / Mass[Time == 0]) %>% 
    ungroup()
  tcm <- tc %>%
    group_by(Treatment, Time) %>%
    summarise(M = mean(Mass), SE = sd(Mass) / sqrt(n()), nM = mean(nMass), nSE = sd(nMass) / sqrt(n()))
  
  g0 <- ggplot() +
    theme_clean +
    scale_colour_manual(values=okabe_ito_palette) +
    scale_fill_manual(values=okabe_ito_palette) +
    scale_shape_manual(values=c(21, 24)) +
    labs(x="Time (week)", y="Body mass (g)") +
    theme(legend.position = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03)), limits = c(0, NA))
  
  
  ps <- 1.5
  g1 <- g0 + 
    geom_line(data=tc, aes(x=Time, y=Mass, colour=Treatment, group=Course), alpha=0.3) +
    geom_point(data=tc, aes(x=Time, y=Mass, fill=Treatment, shape=Treatment), size=ps) +
    ggtitle("Original data")
  g2 <- g0 + 
    geom_line(data=tcm, aes(x=Time, y=M, colour=Treatment, group=Treatment), alpha=0.3) +
    geom_point(dat=tcm, aes(x=Time, y=M, fill=Treatment, shape=Treatment), size=ps) +
    geom_errorbar(data=tcm, aes(x=Time, ymin=M-SE, ymax=M+SE, colour=Treatment), width=0.2) +
    ggtitle("Mean and standard error")
  g3 <- g0 +  
    geom_line(data=tc, aes(x=Time, y=nMass, colour=Treatment, group=Course), alpha=0.3) +
    geom_point(data=tc, aes(x=Time, y=nMass, fill=Treatment, shape=Treatment), size=ps) +
    labs(x="Time (week)", y="Normalized body mass") +
    ggtitle("Normalised to the first time point")
  g4 <- g0 + 
    geom_line(data=tcm, aes(x=Time, y=nM, colour=Treatment, group=Treatment), alpha=0.3) +
    geom_point(data=tcm, aes(x=Time, y=nM, fill=Treatment, shape=Treatment), size=ps) +
    geom_errorbar(data=tcm, aes(x=Time, ymin=nM-nSE, ymax=nM+nSE, colour=Treatment), width=0.2) +
    labs(x="Time (week)", y="Normalized body mass") +
    ggtitle("Mean of normalised data")
  
  plot_grid(g1, g2, g3, g4, ncol=2, scale=0.9)
  
}



area <- function(x, y) {
  n <- length(x)
  stopifnot(length(y) == n)
  d <- x[2:n] - x[1:(n-1)]
  h <- (y[2:n] + y[1:(n-1)]) / 2
  sum(d * h)
}

areaData <- function(dat, t1, t2) {
  courses <- unique(dat$Course)
  map_dfr(courses, function(course) {
    d <- subset(dat, Course == course & Time >= t1 & Time <= t2)
    a <- area(d$Time, d$Mass)
    t <- as.character(d$Treatment[1])
    tibble(Course=course, Treatment=t, Area=a)
  })
}

areaPlot <- function(d) {
  #d[d$Treatment == "Untreated", "Treat"] <- "CTRL"
  #d[d$Treatment == "Treated", "Treat"] <- "Treat"
  #d$Treat <- factor(d$Treat, levels=c("CTRL", "Treat"))
  ggplot(d, aes(x=Treatment, y=Area, fill=Treatment, shape=Treatment)) +
    theme_clean +
    theme(legend.position = "none") +
    #geom_boxplot(aes(fill=Treat), alpha=0.5, outlier.shape = NA) +
    scale_fill_manual(values=okabe_ito_palette) +
    scale_shape_manual(values=c(21, 24)) +
    #geom_jitter(width=0.15, height=0) +
    geom_beeswarm(cex=3, size=1.5, size=1.8) +
    labs(x=NULL, y=NULL) +
    scale_y_continuous(expand=c(0, 0), limits=c(0, 290)) +
    scale_x_discrete(labels=c("CTRL", "Treat"))
}


plot_time_course_area <- function(tc) {
  d1 <- areaData(tc, 0, 10) 
  g1 <- areaPlot(d1)
  t.test(d1[d1$Treatment == "Untreated", "Area"], d1[d1$Treatment == "Treated", "Area"])
  
  d2 <- areaData(tc, 6, 10)
  g2 <- areaPlot(d2)
  t.test(d2[d2$Treatment == "Untreated", "Area"], d2[d2$Treatment == "Treated", "Area"])
  
  d3 <- areaData(tc, 0, 5)
  g3 <- areaPlot(d3)
  t.test(d3[d3$Treatment == "Untreated", "Area"], d3[d3$Treatment == "Treated", "Area"])
  
  plot_grid(g1, g2, g3, ncol=3)
  
}