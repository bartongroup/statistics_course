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
  M |>
    set_names(COUNTRIES) |>
    map_dfr(rnorm, n=n, sd=S) |>
    pivot_longer(everything(), names_to = "Country", values_to = "Mass") |> 
    mutate(Country = factor(Country, levels = COUNTRIES)) |> 
    mutate(x = as.integer(Country))
}


plotWB <- function(d, maxy=40) {
  aov <- aovResults(d)
  dm <- d |> group_by(Country) |> summarise(M=mean(Mass), SD=sd(Mass)) |> mutate(x=as.numeric(Country))
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




generate_F_mice <- function(mice, seed=33, nsim=100000) {
  set.seed(seed)
  n <- nrow(mice)
  map_dbl(1:nsim, function(i) {
    m <- mice |> 
      mutate(Mass = rnorm(n, 20, 5))
    res <- aovResults(m)
    res$F
  })
}

generate_no_effect_anova <- function(mice, seed=8) {
  set.seed(seed)
  n <- nrow(mice)
  repeat {
    m <- mice |> 
      mutate(Mass = rnorm(n, 20, 5))
    res <- aovResults(m)
    if(res$F < 1.1 && res$F > 0.9 && res$p > 0.1) break
  }
  list(dat = m, res = res)
}


plot_anova_2 <- function(m, ylim=c(0,40), text.size=10, col_var="Country", row_var="Colour", val_var="Mass", palette=british.palette) {
  mice <- m |> 
    mutate(col = get(col_var), row = get(row_var), val = get(val_var))
  
  grand.mean <- mean(mice$val)
  mice.gr <- mice |> group_by(col, row) |> summarise(M = mean(val))
  
  g1 <- ggplot(mice, aes(x=1, y=val)) +
    theme_classic() +
    facet_grid(row ~ col, switch="both") +
    geom_hline(yintercept = grand.mean, linetype = "dotted", linewidth=0.3) +
    #geom_boxplot(aes(fill=Country), alpha=0.5, outlier.shape = NA, width=0.7) +
    scale_fill_manual(values=palette) +
    geom_beeswarm(aes(fill=col), cex=3, size=1.5, shape=21) +
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
  
  g2 <- ggplot(mice.gr, aes(x=col, y=M, group=row, colour=row)) +
    theme_classic() +
    geom_line(aes(group=row)) +
    geom_point(aes(fill=row), size=2, shape=22) +
    labs(x="", y="") +
    ylim(ylim) +
    geom_hline(yintercept = grand.mean, linetype = "dotted", linewidth=0.3) +
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
  
  g3 <- ggplot(mice.gr, aes(x=row, y=M, group=col, colour=col)) +
    theme_classic() +
    geom_line(aes(group=col)) +
    geom_point(aes(colour=col, fill=col), size=2, shape=22) +
    labs(x="", y="") +
    ylim(ylim) +
    geom_hline(yintercept = grand.mean, linetype = "dotted", linewidth=0.3) +
    scale_colour_manual(values=palette) +
    scale_fill_manual(values=palette) +
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
    X |>
      as_tibble(rownames = "Colour") |> 
      pivot_longer(-Colour, names_to = "Country", values_to = "Mass")
  }) |> 
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
  dat |> 
    as_tibble(rownames = "Gender") |> 
    pivot_longer(-Gender, names_to = "Treatment", values_to = "Score") |> 
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
  tc <- as_tibble(tc) |> 
    group_by(Treatment, Course) |> 
    mutate(nMass = Mass / Mass[Time == 0]) |> 
    ungroup()
  tcm <- tc |>
    group_by(Treatment, Time) |>
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


time_course_t_test <- function(tc) {
  tc |> 
    select(Treatment, Time, Mass) |>
    nest(data = c(Treatment, Mass)) |>
    mutate(
      fit = map(data, ~t.test(Mass ~ Treatment, data = .x)),
      tidied = map(fit, tidy)
    ) |>
    unnest(tidied) |> 
    select(Time, estimate, p_value = p.value) |> 
    mutate(p_adjust = p.adjust(p_value, method = "BH"))
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


plot_sal <- function(sal, what) {
  sal <- mutate(sal, x = !!sym(what))
  g <- ggplot(sal, aes(x=x, y=salary)) +
    theme_clean +
    labs(x=what, y="salary")
  if(is.factor(sal$x)) {
    g <- g + geom_boxplot(outlier.shape = NA) + geom_beeswarm(size=0.6, colour="grey50")
  } else {
    g <- g + geom_point(size=0.6, colour="grey50")
  }
  g
}


plot_salaries <- function(sal) {
  snames <- colnames(sal |> select(-salary))
  gs <- map(snames, ~plot_sal(sal, .x))
  plot_grid(plotlist = gs)
}


plot_salaries_years <- function(sal) {
  ggplot(sal, aes(x=yrs.since.phd, y=yrs.service)) +
    theme_clean +
    geom_point(size=0.6)
}


plot_female_proportion <- function(sal) {
  fs <- sal |> 
    group_by(rank) |> 
    summarise(n_female = sum(sex == "Female"), n_male = sum(sex == "Male"), n = n()) |> 
    nest(data = c(n_female, n)) |> 
    mutate(
      test = map(data, ~prop.test(.x$n_female, .x$n)),
      tidied = map(test, broom::tidy)
    ) |> 
    unnest(tidied)
  
  ggplot(fs, aes(x=rank, y=estimate, ymin=conf.low, ymax=conf.high)) +
    theme_clean +
    geom_errorbar(width=0.3, colour="grey") +
    geom_point() +
    labs(x="Rank", y="Female propotion") +
    scale_y_continuous(expand=expansion(mult = c(0, 0.05)), limits=c(0, NA))
}
