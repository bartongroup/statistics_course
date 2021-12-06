generate_quant_mice <- function(seed=125) {
  set.seed(seed)
  f <- function(kcal) {
    f <- 20 + 0.5 * kcal + rnorm(length(kcal), 0, 1)
  }
  
  kcal <- runif(10, 2, 10)
  ms <- tibble(
    kcal = round(kcal, 1),
    mass = round(f(kcal),1)
  )
}

generate_m5 <- function(seed=1) {
  set.seed(seed)
  n1 <- 2
  n2 <- 3
  tibble(
    diet = c(rep('norm', n1), rep('hifat', n2)) %>% as_factor(),
    mass = c(rnorm(n1, 20, 5), rnorm(n2, 30, 5)) %>% round(1)
  )
}

generate_m12 <- function(seed=1) {
  set.seed(seed)
  n1 <- 6
  n2 <- 6
  row <- c(rnorm(n1, 20, 5), rnorm(n2, 30, 5)) %>% round(1)
  samples <- c(rep('norm', n1), rep('hifat', n2))
  sex <- c('f', 'f', 'f', 'm', 'm', 'm', 'f', 'f', 'f', 'm', 'm', 'm')
  tibble(mass=row, diet=factor(samples), sex=factor(sex)) %>% 
    mutate(diet = fct_relevel(diet, "norm"), sex = fct_relevel(sex, "f")) %>% 
    unite(group, c(diet, sex), remove=FALSE) %>% 
    mutate(group = factor(group, levels=c("norm_f", "hifat_f", "norm_m", "hifat_m")))
  
  
}

plot_quant_lm <- function(ms) {
  ms.fit <- lm(mass ~ kcal, data=ms)
  cf <- coef(ms.fit)
  ms$model <- predict(ms.fit)
  gg1 <- ggplot(ms, aes(x=kcal, y=mass)) +
    theme_clean +
    labs(x="Daily calorific input (kcal)", y="Body mass (g)")
  
  g1 <- gg1 + geom_point() + geom_abline(intercept=cf[1], slope=cf[2], colour="red") 
  g2 <- gg1 + geom_segment(aes(xend=kcal, yend=model), colour="grey60") +
    geom_point() + geom_abline(intercept=cf[1], slope=cf[2], colour="red") 
  
  list(
    simple_linear = g1,
    simple_linear_residuals = g2
  )
}



plot_fit_coefficients <- function(d, cf, group_var = "group", value_var = "value", nudge=0, text.size=14, with.baseline=TRUE, with.mean=TRUE, with.coef=TRUE) {
  cf.names <- names(cf)
  n <- length(cf)
  if(cf.names[1] == "(Intercept)") {
    arr <- tibble(
      x = 1:n,
      lo = c(0, rep(cf[1], n - 1)),
      up = c(cf[1], cf[2:n] + cf[1])
    )
  } else {
    arr <- tibble(
      x = 1:n,
      lo = c(rep(0, n)),
      up = cf
    )
  }
  
  dat <- d %>% 
    mutate(group = get(group_var), value = get(value_var)) %>% 
    mutate(x = as.numeric(as.factor(group)))
  
  gdat <- dat %>%
    group_by(group) %>%
    summarise(M = mean(value)) %>% 
    mutate(cf.name = cf.names) %>% 
    bind_cols(arr) %>% 
    mutate(text.y = (lo + up) / 2 + nudge)

  dw <- 0.1
  g <- ggplot(dat, aes(x=x, y=value)) +
    theme_clean +
    theme(text = element_text(size=text.size)) +
    geom_point(size=2, shape=21, fill="grey70") +
    scale_x_continuous(breaks=1:n, labels=gdat$group, limits=c(0.7, n+0.3)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, 1.05*max(dat$value))) +
    theme(legend.position = "none") +
    scale_colour_manual(values=c(okabe_ito_palette, "black")) +
    labs(x=NULL, y="Body mass (g)")
  if(with.mean) g <- g + geom_segment(data=gdat, aes(x=x-dw, xend=x+dw, y=M, yend=M), size=1)
  if(with.baseline) g <- g + geom_hline(yintercept = cf[1], colour="grey50", linetype="dotted")
  if(with.coef) {
    g <- g +
      geom_segment(data=gdat, aes(x=x, xend=x, y=lo, yend=up, colour=as.factor(x))) +
      geom_segment(data=gdat, aes(x=x-dw, xend=x+dw, y=up, yend=up, colour=as.factor(x))) +
      geom_text(data=gdat, aes(x=x+0.1, y=text.y, label=cf.name, colour=as.factor(x)), angle=90)
  }
  g
}

plot_coefficients <- function(fit, nudge=0.7) {
  fit %>% 
    broom::tidy() %>% 
    ggplot(aes(x=term, y=estimate, ymin=estimate-std.error, ymax=estimate+std.error)) +
    theme_clean +
    geom_errorbar(width=0.3) +
    geom_point() +
    geom_text(aes(y=estimate+std.error, label=signif(p.value,2)), nudge_y = nudge, size=3) +
    labs(x=NULL, y="Estimate") +
    scale_y_continuous(expand=expansion(mult=c(0, 0.05)), limits=c(0, NA))
}

plot_mice_5_lm <- function(d, cf, cf0) {
  g1 <- plot_fit_coefficients(d, cf, group_var = "diet", value_var = "mass", with.baseline = FALSE, with.coef = FALSE, with.mean = FALSE)
  g2 <- plot_fit_coefficients(d, cf, group_var = "diet", value_var = "mass")
  g3 <- plot_fit_coefficients(d, cf0, group_var = "diet", value_var = "mass", with.baseline = FALSE)
  
  list(
    mice_diet = g1,
    mice_diet_coef = g2,
    mice_diet_coef0 = g3
  )
}

plot_mice_12_lm <- function(d, cf, cfi) {
  cf[4] <- cf[2] + cf[3]
  names(cf)[4] <- "diethifat + sexm"
  cfi[4] <- cfi[4] <- cfi[2] + cfi[3] + cfi[4]
  names(cfi)[4] <- "diethifat + sexm + diethifat:sexm"
  g1 <- plot_fit_coefficients(d, cf, value_var="mass", with.baseline = FALSE, with.coef = FALSE)
  g2 <- plot_fit_coefficients(d, cf, value_var="mass")
  g3 <- plot_fit_coefficients(d, cfi, value_var="mass")
  
  list(
    mice_diet_sex = g1,
    mice_diet_sex_coef = g2,
    mice_diet_sex_coef_i = g3
  )
}


plot_r2 <- function(d) {
  M <- mean(d$y)
  mod <- lm(y ~ x, data=d)
  cf <- coef(mod)
  R2 <- summary(mod)$r.squared
  
  d %>%
    mutate(pred = predict(mod, d)) %>% 
    ggplot(aes(x=x, y=y)) +
    theme_clean +
    geom_segment(aes(xend=x, yend=pred), colour="black") +
    geom_segment(aes(xend=x, yend=M), colour="orange", linetype="dashed") +
    geom_point() +
    geom_abline(slope=cf[2], intercept = cf[1], colour="red") +
    geom_hline(yintercept = M, colour="orange", linetype="dashed") +
    labs(title=sprintf("R2 = %4.2f", R2))
}


plot_r2_examples <- function() {
  set.seed(61234)
  x <- 1:10
  d1 <- tibble(x = x, y = 1*x + rnorm(10, 5))
  g1 <- plot_r2(d1)
  g1

  set.seed(4336)
  d2 <- tibble(x = x, y = 0 + rnorm(10, 5))
  g2 <- plot_r2(d2)
  g2
  
  list(
    r2_large = g1,
    r2_small = g2
  )
}