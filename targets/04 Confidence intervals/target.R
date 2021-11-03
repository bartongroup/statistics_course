
lecture_04 <- function() {
  
  init <- list(
    tar_target(pref04, "figures/04_confidence_intervals")
  )

  read_data <- list(

  )
  
  comp <- list(
    tar_target(dat_cor, generate_cor(seed=3652, n=100000, r=0.7)),
    tar_target(sam_cor, sampling_distribution_cor(dat_cor, seed=221, nsim=100000, n=30))
  
  )
  
  make_figures <- list(
    # CI of count
    tar_target(s_count_ci, plot_poisson_ci_shift(5) %>% gs(pref04, 6, 3.5, "count_ci_exact")),
    tar_target(s_count_ci_ex, plot_count_ci_example() %>% gs(pref04, 7, 3.5, "count_ci_example")),
    
    # CI of correlation coefficient
    tar_target(s_1_cor, (plot_cor_sample(dat_cor, seed=32) + theme_clean) %>% gs(pref04, 3.5, 3, "correlation_1")),
    tar_target(s_9_cor, plot_9_cor(dat_cor, seed=2237) %>% gs(pref04, 5, 3.5, "correlation_9")),
    tar_target(s_cor_d, plot_one_dist(sam_cor$r, "r", "", c(0, 1), with.outline=TRUE) %>% gs(pref04, 5, 2.5, "correlation_dist")),
    tar_target(figs_cs, plot_cor_sampling(sam_cor)),
    tar_target(s_cor_cut, gs(figs_cs$cor_sampling_r, pref04, 3.5, 3.5)),
    tar_target(s_z_cut, gs(figs_cs$cor_sampling_z, pref04, 3.5, 3.5)),
    tar_target(figs_r_z, plot_95_ci_r()),
    tar_target(s_r, gs(figs_r_z$correlation_dist, pref04, 3.5, 2.5)),
    tar_target(s_z, gs(figs_r_z$fisher_dist, pref04, 3.5, 2.5)),
    tar_target(s_cor_6_30, plot_cor_6_30() %>% gs(pref04, 5, 2.5, "correlation_6_30")),
    
    # CI of proportion
    tar_target(s_ci_prop, plot_samp_propotion() %>% gs(pref04, 3, 5.5, "sampling_dist_proportion")),
    tar_target(figs_mice_surv, plot_mice_survival()),
    tar_target(s_sur, gs(figs_mice_surv$survival, pref04, 3.5, 2.5)),
    tar_target(s_sur_ci, gs(figs_mice_surv$survival_ci, pref04, 3.5, 2.5)),
    
    # Bootstrap
    tar_target(s_boot_ex, plot_bootstrap_example() %>% gs(pref04, 4, 3.5, "bootstrap"))
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}