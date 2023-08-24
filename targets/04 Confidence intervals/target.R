
lecture_04 <- function() {
  
  init <- tar_plan(
    path04 = "figures/04_confidence_intervals"
  )

  read_data <- tar_plan(

  )
  
  comp <- tar_plan(
    dat_cor = generate_cor(seed=3652, n=100000, r=0.7),
    sam_cor = sampling_distribution_cor(dat_cor, seed=221, nsim=100000, n=30)
  )
  
  make_figures <- tar_plan(
    # CI of count
    s_count_ci = plot_poisson_ci_shift(5) |> gs(path04, 6, 3.5, "count_ci_exact"),
    s_count_ci_ex = plot_count_ci_example() |> gs(path04, 7, 3.5, "count_ci_example"),
    
    # CI of correlation coefficient
    s_1_cor = (plot_cor_sample(dat_cor, seed=32) + theme_clean) |> gs(path04, 3.5, 3, "correlation_1"),
    s_9_cor = plot_9_cor(dat_cor, seed=2237) |> gs(path04, 5, 3.5, "correlation_9"),
    s_cor_d = plot_one_dist(sam_cor$r, "r", "", c(0, 1), with.outline=TRUE) |> gs(path04, 5, 2.5, "correlation_dist"),
    figs_cs = plot_cor_sampling(sam_cor),
    s_cor_cut = gs(figs_cs$cor_sampling_r, path04, 3.5, 3.5),
    s_z_cut = gs(figs_cs$cor_sampling_z, path04, 3.5, 3.5),
    figs_r_z = plot_95_ci_r(),
    s_r = gs(figs_r_z$correlation_dist, path04, 3.5, 2.5),
    s_z = gs(figs_r_z$fisher_dist, path04, 3.5, 2.5),
    s_cor_6_30 = plot_cor_6_30() |> gs(path04, 5, 2.5, "correlation_6_30"),
    
    # CI of proportion
    s_ci_prop = plot_samp_propotion() |> gs(path04, 3, 5.5, "sampling_dist_proportion"),
    figs_mice_surv = plot_mice_survival(),
    s_sur = gs(figs_mice_surv$survival, path04, 3.5, 2.5),
    s_sur_ci = gs(figs_mice_surv$survival_ci, path04, 3.5, 2.5),
    
    # Bootstrap
    s_boot_ex = plot_bootstrap_example() |> gs(path04, 4, 3.5, "bootstrap")
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
