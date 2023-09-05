
lecture_11 <- function() {
  
  init <- tar_plan(
    path11 = "figures/11_statistical_power"
  )

  read_data <- tar_plan(
  )
  
  comp <- tar_plan(
    cohens_sizes = make_cohen_sizes(),
    t_h0_h1 = generate_t_H0_H1(n = 5),
    anova_h0_h1 = generate_a_H0_H1(n = 5), # this one takes a lot of time!
    
    pow_t_M = c(22, 24, 26, 28, 30),
    t_powers = power_t_test(pow_t_M - 20, S=5, n=5),
    
    tumour = generate_worked_example()
  )
  
  make_figures <- tar_plan(
    # Cohen's d
    s_code = plot_cohens_d_example() |> gs(path11, 2.5, 3.5, "cohens_d_example"),
    s_cfc = plot_cohens_fold_change() |> gs(path11, 5, 3, "cohens_d_fold_change"),
    s_csz = plot_cohens_sample_size() |> gs(path11, 5, 3, "cohens_d_sample_size"),
    
    # ANOVA
    s_af = plot_anova_f() |> gs(path11, 6, 3, "anova_f1"),
    
    # Power t-test
    figs_t_pow = plot_t_h0_h1(t_h0_h1),
    s_tp_1 = gs(figs_t_pow$t_pow, path11, 3.5, 4),
    s_tp_2 = gs(figs_t_pow$t_pow_cut, path11, 3.5, 4),
    
    # t power curves
    s_powt = plot_power_t_test(pow_t_M, S=5, n=5) |> gs(path11, 4, 5, "t_power_5dist"),
    figs_t_pow_curves = plot_t_power_curves(),
    s_tpw_1 = gs(figs_t_pow_curves$t_power_curve_5, path11, 3, 3),
    s_tpw_2 = gs(figs_t_pow_curves$t_power_curves, path11, 3, 3),
    s_tpw_3 = gs(figs_t_pow_curves$t_size_curves, path11, 3, 3),
    
    # Power ANOVA
    s_powa = plot_anova_h0_h1(anova_h0_h1) |> gs(path11, 3.5, 4, "anova_pow_cut"),
    
    # ANOVA power curves
    figs_anova_pow_curves = plot_anova_power_curves(),
    s_apw_2 = gs(figs_anova_pow_curves$anova_power_curves, path11, 3, 3),
    s_apw_3 = gs(figs_anova_pow_curves$anova_size_curves, path11, 3, 3),
    
    # Worked example
    figs_tumour_ex = plot_tumour_example(tumour),
    s_tuex_1 = gs(figs_tumour_ex$tumour_data, path11, 3, 3),
    s_tuex_2 = gs(figs_tumour_ex$tumour_sd, path11, 3, 3),
    s_tuex_3 = gs(figs_tumour_ex$tumour_power, path11, 3, 3),
    s_tuex_4 = gs(figs_tumour_ex$tumour_anova, path11, 3, 3)
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
