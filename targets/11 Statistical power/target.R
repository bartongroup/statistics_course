
lecture_11 <- function() {
  
  init <- list(
    tar_target(pref11, "figures/11_statistical_power")
  )

  read_data <- list(
  )
  
  comp <- list(
    tar_target(cohens_sizes, make_cohen_sizes()),
    tar_target(t_h0_h1, generate_t_H0_H1(n = 5)),
    tar_target(anova_h0_h1, generate_a_H0_H1(n = 5)), # this one takes a lot of time!
    
    tar_target(pow_t_M, c(22, 24, 26, 28, 30)),
    tar_target(t_powers, power_t_test(pow_t_M - 20, S=5, n=5)),
    
    tar_target(tumour, generate_worked_example())
  )
  
  make_figures <- list(
    # Cohen's d
    tar_target(s_code, plot_cohens_d_example() %>% gs(pref11, 2.5, 3.5, "cohens_d_example")),
    tar_target(s_cfc, plot_cohens_fold_change() %>% gs(pref11, 5, 3, "cohens_d_fold_change")),
    tar_target(s_csz, plot_cohens_sample_size() %>% gs(pref11, 5, 3, "cohens_d_sample_size")),
    
    # ANOVA
    tar_target(s_af, plot_anova_f() %>% gs(pref11, 6, 3, "anova_f1")),
    
    # Power t-test
    tar_target(figs_t_pow, plot_t_h0_h1(t_h0_h1)),
    tar_target(s_tp_1, gs(figs_t_pow$t_pow, pref11, 3.5, 4)),
    tar_target(s_tp_2, gs(figs_t_pow$t_pow_cut, pref11, 3.5, 4)),
    
    # t power curves
    tar_target(s_powt, plot_power_t_test(pow_t_M, S=5, n=5) %>% gs(pref11, 4, 5, "t_power_5dist")),
    tar_target(figs_t_pow_curves, plot_t_power_curves()),
    tar_target(s_tpw_1, gs(figs_t_pow_curves$t_power_curve_5, pref11, 3, 3)),
    tar_target(s_tpw_2, gs(figs_t_pow_curves$t_power_curves, pref11, 3, 3)),
    tar_target(s_tpw_3, gs(figs_t_pow_curves$t_size_curves, pref11, 3, 3)),
    
    # Power ANOVA
    tar_target(s_powa, plot_anova_h0_h1(anova_h0_h1) %>% gs(pref11, 3.5, 4, "anova_pow_cut")),
    
    # ANOVA power curves
    tar_target(figs_anova_pow_curves, plot_anova_power_curves()),
    tar_target(s_apw_2, gs(figs_anova_pow_curves$anova_power_curves, pref11, 3, 3)),
    tar_target(s_apw_3, gs(figs_anova_pow_curves$anova_size_curves, pref11, 3, 3)),
    
    # Worked example
    tar_target(figs_tumour_ex, plot_tumour_example(tumour)),
    tar_target(s_tuex_1, gs(figs_tumour_ex$tumour_data, pref11, 3, 3)),
    tar_target(s_tuex_2, gs(figs_tumour_ex$tumour_sd, pref11, 3, 3)),
    tar_target(s_tuex_3, gs(figs_tumour_ex$tumour_power, pref11, 3, 3)),
    tar_target(s_tuex_4, gs(figs_tumour_ex$tumour_anova, pref11, 3, 3))
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}