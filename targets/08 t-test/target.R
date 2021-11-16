
lecture_08 <- function() {
  
  init <- list(
    tar_target(pref08, "figures/08_t_test")
  )

  read_data <- list(
  )
  
  comp <- list(
    tar_target(t_samp_1, srnorm(1878, n=8, mean=25, sd=4)),
    tar_target(z_samp_1, znorm(878, n=12, SE=0.8, M0=20, Z=2)),
    tar_target(z_samp_2, znorm(23, n=12, SE=3, M0=20, Z=-2)),
    
    tar_target(t_nulls, generate_null_distributions()),
    tar_target(t_norms, generate_normality_distributions()),
    
    tar_target(samp_p5, generate_p5()),
    
    tar_target(t2_tests, generate_two_sample_t_test()),
    tar_target(t2_var, generate_t_test_variance()),
    
    tar_target(paired, make_paired_t_data()),
    tar_target(F_dist, generate_f_null())
  )
  
  make_figures <- list(
    # one sample t-test
    tar_target(s_t1s, plot_one_sample(t_samp_1, 20, limits=c(15,35)) %>% gs(pref08, 2, 3.5, "one-sided_t-test")),
    tar_target(s_t1z, plot_one_sample(z_samp_1, 20, limits=c(0,40), with.point=TRUE) %>% gs(pref08, 1.8, 3, "two_SE_1.png")),
    tar_target(s_t2z, plot_one_sample(z_samp_2, 20, limits=c(0,40), with.point=TRUE) %>% gs(pref08, 1.8, 3, "two_SE_2.png")),
    
    # t distribution
    tar_target(s_tan, plot_t_animation() %>% anim_save(filename = file.path(pref08, "t-dist_anim.gif"), animation = .)),
    tar_target(s_tnul, plot_t_nulls(t_nulls) %>% gs(pref08, 7.5, 6, "deviation_mean_null")),
    
    tar_target(figs_tcuts, plot_t_with_cuts(c(19.5, 26.7, 24.5, 21.9, 22.0))),
    tar_target(s_tc1, gs(figs_tcuts$t_dist, pref08, 2.5, 2)),
    tar_target(s_tc2, gs(figs_tcuts$t_dist_cut1, pref08, 2.5, 2)),
    tar_target(s_tc3, gs(figs_tcuts$t_dist_cut2, pref08, 2.5, 2)),
    
    tar_target(s_tnrm, plot_normality_t(t_norms) %>% gs(pref08, 8, 6, "normality_t_test")),
    
    # test vs CI
    tar_target(s_tvci, plot_ci_vs_test(samp_p5) %>% gs(pref08, 3.5, 3.5, "CI_vs_test_5")),
    
    # two-sample t-test
    tar_target(figs_t_null, plot_t2_null(t2_tests)),
    tar_target(s_tn1, gs(figs_t_null$two_sample_tdist, pref08, 3.5, 3)),
    tar_target(s_tn1t, gs(figs_t_null$two_sample_tdist_t, pref08, 3.5, 3)),
    tar_target(s_mb, plot_mice_box(mice2, with.means=TRUE, with.boxes=FALSE, cex=0.35) %>% gs(pref08, 2.5, 3.5, "mouse_eng_sco")),
    
    # variance
    tar_target(figs_tvar, plot_t2_var(mice2)),
    tar_target(s_tvar1, gs(figs_tvar$t_eq, pref08, 2.5, 2)),
    tar_target(s_tvar2, gs(figs_tvar$t_ne, pref08, 2.5, 2)),
    tar_target(s_tvard, plot_t2_var_dist(t2_var) %>% gs(pref08, 3.5, 6, "ttest_variance")),
    
    # p-values effect size
    tar_target(s_peff, plot_pval_effect() %>% gs(pref08, 4, 3, "ttest_pvalue_effect")),
    tar_target(s_pci, plot_ci_conf() %>% gs(pref08, 6, 3, "ttest2_p_ci")),
    
    # paired t-test
    tar_target(figs_paired_t, plot_paired_test(paired)),
    tar_target(s_tpr_1, gs(figs_paired_t$ttest_unpaired, pref08, 2.5, 3.5)),
    tar_target(s_tpr_2, gs(figs_paired_t$ttest_paired, pref08, 2.5, 3.5)),
    
    # F test
    tar_target(figs_f_tests, plot_f_tests(F_dist)),
    tar_target(s_f_1, gs(figs_f_tests$ftest_distribution, pref08, 4, 3)),
    tar_target(s_f_2, gs(figs_f_tests$ftest_distribution_line, pref08, 4, 3)),
    tar_target(s_fdist, plot_f_dist(mice2) %>% gs(pref08, 3, 2.5, "ftest_cut"))
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}