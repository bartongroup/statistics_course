
lecture_08 <- function() {
  
  init <- tar_plan(
    path08 = "figures/08_t_test"
  )

  read_data <- tar_plan(
  )
  
  comp <- tar_plan(
    t_samp_1 = srnorm(1878, n = 8, mean = 25, sd = 4),
    z_samp_1 = znorm(878, n = 12, SE = 0.8, M0 = 20, Z = 2),
    z_samp_2 = znorm(23, n = 12, SE = 3, M0 = 20, Z = -2),
    
    t_nulls = generate_null_distributions(),
    t_norms = generate_normality_distributions(),
    
    samp_p5 = generate_p5(),
    
    t2_tests = generate_two_sample_t_test(),
    t2_var = generate_t_test_variance(),
    
    paired = make_paired_t_data(),
    F_dist = generate_f_null()
  )
  
  make_figures <- tar_plan(
    # one sample t-test
    s_t1s = plot_one_sample(t_samp_1, 20, limits = c(15,35)) |> gs(path08, 2, 3.5, "one-sided_t-test"),
    s_t1z = plot_one_sample(z_samp_1, 20, limits = c(0,40), with.point = TRUE) |> gs(path08, 1.8, 3, "two_SE_1.png"),
    s_t2z = plot_one_sample(z_samp_2, 20, limits = c(0,40), with.point = TRUE) |> gs(path08, 1.8, 3, "two_SE_2.png"),
    
    # t distribution
    anim_t = plot_t_animation(),
    s_tan = anim_save(filename = file.path(path08, "t-dist_anim.gif"), animation = anim_t),
    s_tnul = plot_t_nulls(t_nulls) |> gs(path08, 7.5, 6, "deviation_mean_null"),
    
    figs_tcuts = plot_t_with_cuts(c(19.5, 26.7, 24.5, 21.9, 22.0)),
    s_tc1 = gs(figs_tcuts$t_dist, path08, 2.5, 2),
    s_tc2 = gs(figs_tcuts$t_dist_cut1, path08, 2.5, 2),
    s_tc3 = gs(figs_tcuts$t_dist_cut2, path08, 2.5, 2),
    
    s_tnrm = plot_normality_t(t_norms) |> gs(path08, 8, 6, "normality_t_test"),
    
    # test vs CI
    s_tvci = plot_ci_vs_test(samp_p5) |> gs(path08, 3.5, 3.5, "CI_vs_test_5"),
    
    # two-sample t-test
    figs_t_null = plot_t2_null(t2_tests),
    s_tn1 = gs(figs_t_null$two_sample_tdist, path08, 3.5, 3),
    s_tn1t = gs(figs_t_null$two_sample_tdist_t, path08, 3.5, 3),
    s_mb = plot_mice_box(mice2, with.means = TRUE, with.boxes = FALSE, cex = 0.35) |> gs(path08, 2.5, 3.5, "mouse_eng_sco"),
    
    # variance
    figs_tvar = plot_t2_var(mice2),
    s_tvar1 = gs(figs_tvar$t_eq, path08, 2.5, 2),
    s_tvar2 = gs(figs_tvar$t_ne, path08, 2.5, 2),
    s_tvard = plot_t2_var_dist(t2_var) |> gs(path08, 3.5, 6, "ttest_variance"),
    
    # p-values effect size
    s_peff = plot_pval_effect() |> gs(path08, 4, 3, "ttest_pvalue_effect"),
    s_pci = plot_ci_conf() |> gs(path08, 6, 3, "ttest2_p_ci"),
    
    # paired t-test
    figs_paired_t = plot_paired_test(paired),
    s_tpr_1 = gs(figs_paired_t$ttest_unpaired, path08, 2.5, 3.5),
    s_tpr_2 = gs(figs_paired_t$ttest_paired, path08, 2.5, 3.5),
    
    # F test
    figs_f_tests = plot_f_tests(F_dist),
    s_f_1 = gs(figs_f_tests$ftest_distribution, path08, 4, 3),
    s_f_2 = gs(figs_f_tests$ftest_distribution_line, path08, 4, 3),
    s_fdist = plot_f_dist(mice2) |> gs(path08, 3, 2.5, "ftest_cut")
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
}
