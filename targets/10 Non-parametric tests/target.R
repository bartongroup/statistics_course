
lecture_10 <- function() {
  
  init <- tar_plan(
    path10 = "figures/10_nonparametric_tests"
  )

  read_data <- tar_plan(
   velos_scores = read_scores(),
   mice_lifespan4 = read_mice_lifespan("data/mice_kruskal.txt")
  )
  
  comp <- tar_plan(
    par_nonpar = make_param_nonparam(),
    mice_lifespan = make_lifespan_data(),
    mice_u_diff = make_u_diff(mice_lifespan),
    udist = generate_udist(nx = 7, ny = 6, nsim = 100000),
    udist_small = generate_udist(nx = 3, ny = 3, nsim = 100000),
    apgar = make_apgar_scores(),
    before_after = make_before_after(),
    tab_wilcox = wilcox_table(before_after),
    wdist = generate_wdist(nx = 8, nsim = 100000),
    hdist = generate_hdist(n = c(12, 9, 8, 5), nsim = 100000),
    ddist = generate_ddist(nx = 12, ny = 9, nsim = 100000),
    ddist_small = generate_ddist(nx = 3, ny = 3, nsim = 10000)
    
  )
  
  make_figures <- tar_plan(
    # parametric vs non-parametric
    s_p_np = plot_param_nonparam(par_nonpar) |> gs(path10, 3, 3, "parametric_nonparametric"),
    
    # Mann-Whitney
    figs_mann_u = plot_mann_u(mice_lifespan),
    s_mu_1 = gs(figs_mann_u$lifespan_u_0, path10, 2.5, 3),
    s_mu_2 = gs(figs_mann_u$lifespan_u_1, path10, 2.5, 3),
    s_mu_3 = gs(figs_mann_u$lifespan_u, path10, 2.5, 3),
    
    figs_urange = plot_u_range(mice_u_diff, mice_lifespan),
    s_ux = gs(figs_urange$U_example, path10, 3.5, 3.5),
    s_uxu = gs(figs_urange$U_example_withu, path10, 3.5, 3.5),
    s_ux1 = gs(figs_urange$U_example_1, path10, 1.5, 2),
    s_ux2 = gs(figs_urange$U_example_2, path10, 1.5, 2),
    s_ux3 = gs(figs_urange$U_example_3, path10, 1.5, 2),
    
    figs_udist = plot_udist(udist, 7, 6),
    s_ud_1 = gs(figs_udist$udist, path10, 3.5, 3),
    s_ud_2 = gs(figs_udist$udist_norm, path10, 3.5, 3),
    s_ud_3 = gs(figs_udist$udist_cut, path10, 3.5, 3),
    figs_udist_small = plot_udist(udist_small, 3, 3, xmax = 9, ymax = 0.40),
    s_uds = gs(figs_udist_small$udist_norm, path10, 3.5, 3, "udist_small"),
    s_uxs = plot_udist_small_example() |> gs(path10, 2, 3, "lifespan_small"),
    s_uda = gs(plot_udist_small_all(), path10, 6, 6, "udist_small_all"),
    
    # comparison to t-test
    s_mw_t = plot_lifespan_param_rank(mice_lifespan) |> gs(path10, 3, 3, "MW_vs_ttest"),
    
    # good for
    mw_test_velos = wilcox.test(value ~ name, data = velos_scores, alternative = "less", correct = FALSE),
    s_mw_sc = plot_mw_scores(velos_scores) |> gs(path10, 6, 4, "velos_scores"),
    mw_test_apgar = wilcox.test(score ~ sample, data = apgar, alternative = "less"),
    s_ap = plot_apgar_scores(apgar) |> gs(path10, 2.5, 2.5, "apgar"),
    
    # paired test
    s_prt = plot_paired(before_after) |> gs(path10, 2.3, 3.5, "paired_data"),
    figs_wrange = plot_w_range(before_after),
    s_w1 = gs(figs_wrange$W_plot, path10, 3, 3),
    s_w2 = gs(figs_wrange$W_plot_1, path10, 2, 2.5),
    s_w3 = gs(figs_wrange$W_plot_2, path10, 2, 2.5),
    s_w4 = gs(figs_wrange$W_plot_3, path10, 2, 2.5),
    
    figs_wdist = plot_wdist(wdist, 8),
    s_wd_1 = gs(figs_wdist$w_dist, path10, 3, 3),
    s_wd_2 = gs(figs_wdist$w_dist_norm, path10, 3, 3),
    s_wd_3 = gs(figs_wdist$w_dist_norm_cut, path10, 3, 3),
    
    # Kruskal
    s_vr = plot_value_rank(mice_lifespan4) |> gs(path10, 3.3, 3.5, "value_rank"),
    s_ak = plot_anova_kruskal(mice_lifespan4) |> gs(path10, 6, 3.5, "anova_kruskal"),
    s_m2r = (plot_rank(reduce_mice(mice_lifespan4)) + scale_y_continuous(expand = c(0,0.5))) |> gs(path10, 3, 0.8, "mice2_rank"),
    s_m4r = (plot_rank(mice_lifespan4) + scale_y_continuous(expand = c(0,0.6))) |> gs(path10, 5, 0.95, "mice4_rank"),
    
    # Kruskal variance
    s_kv = plot_variance(mice_lifespan4, within = FALSE, what = "Rank", ylab = "Rank") |> gs(path10, 3.5, 3, "rank_variance_between"),
    s_hdc = plot_hdist_cut(hdist, mice_lifespan4) |> gs(path10, 3.5, 3, "H_dist_cut"),
    
    # Kolmogorov-Smirnov
    s_cum = plot_cumsum(mice_lifespan4, vars = "Scottish") |> gs(path10, 3.5, 3, "cumsum1"),
    figs_ks_es = plot_ks_2(mice_lifespan4, c("English", "Scottish")),
    s_ks_es = gs(figs_ks_es$plot, path10, 5, 2.5, "ks_eng_sco"),
    figs_ks_en = plot_ks_2(mice_lifespan4, c("English", "N.Irish")),
    s_ks_en = gs(figs_ks_en$plot, path10, 5, 2.5, "ks_eng_ni"),
    
    s_d2d = plot_cumsum(mice_lifespan4, vars = c("English", "Scottish")) |> gs(path10, 3, 2.5, "ks_gen_example_cum"),
    figs_ddist = plot_Ddist(ddist, 12, 9, d.cut = 0.42),
    s_ks_1 = gs(figs_ddist$ks_dist, path10, 3, 2.5),
    s_ks_2 = gs(figs_ddist$ks_dist_d, path10, 3, 2.5),
    s_ks_3 = gs(figs_ddist$ks_dist_d_cut, path10, 3, 2.5),
    
    figs_ks_loc_shape = plot_ks_loc_shape(),
    s_ksloc = gs(figs_ks_loc_shape$ks_location, path10, 5, 2.5),
    s_ksshp = gs(figs_ks_loc_shape$ks_shape, path10, 5, 2.5),
    
    figs_ddist_small = plot_Ddist(ddist_small, 3, 3, d.cut = 0.42, ymax = 2.5),
    s_kss_1 = gs(figs_ddist_small$ks_dist_d + scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1)), path10, 3, 2.5, "ks_dist_small")
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
