
lecture_10 <- function() {
  
  init <- list(
    tar_target(path10, "figures/10_nonparametric_tests")
  )

  read_data <- list(
   tar_target(velos_scores, read_scores()),
   tar_target(mice_lifespan4, read_mice_lifespan("data/mice_kruskal.txt"))
  )
  
  comp <- list(
    tar_target(par_nonpar, make_param_nonparam()),
    tar_target(mice_lifespan, make_lifespan_data()),
    tar_target(mice_u_diff, make_u_diff(mice_lifespan)),
    tar_target(udist, generate_udist(nx=7, ny=6, nsim=100000)),
    tar_target(udist_small, generate_udist(nx=3, ny=3, nsim=100000)),
    tar_target(apgar, make_apgar_scores()),
    tar_target(before_after, make_before_after()),
    tar_target(tab_wilcox, wilcox_table(before_after)),
    tar_target(wdist, generate_wdist(nx=8, nsim=100000)),
    tar_target(hdist, generate_hdist(n=c(12, 9, 8, 5), nsim=100000)),
    tar_target(ddist, generate_ddist(nx=12, ny=9, nsim=100000)),
    tar_target(ddist_small, generate_ddist(nx=3, ny=3, nsim=10000))
    
  )
  
  make_figures <- list(
    # parametric vs non-parametric
    tar_target(s_p_np, plot_param_nonparam(par_nonpar) |> gs(path10, 3, 3, "parametric_nonparametric")),
    
    # Mann-Whitney
    tar_target(figs_mann_u, plot_mann_u(mice_lifespan)),
    tar_target(s_mu_1, gs(figs_mann_u$lifespan_u_0, path10, 2.5, 3)),
    tar_target(s_mu_2, gs(figs_mann_u$lifespan_u_1, path10, 2.5, 3)),
    tar_target(s_mu_3, gs(figs_mann_u$lifespan_u, path10, 2.5, 3)),
    
    tar_target(figs_urange, plot_u_range(mice_u_diff, mice_lifespan)),
    tar_target(s_ux, gs(figs_urange$U_example, path10, 3.5, 3.5)),
    tar_target(s_uxu, gs(figs_urange$U_example_withu, path10, 3.5, 3.5)),
    tar_target(s_ux1, gs(figs_urange$U_example_1, path10, 1.5, 2)),
    tar_target(s_ux2, gs(figs_urange$U_example_2, path10, 1.5, 2)),
    tar_target(s_ux3, gs(figs_urange$U_example_3, path10, 1.5, 2)),
    
    tar_target(figs_udist, plot_udist(udist, 7, 6)),
    tar_target(s_ud_1, gs(figs_udist$udist, path10, 3.5, 3)),
    tar_target(s_ud_2, gs(figs_udist$udist_norm, path10, 3.5, 3)),
    tar_target(s_ud_3, gs(figs_udist$udist_cut, path10, 3.5, 3)),
    tar_target(figs_udist_small, plot_udist(udist_small, 3, 3, xmax=9, ymax=0.40)),
    tar_target(s_uds, gs(figs_udist_small$udist_norm, path10, 3.5, 3, "udist_small")),
    tar_target(s_uxs, plot_udist_small_example() |> gs(path10, 2, 3, "lifespan_small")),
    
    # comparison to t-test
    tar_target(s_mw_t, plot_lifespan_param_rank(mice_lifespan) |> gs(path10, 3, 3, "MW_vs_ttest")),
    
    # good for
    tar_target(mw_test_velos, wilcox.test(value ~ name, data=velos_scores, alternative = "less", correct = FALSE)),
    tar_target(s_mw_sc, plot_mw_scores(velos_scores) |> gs(path10, 6, 4, "velos_scores")),
    tar_target(mw_test_apgar, wilcox.test(score ~ sample, data=apgar, alternative="less")),
    tar_target(s_ap, plot_apgar_scores(apgar) |> gs(path10, 2.5, 2.5, "apgar")),
    
    # paired test
    tar_target(s_prt, plot_paired(before_after) |> gs(path10, 2.3, 3.5, "paired_data")),
    tar_target(figs_wrange, plot_w_range(before_after)),
    tar_target(s_w1, gs(figs_wrange$W_plot, path10, 3, 3)),
    tar_target(s_w2, gs(figs_wrange$W_plot_1, path10, 2, 2.5)),
    tar_target(s_w3, gs(figs_wrange$W_plot_2, path10, 2, 2.5)),
    tar_target(s_w4, gs(figs_wrange$W_plot_3, path10, 2, 2.5)),
    
    tar_target(figs_wdist, plot_wdist(wdist, 8)),
    tar_target(s_wd_1, gs(figs_wdist$w_dist, path10, 3, 3)),
    tar_target(s_wd_2, gs(figs_wdist$w_dist_norm, path10, 3, 3)),
    tar_target(s_wd_3, gs(figs_wdist$w_dist_norm_cut, path10, 3, 3)),
    
    # Kruskal
    tar_target(s_vr, plot_value_rank(mice_lifespan4) |> gs(path10, 3.3, 3.5, "value_rank")),
    tar_target(s_ak, plot_anova_kruskal(mice_lifespan4) |> gs(path10, 6, 3.5, "anova_kruskal")),
    tar_target(s_m2r, plot_rank(reduce_mice(mice_lifespan4)) |> gs(path10, 3, 0.7, "mice2_rank")),
    tar_target(s_m4r, (plot_rank(mice_lifespan4) + scale_y_continuous(expand=c(0,0.6))) |> gs(path10, 5, 0.95, "mice4_rank")),
    
    # Kruskal variance
    tar_target(s_kv, plot_variance(mice_lifespan4, within=FALSE, what="Rank", ylab="Rank") |> gs(path10, 3.5, 3, "rank_variance_between")),
    tar_target(s_hdc, plot_hdist_cut(hdist, mice_lifespan4) |> gs(path10, 3.5, 3, "H_dist_cut")),
    
    # Kolmogorov-Smirnov
    tar_target(s_cum, plot_cumsum(mice_lifespan4, vars="Scottish") |> gs(path10, 3.5, 3, "cumsum1")),
    tar_target(figs_ks_es, plot_ks_2(mice_lifespan4, c("English", "Scottish"))),
    tar_target(s_ks_es, gs(figs_ks_es$plot, path10, 5, 2.5, "ks_eng_sco")),
    tar_target(figs_ks_en, plot_ks_2(mice_lifespan4, c("English", "N.Irish"))),
    tar_target(s_ks_en, gs(figs_ks_en$plot, path10, 5, 2.5, "ks_eng_ni")),
    
    tar_target(s_d2d, plot_cumsum(mice_lifespan4, vars=c("English", "Scottish")) |> gs(path10, 3, 2.5, "ks_gen_example_cum")),
    tar_target(figs_ddist, plot_Ddist(ddist, 12, 9, d.cut=0.42)),
    tar_target(s_ks_1, gs(figs_ddist$ks_dist, path10, 3, 2.5)),
    tar_target(s_ks_2, gs(figs_ddist$ks_dist_d, path10, 3, 2.5)),
    tar_target(s_ks_3, gs(figs_ddist$ks_dist_d_cut, path10, 3, 2.5)),
    
    tar_target(figs_ks_loc_shape, plot_ks_loc_shape()),
    tar_target(s_ksloc, gs(figs_ks_loc_shape$ks_location, path10, 5, 2.5)),
    tar_target(s_ksshp, gs(figs_ks_loc_shape$ks_shape, path10, 5, 2.5)),
    
    tar_target(figs_ddist_small, plot_Ddist(ddist_small, 3, 3, d.cut=0.42, ymax=2.5)),
    tar_target(s_kss_1, gs(figs_ddist_small$ks_dist_d + scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1)), path10, 3, 2.5, "ks_dist_small"))
    
    
    
    
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
