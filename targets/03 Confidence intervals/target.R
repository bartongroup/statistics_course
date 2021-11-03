
lecture_03 <- function() {
  
  init <- list(
    tar_target(pref03, "figures/03_confidence_intervals")
  )

  read_data <- list(

  )
  
  comp <- list(
    tar_target(samp_dists, generate_4_sampling_distributions()),
    tar_target(qt4, qt(0.975, 4))
  )
  
  make_figures <- list(
    # sampling distribution
    tar_target(f_ci_95_100, plot_95_ci() %>% gs(pref03, 6, 2.5, "ci_95_100")),
    tar_target(f_4_samp_dist, plot_4_sampling_distributions(samp_dists) %>% gs(pref03, 4, 4, "four_sampling_dist")),
    
    # CI of the mean
    tar_target(f_dist_ci_mean, plot_dist_ci(samp_dists$Mean, brks=seq(10, 30, 0.1))),
    tar_target(s_dist_ci_mean, f_dist_ci_mean %>% gs(pref03, 3.5, 3, "sampling_distributions_mean_cut")),
    tar_target(s_dist_ci_mean_small, (f_dist_ci_mean + theme_dist) %>% gs(pref03, 2, 1.5, "sampling_distributions_mean_cut_small")),
    tar_target(s_dist_ci_t, plot_dist_ci(samp_dists$t, brks=seq(-6, 6, 0.05)) %>% gs(pref03, 3.5, 3, "sampling_distributions_t_cut")),
    
    # t-distribution
    tar_target(s_t_cut, t_cut(qt4, 4)$t.two %>% gs(pref03, 3.5, 3, "t_distribution_cut")),
    tar_target(s_samp, plot_distribution_cut(samp_dists$Mean, cut=NULL, brks=seq(10,30,0.1), fill=fill.colour, xlab="") %>% gs(pref03, 3.5, 3, "sampling_distribution")),
    
    # R
    tar_target(s_dt, (plot_fun(dt, df=4, x.grid=seq(-5, 5, 0.01), cut.up = qt4) + theme_d) %>% gs(pref03, 2, 1.4, "dt")),
    tar_target(s_dt2, (plot_fun(dt, df=4, x.grid=seq(-5, 5, 0.01), cut.up = qt4, cut.lo = -qt4) + theme_d) %>%  gs(pref03, 2, 1.4, "dt2")),
    
    # SE
    tar_target(s_se_2, plot_err(seed=10, n=2) %>%  gs(pref03, 4, 3, "sd_se_ci_2")),
    tar_target(s_se_8, plot_err(seed=1112, n=8) %>% gs(pref03, 4, 3, "sd_se_ci_8")),
    tar_target(s_se_ci, plot_se_ci_comparison() %>% gs(pref03, 5, 3, "sd_se_ci_comparison")),
    tar_target(s_se_ci_lim, plot_se_ci_limits() %>% gs(pref03, 3.2, 4, "se_ci_limits")),
    
    # example
    tar_target(x_ci, example_ci()),
    tar_target(s_ci_1, gs(x_ci$ex1_no_ci, pref03, 3, 3.5)),
    tar_target(s_ci_2, gs(x_ci$ex1_with_ci, pref03, 3, 3.5)),
    
    # CI of the median
    tar_target(s_ci_med, plot_binomial_median_ci() %>% gs(pref03, 4.5, 1.8, "binomial_median_ci"))
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}