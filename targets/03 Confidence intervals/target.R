
lecture_03 <- function() {
  
  init <- tar_plan(
    path03 = "figures/03_confidence_intervals"
  )

  read_data <- tar_plan(

  )
  
  comp <- tar_plan(
    samp_dists = generate_4_sampling_distributions(),
    qt4 = qt(0.975, 4)
  )
  
  make_figures <- tar_plan(
    # sampling distribution
    f_ci_95_100 = plot_95_ci() |> gs(path03, 6, 2.5, "ci_95_100"),
    f_4_samp_dist = plot_4_sampling_distributions(samp_dists) |> gs(path03, 4, 4, "four_sampling_dist"),
    
    # CI of the mean
    f_dist_ci_mean = plot_dist_ci(samp_dists$Mean, brks=seq(10, 30, 0.1)),
    s_dist_ci_mean = f_dist_ci_mean |> gs(path03, 3.5, 3, "sampling_distributions_mean_cut"),
    s_dist_ci_mean_small = (f_dist_ci_mean + theme_dist) |> gs(path03, 2, 1.5, "sampling_distributions_mean_cut_small"),
    s_dist_ci_t = plot_dist_ci(samp_dists$t, brks=seq(-6, 6, 0.05)) |> gs(path03, 3.5, 3, "sampling_distributions_t_cut"),
    
    # t-distribution
    s_t_cut = t_cut(qt4, 4)$t.two |> gs(path03, 3.5, 3, "t_distribution_cut"),
    s_samp = plot_distribution_cut(samp_dists$Mean, cut=NULL, brks=seq(10,30,0.1), fill=fill.colour, xlab="") |> gs(path03, 3.5, 3, "sampling_distribution"),
    
    # R
    s_dt = (plot_fun(dt, df=4, x.grid=seq(-5, 5, 0.01), cut.up = qt4) + theme_d) |> gs(path03, 2, 1.4, "dt"),
    s_dt2 = (plot_fun(dt, df=4, x.grid=seq(-5, 5, 0.01), cut.up = qt4, cut.lo = -qt4) + theme_d) |>  gs(path03, 2, 1.4, "dt2"),
    
    # SE
    s_se_2 = plot_err(seed=10, n=2) |>  gs(path03, 4, 3, "sd_se_ci_2"),
    s_se_8 = plot_err(seed=1112, n=8) |> gs(path03, 4, 3, "sd_se_ci_8"),
    s_se_ci = plot_se_ci_comparison() |> gs(path03, 5, 3, "sd_se_ci_comparison"),
    s_se_ci_lim = plot_se_ci_limits() |> gs(path03, 3.2, 4, "se_ci_limits"),
    
    # example
    x_ci = example_ci(),
    s_ci_1 = gs(x_ci$ex1_no_ci, path03, 3, 3.5),
    s_ci_2 = gs(x_ci$ex1_with_ci, path03, 3, 3.5),
    
    # CI of the median
    s_ci_med = plot_binomial_median_ci() |> gs(path03, 4.5, 1.8, "binomial_median_ci")
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
