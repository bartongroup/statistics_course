
lecture_02 <- function() {
  
  init <- list(
    tar_target(pref02, "figures/02_errors_and_estimators")
  )

  read_data <- list(
    tar_target(gene_expr, read_tsv("data/WT_lev.tsv", col_names=c("gene_id", glue::glue("rep_{1:48}"))))
    
  )
  
  comp <- list(
    tar_target(samp, generate_sampling_distributions(seed=1492, ns=c(5, 30)))
  )
  
  make_figures <- list(
    # random measurement error
    tar_target(fig_mini_normal, plot_mini_normal()),
    tar_target(sav_mini_normal, gs(fig_mini_normal, pref02, 2, 2)),
    
    # counting error
    tar_target(figs_poiss2d, plot_counting_2d(seed=154, n=200, nd=5)),
    tar_target(sav_poiss2d_1, gs(figs_poiss2d$poiss2d, pref02, 5, 5)),
    tar_target(sav_poiss2d_2, gs(figs_poiss2d$poiss2d_grid, pref02, 5, 5)),
    tar_target(sav_poiss2d_3, gs(figs_poiss2d$poiss2d_grid_count, pref02, 5, 5)),
    tar_target(fig_counting_error, plot_counting_error()),
    tar_target(sav_counting_error, gs(fig_counting_error, pref02, 4, 4.5)),
    
    # Dundee murders
    tar_target(figs_murders, murder_plots()),
    tar_target(sav_murder_1, gs(figs_murders$murder_1, pref02, 3, 3)),
    tar_target(sav_murder_2, gs(figs_murders$murder_2, pref02, 3, 3)),
    
    # Population and sample
    tar_target(fig_population_sample, plot_population_and_sample()),
    tar_target(sav_population_sample, gs(fig_population_sample, pref02, 3, 3.5)),
    
    # Median
    tar_target(fig_median, plot_median()),
    tar_target(sav_median, gs(fig_median, pref02, 3.5, 3.5)),
    
    # Standard deviation
    tar_target(fig_sd, plot_standard_deviation()),
    tar_target(sav_sd, gs(fig_sd, pref02, 3.5, 3)),
    
    # Sampling distribution
    tar_target(fig_population_sample_dist, plot_pop_sammean(samp, 5)),
    tar_target(sav_pop_sam_dist, gs(fig_population_sample_dist, pref02, 8, 4)),
    
    tar_target(fig_sampling_mean5,  plot_sampling_mean(samp$`5`$samples, seed=1007, bins=150)),
    tar_target(sav_sampling_mean5, gs(fig_sampling_mean5, pref02, 4, 4.5)),
    tar_target(fig_sampling_mean30,  plot_sampling_mean(samp$`30`$samples, seed=13, cex=1, size=0.5, bins=300)),
    tar_target(sav_sampling_mean30, gs(fig_sampling_mean30, pref02, 4, 4.5)),
    
    tar_target(fig_sampling_mean_comp, plot_sampling_mean_comparison(samp)),
    tar_target(sav_sampling_mean_comp, gs(fig_sampling_mean_comp, pref02, 9, 6)),
    
    # correlation
    tar_target(fig_correlation, plot_correlation_examples()),
    tar_target(sav_correlation, gs(fig_correlation, pref02, 7, 2.5))
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}