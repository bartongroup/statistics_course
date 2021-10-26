
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
    tar_target(sav_mini_normal, gs(pref02, fig_mini_normal, 2, 2)),
    
    # counting error
    tar_target(fig_counting_error, plot_counting_error()),
    tar_target(sav_counting_error, gs(pref02, fig_counting_error, 4, 4.5)),
    
    # Dundee murders
    tar_target(figs_murders, murder_plots()),
    tar_target(sav_murder_1, gs(pref02, figs_murders$murder_1, 3, 3)),
    tar_target(sav_murder_2, gs(pref02, figs_murders$murder_2, 3, 3)),
    
    # Population and sample
    tar_target(fig_population_sample, plot_population_and_sample()),
    tar_target(sav_population_sample, gs(pref02, fig_population_sample, 3, 3.5)),
    
    # Median
    tar_target(fig_median, plot_median()),
    tar_target(sav_median, gs(pref02, fig_median, 3.5, 3.5)),
    
    # Standard deviation
    tar_target(fig_sd, plot_standard_deviation()),
    tar_target(sav_sd, gs(pref02, fig_sd, 3.5, 3)),
    
    # Sampling distribution
    tar_target(fig_population_sample_dist, plot_pop_sammean(samp, 5)),
    tar_target(sav_pop_sam_dist, gs(pref02, fig_population_sample_dist, 8, 4)),
    
    tar_target(fig_sampling_mean5,  plot_sampling_mean(samp$`5`$samples, seed=1007, bins=150)),
    tar_target(sav_sampling_mean5, gs(pref02, fig_sampling_mean5, 4, 4.5)),
    tar_target(fig_sampling_mean30,  plot_sampling_mean(samp$`30`$samples, seed=13, cex=1, size=0.5, bins=300)),
    tar_target(sav_sampling_mean30, gs(pref02, fig_sampling_mean30, 4, 4.5)),
    
    tar_target(fig_sampling_mean_comp, plot_sampling_mean_comparison(samp)),
    tar_target(sav_sampling_mean_comp, gs(pref02, fig_sampling_mean_comp, 9, 6)),
    
    # correlation
    tar_target(fig_correlation, plot_correlation_examples()),
    tar_target(sav_correlation, gs(pref02, fig_correlation, 7, 2.5))
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}