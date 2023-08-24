
lecture_02 <- function() {
  
  init <- tar_plan(
    path02 = "figures/02_errors_and_estimators"
  )

  read_data <- tar_plan(
    gene_expr = read_tsv("data/WT_lev.tsv", col_names = c("gene_id", glue::glue("rep_{1:48}")))
    
  )
  
  comp <- tar_plan(
    samp = generate_sampling_distributions(seed = 1492, ns = c(5, 30))
  )
  
  make_figures <- tar_plan(
    # random measurement error
    fig_mini_normal = plot_mini_normal(),
    sav_mini_normal = gs(fig_mini_normal, path02, 2, 2),
    
    # counting error
    figs_poiss2d = plot_counting_2d(seed = 154, n = 200, nd = 5),
    sav_poiss2d_1 = gs(figs_poiss2d$poiss2d, path02, 5, 5),
    sav_poiss2d_2 = gs(figs_poiss2d$poiss2d_grid, path02, 5, 5),
    sav_poiss2d_3 = gs(figs_poiss2d$poiss2d_grid_count, path02, 5, 5),
    fig_counting_error = plot_counting_error(),
    sav_counting_error = gs(fig_counting_error, path02, 4, 4.5),
    
    # Dundee murders
    figs_murders = murder_plots(),
    sav_murder_1 = gs(figs_murders$murder_1, path02, 3, 3),
    sav_murder_2 = gs(figs_murders$murder_2, path02, 5, 3),
    
    # Population and sample
    fig_population_sample = plot_population_and_sample(),
    sav_population_sample = gs(fig_population_sample, path02, 3, 3.5),
    
    # Median
    fig_median = plot_median(),
    sav_median = gs(fig_median, path02, 3.5, 3.5),
    
    # Standard deviation
    fig_sd = plot_standard_deviation(),
    sav_sd = gs(fig_sd, path02, 3.5, 3),
    
    # Sampling distribution
    fig_population_sample_dist = plot_pop_sammean(samp, 5),
    sav_pop_sam_dist = gs(fig_population_sample_dist, path02, 2.5, 5),
    
    fig_sampling_mean5 = plot_sampling_mean(samp$`5`$samples, seed = 1007, bins = 150),
    sav_sampling_mean5 = gs(fig_sampling_mean5, path02, 4, 4.5),
    fig_sampling_mean30 =  plot_sampling_mean(samp$`30`$samples, seed = 13, cex = 1, size = 0.5, bins = 300),
    sav_sampling_mean30 = gs(fig_sampling_mean30, path02, 4, 4.5),
    
    fig_sampling_mean_comp = plot_sampling_mean_comparison(samp),
    sav_sampling_mean_comp = gs(fig_sampling_mean_comp, path02, 9, 6),
    
    # correlation
    fig_correlation = plot_correlation_examples(),
    sav_correlation=  gs(fig_correlation, path02, 7, 2.5)
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
