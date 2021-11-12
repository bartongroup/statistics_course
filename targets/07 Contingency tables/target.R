
lecture_07 <- function() {
  
  init <- list(
    tar_target(pref07, "figures/07_contingency_tables")
  )

  read_data <- list(
  )
  
  comp <- list(
    tar_target(pip_dat, make_pipetting_data()),
    tar_target(pip_sim, generate_pipetting(pip_dat)),
    tar_target(geissler, make_geissler_data()),
    tar_target(flowcyt, make_flow_cytometry_data())
  )
  
  make_figures <- list(
    # pipetting
    tar_target(figs_pip, plot_pip_uni_nonuni()),
    tar_target(s_pip_uni, gs(figs_pip$pipetting_uniform, pref07, 3, 2.2)),
    tar_target(s_pip_nuni, gs(figs_pip$pipetting_nonuniform, pref07, 3, 2.2)),
    tar_target(s_pip, plot_pipetting(pip_dat) %>% gs(pref07, 3.8, 2.5, "pipetting")),
    tar_target(s_psim, plot_pipetting_sim(pip_sim) %>% gs(pref07, 5, 2, "pipetting_sim")),
    
    # chisq
    tar_target(s_chid, plot_chisq_dist() %>% gs(pref07, 4, 3, "chisq_dist")),
    tar_target(s_chip, plot_pipetting_chisq_dist(pip_sim) %>% gs(pref07, 3.5, 2.5, "chi2_dist")),
    tar_target(s_chic, plot_chisq_cut(pip_dat) %>% gs(pref07, 3.5, 2.5, "chi2_dist_cut")),
    
    # Geissler
    tar_target(figs_gei, plot_geissler(geissler)),
    tar_target(s_gei_1, gs(figs_gei$geissler, pref07, 4, 3.5)),
    tar_target(s_gei_2, gs(figs_gei$geissler_biom, pref07, 4, 4)),
    tar_target(s_bn_ex, plot_binomial_example() %>% gs(pref07, 3.5, 2.5, "binomial_example")),
    
    # cytometry
    tar_target(s_fc, plot_flow_cytometry(flowcyt) %>% gs(pref07, 4, 3, "flow_cytometry"))
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}