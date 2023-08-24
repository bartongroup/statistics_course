
lecture_07 <- function() {
  
  init <- tar_plan(
    path07 = "figures/07_contingency_tables"
  )

  read_data <- tar_plan(
  )
  
  comp <- tar_plan(
    pip_dat = make_pipetting_data(),
    pip_sim = generate_pipetting(pip_dat),
    geissler = make_geissler_data(),
    flowcyt = make_flow_cytometry_data()
  )
  
  make_figures <- tar_plan(
    # pipetting
    figs_pip = plot_pip_uni_nonuni(),
    s_pip_uni = gs(figs_pip$pipetting_uniform, path07, 3, 2.2),
    s_pip_nuni = gs(figs_pip$pipetting_nonuniform, path07, 3, 2.2),
    s_pip = plot_pipetting(pip_dat) |> gs(path07, 3.8, 2.5, "pipetting"),
    s_psim = plot_pipetting_sim(pip_sim) |> gs(path07, 5, 2, "pipetting_sim"),
    
    # chisq
    s_chid = plot_chisq_dist() |> gs(path07, 4, 3, "chisq_dist"),
    figs_chip =  plot_pipetting_chisq_dist(pip_sim),
    s_chip_1 = figs_chip$g1 |> gs(path07, 2, 2, "chi2_dist"),
    s_chip_2 = figs_chip$g2 |> gs(path07, 3.5, 2.5, "chi2_dist_theor"),
    s_chic = plot_chisq_cut(pip_dat) |> gs(path07, 3.5, 2.5, "chi2_dist_cut"),
    
    # Geissler
    figs_gei = plot_geissler(geissler),
    s_gei_1 = gs(figs_gei$geissler, path07, 4, 3.5),
    s_gei_2 = gs(figs_gei$geissler_biom, path07, 4, 4),
    s_bn_ex = plot_binomial_example() |> gs(path07, 3.5, 2.5, "binomial_example"),
    
    # cytometry
    s_fc = plot_flow_cytometry(flowcyt) |> gs(path07, 4, 3, "flow_cytometry")
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
