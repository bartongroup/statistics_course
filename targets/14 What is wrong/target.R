
lecture_14 <- function() {
  
  init <- list(
    tar_target(pref14, "figures/14_what_is_wrong")
  )

  read_data <- list(
    tar_target(strain_distance, read_strains("data/all_update.txt"))
  )
  
  comp <- list(
    tar_target(dist_97_3, generate_two_dist(n1=97000, n2=3000)),
    tar_target(p_97_3, sampling_two_dist(dist_97_3)),
    tar_target(dist_50_50, generate_two_dist(n1=50000, n2=50000)),
    tar_target(p_50_50, sampling_two_dist(dist_50_50)),
    tar_target(dist_100, generate_two_dist(n1=0, n2=100000, M2=25)),
    tar_target(p_100_3, sampling_two_dist(dist_100, n=3)),
    tar_target(p_100_10, sampling_two_dist(dist_100, n=10)),
    
    tar_target(eff_size, generate_effect_size())
  )
  
  make_figures <- list(
    # 97/3
    tar_target(s_2md, plot_mix_dist(dist_97_3, breaks = seq(0, 50, 0.5)) %>% gs(pref14, 3, 3, "mice_dist_97_3")),
    tar_target(s_2mp, plot_mix_p(p_97_3, ymax=5) %>% gs(pref14, 3, 3, "mice_p_97_3")),
    tar_target(s_2mpz, plot_mix_p(p_97_3, ymax=5, x.lim=c(0, 0.05), bins=0.001, x.breaks=seq(0, 0.05, 0.01)) %>% gs(pref14, 3, 3, "mice_p_97_3_zoom")),
    
    # 50/50
    tar_target(s_2md5, plot_mix_dist(dist_50_50, breaks = seq(0, 50, 0.5), alpha=0.6) %>% gs(pref14, 3, 3, "mice_dist_50_50")),
    tar_target(s_2mp5, plot_mix_p(p_50_50, ymax=60, x.lim=c(0, 0.05), bins=0.001, x.breaks=seq(0, 0.05, 0.01)) %>% gs(pref14, 3, 3, "mice_p_50_50")),
    tar_target(s_2mp5z, plot_mix_p(p_50_50, ymax=3, x.lim=c(0.04, 0.06), bins=0.001, x.breaks=seq(0, 0.1, 0.005)) %>% gs(pref14, 3, 3, "mice_p_50_50_zoom")),
    
    # reliability
    tar_target(s_rel_1, plot_mix_p(p_100_3, ymax=4, x.lim=c(0, 1), bins=0.01, x.breaks=seq(0, 1, 0.2)) %>% gs(pref14, 3, 3, "mice_p_n3")),
    tar_target(s_rel_2, plot_mix_p(p_100_10, ymax=50, x.lim=c(0, 1), bins=0.01, x.breaks=seq(0, 1, 0.2)) %>% gs(pref14, 3, 3, "mice_p_n10")),
    
    # effect size
    tar_target(figs_efs, plot_effect_sizes(eff_size)),
    tar_target(s_ef_1, gs(figs_efs$effect_size_M, pref14, 3, 3)),
    tar_target(s_ef_2, gs(figs_efs$effect_size_p, pref14, 3, 3)),
    
    # large sample
    tar_target(figs_ls, plot_strain_dist(strain_distance)),
    tar_target(s_ls_1, gs(figs_ls$large_sample_shitplot, pref14, 2, 3)),
    tar_target(s_ls_2, gs(figs_ls$large_sample, pref14, 2.5, 3))
  )
  
  make_tables <- list(
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures,
    make_tables
  )
  
  
}