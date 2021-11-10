
lecture_06 <- function() {
  
  init <- list(
    tar_target(pref06, "figures/06_hypothesis_testing")
  )

  read_data <- list(
    tar_target(mice2, read_tsv("data/mice_2samples.txt"))
  )
  
  comp <- list(
    tar_target(mouse_delta, generate_mouse_delta())
  )
  
  make_figures <- list(
    # intro plot
    tar_target(s_es_mice, plot_mice_box(mice2, with.means=TRUE, with.boxes=FALSE, cex=1) %>% gs(pref06, 2.1, 3.5, "mice_eng_sco")),
    
    # null distribution
    tar_target(fig_null, plot_distribution_cut(mouse_delta, brks=seq(-10, 10, 0.1), xlab="Body mass difference (g)")),
    tar_target(fig_null_cut, plot_distribution_cut(mouse_delta, cut=5, brks=seq(-10, 10, 0.1), xlab="Body mass difference (g)")),
    tar_target(s_mn_1, fig_null %>% gs(pref06, 4, 3, "mouse_null")),
    tar_target(s_mn_2, fig_null %>% gs(pref06, 3, 2.25, "mouse_null_medium")),
    tar_target(s_mnc_1, fig_null_cut %>% gs(pref06, 4, 3, "mouse_null_cut")),
    tar_target(s_mnc_2, fig_null_cut %>% gs(pref06, 3, 2.25, "mouse_null_cut_medium")),
    tar_target(s_mnc_3, (fig_null_cut + theme_d) %>% gs(pref06, 2, 2, "mouse_null_cut_small")),
    
    # hypergeometric
    tar_target(figs_hyper, plot_hyper()),
    tar_target(s_hyp_1, gs(figs_hyper$hyper, pref06, 3.2, 2)),
    tar_target(s_hyp_2, gs(figs_hyper$hyper8, pref06, 3.2, 2)),
    tar_target(s_hyp_3, gs(figs_hyper$hyper2, pref06, 3.2, 2)),
    tar_target(s_tea, plot_tea() %>% gs(pref06, 3.2, 2, "tea_tasting")),
    tar_target(figs_tea_fisher, plot_tea_fisher()),
    tar_target(s_tf_1, gs(figs_tea_fisher$fisher_bristol_1, pref06, 2.8, 1.8)),
    tar_target(s_tf_2, gs(figs_tea_fisher$fisher_bristol_2, pref06, 2.8, 1.8))
    
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}