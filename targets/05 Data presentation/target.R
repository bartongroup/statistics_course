
lecture_05 <- function() {
  
  init <- list(
    tar_target(pref05, "figures/05_data_presentation")
  )

  read_data <- list(
    tar_target(WT_lev, read_tsv("data/WT_lev.tsv", col_names=FALSE)),
    tar_target(fastq, read_table("data/fastq_quality.txt"))
    
  )
  
  comp <- list(
  )
  
  make_figures <- list(
    # intro
    tar_target(s_gd, good_plot() %>% gs(pref05, 4.5, 3, "good_plot")),
    tar_target(s_showd, show_your_data() %>% gs(pref05, 7, 3, "show_your_data")),
    tar_target(s_lin_sym, lines_and_symbols() %>% gs(pref05, 7, 3, "lines_and_symbols")),
    
    # colour blindness
    tar_target(figs_cb, plot_colour_blind()),
    tar_target(s_cb_1, gs(figs_cb$colour_blind_1, pref05, 4, 3)),
    tar_target(s_cb_2, gs(figs_cb$colour_blind_2, pref05, 4, 3)),
    
    # logarithmic scale
    tar_target(s_ll_1, plot_loglin_1(WT_lev) %>% gs(pref05, 6, 5, "linear_vs_logarithmic_1")),
    tar_target(s_ll_2, plot_loglin_2() %>% gs(pref05, 6, 5, "linear_vs_logarithmic_2")),
    
    # error bars
    tar_target(s_erb, plot_errorbars() %>% gs(pref05, 9, 2.3, "error_bars")),
    
    # boxplots
    tar_target(s_bxp, plot_boxplot() %>% gs(pref05, 1.5, 5, "boxplot")),
    tar_target(s_bxp_ex, plot_boxplot_examples(fastq) %>% gs(pref05, 7, 3, "boxplot_examples")),
    
    # bar plots
    tar_target(s_bp_1, plot_barplot_1() %>% gs(pref05, 4, 3, "barplot_example_1")),
    tar_target(s_bp_2, plot_barplot_2() %>% gs(pref05, 4.5, 3.5, "barplot_example_2")),
    
    tar_target(figs_badbar, bad_bar_plots()),
    tar_target(s_bb_1, gs(figs_badbar$badbard_baseline, pref05, 5, 2.5)),
    tar_target(s_bb_2, gs(figs_badbar$badbar_log, pref05, 5, 2.5)),
    
    tar_target(figs_bar_problems, bar_problems()),
    tar_target(s_bpr_1, gs(figs_bar_problems$bar_problem_1, pref05, 2.5, 2.5)),
    tar_target(s_bpr_2, gs(figs_bar_problems$bar_problem_2, pref05, 2.5, 2.5)),
    tar_target(s_bpr_3, gs(figs_bar_problems$bar_problem_3, pref05, 2.5, 2.5)),
    tar_target(s_bpr_4, gs(figs_bar_problems$bar_problem_4, pref05, 3.5, 2.5)),
    tar_target(s_bpr_5, gs(figs_bar_problems$bar_problem_5, pref05, 3.5, 2.5)),
    
    # dynamite plots
    tar_target(figs_dynamite, dynamite_plots()),
    tar_target(s_dyn_1, gs(figs_dynamite$dynamite_1, pref05, 2, 4)),
    tar_target(s_dyn_2, gs(figs_dynamite$dynamite_2, pref05, 2, 4)),
    tar_target(s_dyn_3, gs(figs_dynamite$dynamite_3, pref05, 2, 4))
    
    
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}