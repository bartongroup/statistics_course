
lecture_05 <- function() {
  
  init <- tar_plan(
    path05 = "figures/05_data_presentation"
  )

  read_data <- tar_plan(
    WT_lev = read_tsv("data/WT_lev.tsv", col_names=FALSE),
    fastq = read_table("data/fastq_quality.txt")
    
  )
  
  comp <- tar_plan(
  )
  
  make_figures <- tar_plan(
    # intro
    s_gd = good_plot() |> gs(path05, 4.5, 3, "good_plot"),
    s_showd = show_your_data() |> gs(path05, 7, 3, "show_your_data"),
    s_lin_sym = lines_and_symbols() |> gs(path05, 7, 3, "lines_and_symbols"),
    
    # colour blindness
    figs_cb = plot_colour_blind(),
    s_cb_1 = gs(figs_cb$colour_blind_1, path05, 4, 3),
    s_cb_2 = gs(figs_cb$colour_blind_2, path05, 4, 3),
    
    # logarithmic scale
    s_ll_1 = plot_loglin_1(WT_lev) |> gs(path05, 6, 5, "linear_vs_logarithmic_1"),
    s_ll_2 = plot_loglin_2() |> gs(path05, 6, 5, "linear_vs_logarithmic_2"),
    
    # error bars
    s_erb = plot_errorbars() |> gs(path05, 9, 2.3, "error_bars"),
    
    # boxplots
    s_bxp = plot_boxplot() |> gs(path05, 1.5, 5, "boxplot"),
    s_bxp_ex = plot_boxplot_examples(fastq) |> gs(path05, 7, 3, "boxplot_examples"),
    
    # bar plots
    s_bp_1 = plot_barplot_1() |> gs(path05, 4, 3, "barplot_example_1"),
    s_bp_2 = plot_barplot_2() |> gs(path05, 4.5, 3.5, "barplot_example_2"),
    
    figs_badbar = bad_bar_plots(),
    s_bb_1 = gs(figs_badbar$badbard_baseline, path05, 5, 2.5),
    s_bb_2 = gs(figs_badbar$badbar_log, path05, 5, 2.5),
    
    figs_bar_problems = bar_problems(),
    s_bpr_1 = gs(figs_bar_problems$bar_problem_1, path05, 2.5, 2.5),
    s_bpr_2 = gs(figs_bar_problems$bar_problem_2, path05, 2.5, 2.5),
    s_bpr_3 = gs(figs_bar_problems$bar_problem_3, path05, 2.5, 2.5),
    s_bpr_4 = gs(figs_bar_problems$bar_problem_4, path05, 3.5, 2.5),
    s_bpr_5 = gs(figs_bar_problems$bar_problem_5, path05, 3.5, 2.5),
    
    # dynamite plots
    figs_dynamite = dynamite_plots(),
    s_dyn_1 = gs(figs_dynamite$dynamite_1, path05, 2, 4),
    s_dyn_2 = gs(figs_dynamite$dynamite_2, path05, 2, 4),
    s_dyn_3 = gs(figs_dynamite$dynamite_3, path05, 2, 4)
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
