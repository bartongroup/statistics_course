
lecture_06 <- function() {
  
  init <- tar_plan(
    path06 = "figures/06_hypothesis_testing"
  )

  read_data <- tar_plan(
    mice2 = read_tsv("data/mice_2samples.txt")
  )
  
  comp <- tar_plan(
    mouse_delta = generate_mouse_delta()
  )
  
  make_figures <- tar_plan(
    # intro plot
    s_es_mice = plot_mice_box(mice2, with.means = TRUE, with.boxes = FALSE, cex = 1) |> gs(path06, 2.1, 3.5, "mice_eng_sco"),
    
    # null distribution
    fig_null = plot_distribution_cut(mouse_delta, brks = seq(-10, 10, 0.1), xlab = "Body mass difference (g)"),
    fig_null_cut = plot_distribution_cut(mouse_delta, cut = 5, brks = seq(-10, 10, 0.1), xlab = "Body mass difference (g)"),
    s_mn_1 = fig_null |> gs(path06, 4, 3, "mouse_null"),
    s_mn_2 = fig_null |> gs(path06, 3, 2.25, "mouse_null_medium"),
    s_mnc_1 = fig_null_cut |> gs(path06, 4, 3, "mouse_null_cut"),
    s_mnc_2 = fig_null_cut |> gs(path06, 3, 2.25, "mouse_null_cut_medium"),
    s_mnc_3 = (fig_null_cut + theme_d) |> gs(path06, 2, 2, "mouse_null_cut_small"),
    
    # hypergeometric
    figs_hyper = plot_hyper(),
    s_hyp_1 = gs(figs_hyper$hyper, path06, 3.2, 2),
    s_hyp_2 = gs(figs_hyper$hyper4, path06, 3.2, 2),
    s_hyp_3 = gs(figs_hyper$hyper1, path06, 3.2, 2),
    s_tea = plot_tea() |> gs(path06, 3.2, 2, "tea_tasting"),
    figs_tea_fisher = plot_tea_fisher(),
    s_tf_1 = gs(figs_tea_fisher$fisher_bristol_1, path06, 2.8, 1.8),
    s_tf_2 = gs(figs_tea_fisher$fisher_bristol_2, path06, 2.8, 1.8)
    
  )

  
  c(
    init,
    comp, 
    read_data, 
    make_figures
  )
  
  
}
