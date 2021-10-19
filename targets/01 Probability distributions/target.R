
lecture_01 <- function() {
  
  init <- list(
    tar_target(pref01, "figures/01_probability_distributions")
  )

  read_data <- list(
    tar_target(baseball, read_tsv("data/baseball.txt")),
    tar_target(testdist, read_table("data/testdist.dat", col_names=c("value"))),
    tar_target(diaviper, read_tsv("data/diaviper.txt"))
    
  )
  
  comp <- list(
  )
  
  make_figures <- list(
    # Need of statistics: gene with one replicate vs lots
    tar_target(gene_2, "yfr017c"),
    tar_target(fig_grna_stats_1, plot_gene_cnt(grna, gene_2, sel=c("WT_33", "Snf2_31"))),
    tar_target(fig_grna_stats_2, plot_gene_cnt(grna, gene_2)),
    tar_target(sav_grna_stats_1, gs(pref01, fig_grna_stats_1, 2.1, 3.5)),
    tar_target(sav_grna_stats_2, gs(pref01, fig_grna_stats_2, 2.1, 3.5)),
    
    # 2 dice distribution
    tar_target(fig_2dice_distribution, plot_2dice_binomial()),
    tar_target(sav_2dice_distribution, gs(pref01, fig_2dice_distribution, 3, 3)),
    
    # discrete distribution example
    tar_target(fig_discrete_distribution, plot_discrete_distribution_cut(0, 12, dpois, lambda=4, dcut=c(5, 7))),
    tar_target(sav_discrete_distribution, gs(pref01, fig_discrete_distribution, 3, 2.2)),
    
    # continuous distribution example
    tar_target(fig_continuous_distribution, plot_continuous_distributon_cut(0, 25, dchisq, df=5, dcut=c(10,25))),
    tar_target(sav_contiuous_distribution, gs(pref01, fig_continuous_distribution, 3, 2.2)),
    
    # normal distribution with 1, 2 and 3 sigma
    tar_target(figs_normal_sigmas, plot_normal_sigmas(M=10, S=1.5)),
    tar_target(sav_normal_sigmas_1, gs(pref01, figs_normal_sigmas$normal_sigmas_1, 3, 2.5)),
    tar_target(sav_normal_sigmas_2, gs(pref01, figs_normal_sigmas$normal_sigmas_2, 3, 2.5)),
    
    # baseball players
    tar_target(fig_baseball_normal, plot_baseball_normal(baseball)),
    tar_target(sav_baseball_normal, gs(pref01, fig_baseball_normal, 5, 4)),
    
    # log-normal distribution
    tar_target(figs_lognormals, plot_lognormals(testdist)),
    tar_target(sav_lognormal_lin, gs(pref01, figs_lognormals$lognormal_lin, 3.5, 3)),
    tar_target(sav_lognormal_lin_1, gs(pref01, figs_lognormals$lognormal_lin_1, 3.5, 3)),
    tar_target(sav_lognormal_log, gs(pref01, figs_lognormals$lognormal_log, 3.5, 3)),
    tar_target(sav_lognormal_log_1, gs(pref01, figs_lognormals$lognormal_log_1, 3.5, 3)),
    tar_target(figs_replicates_loglin, plot_replicates_loglin(diaviper, c("WT_cyto_1", "WT_cyto_2"))),
    tar_target(sav_replicates_lin, gs(pref01, figs_replicates_loglin$lin, 3, 2.5)),
    tar_target(sav_replicates_log, gs(pref01, figs_replicates_loglin$log, 3, 2.5))
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}