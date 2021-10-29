
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
    tar_target(sav_grna_stats_1, gs(fig_grna_stats_1, pref01, 2.1, 3.5)),
    tar_target(sav_grna_stats_2, gs(fig_grna_stats_2, pref01, 2.1, 3.5)),
    
    # 2 dice distribution
    tar_target(fig_2dice_distribution, plot_2dice_binomial()),
    tar_target(sav_2dice_distribution, gs(fig_2dice_distribution, pref01, 3, 3)),
    
    # discrete distribution example
    tar_target(fig_discrete_distribution, plot_discrete_distribution_cut(0, 12, dpois, lambda=4, dcut=c(5, 7))),
    tar_target(sav_discrete_distribution, gs(fig_discrete_distribution, pref01, 3, 2.2)),
    
    # continuous distribution example
    tar_target(fig_continuous_distribution, plot_continuous_distributon_cut(0, 25, dchisq, df=5, dcut=c(10,25))),
    tar_target(sav_contiuous_distribution, gs(fig_continuous_distribution, pref01, 3, 2.2)),
    
    # normal distribution with 1, 2 and 3 sigma
    tar_target(figs_normal_sigmas, plot_normal_sigmas(M=10, S=1.5)),
    tar_target(sav_normal_sigmas_1, gs(figs_normal_sigmas$normal_sigmas_1, pref01, 3, 2.5)),
    tar_target(sav_normal_sigmas_2, gs(figs_normal_sigmas$normal_sigmas_2, pref01, 3, 2.5)),
    
    # baseball players
    tar_target(fig_baseball_normal, plot_baseball_normal(baseball)),
    tar_target(sav_baseball_normal, gs(fig_baseball_normal, pref01, 5, 4)),
    
    # log-normal distribution
    tar_target(figs_lognormals, plot_lognormals(testdist)),
    tar_target(sav_lognormal_lin, gs(figs_lognormals$lognormal_lin, pref01, 3.5, 3)),
    tar_target(sav_lognormal_lin_1, gs(figs_lognormals$lognormal_lin_1, pref01, 3.5, 3)),
    tar_target(sav_lognormal_log, gs(figs_lognormals$lognormal_log, pref01, 3.5, 3)),
    tar_target(sav_lognormal_log_1, gs(figs_lognormals$lognormal_log_1, pref01, 3.5, 3)),
    tar_target(figs_replicates_loglin, plot_replicates_loglin(diaviper, c("WT_cyto_1", "WT_cyto_2"))),
    tar_target(sav_replicates_lin, gs(figs_replicates_loglin$lin, pref01, 3, 2.5)),
    tar_target(sav_replicates_log, gs(figs_replicates_loglin$log, pref01, 3, 2.5)),
    
    # Poisson distribution
    tar_target(figs_plates, plot_poisson_plates_dist()),
    tar_target(sav_plates_plates, gs(figs_plates$poisson_plates, pref01, 3.5, 2.5)),
    tar_target(sav_plates_dist, gs(figs_plates$poisson_dist, pref01, 3.5, 2.5)),
    tar_target(fig_poisson_examples, plot_poisson_dist_examples()),
    tar_target(sav_poisson_ex, gs(fig_poisson_examples, pref01, 3, 5)),
    tar_target(fig_horse_kicks, plot_horse_kicks()),
    tar_target(sav_horse_kicks, gs(fig_horse_kicks, pref01, 4, 5)),
    
    # Binomial distribution
    tar_target(figs_binomial, plot_binoms()),
    tar_target(sav_binom, gs(figs_binomial$binomial_example, pref01, 4, 3)),
    tar_target(sav_binom_norm, gs(figs_binomial$binomial_example_normal, pref01, 4, 3)),
    tar_target(sav_binom_poiss, gs(figs_binomial$binomial_example_poisson, pref01, 4, 3)),
    
    # R
    tar_target(figs_r_1, plot_r_dists()),
    tar_target(sav_r_norm, gs(figs_r_1$dnorm, pref01, 2, 1.4)),
    tar_target(sav_r_norm2, gs(figs_r_1$dnorm2, pref01, 2, 1.4)),
    tar_target(sav_r_binom, gs(figs_r_1$dbinom, pref01, 2, 1.4))
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}