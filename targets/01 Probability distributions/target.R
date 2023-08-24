
lecture_01 <- function() {
  
  init <- tar_plan(
    path01 = "figures/01_probability_distributions"
  )

  read_data <- tar_plan(
    baseball = read_tsv("data/baseball.txt"),
    testdist = read_table("data/testdist.dat", col_names=c("value")),
    diaviper = read_tsv("data/diaviper.txt")
  )
  
  comp <- tar_plan(
  )
  
  make_figures <- tar_plan(
    # 2 dice distribution
    fig_2dice_distribution = plot_2dice_binomial(),
    sav_2dice_distribution = gs(fig_2dice_distribution, path01, 3, 3),
    
    # discrete distribution example
    fig_discrete_distribution = plot_discrete_distribution_cut(0, 12, dpois, lambda=4, dcut=c(5, 7)),
    sav_discrete_distribution = gs(fig_discrete_distribution, path01, 3, 2.2),
    
    # continuous distribution example
    fig_continuous_distribution = plot_continuous_distributon_cut(0, 25, dchisq, df=5, dcut=c(10,25)),
    sav_contiuous_distribution = gs(fig_continuous_distribution, path01, 3, 2.2),
    
    # normal distribution with 1, 2 and 3 sigma
    figs_normal_sigmas = plot_normal_sigmas(M=10, S=1.5),
    sav_normal_sigmas_1 = gs(figs_normal_sigmas$normal_sigmas_1, path01, 3, 2.5),
    sav_normal_sigmas_2 = gs(figs_normal_sigmas$normal_sigmas_2, path01, 3, 2.5),
    
    # baseball players
    fig_baseball_normal = plot_baseball_normal(baseball),
    sav_baseball_normal = gs(fig_baseball_normal, path01, 5, 4),
    
    # log-normal distribution
    figs_lognormals = plot_lognormals(testdist),
    sav_lognormal_lin = gs(figs_lognormals$lognormal_lin, path01, 3.5, 3),
    sav_lognormal_lin_1 = gs(figs_lognormals$lognormal_lin_1, path01, 3.5, 3),
    sav_lognormal_log = gs(figs_lognormals$lognormal_log, path01, 3.5, 3),
    sav_lognormal_log_1 = gs(figs_lognormals$lognormal_log_1, path01, 3.5, 3),
    figs_replicates_loglin = plot_replicates_loglin(diaviper, c("WT_cyto_1", "WT_cyto_2")),
    sav_replicates_lin = gs(figs_replicates_loglin$lin, path01, 3, 2.5),
    sav_replicates_log = gs(figs_replicates_loglin$log, path01, 3, 2.5),
    
    # Poisson distribution
    figs_plates = plot_poisson_plates_dist(),
    sav_plates_plates = gs(figs_plates$poisson_plates, path01, 3.5, 2.5),
    sav_plates_dist = gs(figs_plates$poisson_dist, path01, 3.5, 2.5),
    fig_plates_anim = anim_poisson(mu = 7, n_sim = 1000),
    sav_plates_anim = ans(fig_plates_anim, path01, 3.5, 2.5),
    fig_poisson_examples = plot_poisson_dist_examples(),
    sav_poisson_ex = gs(fig_poisson_examples, path01, 4, 5),
    fig_horse_kicks = plot_horse_kicks(),
    sav_horse_kicks = gs(fig_horse_kicks, path01, 4, 5),
    
    # Binomial distribution
    figs_binomial = plot_binoms(),
    sav_binom = gs(figs_binomial$binomial_example, path01, 4, 3),
    sav_binom_norm = gs(figs_binomial$binomial_example_normal, path01, 4, 3),
    sav_binom_poiss = gs(figs_binomial$binomial_example_poisson, path01, 4, 3),
    fig_coins_anim = anim_coins(n = 8, n_sim = 1000),
    sav_coins_anim = ans(fig_coins_anim, path01, 3.5, 2.5),
    
    # R
    figs_r_1 = plot_r_dists(),
    sav_r_norm = gs(figs_r_1$dnorm, path01, 2, 1.4),
    sav_r_norm2 = gs(figs_r_1$dnorm2, path01, 2, 1.4),
    sav_r_binom = gs(figs_r_1$dbinom, path01, 2, 1.4),
    sav_r_t = gs(figs_r_1$dt, path01, 2, 1.4),
    sav_r_t2 = gs(figs_r_1$dt2, path01, 2, 1.4)
  )
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
