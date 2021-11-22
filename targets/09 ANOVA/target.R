
lecture_09 <- function() {
  
  init <- list(
    tar_target(pref09, "figures/09_anova")
  )

  read_data <- list(
    tar_target(mice_1way, read_tsv("data/mice_1way.txt") %>% mutate(Country = factor(Country, levels = COUNTRIES))),
    tar_target(mice_2way, read_tsv("data/mice_2way.txt") %>% mutate(Country = factor(Country, levels = COUNTRIES), Colour = factor(Colour, levels = COLOURS))),
    tar_target(time_course, read_rds("data/time_course.rds"))
  )
  
  comp <- list(
    tar_target(F_mice, generate_F_mice(mice_1way)),
    tar_target(mice_1way_ne, generate_no_effect_anova(mice_1way))
  )
  
  make_figures <- list(
    tar_target(s_mb_1, plot_mice_box(mice_1way, cex=0.6) %>% gs(pref09, 3.2, 3.2, "mice_1way")),
    
    # variance within between
    tar_target(s_vwb, plot_var_within_between() %>% gs(pref09, 5.8, 3, "variance_between_within")),
    tar_target(s_varw, plot_variance(mice_1way, within=TRUE) %>% gs(pref09, 3.3, 3, "variance_within")),
    tar_target(s_varb, plot_variance(mice_1way, within=FALSE) %>% gs(pref09, 3.3, 3, "variance_between")),
    
    # F distribution
    tar_target(s_fmice, plot_one_dist(F_mice, "F", "", c(0, 6), df, 3, 30) %>% gs(pref09, 3.5, 3.5, "mice_1way_F")),
    
    # Effect no effect
    tar_target(s_ene1, plot_mice_box(mice_1way, cex=0.6, limits=c(0,40)) %>% gs(pref09, 3.2, 3.2, "mice_1way_effect")),
    tar_target(s_ene2, plot_mice_box(mice_1way_ne$dat, cex=0.6, limits=c(0,40)) %>% gs(pref09, 3.2, 3.2, "mice_1way_no_effect")),
    
    # 2 way
    tar_target(s_2ww, plot_mice_box(filter(mice_2way, Colour=="White"), limits=c(0,42), cex=0.6) %>% gs(pref09, 3.2, 3.2, "mice_white")),
    tar_target(s_2wb, plot_mice_box(filter(mice_2way, Colour=="Black"), limits=c(0,42), cex=0.6) %>% gs(pref09, 3.2, 3.2, "mice_black")),
    tar_target(s_a2w, plot_anova_2(mice_2way) %>% gs(pref09, 6, 3.5, "mice_2way")),
    
    # null hypotheses
    tar_target(figs_anova_2_nulls, plot_anova_2_4nulls()),
    tar_target(s_4n_1, gs(figs_anova_2_nulls$anova2_true, pref09, 6, 3.5)),
    tar_target(s_4n_2, gs(figs_anova_2_nulls$anova2_cols, pref09, 6, 3.5)),
    tar_target(s_4n_3, gs(figs_anova_2_nulls$anova2_rows, pref09, 6, 3.5)),
    tar_target(s_4n_4, gs(figs_anova_2_nulls$anova2_inter, pref09, 6, 3.5)),
    tar_target(s_da2, plot_drug_anova() %>% gs(pref09, 8, 3, "simple_anova")),
    
    # time course
    tar_target(s_tc, plot_time_course(time_course) %>% gs(pref09, 8, 6, "time_course")),
    tar_target(s_tca, plot_time_course_area(time_course) %>% gs(pref09, 4, 3.5, "area_diff"))
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}