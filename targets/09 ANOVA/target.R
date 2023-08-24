
lecture_09 <- function() {
  
  init <- tar_plan(
    path09 = "figures/09_anova"
  )

  read_data <- tar_plan(
    mice_1way = read_tsv("data/mice_1way.txt") |> mutate(Country = factor(Country, levels = COUNTRIES)),
    mice_2way = read_tsv("data/mice_2way.txt") |> mutate(Country = factor(Country, levels = COUNTRIES), Colour = factor(Colour, levels = COLOURS)),
    time_course = read_rds("data/time_course.rds")
  )
  
  comp <- tar_plan(
    F_mice = generate_F_mice(mice_1way),
    mice_1way_ne = generate_no_effect_anova(mice_1way)
  )
  
  make_figures <- tar_plan(
    s_mb_1 = plot_mice_box(mice_1way, cex=0.6) |> gs(path09, 3.2, 3.2, "mice_1way"),
    
    # variance within between
    s_vwb = plot_var_within_between() |> gs(path09, 5.8, 3, "variance_between_within"),
    s_varw = plot_variance(mice_1way, within=TRUE) |> gs(path09, 3.3, 3, "variance_within"),
    s_varb = plot_variance(mice_1way, within=FALSE) |> gs(path09, 3.3, 3, "variance_between"),
    
    # F distribution
    s_fmice = plot_one_dist(F_mice, "F", "", c(0, 6), df, 3, 30) |> gs(path09, 3.5, 3.5, "mice_1way_F"),
    
    # Effect no effect
    s_ene1 = plot_mice_box(mice_1way, cex=0.6, limits=c(0,40)) |> gs(path09, 3.2, 3.2, "mice_1way_effect"),
    s_ene2 = plot_mice_box(mice_1way_ne$dat, cex=0.6, limits=c(0,40)) |> gs(path09, 3.2, 3.2, "mice_1way_no_effect"),
    
    # 2 way
    s_2ww = plot_mice_box(filter(mice_2way, Colour=="White"), limits=c(0,42), cex=0.6) |> gs(path09, 3.2, 3.2, "mice_white"),
    s_2wb = plot_mice_box(filter(mice_2way, Colour=="Black"), limits=c(0,42), cex=0.6) |> gs(path09, 3.2, 3.2, "mice_black"),
    s_a2w = plot_anova_2(mice_2way) |> gs(path09, 6, 3.5, "mice_2way"),
    
    # null hypotheses
    figs_anova_2_nulls = plot_anova_2_4nulls(),
    s_4n_1 = gs(figs_anova_2_nulls$anova2_true, path09, 6, 3.5),
    s_4n_2 = gs(figs_anova_2_nulls$anova2_cols, path09, 6, 3.5),
    s_4n_3 = gs(figs_anova_2_nulls$anova2_rows, path09, 6, 3.5),
    s_4n_4 = gs(figs_anova_2_nulls$anova2_inter, path09, 6, 3.5),
    s_da2 = plot_drug_anova() |> gs(path09, 8, 3, "simple_anova"),
    
    # time course
    s_tc = plot_time_course(time_course) |> gs(path09, 8, 6, "time_course"),
    s_tca = plot_time_course_area(time_course) |> gs(path09, 4, 3.5, "area_diff")
  )

  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
