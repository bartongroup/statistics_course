
lecture_13 <- function() {
  
  init <- tar_plan(
    path13 = "figures/13_linear_models"
  )

  read_data <- tar_plan(
    salaries = as_tibble(carData::Salaries) |> mutate(rank = fct_relevel(rank, "AsstProf"))
  )
  
  comp <- tar_plan(
    mice_quant = generate_quant_mice(),
    m5 = generate_m5(),
    m12 = generate_m12()
  )
  
  models <- tar_plan(
    fit_m5 = lm(mass ~ diet, data = m5),
    fit_m5_0 = lm(mass ~ 0 + diet, data = m5),
    fit_m12 = lm(mass ~ diet + sex, data = m12),
    fit_m12_i = lm(mass ~ diet + sex + diet:sex, data = m12),
    
    fit_sal_sex = lm(salary ~ sex, data=salaries),
    fit_sal_all = lm(salary ~ rank + discipline + yrs.since.phd + yrs.service + sex, data=salaries),
    fit_sal_red = lm(salary ~ rank + discipline + yrs.since.phd + sex, data=salaries)
  )
  
  make_figures <- tar_plan(
    # quantitative model
    figs_quant_lm = plot_quant_lm(mice_quant),
    s_qlm1 = gs(figs_quant_lm$simple_linear, path13, 3, 3),
    s_qlm2 = gs(figs_quant_lm$simple_linear_residuals, path13, 3, 3),
    
    # mice diet 5
    figs_mice_diet = plot_mice_5_lm(m5, coef(fit_m5), coef(fit_m5_0)),
    s_m5_1 = gs(figs_mice_diet$mice_diet, path13, 3, 3),
    s_m5_2 = gs(figs_mice_diet$mice_diet_coef, path13, 3, 3),
    s_m5_3 = gs(figs_mice_diet$mice_diet_coef0, path13, 3, 3),
    
    s_m5c_1 = plot_coefficients(fit_m5, nudge=0.8) |> gs(path13, 3, 3, "mice_diet_estim"),
    s_m5c_2 = plot_coefficients(fit_m5_0, nudge=1.2) |> gs(path13, 3, 3, "mice_diet_estim_0"),
    
    # mice diet sex 12
    figs_mice_diet_sex = plot_mice_12_lm(m12, coef(fit_m12), coef(fit_m12_i)),
    s_m12_1 = gs(figs_mice_diet_sex$mice_diet_sex, path13, 4, 4),
    s_m12_2 = gs(figs_mice_diet_sex$mice_diet_sex_coef, path13, 4, 4),
    s_m12_3 = gs(figs_mice_diet_sex$mice_diet_sex_coef_i, path13, 4, 4),
    s_m12a = plot_anova_2(m12, col_var="diet", row_var="sex", val_var="mass", palette=c(norm=okabe_ito_palette[1], hifat=okabe_ito_palette[2])) |> gs(path13, 5, 3.5, "mice_diet_sex_anova"),
    
    # R2
    figs_r2 = plot_r2_examples(),
    s_r2_1 = gs(figs_r2$r2_large, path13, 6, 3),
    s_r2_2 = gs(figs_r2$r2_small, path13, 6, 3),
    
    # Example
    s_sal = plot_salaries(salaries) |> gs(path13, 8, 5, "salaries"),
    s_saly = plot_salaries_years(salaries) |> gs(path13, 3, 2.5, "salaries_years"),
    s_salp = plot_female_proportion(salaries) |> gs(path13, 4, 4, "salaries_female_proportion")
  )
  
  make_tables <- tar_plan(
  )

  
  c(
    init,
    comp,
    models,
    read_data,
    make_figures,
    make_tables
  )
  
  
}
