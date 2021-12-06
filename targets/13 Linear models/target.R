
lecture_13 <- function() {
  
  init <- list(
    tar_target(pref13, "figures/13_linear_models")
  )

  read_data <- list(
    tar_target(salaries, as_tibble(carData::Salaries) %>% mutate(rank = fct_relevel(rank, "AsstProf")))
  )
  
  comp <- list(
    tar_target(mice_quant, generate_quant_mice()),
    tar_target(m5, generate_m5()),
    tar_target(m12, generate_m12())
  )
  
  models <- list(
    tar_target(fit_m5, lm(mass ~ diet, data = m5)),
    tar_target(fit_m5_0, lm(mass ~ 0 + diet, data = m5)),
    tar_target(fit_m12, lm(mass ~ diet + sex, data = m12)),
    tar_target(fit_m12_i, lm(mass ~ diet + sex + diet:sex, data = m12)),
    
    tar_target(fit_sal_sex, lm(salary ~ sex, data=salaries)),
    tar_target(fit_sal_all, lm(salary ~ rank + discipline + yrs.since.phd + yrs.service + sex, data=salaries)),
    tar_target(fit_sal_red, lm(salary ~ rank + discipline + yrs.since.phd + sex, data=salaries))
  )
  
  make_figures <- list(
    # quantitative model
    tar_target(figs_quant_lm, plot_quant_lm(mice_quant)),
    tar_target(s_qlm1, gs(figs_quant_lm$simple_linear, pref13, 3, 3)),
    tar_target(s_qlm2, gs(figs_quant_lm$simple_linear_residuals, pref13, 3, 3)),
    
    # mice diet 5
    tar_target(figs_mice_diet, plot_mice_5_lm(m5, coef(fit_m5), coef(fit_m5_0))),
    tar_target(s_m5_1, gs(figs_mice_diet$mice_diet, pref13, 3, 3)),
    tar_target(s_m5_2, gs(figs_mice_diet$mice_diet_coef, pref13, 3, 3)),
    tar_target(s_m5_3, gs(figs_mice_diet$mice_diet_coef0, pref13, 3, 3)),
    
    tar_target(s_m5c_1, plot_coefficients(fit_m5, nudge=0.8) %>% gs(pref13, 3, 3, "mice_diet_coef")),
    tar_target(s_m5c_2, plot_coefficients(fit_m5_0, nudge=1.2) %>% gs(pref13, 3, 3, "mice_diet_coef_0")),
    
    # mice diet sex 12
    tar_target(figs_mice_diet_sex, plot_mice_12_lm(m12, coef(fit_m12), coef(fit_m12_i))),
    tar_target(s_m12_1, gs(figs_mice_diet_sex$mice_diet_sex, pref13, 4, 4)),
    tar_target(s_m12_2, gs(figs_mice_diet_sex$mice_diet_sex_coef, pref13, 4, 4)),
    tar_target(s_m12_3, gs(figs_mice_diet_sex$mice_diet_sex_coef_i, pref13, 4, 4)),
    tar_target(s_m12a, plot_anova_2(m12, col_var="diet", row_var="sex", val_var="mass", palette=c(norm=okabe_ito_palette[1], hifat=okabe_ito_palette[2])) %>% gs(pref13, 5, 3.5, "mice_diet_sex_anova")),
    
    # R2
    tar_target(figs_r2, plot_r2_examples()),
    tar_target(s_r2_1, gs(figs_r2$r2_large, pref13, 4, 4)),
    tar_target(s_r2_2, gs(figs_r2$r2_small, pref13, 4, 4)),
    
    # Example
    tar_target(s_sal, plot_salaries(salaries) %>% gs(pref13, 8, 5, "salaries")),
    tar_target(s_saly, plot_salaries_years(salaries) %>% gs(pref13, 3, 2.5, "salaries_years")),
    tar_target(s_salp, plot_female_proportion(salaries) %>% gs(pref13, 4, 4, "salaries_female_proportion"))
  )
  
  make_tables <- list(
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