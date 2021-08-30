
lecture_00 <- function() {
  
  init <- list(
    tar_target(pref00, "figures/00_why_statistics")
  )

  read_data <- list(
    tar_target(grna, read_tsv("data/grna.txt")),
    # data from Shoemaker A. L.,  College C., Journal of Statistics Education 4 (1996)
    tar_target(body, read_table("data/normtemp.dat.txt", col_names = c("temperature.f", "sex", "hrate"))),
    tar_target(tumour_ptpn, read_tumour_data("data/tumour_ptpn.txt")),
    tar_target(devspine, read_devspine("data/expr_e125-e165-adult_raw.tsv")),
    tar_target(mouse_genes, get_mouse_genes(version=104)),
    tar_target(drug_data, read_drugs())
  )
  
  comp <- list(
    tar_target(devspine_de, run_devspine_de(devspine)),
    tar_target(te_selection, t_edger_selection(devspine_de, mouse_genes)),
    tar_target(drug_model, lm(`logVDss` ~ ., data=drug_data$drugs[, -1]))
  )
  
  make_figures <- list(
    
    # gene expression
    tar_target(gene_1, "yfr017c"),
    tar_target(fig_grna_expression_1, plot_gene_cnt(grna, gene_1)),
    tar_target(fig_grna_expression_2, plot_gene_cnt(grna, gene_1, p.alpha=0.3)),
    tar_target(sav_grna_exp_1, gs(pref00, fig_grna_expression_1, 2.1, 3.5)),
    tar_target(sav_grna_exp_2, gs(pref00, fig_grna_expression_2, 2.6, 3.5)),
    
    # Poisson counts on plates
    tar_target(fig_poisson_plates, plot_poisson_plates(seed=223)),
    tar_target(sav_poisson_plates, gs(pref00, fig_poisson_plates, width=3, height=3)),
    
    # Normal body temperature
    tar_target(fig_body_temperature, plot_body_temperature(body)),
    tar_target(sav_body_temperature, gs(pref00, fig_body_temperature, width=3.5, height=3.5)),
    
    # Mouse tumour
    tar_target(fig_tumour_growth, plot_tumour_lines(tumour_ptpn)),
    tar_target(sav_tumour_growth, gs(pref00, fig_tumour_growth, 7, 3)),
    
    # Clustering devspine data
    tar_target(fig_clustering_devspine, plot_clustering_devspine(devspine$ds)),
    tar_target(sav_clustering_devspine, gs(pref00, fig_clustering_devspine, 3, 3.5)),
    
    # Distance matrix for devspine data
    tar_target(fig_matrix_devspine, plot_distance_matrix(devspine$ds, devspine$metadata)),
    tar_target(sav_matrix_devspine, gs(pref00, fig_matrix_devspine, 4, 3)),
    
    # Gene examples
    tar_target(genes_examples, c("ENSMUSG00000051951", "ENSMUSG00000009281","ENSMUSG00000013415")),
    tar_target(fig_mouse_genes, plot_genes_cnt(devspine$dsn, genes_examples, mouse_genes, cex=4)),
    tar_target(sav_mouse_genes, gs(pref00, fig_mouse_genes, 5, 2)),
    
    # Difference between edgeR and t-test
    tar_target(te_examples, c("ENSMUSG00000000416", "ENSMUSG00000020289")),
    tar_target(fig_te_examples, plot_genes_cnt(devspine$dsn2, te_examples, mouse_genes)),
    tar_target(te_examples_data, te_selection %>% filter(gene_id %in% te_examples)),
    tar_target(sav_te_examples, gs(pref00, fig_te_examples, 4, 2)),
    
    # Drug data table fragment and model
    tar_target(drug_table, drug_data$drugs %>% 
      select(Name, `logVDss`, LogBB, `S+pH_Satd`, FAnion, `log MaxQ`, MOLECULAR_SPECIES, moka_ionState7.4, TerAmine_) %>% 
      head(10)
    ),
    tar_target(show_drug_model, show_model(drug_model, drug_data$coef_names))
    
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}