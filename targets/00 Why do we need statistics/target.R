
lecture_00 <- function() {
  
  init <- tar_plan(
    path00 = "figures/00_why_statistics"
  )

  read_data <- tar_plan(
    grna = read_tsv("data/grna.txt"),
    # data from Shoemaker A. L. =  College C., Journal of Statistics Education 4 (1996)
    body = read_table("data/normtemp.dat.txt", col_names = c("temperature.f", "sex", "hrate")),
    tumour_ptpn = read_tumour_data("data/tumour_ptpn.txt"),
    devspine = read_devspine("data/expr_e125-e165-adult_raw.tsv"),
    mouse_genes = get_mouse_genes(version=104),
    drug_data = read_drugs()
  )
  
  comp <- tar_plan(
    devspine_de = run_devspine_de(devspine),
    te_selection = t_edger_selection(devspine_de, mouse_genes),
    drug_model = lm(`logVDss` ~ ., data=drug_data$drugs[, -1])
  )
  
  make_figures <- tar_plan(
    
    # gene expression
    gene_1 = "yfr017c",
    fig_grna_expression_1 = plot_gene_cnt(grna, gene_1, sel = c("WT_33", "Snf2_31")),
    fig_grna_expression_2 = plot_gene_cnt(grna, gene_1),
    fig_grna_expression_3 = plot_gene_cnt(grna, gene_1, p.alpha = 0.2),
    sav_grna_exp_1 = gs(fig_grna_expression_1, path00, 2.1, 3.5),
    sav_grna_exp_2 = gs(fig_grna_expression_2, path00, 2.1, 3.5),
    sav_grna_exp_3 = gs(fig_grna_expression_3, path00, 2.1, 3.5),
    
    # Poisson counts on plates
    fig_poisson_plates = plot_poisson_plates(seed=223),
    sav_poisson_plates = gs(fig_poisson_plates, path00, width=3, height=3),
    
    # Normal body temperature
    fig_body_temperature = plot_body_temperature(body),
    sav_body_temperature = gs(fig_body_temperature, path00, width=3.5, height=3.5),
    
    # Mouse tumour
    fig_tumour_growth = plot_tumour_lines(tumour_ptpn),
    sav_tumour_growth = gs(fig_tumour_growth, path00, 7, 3),
    
    # Clustering devspine data
    fig_clustering_devspine = plot_clustering_devspine(devspine$ds),
    sav_clustering_devspine = gs(fig_clustering_devspine, path00, 3, 3.5),
    
    # Distance matrix for devspine data
    fig_matrix_devspine = plot_distance_matrix(devspine$ds, devspine$metadata),
    sav_matrix_devspine = gs(fig_matrix_devspine, path00, 5, 4),
    
    # Gene examples
    genes_examples = c("ENSMUSG00000051951", "ENSMUSG00000009281","ENSMUSG00000013415"),
    fig_mouse_genes = plot_genes_cnt(devspine$dsn, genes_examples, mouse_genes, cex=4),
    sav_mouse_genes = gs(fig_mouse_genes, path00, 5, 2),
    
    # Difference between edgeR and t-test
    te_examples = c("ENSMUSG00000000416", "ENSMUSG00000020289"),
    fig_te_examples = plot_genes_cnt(devspine$dsn2, te_examples, mouse_genes),
    te_examples_data = te_selection |> filter(gene_id %in% te_examples),
    sav_te_examples = gs(fig_te_examples, path00, 4, 2),
    
    # Drug data table fragment and model
    drug_table = drug_data$drugs |> 
      select(Name = `logVDss`, LogBB, `S+pH_Satd`, FAnion, `log MaxQ`, MOLECULAR_SPECIES, moka_ionState7.4, TerAmine_) |> 
      head(10),
    show_drug_model = show_model(drug_model, drug_data$coef_names)
    
  )
  
  
  
  
  
  
  
  c(
    init,
    comp,
    read_data,
    make_figures
  )
  
  
}
