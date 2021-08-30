geom_outline <- function(d, breaks, ..., y.value="density") {
  bins <- breaks[2] - breaks[1]
  m <- length(d)
  x <- breaks[-1] - bins
  y <- as.numeric(table(cut(d, breaks, right=TRUE)))
  if(y.value == "density") {
    y <- y / (bins * sum(y))
  }
  
  n <- length(x)
  st <- data.frame(
    x = c(x, x[n] + bins),
    y = c(y, y[n])
  )
  geom_step(data=st, aes(x, y), ...)
}



biomart_gene_download <- function(mart) {
  getBM(attributes = c(
    "chromosome_name",
    "start_position",
    "end_position",
    "strand",
    "gene_biotype",
    "ensembl_gene_id",
    "external_gene_name",
    "description"
  ), mart=mart) %>% 
    as_tibble() %>% 
    rename(
      chr = chromosome_name,
      start = start_position,
      end = end_position,
      gene_id = ensembl_gene_id,
      gene_name = external_gene_name
    )
}