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



plot_discrete_distribution_cut <- function(lo, up, FUN, ..., dcut=NULL) {
  dist <- tibble(
    x = lo:up,
    P = FUN(x, ...)
  )
  g <- ggplot(dist, aes(x, P)) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_col(fill=fill.colour, colour="black", width=0.7, size=0.3) +
    scale_x_continuous(breaks=0:max(dist$x)) +
    scale_y_continuous(expand=expansion(mult = c(0, 0.03))) +
    labs(x = "k", y = "P(X = k)")
  if(!is.null(dcut)) {
    dist_cut <- dist %>% filter(x >= dcut[1] & x <= dcut[2])
    g <- g + geom_col(data=dist_cut, fill=fill.colour.dark, colour="black", width=0.7, size=0.3)
  }
  g
}

geom_cut <- function(lo, up, FUN, ..., n=100, colour="black", fill=fill.colour.dark) {
  x <- lo + (0:(n - 1)) * (up - lo) / (n - 1)
  d <- data.frame(
    x = c(lo, x, up),
    y = c(0, FUN(x, ...), 0)
  )
  geom_polygon(data=d, aes(x, y), colour=colour, fill=fill)
}

plot_continuous_distributon_cut <- function(lo, up, FUN, ..., dcut=NULL, n=100) {
 
  g <- ggplot() +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    geom_cut(lo, up, FUN, ..., n=n, fill=fill.colour) +
    labs(x="x", y="f(x)") +
    scale_x_continuous(limits=c(lo, up), expand=c(0,0)) +
    scale_y_continuous(expand=expansion(mult=c(0, 0.03)))
  if(!is.null(dcut)) {
    g <- g + geom_cut(dcut[1], dcut[2], FUN, ...)
  }
  g
}