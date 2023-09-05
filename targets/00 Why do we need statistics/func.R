plot_gene_cnt <- function(cnt, gene, sel=NULL, p.alpha=1) {
  m <- cnt |> 
    filter(gene_id == gene) |> 
    pivot_longer(-gene_id, names_to="sample") |> 
    separate(sample, c("condition", "replicate"), sep="_", remove=FALSE) |> 
    mutate(condition = fct_relevel(condition, "WT"))
  lims <- c(min(m$value), max(m$value))
  
  if(!is.null(sel)) m <- m |> filter(sample %in% sel)
  #if(is.null(sel)) test <- t.test(value~condition, data=m)
  
  g <- ggplot() +
    theme_bw() +
    scale_fill_manual(values=okabe_ito_palette, guide="none") +
    geom_beeswarm(data=m, aes(x=condition, y=value, fill=condition), cex=3, size=1, shape=21, priority="density", alpha=p.alpha) +
    labs(x=NULL, y="Read count") +
    scale_x_discrete(labels=c("WT", expression(Delta*Snf2))) +
    scale_y_continuous(limits = lims)
  if(is.null(sel)) {
    mm <- m |> group_by(condition) |> summarise(M=mean(value), SD=sd(value), n=n()) |> mutate(CI = SD/sqrt(n) * qt(0.975, n - 1))
    mm$i <- c(1.3, 1.7)
    g <- g +
      geom_errorbar(data=mm, aes(x=i, ymin=M-CI, ymax=M+CI), width=0.15) +
      geom_point(data=mm, aes(x=i, y=M, fill=condition), size=3, shape=22)
  }
  g
}


plot_poisson_plates <- function(seed) {
  set.seed(seed)
  p <- rnbinom(8, mu=20, size=5)
  p <- c(10, 30, p)
  d <- tibble(plate=seq_along(p), cnt = p)
  
  ggplot(d, aes(plate, cnt)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(colour="grey80")
    ) +
    #geom_segment(aes(x=plate, xend=plate, y=cnt, yend=0), colour="grey80") +
    geom_point(shape=22, fill=fill.colour) +
    scale_x_continuous(breaks=1:10) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(p) * 1.3)) +
    labs(x="Plate", y="Count")
}


plot_body_temperature <- function(body) {
  body$temperature.c <- (body$temperature.f - 32) *5 / 9
  
  M <- mean(body$temperature.c)
  S <- sd(body$temperature.c)
  x <- seq(35,39,0.01)
  m <- data.frame(
    x = x,
    y = dnorm(x, mean=M, sd=S)
  )
  
  g1 <- ggplot(body, aes(x=temperature.c, y=1)) +
    theme_bw() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      plot.margin = margin(t=-2,r=6,l=3),
      panel.grid = element_blank()
    ) +
    geom_beeswarm(groupOnX = FALSE, cex=1, size=0.8, shape=21, fill=fill.colour) +
    scale_x_continuous(expand=c(0,0), limits=c(35, 39)) +
    scale_y_continuous(expand=c(0.1,0)) +
    labs(x=expression(Body~temperature~(degree*C)))
  
  brks <- seq(35,39,0.2)
  g2 <- ggplot() +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      plot.margin = margin(b=0,r=6,t=10,l=3),
      panel.grid = element_blank()
    ) +
    geom_histogram(data=body, aes(x=temperature.c, y=after_stat(density)), breaks=brks, fill=fill.colour) +
    geom_outline(body$temperature.c, breaks=brks) +
    geom_line(data=m, aes(x,y), colour="brown3") +
    scale_y_continuous(expand=c(0,0), limits=c(0,1.2), breaks=seq(0.2,1.2,0.2)) +
    scale_x_continuous(expand=c(0,0), limits=c(35, 39)) +
    labs(x=NULL, y="Density")
  
  
  plot_grid(g2, g1, ncol=1, align="v", rel_heights = c(3,1))
}



read_tumour_data <- function(file) {
  ptpn <- read_tsv(file, name_repair = make.names) |>
    set_names(c("day", paste0("KO_", 1:12), paste0("WT_", 1:12))) |> 
    mutate(day = as.numeric(str_remove(day, "day"))) |>
    pivot_longer(-day, names_to="sample", values_to="volume") |>
    separate("sample", c("condition", "replicate"), "_", remove=FALSE) |>
    filter(!is.na(volume) & volume > 0) |>
    mutate(lvolume = log10(volume), lday=log10(day))
}

plot_tumour_lines <- function(ptpn) {
  m <- ptpn |>
    group_by(condition, day) |>
    summarise(M = mean(volume), SE = sd(volume) / sqrt(n()))
 
  y.lims <- c(0.3, 1300)
  d.brks <- 1:21
  d.labs <- rep("", length(d.brks))
  d.labs[c(5, 10, 20)] <- c(5, 10, 20)
  t.size <- 9
  
  g1 <- ggplot(ptpn, aes(x=day, y=volume, group=sample, colour=condition)) +
    theme_bw() +
    geom_line() +
    labs(x="Day", y=expression(Tumor~volume~(mm^3)), title="Raw data") +
    scale_x_log10() +
    scale_y_log10(expand=c(0,0), limits=y.lims) +
    scale_colour_manual(values=okabe_ito_palette, guide="none") +
    theme(plot.title=element_text(size=t.size))
  
  g2 <- ggplot(m) +
    theme_bw() +
    geom_line(aes(x=day, y=M, colour=condition)) +
    geom_ribbon(aes(x=day, ymin=M-SE, ymax=M+SE, fill=condition), alpha=0.3) +
    scale_x_log10() +
    scale_y_log10(expand=c(0,0), limits=y.lims) +
    scale_colour_manual(values=okabe_ito_palette, guide="none") +
    scale_fill_manual(values=okabe_ito_palette, guide="none") +
    labs(x="Day", y=NULL, title="Mean and standard error") +
    theme(plot.title=element_text(size=t.size))
  
  
  g3 <- ggplot(ptpn, aes(x=lday, y=lvolume, colour=condition)) +
    theme_bw() +
    theme(
      legend.key = element_rect(fill="white", colour=NA),
      legend.text = element_text(size=8),
      legend.title = element_text(size=9),
      plot.title = element_text(size=t.size)
    ) +
    geom_point(size=0.5, alpha=0.4) +
    stat_smooth(method="lm") +
    scale_colour_manual(values=okabe_ito_palette) +
    labs(x="Day", y=NULL, title="Linear model") +
    scale_x_continuous(breaks=log10(c(5,10,20)), labels=c(5,10,20)) +
    scale_y_continuous(breaks=c(0,1,2,3), labels=c(1,10,100,1000), expand=c(0,0), limits=log10(y.lims)) +
    guides(color=guide_legend(override.aes=list(fill=NA)))
  
  
  g <- plot_grid(g1, g2, g3, nrow=1, rel_widths = c(1.05, 0.9, 1.3))
  
}

read_devspine <- function(file) {
  conditions <- c("E12.5", "E16.5", "Adult")
  replicates <- 1:3
  metadata <- expand.grid(replicate=replicates, condition=conditions) |>
    unite(sample, c(condition, replicate), sep="-", remove=FALSE) |>
    mutate(column = tolower(sample)) |> 
    mutate(column = gsub("\\.", "", column)) |>
    mutate(column = gsub("-", "_", column))
  
  ds <- read_tsv(file) |>
    mutate(GeneID = toupper(GeneID)) |> 
    filter(rowSums(across(where(is.numeric))) > 0) |>
    filter(str_detect(GeneID, pattern="ENSMUSG")) |>
    column_to_rownames("GeneID") |>
    set_names(metadata$sample)
  
  norms <- as.matrix(ds) |>
    DGEList(group=metadata$condition) |>
    calcNormFactors() |>
    pluck("samples", "norm.factors")
  
  dsn <- t(t(ds) * norms) |> as.data.frame()
  
  
  ds2 <- ds[, c(1:3, 7:9)]
  dsn2 <- dsn[, c(1:3, 7:9)]
  
  list(ds=ds, norms=norms, dsn=dsn, ds2=ds2, dsn2=dsn2, metadata=metadata)
}


plot_clustering_devspine <- function(cnt) {
  cnt[cnt==0] <- NA
  corr.mat <- cor(log(cnt), use = "complete.obs") 
  dend <- as.dist(1 - corr.mat) |> hclust() |> as.dendrogram()
  cond <- gsub("-\\d", "", colnames(cnt), perl=TRUE) |>
    factor(levels=c("E12.5", "E16.5", "Adult")) |>
    as.numeric()
  cond <- cond[order.dendrogram(dend)]
  labels_colors(dend) <- okabe_ito_palette[cond]
  dend <- dend |> set("labels_cex", 0.6) |> set("branches_lwd", 0.3)
  gg <- as.ggdend(dend)
  ggplot(gg, horiz=FALSE, offset_labels=-0.01) + ylim(-0.1, max(get_branches_heights(dend)))
}


plot_distance_matrix <- function(cnt, metadata, distance=c("correlation"), text.size=10) {
  distance <- match.arg(distance)
  
  cnt[cnt==0] <- NA
  d <- log(cnt) |> 
    cor(use = "complete.obs") |> 
    as_tibble(rownames = "sample1") |> 
    pivot_longer(-sample1, names_to="sample2") |> 
    mutate_at(vars(sample1, sample2), ~factor(.x, levels = metadata$sample))
  clr <- okabe_ito_palette[as_factor(metadata$condition)]
  ggplot(d, aes(x=sample1, y=sample2)) +
    theme_bw() +
    geom_tile(aes_(fill=~value)) +
    scale_fill_viridis_c(option="cividis") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size=text.size, colour=clr),
      axis.text.y = element_text(size=text.size, colour=clr)
    ) +
    labs(x=NULL, y=NULL, fill="Correlation")
}


get_mouse_genes <- function(version) {
  mart <- useEnsembl(biomart="ensembl", dataset="mmusculus_gene_ensembl", version=version)
  biomart_gene_download(mart)
}


plot_genes_cnt <- function(cnt, gene_ids, genes, cex=3, conditions=c("E12.5", "E16.5", "Adult")) {
  d <- cnt[gene_ids, ] |>
    as_tibble(rownames = "gene_id") |> 
    pivot_longer(-gene_id, names_to="sample", values_to="count") |> 
    mutate(condition = str_remove(sample, "-\\d") |> factor(levels=conditions)) |> 
    left_join(genes, by="gene_id")
  ggplot(d, aes(x=condition, y=count, fill=condition)) +
    theme_bw() +
    theme(legend.position = "none") +
    facet_wrap(~gene_name) +
    geom_beeswarm(shape=21, cex=cex, size=1.8) +
    #scale_y_continuous() +
    scale_fill_manual(values=okabe_ito_palette) +
    labs(x=NULL, y="Count")
}


run_devspine_de <- function(devspine) {
  cond2 <- droplevels(devspine$metadata$condition[c(1:3,7:9)])
  design <- model.matrix(~0 + cond2)
  colnames(design) <- levels(cond2)
  
  gt.contrasts <- makeContrasts(
    "Adult-E12.5" = "Adult-E12.5",
    levels = design
  )
  
  de.edger <- as.matrix(devspine$ds2) |>
    DGEList(group=cond2) |>
    calcNormFactors() |>
    estimateDisp(design=design) |>
    glmQLFit(design=design) |>
    glmQLFTest(contrast=gt.contrasts[, "Adult-E12.5"]) |>
    topTags(n=1e6, adjust.method="BH", sort.by="none") |>
    pluck("table") |> 
    as_tibble(rownames = "gene_id")
  
  test <- function(v) {
    x <- v[1:3]
    y <- v[4:6]
    if(sum(is.na(x)) > 1 | sum(is.na(y)) > 1) {
      return(NA)
    } else {
      return(t.test(x, y)$p.value)
    }
  }
  
  de.t <- devspine$dsn2 |>
    mutate(across(everything(), ~na_if(.x, 0))) |> 
    log10() |>
    apply(1, test)
  
  tibble(gene_id = names(de.t), P = de.t) |> 
    right_join(de.edger, by="gene_id")
}

# select gene examples that are signficant only in t-test and only in edger
t_edger_selection <- function(de, genes) {
  only.t <- de |>
    filter(P < 0.05 & PValue < 0.05 & FDR > 0.1 & logCPM > 2) |> 
    left_join(genes, by="gene_id") |> 
    mutate(selection = "only_t")
  only.edger <- de |> filter(P > 0.1 & FDR < 0.01 & logCPM > 2) |> 
    left_join(genes, by="gene_id") |> 
    mutate(selection = "only_edger")
  
  bind_rows(only.t, only.edger) |> 
    select(selection, gene_id, gene_name, logFC, P, PValue, FDR)
}


read_drugs <- function() {
  list(
    drugs = readRDS("data/drugpred.rds") |> 
      rename(logVDss = response),
    coef_names =  readRDS("data/coef_names.rds")
  )
}

make_model <- function(dat) {
  lm(logVDss ~ ., data=dat[, -1])
}

show_model <- function(mod, coef_names, alpha=0.05) {
  summary(mod)$coefficients |>
    as.data.frame() |>
    rownames_to_column(var="coefficient") |>
    as_tibble() |> 
    mutate(coefficient = gsub("`", "", coefficient)) |>
    filter(`Pr(>|t|)` < alpha) |>
    arrange(`Pr(>|t|)`) |> 
    left_join(coef_names, by="coefficient") |> 
    mutate(value = ifelse(is.na(value), " ", value)) |> 
    select(coefficient, variable, value, estimate = Estimate, t = `t value`, `p-value` = `Pr(>|t|)`)
}
