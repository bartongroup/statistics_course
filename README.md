# Statistics course figures

R code to create figures for [lecture series on statistics](https://dag.compbio.dundee.ac.uk/training_statistics_lectures.html).

## Usage

We recommend using RStudio. First, you need to install all the packages required by the project. This can be done using [renv](https://cran.r-project.org/web/packages/renv/index.html):

```
install.packages("renv")
renv::restore()
```

Then, the figures can be created using [targets](https://books.ropensci.org/targets/):

```
targets::tar_make()
```

This should create a subdirectory `figures` further divided into individual lectures.
