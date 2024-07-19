![](images/NZILBB2.png)

# How stable are patterns of covariation across time?

Gia Hurring, Joshua Wilson Black, Jen Hay, Lynn Clark

## How to use this repository

This repository contains the analysis for "How stable are patterns of 
covariation across time" (under review).

There are two primary ways to use this repository:

1. The easy way: read through the full analysis as a website by clicking 
[here](https://nzilbb.github.io/qb_stability_public/markdown/QB-analysis.html).
2. The harder way: clone the repository to rerun our analysis locally or to
modify it. If you don't know how to do this in RStudio, go here: <https://happygitwithr.com/new-github-first#new-rstudio-project-via-git>.

## Repository structure

The repository contains a series of sub-directories.
  - `data`: contains anonymised data used in the project.
  - `images`: contains logos and images used in the repository.
  - `markdown`: contains the Rmarkdown file with the analysis code and the 
  'knit' html version.
  - `plots`: contains plots used in the paper (generated in the R markdown file).
  - `scripts`: contains additional scripts used in the analysis. These include, e.g.:
    - `qb1_extraction.R`: a script to extract the raw data from LaBB-CAT.
    - `anonymisation.R`: a script to anonymise the raw data.
    
## Models

The models fit in the paper are not completely deterministic (even if one 
sets a random number seed). 

The models reported in the paper are available at <https://osf.io/jrtmx/>. 
Simply download the `models` directory from OSF storage and place it in the 
project directory.

