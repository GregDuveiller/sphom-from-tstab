#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Orchestrate the creation of figures for the article
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


library(ggplot2)
library(grid)
library(scales)
library(here)


fig.fmt <- 'png'
fig.path <- paste0('docs/article_Figures/', fig.fmt)
dir.create(fig.path, showWarnings = F, recursive = T)


source('code/figures/fig___simulation.R')
