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


# Set-up ---- 

# (Re)Harvest the data for the figures?
harvestData <- F
if(harvestData){
  source('code/figures/harvest___vectors4figs.R')
  source('code/figures/harvest___california-TCI-ts.R')
}


# set printing properties (place and format)
fig.fmt <- 'pdf'
fig.path <- paste0('docs/article_Figures')
dir.create(fig.path, showWarnings = F, recursive = T)


source('code/figures/fig___simulation.R')
source('code/figures/fig___california-TCI-rs.R')
source('code/figures/fig___local-TCI.R')
source('code/figures/fig___explain-res-calc.R')
