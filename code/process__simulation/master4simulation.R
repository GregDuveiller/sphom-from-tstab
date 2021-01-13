#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Orchestrate a simulation exercise to generate an landscape that can 
#           serve to characterize the properties of a temporal coherence metric
#           for MODIS times series. This metric (called TCI) should be capable 
#           to describe the landscape spatial homogeneity/heterogeneity.
# License:  GPL v3
# Authors:  Gregory Duveiller - Jul 2020
################################################################################

library(here)

# setup general properties of the landscape
batch_name <- 'simulation_final'
TS2LC <- c('LC1'='TS1', 'LC2'='TS8', 'LC3'='TS3', 'LC4'='TS6')
dir.create(path = paste0('data/inter_data/', batch_name,'/'), 
           showWarnings = F, recursive = T)

# generate the landscape
source('code/process__simulation/step1___generate-synthetic-landscape.R')
gen_syn_landscape(batch_name, landscape_seed = 42, TS2LC)

# simulate the MODIS spatial response
source('code/process__simulation/step2___simulate-MODIS-purity.R')
sim_MODIS_purity(batch_name, TS2LC, spres = 100, platform = 'TERRA', lat = 48)
sim_MODIS_purity(batch_name, TS2LC, spres = 100, platform = 'AQUA', lat = 48)

# define idealized temporal behaviours
source('codeProcessing/step3___simulate-temporal-behaviour.R')

# generate the synthethic data blocks
source('codeProcessing/step4___generate-synthetic-datablock.R')
gen_syn_datablock(batch_name = batch_name,
                  psf_fname = 'PSF-TERRA-48-20', 
                  TS2LC = TS2LC, 
                  spat_perturb = NULL,
                  ndvi_noise = 0.01)
gen_syn_datablock(batch_name = batch_name,
                  psf_fname = 'PSF-AQUA-48-20', 
                  TS2LC = TS2LC,
                  spat_perturb = NULL,
                  ndvi_noise = 0.01)

# calculate the desired metrics
source('codeProcessing/step5___calculate-temporal-metrics.R')
calc_temp_metrics(batch_name = batch_name,
                  TS2LC = TS2LC, 
                  temp_subsample = 5)
