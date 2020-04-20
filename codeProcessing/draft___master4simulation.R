# SpHomogeneity G. Duveiller - feb 2020
#
# Objective: To characterise the properties of temporal coherences metrics to
# capture spatial homogeneity using a simulated landscape with temporal dynamics
#
# 




library(here)
dir.create(path = 'dataProcessing', showWarnings = F, recursive = T)
dir.create(path = 'textNotebooks', showWarnings = F, recursive = T)

# Generate landscape

batch_name <- 'batch_002'
psf_fname <- 'PSF-AQUA-48-20'
TS2LC <- c('LC1'='TS1', 'LC2'='TS8', 'LC3'='TS3', 'LC4'='TS9')

dir.create(path = paste0('dataProcessing/',batch_name,'/'), 
           showWarnings = F, recursive = T)


source('codeProcessing/step1___generate-synthetic-landscape.R')
gen_syn_landscape(batch_name, landscape_seed = 42, TS2LC)

source('codeProcessing/step2___simulate-MODIS-purity.R')
sim_MODIS_purity(batch_name, TS2LC, spres = 100)

source('codeProcessing/step3___simulate-temporal-behaviour.R')


source('codeProcessing/step4___generate-synthetic-datablock.R')

gen_syn_datablock(batch_name = batch_name,
                  psf_fname = psf_fname, 
                  TS2LC = TS2LC)

source('codeProcessing/step5___calculate-temporal-metrics.R')

calc_temp_metrics(batch_name = batch_name,
                  psf_fname = psf_fname,
                  TS2LC = TS2LC)

# for(iTS in c('TS8','TS9')){
#   TS_ref <- 'TS1'
#   print(paste('>>> Processing', TS_ref, 'vs', iTS, '...'))
#     gen_syn_datablock(batch_name = 'batch_001',
#                     psf_fname = 'PSF-AQUA-48-10', 
#                     LC1 = TS_ref, LC0 = iTS)
#   print('<<< operation complete!')
# }
# 
# for(iTS in c('TS8','TS9')){
#   TS_ref <- 'TS6'
#   print(paste('>>> Processing', TS_ref, 'vs', iTS, '...'))
#   gen_syn_datablock(batch_name = 'batch_001',
#                     psf_fname = 'PSF-AQUA-48-10', 
#                     LC1 = TS_ref, LC0 = iTS)
#   print('<<< operation complete!')
# }
# 
# gen_syn_datablock(batch_name = 'batch_001',
#                   psf_fname = 'PSF-AQUA-48-10', 
#                   LC1 = 'TS8', LC0 = 'TS9')
# 
# 
# source('codeProcessing/step5___calculate-temporal-metrics.R')
# 
# for(iTS in c('TS2','TS3','TS4','TS5','TS6','TS7','TS8','TS9')){
#   TS_ref <- 'TS1'
#   print(paste('>>> Processing', TS_ref, 'vs', iTS, '...'))
#   calc_temp_metrics(batch_name = 'batch_001',
#                     psf_fname = 'PSF-AQUA-48-10', 
#                     LC1 = TS_ref, LC0 = iTS)
#   print('<<< operation complete!')
# }
# 
# for(iTS in c('TS8','TS9')){
#   TS_ref <- 'TS6'
#   print(paste('>>> Processing', TS_ref, 'vs', iTS, '...'))
#   calc_temp_metrics(batch_name = 'batch_001',
#                     psf_fname = 'PSF-AQUA-48-10', 
#                     LC1 = TS_ref, LC0 = iTS)
#   print('<<< operation complete!')
# }
# 
# calc_temp_metrics(batch_name = 'batch_001',
#                   psf_fname = 'PSF-AQUA-48-10', 
#                   LC1 = 'TS1', LC0 = 'TS1')