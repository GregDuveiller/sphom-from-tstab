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
source('codeProcessing/step4___generate-synthetic-datablock.R')

gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS7', LC0 = 'TS1', ndvi_noise = 0.01)

gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS4', LC0 = 'TS1', ndvi_noise = 0.01)

gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.05)

gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.10)

# gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
#                   psf_fname = 'PSF-AQUA-48-10', 
#                   LC1 = 'TS4', LC0 = 'TS8', ndvi_noise = 0.01)
# 
# gen_syn_datablock(landscape_ID = 'landscape-2LC-id42', 
#                   psf_fname = 'PSF-AQUA-48-10', 
#                   LC1 = 'TS2', LC0 = 'TS7', ndvi_noise = 0.01)

source('codeProcessing/step5___calculate-temporal-metrics.R')

calc_temp_metrics(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS7', LC0 = 'TS1', ndvi_noise = 0.01)

calc_temp_metrics(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS4', LC0 = 'TS1', ndvi_noise = 0.01)

calc_temp_metrics(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.05)

calc_temp_metrics(landscape_ID = 'landscape-2LC-id42', 
                  psf_fname = 'PSF-AQUA-48-10', 
                  LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.10)
