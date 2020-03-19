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
source('codeProcessing/make_synthetic_landscape.R')

