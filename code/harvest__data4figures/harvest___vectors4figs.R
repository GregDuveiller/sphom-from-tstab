#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Get vector data for global figure in the article 
# License:  GPL v3
# Authors:  Gregory Duveiller - Jan 2021
################################################################################


require(rnaturalearth)

path.name <- 'data/inter_data/natural_earth'

dir.create(path.name, showWarnings = F, recursive = T)
provs <- ne_download(scale = 50, type = 'admin_1_states_provinces_lines',
                     destdir = path.name, returnclass = 'sf')
ocean <- ne_download(scale = 50, type = 'ocean', category = 'physical',
                     destdir = path.name, returnclass = 'sf')
lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical',
                     destdir = path.name, returnclass = 'sf')
countries <- ne_download(scale = 50, type = 'admin_0_countries', 
                     destdir = path.name, returnclass = 'sf')
