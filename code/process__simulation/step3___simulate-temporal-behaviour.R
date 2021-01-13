#!/usr/local/bin/Rscript
################################################################################
# Project:  spHomogeneity
# Purpose:  Function to the prescribed NDVI curves of different land covers
#           (STEP 3 in simulation exercise)
# License:  GPL v3
# Authors:  Gregory Duveiller - Jul 2020
################################################################################

require(ggplot2)
require(tidyr)
require(dplyr)

# temporal vector
doi_vctr <- 1:365

# function to simulate 
glogf <- function(doi_vctr, A, K, Q, B, M, v){
  # B : steepness at inflexion point
  # M : position of the inflexion point
  # K : amplitude
  # Q : (not sure anymore, but linked to the position)
  # A : baseline (at the beginning, not sure how useful it is)
  # v : overall shape of the curve
  z <- A + (K-A)/(1 + Q*exp(-B*(doi_vctr - M)))^(1/v)}

# base time series
TS1 <- glogf(doi_vctr, A = 0, K = 0.8, B = 0.1, Q = 1, v = 1, M = 70) -
       glogf(doi_vctr, A = 0, K = 0.8, B = 0.05, Q = 1, v = 1, M = 220)

# shift in pheno
temp_shift <- 5
TS2 <- c(TS1[(temp_shift+1):365], TS1[1:temp_shift])
temp_shift <- 50
TS3 <- c(TS1[(temp_shift+1):365], TS1[1:temp_shift])
temp_shift <- 182
TS4 <- c(TS1[(temp_shift+1):365], TS1[1:temp_shift])

# shift in value
TS5 <- TS1 + 0.1 

# scaling
TS6 <- (TS1/2) + 0.1

# flat signal high
TS7 <- rep(0.6, times = length(doi_vctr))

# flat signal low
TS8 <- rep(0.1, times = length(doi_vctr))

# scaling
TS9 <- TS6 + 0.4
# 

df.ideal.ts <- data.frame(DOI = doi_vctr, TS1, TS2, TS3, TS4, TS5, TS6, TS7, TS8, TS9) 

save('df.ideal.ts', 
     file = paste0('data/inter_data/', batch_name, '/df-ideal-ts.Rda'))



g <- ggplot(df.ideal.ts %>%
              pivot_longer(-DOI, names_to = 'TS', values_to = "NDVI")) + 
  geom_line(aes(x = DOI, y = NDVI, colour = TS)) +
  scale_color_viridis_d()

ggsave(filename = 'test_profiles.png', plot = g, 
       path = paste0('data/inter_data/', batch_name, '/'),
       width = 7, height = 5)


