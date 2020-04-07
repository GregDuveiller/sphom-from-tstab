# step3___simulate-temporal-behaviour.R
#
# Step 3 in spHomogeneity simulation: define the prescribed NDVI curves
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
TS6 <- TS1/2

# flat signal high
TS7 <- rep(0.6, times = length(doi_vctr))

# flat signal low
TS8 <- rep(0.1, times = length(doi_vctr))

# scaling
TS9 <- TS6 + 0.4
# 

df.ideal.ts <- data.frame(DOI = doi_vctr, TS1, TS2, TS3, TS4, TS5, TS6, TS7, TS8, TS9) 

save('df.ideal.ts', 
     file = paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))



g <- ggplot(df.ideal.ts %>%
              pivot_longer(-DOI, names_to = 'TS', values_to = "NDVI")) + 
  geom_line(aes(x = DOI, y = NDVI, colour = TS)) +
  scale_color_viridis_d()

ggsave(filename = 'test_profiles.png', plot = g, 
       path = paste0('dataProcessing/', batch_name, '/'),
       width = 7, height = 5)




# # OLD ONES
#
# TS1 <- glogf(doi_vctr, A = 0, K = 0.9, B = 0.1, Q = 1, v = 1, M = 80) -
#   glogf(doi_vctr, A = 0, K = 0.9, B = 0.05, Q = 1, v = 1, M = 260)
# 
# TS2 <- glogf(doi_vctr, A = 0, K = 0.7, B = 0.05, Q = 1, v = 1, M = 60) -
#   glogf(doi_vctr, A = 0, K = 0.7, B = 0.025, Q = 1, v = 1, M = 240)
# 
# TS3 <- glogf(doi_vctr, A = 0, K = 0.85, B = 0.09, Q = 1, v = 1, M = 90) -
#   glogf(doi_vctr, A = 0, K = 0.85, B = 0.05, Q = 1, v = 1, M = 280)
# 
# TS4 <- glogf(doi_vctr, A = 0, K = 3.2, B = 0.005, Q = 1, v = 1, M = 80) -
#   glogf(doi_vctr, A = 0, K = 3.2, B = 0.005, Q = 1, v = 1, M = 260)
# 
# TS5 <- glogf(doi_vctr, A = 0, K = 0.9, B = 0.1, Q = 1, v = 1, M = 140) -
#   glogf(doi_vctr, A = 0, K = 0.9, B = 0.05, Q = 1, v = 1, M = 320)
# 
# TS6 <- glogf(doi_vctr, A = 0, K = 0.5, B = 0.1, Q = 1, v = 1, M = 80) -
#   glogf(doi_vctr, A = 0, K = 0.5, B = 0.05, Q = 1, v = 1, M = 260)
# 
# TS7 <- glogf(doi_vctr, A = 0, K = 0.7, B = 0.05, Q = 1, v = 1, M = 120) -
#   glogf(doi_vctr, A = 0, K = 0.7, B = 0.025, Q = 1, v = 1, M = 300)
# 
# TS8 <- TS4 + 0.2
# 
# TS9 <- glogf(doi_vctr, A = 0, K = 0.9, B = 0.1, Q = 1, v = 1, M = 200) -
#   glogf(doi_vctr, A = 0, K = 0.9, B = 0.05, Q = 1, v = 1, M = 380)
# 
# 
# df.ideal.ts <- data.frame(DOI = doi_vctr, TS1, TS2, TS3, TS4, TS5, TS6, TS7, TS8, TS9)
# save('df.ideal.ts', file = 'dataProcessing/df-ideal-ts.Rda')

