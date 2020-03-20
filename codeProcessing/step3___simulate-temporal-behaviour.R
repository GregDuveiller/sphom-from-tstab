# step3___simulate-temporal-behaviour.R
#
# Step 3 in spHomogeneity simulation: define the prescribed NDVI curves


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


TS1 <- glogf(doi_vctr, A = 0, K = 0.9, B = 0.1, Q = 1, v = 1, M = 80) -
       glogf(doi_vctr, A = 0, K = 0.9, B = 0.05, Q = 1, v = 1, M = 260)

TS2 <- glogf(doi_vctr, A = 0, K = 0.7, B = 0.05, Q = 1, v = 1, M = 60) -
  glogf(doi_vctr, A = 0, K = 0.7, B = 0.025, Q = 1, v = 1, M = 240)

TS3 <- glogf(doi_vctr, A = 0, K = 0.85, B = 0.09, Q = 1, v = 1, M = 90) -
  glogf(doi_vctr, A = 0, K = 0.85, B = 0.05, Q = 1, v = 1, M = 280)

TS4 <- glogf(doi_vctr, A = 0, K = 3.2, B = 0.005, Q = 1, v = 1, M = 80) -
  glogf(doi_vctr, A = 0, K = 3.2, B = 0.005, Q = 1, v = 1, M = 260)

TS5 <- glogf(doi_vctr, A = 0, K = 0.9, B = 0.1, Q = 1, v = 1, M = 140) -
  glogf(doi_vctr, A = 0, K = 0.9, B = 0.05, Q = 1, v = 1, M = 320)

TS6 <- glogf(doi_vctr, A = 0, K = 0.5, B = 0.1, Q = 1, v = 1, M = 80) -
  glogf(doi_vctr, A = 0, K = 0.5, B = 0.05, Q = 1, v = 1, M = 260)

TS7 <- glogf(doi_vctr, A = 0, K = 0.7, B = 0.05, Q = 1, v = 1, M = 120) -
  glogf(doi_vctr, A = 0, K = 0.7, B = 0.025, Q = 1, v = 1, M = 300)

# 
plot(TS1, type = 'l', ylim = c(0,1))
lines(TS2, col = 'red')
lines(TS3, col = 'cornflowerblue')
lines(TS4, col = 'darkgreen')
lines(TS5, col = 'grey50')
lines(TS6, col = 'grey30')
lines(TS7, col = 'orange')

# 

df.ideal.ts <- data.frame(DOI = doi_vctr, TS1, TS2, TS3, TS4, TS5, TS6, TS7)
save('df.ideal.ts', file = 'dataProcessing/df-ideal-ts.Rda')

