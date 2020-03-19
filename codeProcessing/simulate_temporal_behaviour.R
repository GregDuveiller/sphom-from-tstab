# simulate_temporal_behaviour.R
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
  # A : baseline (at the beginning)
  #
  z <- A + (K-A)/(1 + Q*exp(-B*(doi_vctr - M)))^(1/v)}

TS1 <- glogf(doi_vctr, A = 0.25, K = 0.9, B = 0.04, Q = 1, v = 1, M = 40) -
       glogf(doi_vctr, A = 0,    K = 0.1, B = 0.01, Q = 1, v = 1, M = 180)


TS1 <- glogf(doi_vctr, A = 0, K = 1, B = 0.1, Q = 1, v = 1, M = 80) -
       glogf(doi_vctr, A = 0, K = 1, B = 0.05, Q = 1, v = 1, M = 260)
  
plot(TS1, type = 'l')

TS2 <- glogf(doi_vctr, A = 0.25, K = 0.9, B = 0.04, Q = 1, v = 1, M = 40) -
  glogf(doi_vctr, A = 0,    K = 0.1, B = 0.01, Q = 1, v = 1, M = 180)

lines(TS2, col = 'grey50')


df.ideal.ts <- data.frame(DOI = doi_vctr, TS1, TS2, TS3)




# # DBF 
# gro <- glogf(doi_vctr, A = 0.25, K = 0.9, B = 0.04, Q = 1, v = 1, M = 40)
# sen <- -glogf(doi_vctr, A = 0, K = 0.1, B = 0.01, Q = 1, v = 1, M = 180)
# DBF <- gro + sen
# 
# # WCR 
# gro <- glogf(doi_vctr, A=0.20, K=0.9, B=0.04, Q=1, v=1, M=70)
# sen <- glogf(doi_vctr, A=0, K=0.6, B=0.07, Q=1, v=1, M=180)
# WCR <- gro-sen
# 
# # SCR 
# gro <- glogf(doi_vctr, A=0.20, K=0.9, B=0.05, Q=1, v=1, M=140)
# sen <- glogf(doi_vctr, A=0, K=0.6, B=0.06, Q=1, v=1, M=230)
# SCR <- gro-sen
# 
# # GRA 
# gro <- glogf(doi_vctr, A=0.20, K=0.7, B=0.03, Q=1, v=1, M=100)
# sen <- glogf(doi_vctr, A=0, K=0.2, B=0.04, Q=1, v=1, M=230)
# GRA <- gro-sen
# 
# # # TRO 
# # gro <- glogf(doi_vctr, A=0.20, K=0.7, B=0.03, Q=1, v=1, M=100)
# # sen <- glogf(doi_vctr, A=0, K=0.2, B=0.04, Q=1, v=1, M=230)
# # TRO <- gro-sen
# 
# df.ideal.ts <- data.frame(DOI = doi_vctr, DBF, WCR, SCR, GRA)
# 
# 
# # quickplot
# plot(doi_vctr, DBF, type = 'l', col = 'darkgreen', ylim = c(0,1))
# lines(doi_vctr, WCR, col = 'wheat3')
# lines(doi_vctr, SCR, col = 'cornflowerblue')
# lines(doi_vctr, GRA, col = 'red')
