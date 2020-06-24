set.seed(1)

x <- rnorm(1000)

bin_range <- c(-5,5)
bin_width <- 0.5
bins <- seq(bin_range[1], bin_range[2], bin_width)

freqs <- table(cut(x, breaks = bins, include.lowest = T))

n <- sum(freqs)
m <- sum(freqs > 0)

freqs <- freqs/n

# Plot to ensure it is more or less Gaussian
plot(freqs) 

# Entropy
H <- -sum(ifelse(freqs > 0, freqs * log2(freqs), 0))/bin_width
# 2.765823

# Entropy.MillerMadow
H.MM <- H + (m - 1)/(2 * n)
# 2.860823

# Entropy if Gaussian (eq 1)
H.norm <- 0.5 * log(2 * pi * exp(1) * sd(x)^2)
# 1.448215
log(sd(x)) + 1.42  # 1.418939



# Entropy if Gaussian (2 * eq 1)
log(2 * pi * exp(1) * sd(x)^2)
# 2.89643

H - H.norm
