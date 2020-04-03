calc_temp_metrics = function(batch_name, psf_fname, LC1, LC0){
# step5___calculate-temporal-metrics.R
#
# Step 5 in spHomogeneity simulation: calculating temporal metrics 


require(dplyr)
require(tidyr)
require(purrr)

# psf_fname <- 'PSF-AQUA-48-20'
# LC1 <- 'TS1'
# LC0 <- 'TS4'

# load target dataset
load(paste0('dataProcessing/', batch_name, 
            '/datablock-', LC1, '-', LC0, '___', psf_fname, '.Rda'))  # 'df.all' & 'df.grd'


# load('dataProcessing/landscape-2LC-id42___datablock-TS1-TS2-0.01___PSF-AQUA-48-10.Rda')

load(paste0('dataProcessing/', batch_name, '/df-ideal-ts.Rda'))  # df.ideal.ts
doi_vctr <- df.ideal.ts$DOI
temp_subsample <- NULL
# should set.seed to simulate frequency of image acquisition
if(!is.null(temp_subsample)){
  set.seed(42)
  doi_vctr_sub <- sort(sample(doi_vctr, length(doi_vctr)/4, replace = F))
} else {doi_vctr_sub <- doi_vctr}


# # function to calculate entropy
# entropy_mm <- function(p){
# 
# # mle estimator with miller-maddow correction
# 
# c <- 0.5 * (sum(p>0)-1)/sum(p)  # miller maddow correction
# p <- p/sum(p);                #  empirical estimate of the distribution
# idx = p!=0;
# H = -sum(p[idx]*log(p[idx])) + c;   # plug-in estimator of the entropy with correction
# }




df.sum <- data.frame(NULL)
for(iGrd in unique(df.all$grd_id)){
  
  df.1 <- df.all %>% 
    filter(grd_id %in% iGrd, DOI %in% doi_vctr_sub) %>%
    filter(!is.na(NDVI)) %>%    #  for some reason, sometimes we have some
    mutate(NDVI.smo = smooth.spline(x = DOI, y = NDVI, df = 8)$y,
           NDVI.res = NDVI - NDVI.smo,
           prob.res = cut(NDVI.res, breaks = seq(-1,1,0.01)))
  
  df.2 <- df.1 %>% count(prob.res)

  df.sum <- df.1 %>%
    summarise(grd_id = iGrd,
              pur_avg = mean(Purity),
              pur_std = sd(Purity),
              var_res = var(NDVI.smo - NDVI),
              var_sig = var(NDVI.smo),
              var_tot = var(NDVI)) %>% 
    bind_cols(ent_nat = entropy.MillerMadow(df.2$n),
              ent_log = entropy.MillerMadow(df.2$n, unit = "log10")) %>%
    bind_rows(df.sum)
  
}


df.sum <- df.sum %>% 
  left_join(df.grd, by = 'grd_id') # add grid info




# save outputs
save('df.sum', 
     file = paste0('dataProcessing/', batch_name,
                   '/metrics-', LC1, '-', LC0, '___', psf_fname, '.Rda'))
}
