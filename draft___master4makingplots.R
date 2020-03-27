

source('codeProcessing/draft___testmaps_basemetrics.R')

landscape_ID <- 'landscape-2LC-id42'
psf_fname <- 'PSF-AQUA-48-10'

plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS1', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS2', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS4', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS5', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS6', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS7', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS9', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS2', LC0 = 'TS1', ndvi_noise = 0.01)
pplot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS2', LC0 = 'TS4', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS5', LC0 = 'TS1', ndvi_noise = 0.01)


plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS2', ndvi_noise = 0.05)

plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.05)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS1', LC0 = 'TS3', ndvi_noise = 0.1)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS4', LC0 = 'TS1', ndvi_noise = 0.01)
plot_metrics(landscape_ID = landscape_ID, psf_fname = psf_fname, 
             LC1 = 'TS7', LC0 = 'TS1', ndvi_noise = 0.01)
