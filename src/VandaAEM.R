# The following R code is used to plot flight lines and resistivity 
# cross sections in the Wright Valley, Antarctica. 

# Data were collected in 2018 as part of an AEM campaign.
# This work was supported by the National Science Foundation Office of 
# Polar Programs Award 1643536, 1643687, 1643775, and 1644187. 

# The original flight line IDs are changed for presentation
# Line 2902 = Line 1
# Line 2901 = Line 2
# Line 21208 = Line 3

# Load libraries
library(zoo)
library(tidyverse)
library(RStoolbox) # for function ggRGB
library(metR) # for function geom_contour_fill
library(MetBrewer)
library(patchwork)
library(raster)

# Load functions
source('src/plotFlightLine.R')
# Load Vanda Sentinel imagery
v1 = brick('spatialdata/vanda_sentinel2_2022_01_16.tif')
# Load Vanda DEM
DEM3 = raster('spatialdata/VandaDEM3.grd')

# Load AEM data
df.vanda.profiles = read_csv('data/Vanda_AEM.csv')
df.djp.profiles = read_csv('data/DJP_AEM.csv')

# Filter map coordinates
mapV = df.vanda.profiles |> dplyr::select(LINE_NO:UTMY)
mapDJP = df.djp.profiles |> dplyr::select(LINE_NO:UTMY)

# Plot flight lines
p1 = RStoolbox::ggRGB(v1, r=1, g=2, b=3, stretch = 'sqrt') +
  geom_point(data = mapV, aes(x = UTMX, UTMY, col = as.character(LINE_NO)), shape = 'x') +
  scale_color_manual(values = c('#EF8354', '#F9DC5C')) +
  geom_point(data = mapDJP, aes(x = UTMX, UTMY), shape = 'x', col = '#F6C6B6') +
  # Add DVDP borehole locations 
  geom_point(aes(x = 416673, y = 1391082), fill = 'white', shape = 21) + # DVDP 4 Vanda
  geom_point(aes(x = 413490.2, y = 1390340), fill = 'white', shape = 21) + # DVDP 14 Northfok 
  geom_point(aes(x = 408190, y = 1387364), fill = 'white', shape = 21) + # DVDP 13 DJP
  annotate('text', label = 'DVDP14', x = 414000, y = 1391200, hjust = 1, vjust = 1, size = 2, color = 'white') +
  annotate('text', label = 'DVDP4', x = 416500, y = 1392000, hjust = 1, vjust = 1, size = 2, color = 'white') +
  annotate('text', label = 'DVDP13', x = 409500, y = 1388100, hjust = 1, vjust = 1, size = 2, color = 'white') +
  # Label Lines
  annotate('text', label = 'Line 1', x = 412000, y = 1392000, hjust = 1, vjust = 1, size = 2.5, color = '#F9DC5C') +
  annotate('text', label = 'Line 3', x = 418000, y = 1390000, hjust = 1, vjust = 1, size = 2.5, color = '#EF8354') +
  annotate('text', label = 'Line 2', x = 409500, y = 1386800, hjust = 1, vjust = 1, size = 2.5, color = '#F6C6B6') +
  theme_minimal(base_size = 8) +
  theme(axis.title = element_blank(), 
        axis.text = element_text(size = 6),
        legend.position = 'none')

plots = list()
# Line 1 
plots[[1]] = plotFlightLine(lineNo = 2902, aemDF = df.vanda.profiles, 
                            dvdpLocation = c(416673, 1391082, 87.73), dvdpLocation2 = c(413490.2, 1390340, 78)) + 
  ylim(-425, 275) +
  labs(title = 'Line 1') + theme(axis.title.x = element_blank())
# Line 2
plots[[2]] = plotFlightLine(lineNo = 2901, aemDF = df.vanda.profiles) + 
  ylim(-425, 275) + labs(title = 'Line 3')
# Line 2
plots[[3]] = plotFlightLine(lineNo = 21208, aemDF = df.djp.profiles, 
                            dvdpLocation = c(408190, 1387364, 75)) + 
  ylim(-425, 275) + labs(title = 'Line 2')

# Assemble plots
layout <- c(
  patchwork::area(t = 1, l = 1, b = 2, r = 5), # map
  patchwork::area(t = 3, l = 1, b = 4, r = 6), # Line 1
  patchwork::area(t = 5, l = 1, b = 6, r = 3), # Line 2
  patchwork::area(t = 5, l = 4, b = 6, r = 6), # Line 3
  patchwork::area(t = 1, l = 6, b = 2, r = 6) #legend
)

output = p1 + plots[[1]] + plots[[3]] + plots[[2]] + guide_area() +
     plot_layout(design = layout, guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 8))

ggsave(plot = output, 'figures/Figure1.png', height = 8, width = 6.5, dpi = 500)


