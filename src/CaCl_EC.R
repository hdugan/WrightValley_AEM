library(tidyverse)
# https://escholarship.org/content/qt5v01s3c6/qt5v01s3c6.pdf?t=p0fug0
# THE ELECTRICAL CONDUCTIVITY OF
# AQUEOUS SOLUTIONS OF CALCIUM CHLORIDE 

df = data.frame(conc_molL = c(0.3857, 0.965, 1.488, 1.913, 2.406, 2.754, 3.009, 3.046, 3.830, 4.058), 
                EC_ohmCm = c(0.0617, 0.1284, 0.1704, 0.1902, 0.2023, 0.2034, 0.2002, 0.2005, 0.1766, 0.1672)) |> 
  mutate(SpC_mS_cm = EC_ohmCm * 1000) |> 
  mutate(conc_g_L = conc_molL * 110.98)

ggplot(df) +
  geom_point(aes(x = conc_g_L, y = SpC_mS_cm)) +
  geom_line(aes(x = conc_g_L, y = SpC_mS_cm)) +
  labs(title = 'Conductivity of aqueous solutions of calcium chloride at 25°C', 
       caption = 'data from: https://escholarship.org/content/qt5v01s3c6/qt5v01s3c6.pdf?t=p0fug0') +
  theme_bw()

