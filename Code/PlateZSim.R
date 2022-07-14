# Plate Z Simulator
# Compare standard Z' and robust Z' as a function of the number of control wells/plate

library(tidyverse)

# Standard Z' calculation
Zstd <- function(ctrlmin, ctrlmax) {
  1 - ((3 * (sd(ctrlmax) + sd(ctrlmin))) / abs(mean(ctrlmax) - mean(ctrlmin)))
}

# Robust Z' calculation
Zrob <- function(ctrlmin, ctrlmax) {
  1 - ((3 * (mad(ctrlmax) + mad(ctrlmin))) / abs(median(ctrlmax) - median(ctrlmin)))
}

# simulate plate control wells 
sim_plate <- function(z, nwells, platenum) {
  True_Z <- z
  PlateNum <- platenum
  CtrlN <- nwells
  MaxCtrl <- rnorm(nwells, 1, (1-z) / 6)
  MinCtrl <- rnorm(nwells, 0, (1-z) / 6)
  Std_Z <- Zstd(MinCtrl, MaxCtrl)
  Rob_Z <- Zrob(MinCtrl,MaxCtrl)
  tibble(True_Z, PlateNum, CtrlN, Std_Z, Rob_Z)
}

TrueZ <- c(.4, .6, .8)
Plates <- seq.int(1, 1000, 1)
CtrlWell <- seq.int(4, 64, 4)
nplates <- length(TrueZ) * length(Plates) * length(CtrlWell)

set.seed(90362)
PlateZs <- vector('list', length = nplates)
Plate <- 1

for (i in seq_along(TrueZ)) {
  for (j in seq_along(CtrlWell)) {
    for (k in seq_along(Plates)) {
      PlateZs[[Plate]] <- sim_plate(TrueZ[i], CtrlWell[j], Plates[k])
      Plate <- Plate + 1
    }
  }
}

PlateZs <- bind_rows(PlateZs) %>% 
  mutate(True_Z = as_factor(True_Z))

Zsummary <- PlateZs %>% 
  pivot_longer(cols = c(Std_Z, Rob_Z), names_to = 'StatMethod', values_to = 'Z') %>% 
  mutate(StatMethod = if_else(StatMethod == 'Std_Z', 'Standard', 'Robust')) %>% 
  group_by(True_Z, CtrlN, StatMethod) %>% 
  summarise(Zavg = mean(Z),
            StdDev = sd(Z),
            MAD = mad(Z),
            UCL = Zavg + 2 * StdDev,
            LCL = Zavg - 2 * StdDev,
            rUCL = quantile(Z, 0.995),
            rLCL = quantile(Z, 0.005))%>% 
  ungroup()

ggplot(Zsummary, aes(x = CtrlN, y = Zavg, color = True_Z)) +
  geom_smooth(se = FALSE) + 
  geom_ribbon(aes(ymin = rLCL, ymax = rUCL, alpha = 0.2, fill = True_Z )) +
  theme_minimal() +
  facet_wrap(~ StatMethod)

