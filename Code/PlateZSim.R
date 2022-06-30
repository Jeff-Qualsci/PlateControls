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
make_plate <- function(z, nwells, platenum) {
  True_Z <- z
  PlateNum <- platenum
  CtrlN <- nwells
  MaxCtrl <- rnorm(nwells, 1, (1-z) / 6)
  MinCtrl <- rnorm(nwells, 0, (1-z) / 6)
  tibble(True_Z, PlateNum, CtrlN, MaxCtrl, MinCtrl)
}


TrueZ <- c(.4, .6, .8)
Plates <- seq.int(1, 1000, 1)
CtrlWell <- seq.int(4, 64, 2)

set.seed(903)

PlateCtrls <- tibble()

for (i in seq_along(TrueZ)) {
  for (j in seq_along(CtrlWell)) {
    for (k in seq_along(Plates)) {
      newplate <- make_plate(TrueZ[i], CtrlWell[j], Plates[k])
      PlateCtrls <- bind_rows(PlateCtrls, newplate)
    }
  }
}

saveRDS(PlateCtrls, file = 'PlateCtrls.RDS')

PlateZs <- PlateCtrls %>% 
  group_by(True_Z, CtrlN, PlateNum) %>% 
  summarise(StdZ = Zstd(MinCtrl, MaxCtrl),
            RobZ = Zrob(MinCtrl, MaxCtrl),
            MaxMean = mean(MaxCtrl),
            MaxMed = median(MaxCtrl),
            MaxSD = sd(MaxCtrl),
            MaxMAD = mad(MaxCtrl),
            MinMean = mean(MinCtrl),
            MinMed = median(MinCtrl),
            MinSD = sd(MinCtrl),
            MinMad = mad(MinCtrl)
          ) %>%
  ungroup() 

saveRDS(PlateZs, file = 'PlateZs.RDS')

PlateCtrls <- readRDS('PlateCtrls.RDS')
PlateZs <- readRDS('PlateZs.RDS')
