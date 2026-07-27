# Temp script to run multiple prepared scripts

#finishing off water, using a mosaic atl 2026 baseline mask
source(file.path("scripts", "stitch4_water_fvs_mask.R"))

#params set to run fvs var mcuft and tcuft
source(file.path("scripts", "prep4_fvs_thin_combine.R"))
source(file.path("scripts", "stitch2_fvs_withbaseline.R"))
#mosaic needs to wait until fvs3-var is done, and script updated.

#set up for fvs 3-var (atl, pot smoke, fl)
source(file.path("scripts", "qa", "qa3_fvs_perc_change.R"))
