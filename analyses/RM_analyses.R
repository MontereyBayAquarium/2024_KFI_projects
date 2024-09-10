#



rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here)

figdir <- here::here(figures)

################################################################################
#step1 - process data

#calculate total recruit density. Recruit - recruit and juvenile

recruit_build1 <- quad_build_combined %>%
  mutate(
    # Calculate recruit density
    recruit_density = lamr + macr + macj + nerj + ptej + 
      lsetj + eisj,
    # Calculate gastropod density
    gast_density = tegula_densitym2 + pomaulax_densitym2,
    # Calculate urchin & gastropod density
    grazer_density = gast_density + purple_urchin_densitym2,
    # Calculate proportion urchins exposed
    prop_exp = ifelse(purple_urchin_densitym2 > 0, 
                      (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2,
                      1)
  )

                   

plot(recruit_build1$recruit_density ~ recruit_build1$grazer_density)

View(quad_build_combined)

