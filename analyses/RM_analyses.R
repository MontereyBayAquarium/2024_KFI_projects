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
                      ifelse(purple_urchin_densitym2 == 0 & purple_urchin_conceiledm2 == 0, NA, 1)))%>%
  #select focal vars to reduce size of df
  select(survey_date, site, site_type, zone, transect, quadrat, recruit_density, substrate, 
         purple_urchin_densitym2, prop_exp, gast_density, grazer_density,
         density20m2_macro_plants, density20m2_macro_stipes, substrate,
         relief, risk) %>%
  #drop outliers
  filter(gast_density < 50)

################################################################################
#step2 - plot


base_theme <-  theme(axis.text=element_text(size=8, color = "black"),
                     axis.title=element_text(size=9,color = "black"),
                     plot.tag=element_text(size=8,color = "black"),
                     plot.title=element_text(size=9,color = "black", face = "bold"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.4, "cm"), 
                     #legend.key = element_rect(fill = "white"), # Set it to transparent
                     legend.spacing.y = unit(0.4, "cm"),  
                     legend.text=element_text(size=8,color = "black"),
                     legend.title=element_blank(),
                     #legend.key.height = unit(0.1, "cm"),
                     #legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=8, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())


p1 <- ggplot(recruit_build1, aes(x = gast_density, y = recruit_density)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Gastropod density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
p1



p2 <- ggplot(recruit_build1, aes(x = gast_density, y = purple_urchin_densitym2)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Purple sea urchin density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
p2

p3 <- ggplot(recruit_build1, aes(x = gast_density, y = grazer_density)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Purple sea urchin density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
p3



p4 <- ggplot(recruit_build1, aes(x = gast_density, y = prop_exp)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Purple sea urchin density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
p4

ggpubr::ggarrange(p1,p2,p3,p4)




