#


#rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here, ggeffects)

figdir <- here::here("figures","RM_figures")


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
  mutate(substrate = factor(substrate, levels = c("Sand","Cobble","Boulder","Bedrock")))
  #drop outliers
  #filter(recruit_density > 1)

################################################################################
#step2 - plot Figure 1


# Fit a GLM model, first with interation
glm_model <- glm(recruit_density ~ gast_density + purple_urchin_densitym2 + prop_exp +
                   gast_density * purple_urchin_densitym2,
                 data = recruit_build1, family = "poisson")

#interaction was non-sig so dop
# Fit the GLM model
glm_model <- glm(recruit_density ~ gast_density + purple_urchin_densitym2 + prop_exp,
                 data = recruit_build1, family = "poisson")

# Summarize the model
summary(glm_model)

#plot the output

base_theme <-  theme(axis.text=element_text(size=9, color = "black"),
                     axis.title=element_text(size=10,color = "black"),
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


# Generate predictions using ggpredict for each term
pred_gast <- ggpredict(glm_model, terms = "gast_density")
pred_urchin <- ggpredict(glm_model, terms = "purple_urchin_densitym2")
pred_prop_exp <- ggpredict(glm_model, terms = "prop_exp")


# Combine the predictions into one data frame and add a term column for faceting
pred_combined <- rbind(
  data.frame(pred_gast, term = "gast_density"),
  data.frame(pred_urchin, term = "purple_urchin_densitym2"),
  data.frame(pred_prop_exp, term = "prop_exp")
)

# Modify the order of the 'term' factor
pred_combined$term <- factor(pred_combined$term, 
                             levels = c("gast_density", "purple_urchin_densitym2", "prop_exp"))


# Define custom labels for each predictor
custom_labels <- c(
  gast_density = "Gastropod density \n(no. per m²)",
  purple_urchin_densitym2 = "Purple urchin density \n(no. per m²)",
  prop_exp = "Proportion of exposed urchins \n(per m²)"
)



# Extract p-values and coefficients from the model summary
summary_info <- summary(glm_model)

# Create a data frame for coefficients and p-values
model_terms <- data.frame(
  term = rownames(summary_info$coefficients),
  estimate = summary_info$coefficients[, "Estimate"],
  p_value = summary_info$coefficients[, "Pr(>|z|)"]
)

# Filter to only include the terms we're plotting
model_terms <- model_terms[model_terms$term %in% c("gast_density", "purple_urchin_densitym2", "prop_exp"), ]

# Format the coefficient and p-value for each term
model_terms$label <- paste0("Coefficient: ", round(model_terms$estimate, 3),
                            "\nP-value: ", ifelse(model_terms$p_value < 0.001, "< 0.001", round(model_terms$p_value, 3)))

# Map the terms to match the custom labels in the plot
model_terms$term <- factor(model_terms$term, 
                           levels = c("gast_density", "purple_urchin_densitym2", "prop_exp"))


# Plot using ggplot with p-values and coefficients in each facet
p <- ggplot(pred_combined, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  facet_wrap(~ term, scales = "free", labeller = as_labeller(custom_labels), strip.position = "bottom") +  # Move labels below
  labs(title = "",
       x = "",
       y = "Predicted kelp recruit density \n(no. per m²)") +
  theme_bw() + 
  theme(strip.placement = "outside",  # Ensure labels are placed outside
        strip.background = element_blank(),  # Remove background of strip
        strip.text.x = element_text(hjust = 0.5, size = 10, face = "plain")) +  # Center and remove bold
  geom_text(data = model_terms, aes(x = Inf, y = Inf, label = label),
            hjust = 1.1, vjust = 1.1, size = 3) +  # Add the labels for p-value and coefficient
  base_theme



ggsave(p, filename = file.path(figdir, "Fig1_glm_model.png"), 
       width =7, height = 5, units = "in", dpi = 600, bg = "white")




################################################################################
#step3 - plot Figure 2


g1 <- ggplot(recruit_build1, aes(x = density20m2_macro_stipes, y = recruit_density)) +
  geom_point() +
 # geom_smooth(method = "nls", 
  #            formula = y ~ a * exp(b * x), 
   #           method.args = list(start = list(a = 50, b = -1)),  
    #          se = FALSE) +
  labs(title = "", 
       x = "Adult kelp stipe density", 
       y = "Kelp recruit density")+
  theme_bw() + base_theme
g1



g2 <- ggplot(recruit_build1, aes(x = density20m2_macro_plants, y = recruit_density)) +
  geom_point() +
  # geom_smooth(method = "nls", 
  #            formula = y ~ a * exp(b * x), 
  #           method.args = list(start = list(a = 50, b = -1)),  
  #          se = FALSE) +
  labs(title = "", 
       x = "Adult kelp plant \ndensity (n. plants per 20m²)", 
       y = "Kelp recruit density")+
  theme_bw() + base_theme
g2

ggpubr::ggarrange(g1, g2)








#


#rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here)

figdir <- here::here("figures")


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
  mutate(substrate = factor(substrate, levels = c("Sand","Cobble","Boulder","Bedrock")))
#drop outliers
#filter(recruit_density > 1)

################################################################################
#step2 - plot Figure 1


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



p2 <- ggplot(recruit_build1, aes(x = purple_urchin_densitym2, y = recruit_density)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Purple sea urchin density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
#p2

p3 <- ggplot(recruit_build1, aes(x = grazer_density, y = recruit_density)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "COmbined grazer density", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
#p3



p4 <- ggplot(recruit_build1, aes(x = prop_exp, y = recruit_density)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Proportion exposed sea urchins", 
       y = "Kelp recruit Density")+
  theme_bw() + base_theme
#p4

p <- ggpubr::ggarrange(p1,p2,p3,p4)
p


#run GLM
# Fit the GLM model
glm_model <- glm(recruit_density ~ gast_density + purple_urchin_densitym2 + prop_exp +
                   gast_density * purple_urchin_densitym2,
                 data = recruit_build1, family = "poisson")

summary(glm_model)


install.packages("ggeffects")
library(ggeffects)

# Generate the effects for the GLM model
effects <- ggeffect(glm_model, terms = c("gast_density", "purple_urchin_densitym2", "prop_exp"))

# Plot the effects using ggplot2
plot(effects) +
  labs(title = "Estimated Marginal Means of Recruit Density",
       x = "Predictor",
       y = "Estimated Marginal Mean") +
  theme_minimal()

################################################################################
#step3 - plot Figure 2


g1 <- ggplot(recruit_build1, aes(x = density20m2_macro_stipes, y = recruit_density)) +
  geom_point() +
  # geom_smooth(method = "nls", 
  #            formula = y ~ a * exp(b * x), 
  #           method.args = list(start = list(a = 50, b = -1)),  
  #          se = FALSE) +
  labs(title = "", 
       x = "Adult kelp stipe density", 
       y = "Kelp recruit density")+
  theme_bw() + base_theme
g1



g2 <- ggplot(recruit_build1, aes(x = density20m2_macro_plants, y = recruit_density)) +
  geom_point() +
  # geom_smooth(method = "nls", 
  #            formula = y ~ a * exp(b * x), 
  #           method.args = list(start = list(a = 50, b = -1)),  
  #          se = FALSE) +
  labs(title = "", 
       x = "Adult kelp plant \ndensity (n. plants per 20m²)", 
       y = "Kelp recruit density")+
  theme_bw() + base_theme
g2

ggpubr::ggarrange(g1, g2)


################################################################################
#step3 - plot Figure 3


m1 <- ggplot(recruit_build1 %>% filter(recruit_density > 0), aes(x = substrate, y = recruit_density)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Substrate Type", y = "Recruit Density", title = "Recruit Density by Substrate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + base_theme
m1





# Scatter plot of recruit_density as a function of relief with a curve over the top
m_recruit_density_top <- ggplot(recruit_build1, aes(x = relief, y = recruit_density)) +
  
  # Scatter plot of the points
  geom_point(color = "navyblue", alpha = 0.6) +
  
  # Add a quantile regression line for the upper quantile (0.95 for example)
  geom_smooth(method = "rq", formula = y ~ x, method.args = list(tau = 0.95), 
              se = FALSE, color = "navyblue", fill = "navyblue", alpha = 0.3) +
  
  labs(title = "Recruit Density as a Function of Relief (Upper Bound Curve)",
       x = "Relief",
       y = "Recruit Density") +
  theme_bw()

# Plot the result
m_recruit_density_top






################################################################################
#step3 - plot Figure 3


m1 <- ggplot(recruit_build1 %>% filter(recruit_density > 0), aes(x = substrate, y = recruit_density)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Substrate Type", y = "Recruit Density", title = "Recruit Density by Substrate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + base_theme
m1





# Scatter plot of recruit_density as a function of relief with a curve over the top
m_recruit_density_top <- ggplot(recruit_build1, aes(x = relief, y = recruit_density)) +
  
  # Scatter plot of the points
  geom_point(color = "navyblue", alpha = 0.6) +
  
  # Add a quantile regression line for the upper quantile (0.95 for example)
  geom_smooth(method = "rq", formula = y ~ x, method.args = list(tau = 0.95), 
              se = FALSE, color = "navyblue", fill = "navyblue", alpha = 0.3) +
  
  labs(title = "Recruit Density as a Function of Relief (Upper Bound Curve)",
       x = "Relief",
       y = "Recruit Density") +
  theme_bw()

# Plot the result
m_recruit_density_top












