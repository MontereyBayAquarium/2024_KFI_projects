#


#rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here, ggeffects, RColorBrewer, ggpubr)

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
glm_model <- glm(recruit_density ~ prop_exp + gast_density + purple_urchin_densitym2,
                 data = recruit_build1, family = "poisson")

# Summarize the model
summary(glm_model)

#plot the output

base_theme <-  theme(axis.text=element_text(size=11, color = "black"),
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
                     strip.text = element_text(size=10, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())


# Generate predictions using ggpredict for each term
pred_prop_exp <- ggpredict(glm_model, terms = "prop_exp")
pred_gast <- ggpredict(glm_model, terms = "gast_density")
pred_urchin <- ggpredict(glm_model, terms = "purple_urchin_densitym2")


# Combine the predictions into one data frame and add a term column for faceting
pred_combined <- rbind(
  data.frame(pred_prop_exp, term = "prop_exp"),
  data.frame(pred_gast, term = "gast_density"),
  data.frame(pred_urchin, term = "purple_urchin_densitym2")
)

# Modify the order of the 'term' factor
pred_combined$term <- factor(pred_combined$term, 
                             levels = c("prop_exp", "gast_density", "purple_urchin_densitym2"))


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
model_terms <- model_terms[model_terms$term %in% c("prop_exp", "gast_density", "purple_urchin_densitym2"), ]

# Format the coefficient and p-value for each term
model_terms$label <- paste0("Coefficient: ", round(model_terms$estimate, 3),
                            "\nP-value: ", ifelse(model_terms$p_value < 0.001, "< 0.001", round(model_terms$p_value, 3)))

# Map the terms to match the custom labels in the plot
model_terms$term <- factor(model_terms$term, 
                           levels = c("prop_exp", "gast_density", "purple_urchin_densitym2"))


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

p

#ggsave(p, filename = file.path(figdir, "Fig1_glm_model.png"), 
 #     width =7, height = 5, units = "in", dpi = 600, bg = "white")


################################################################################
#step3 - plot Figure 2

base_theme2 <-  theme(axis.text=element_text(size=10, color = "black"),
                     axis.title=element_text(size=11,color = "black"),
                     plot.tag=element_text(size=8,color = "black"),
                     plot.title=element_text(size=10,color = "black", face = "bold"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.4, "cm"), 
                     #legend.key = element_rect(fill = "white"), # Set it to transparent
                     legend.spacing.y = unit(0.4, "cm"),  
                     legend.text=element_text(size=10,color = "black"),
                     legend.title=element_text(size=10,color = "black"),
                     #legend.key.height = unit(0.1, "cm"),
                     #legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=10, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())


# Bubble plot with plant density on the x-axis, bubble size scaled to stipe density, and color corresponding to substrate type using Dark2 palette
g <- ggplot(recruit_build1, aes(x = density20m2_macro_plants, y = recruit_density, size = density20m2_macro_stipes, fill = substrate)) +
  geom_jitter(alpha = 0.9, width = 0.3, height = 0.1, color = "black", shape = 21) +  # Add jitter with black outline
  scale_fill_brewer(palette = "Dark2") +  # Use the Dark2 color palette for substrate
  labs(title = "",
       x = "Adult kelp plant density \n(n. plants per 20m²)",
       y = "Kelp recruit density",
       size = "Stipes per plant",
       fill = "Substrate Type") +  # Updated legend title for substrate type
  theme_bw() + base_theme2

# Display the plot
g



ggsave(g, filename = file.path(figdir, "Fig2_bubble_plot.png"), 
      width =6, height = 5, units = "in", dpi = 600, bg = "white")


################################################################################
#step3 - plot Figure 3


# Calculate sample size for each substrate type
sample_size <- recruit_build1 %>% 
  filter(recruit_density > 0) %>%
  group_by(substrate) %>%
  summarise(n = n())

m1 <- ggplot(recruit_build1 %>% filter(recruit_density > 0), aes(x = substrate, y = recruit_density, fill = substrate)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2", guide = "none") +  # Apply Dark2 palette and remove legend
  labs(x = "Substrate Type", y = "Recruit Density", title = "Recruit Density by Substrate Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw() + 
  base_theme2 +
  geom_text(data = sample_size, aes(x = substrate, y = max(recruit_build1$recruit_density) + 1, label = paste0("n=", n)), 
            vjust = -0.5)  # Adjust the position of the sample size labels

m1

# Adjusted plots with common scale
fill_scale <- scale_fill_gradient(
  low = "navyblue", 
  high = "indianred", 
  name = "No. \nquadrats", 
  breaks = scales::pretty_breaks(n = 4),  # Define the number of breaks
  labels = scales::label_number(accuracy = 1)  # Round to nearest integer
)

# First density plot
m2 <- ggplot(recruit_build1 %>% filter(recruit_density > 0), aes(x = relief, y = recruit_density)) +
  geom_hex(bins = 30) +  # Hexagonal binning
  fill_scale +  # Apply the common scale
  labs(title = "", x = "Relief", y = "Recruit Density") +
  theme_bw() + base_theme2

# Second density plot
m3 <- ggplot(recruit_build1 %>% filter(recruit_density > 0), aes(x = risk, y = recruit_density)) +
  geom_hex(bins = 30) +  # Hexagonal binning
  fill_scale +  # Apply the common scale
  labs(title = "", x = "Rugosity", y = "") +
  theme_bw() + base_theme2

# Arrange the plots with a common legend, legend placed at the bottom
m <- ggarrange(m2, m3, common.legend = TRUE, legend = "bottom")

# Optionally adjust the size of the legend elements to prevent smushing
m <- m + theme(legend.key.size = unit(1.5, 'cm'),   # Adjust key size
               legend.text = element_text(size = 10))  # Adjust text size
m

#ggsave(m1, filename = file.path(figdir, "Fig3_boxplot.png"), 
 #    width =6, height = 5, units = "in", dpi = 600, bg = "white")

#ggsave(m, filename = file.path(figdir, "Fig4_hex_plot.png"), 
 #    width =7, height = 4, units = "in", dpi = 600, bg = "white")








