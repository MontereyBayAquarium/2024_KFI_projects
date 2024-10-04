#


#rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here, ggeffects, RColorBrewer, ggpubr, ggridges)

figdir <- here::here("figures","CQ_figures")


################################################################################
#step1 - process data

#calcualte average rugosity for each site

risk_avg <- quad_build_combined %>%
            group_by(site, site_type, zone, survey_date) %>%
            summarise(risk_avg = mean(risk, na.rm=TRUE)) %>%
            mutate(zone = factor(trimws(toupper(zone))),
                   site = factor(trimws(site)),
                   site_type = factor(site_type))

gonad_dat2 <- gonad_dat %>% filter(!(is.na(transect))) %>%
                ungroup()%>%
                select(-transect, -site) %>%
                rename(purple_GI_avg = purple_urchin,
                       red_GI_avg = red_urchin)

# Perform the join on multiple columns, including date and site if relevant
gonad_merge <- left_join(risk_avg, gonad_dat2, 
                         by = c("survey_date" = "date_collected", "site_type", "zone"))


quad_build_combined1 <- quad_build_combined %>%
  mutate(prop_exp = ifelse(purple_urchin_densitym2 > 0, 
                                          (purple_urchin_densitym2 - purple_urchin_conceiledm2) / purple_urchin_densitym2,
                                          ifelse(purple_urchin_densitym2 == 0 & purple_urchin_conceiledm2 == 0, NA, 1)),
         zone = toupper(zone)) %>%
  left_join(gonad_merge, by =c("survey_date","site","site_type","zone"))


# Calculate the tertiles (33rd and 66th percentiles)
tertiles <- quantile(quad_build_combined1$risk, probs = c(0.33, 0.66), na.rm = TRUE)

# Add a new column to indicate the tertile
quad_build_combined1$risk_tertiles <- cut(quad_build_combined1$risk,
                                          breaks = c(-Inf, tertiles[1], tertiles[2], Inf),
                                          labels = c("1st Tertile", "2nd Tertile", "3rd Tertile"),
                                          include.lowest = TRUE)

# Check the first few rows to verify the new column
head(quad_build_combined1)


################################################################################
#Plot figure 1

base_theme <-  theme(axis.text=element_text(size=11, color = "black"),
                     axis.title=element_text(size=12,color = "black"),
                     plot.tag=element_text(size=10,color = "black"),
                     plot.title=element_text(size=11,color = "black", face = "bold"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.4, "cm"), 
                     #legend.key = element_rect(fill = "white"), # Set it to transparent
                     legend.spacing.y = unit(0.4, "cm"),  
                     legend.text=element_text(size=11,color = "black"),
                     legend.title=element_text(size=12,color = "black"),
                     #legend.key.height = unit(0.1, "cm"),
                     #legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=10, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())



p1 <- ggplot(quad_build_combined1, aes(x = risk, y = purple_GI_avg)) +
  geom_point(aes(color = prop_exp), size = 4, alpha = 0.9, position = position_jitter(width = 0, height = 0.2)) +  # Jittered points with color
  scale_color_gradient(low = "darkblue", high = "indianred", name = "Proportion Exposed") +  # Color gradient based on prop_exp
  labs(title = "",  # Plot title
       x = "Rugosity (Risk index)", 
       y = "Purple Sea Urchin \nGonad Index") +
  theme_minimal(base_size = 15) +  # Clean theme with larger font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centered and bold title
    legend.position = "right",  # Legend on the right
    panel.grid.major = element_line(color = "gray80"),  # Style major grid lines
    panel.grid.minor = element_blank()
  ) + 
  theme_bw() + base_theme  

p1

ggsave(p1, filename = file.path(figdir, "Fig1.png"), 
     width =7, height = 5, units = "in", dpi = 600, bg = "white")




################################################################################
#Plot figure 2
#plot proportion exposed as function of rugosity

# Bin risk into three categories with custom labels
quad_build_combined1$risk_category <- cut(quad_build_combined1$risk, breaks = 3, labels = c("Low", "Moderate", "High"))

base_theme2 <-  theme(axis.text=element_text(size=9, color = "black"),
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
                     legend.text=element_text(size=9,color = "black"),
                     axis.text.y = element_blank(),  
                     legend.title = element_text(size = 10, color = "black"),
                     #legend.key.height = unit(0.1, "cm"),
                     #legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=8, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())

g1 <- ggplot(quad_build_combined1, aes(x = prop_exp, y = risk_category, fill = risk_category)) +
  geom_density_ridges() +
  scale_x_continuous(limits = c(0, 1)) + 
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.5))) +
  scale_fill_brewer(palette = "Dark2", name = "Risk index") +  
  labs(title = "", x = "Proportion Exposed", y = "Relative Frequency") +
  theme_bw() + 
  base_theme2 

g1


################################################################################
#Plot figure 3


# Add vertical lines for the first and second quartiles
h1 <- ggplot(quad_build_combined1, aes(x = risk)) +
  geom_histogram(binwidth = 1, fill = "navyblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Risk Index", y = "Frequency") +
  geom_vline(aes(xintercept = tertiles[1]), color = "indianred", linetype = "solid", size = 1) +  # 1st tertile
  geom_vline(aes(xintercept = tertiles[2]), color = "indianred", linetype = "solid", size = 1) +  # 2nd tertile (median)
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+  # Ensure y-axis starts at 0 and avoid extra space at the bottom
  theme_bw() + 
  base_theme2
  
h1

#ggsave(h1, filename = file.path(figdir, "Fig2.png"), 
 #      width =7, height = 5, units = "in", dpi = 600, bg = "white")



# Ensure tertiles are created for the 'risk' column
quad_build_combined1 <- quad_build_combined1 %>%
  mutate(risk_tertiles = cut(risk, 
                             breaks = c(-Inf, tertiles[1], tertiles[2], Inf), 
                             labels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                             include.lowest = TRUE))

# Now calculate the median size for each tertile
medians_df <- quad_build_combined1 %>% 
  filter(!is.na(risk_tertiles)) %>% 
  # Rename tertiles to descriptive names
  mutate(risk_tertiles = factor(risk_tertiles, 
                                levels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                                labels = c("Low rugosity", "Medium rugosity", "High rugosity"))) %>%
  group_by(risk_tertiles) %>% 
  summarise(median_size = median(size_purple_mean_size_cm, na.rm = TRUE))

medians_df



# Create the combined boxplot and density plot faceted by rugosity levels
h2 <- ggplot(quad_build_combined1 %>% 
               filter(!is.na(risk_tertiles)) %>% 
               # Rename tertiles to descriptive names
               mutate(risk_tertiles = factor(risk_tertiles, 
                                             levels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                                             labels = c("Low rugosity", "Medium rugosity", "High rugosity"))),
             aes(x = size_purple_mean_size_cm, fill = risk_tertiles)) +  # Map fill to risk_tertiles
  # Add horizontal boxplot
  geom_boxplot(aes(group = risk_tertiles), width = 0.15, position = position_nudge(y = 0.4), color = "black", alpha = 0.6) +
  # Add density plot
  geom_density(alpha = 0.7) +  # Use fill based on risk_tertiles
  # Add vertical line for the median per facet
  geom_vline(data = medians_df, aes(xintercept = median_size), color = "black", linetype = "solid", size = 1) +
  # Labels
  labs(title = "Size distribution", 
       x = "Purple sea urchin size (cm)", 
       y = "Frequency") +
  # Facet by renamed tertiles
  facet_wrap(~ risk_tertiles, ncol = 1) +
  # Adjust the x-axis limits
  scale_x_continuous(limits = c(0, 7)) +
  # Use Dark2 palette from RColorBrewer
  scale_fill_brewer(palette = "Dark2") +
  # Themes
  theme_bw() + base_theme + theme(legend.position = "none")

h2

#ggsave(h2, filename = file.path(figdir, "Fig3.png"), 
 #      width = 5.5, height = 5, units = "in", dpi = 600, bg = "white")


# Create the combined boxplot and density plot faceted by rugosity levels
h3 <- ggplot(quad_build_combined1 %>% 
               filter(!is.na(risk_tertiles)) %>% 
               # Rename tertiles to descriptive names
               mutate(risk_tertiles = factor(risk_tertiles, 
                                             levels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                                             labels = c("Low rugosity", "Medium rugosity", "High rugosity"))),
             aes(x = prop_exp, fill = risk_tertiles)) +  # Map fill to risk_tertiles
  # Add horizontal boxplot
  #geom_boxplot(aes(group = risk_tertiles), width = 0.15, position = position_nudge(y = 1.9), color = "black", alpha = 0.6) +
  # Add density plot
  geom_density(alpha = 0.7) +  # Use fill based on risk_tertiles
  # Add vertical line for the median per facet
  geom_vline(data = medians_df, aes(xintercept = median_size), color = "black", linetype = "solid", size = 1) +
  # Labels
  labs(title = "Behavior", 
       x = "Proportion exposed", 
       y = "Frequency") +
  # Facet by renamed tertiles
  facet_wrap(~ risk_tertiles, ncol = 1) +
  # Adjust the x-axis limits
  scale_x_continuous(limits = c(0, 1)) +
  # Use Dark2 palette from RColorBrewer
  scale_fill_brewer(palette = "Dark2") +
  # Themes
  theme_bw() + base_theme + theme(legend.position = "none")

h3

#ggsave(h3, filename = file.path(figdir, "Fig4.png"), 
 #      width =4, height = 5, units = "in", dpi = 600, bg = "white")



#comined
h <- ggpubr::ggarrange(h2, h3)
h

#ggsave(h, filename = file.path(figdir, "Figs3_and_4.png"), 
 #      width =7, height = 5, units = "in", dpi = 600, bg = "white")
















