#


#rm(list=ls())

#### MUST RUN Step1_process_recovery_data first! ####

################################################################################
#load packages and set directories
librarian::shelf(tidyverse, here, RColorBrewer)

figdir <- here::here("figures")


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
                                          ifelse(purple_urchin_densitym2 == 0 & purple_urchin_conceiledm2 == 0, NA, 1)))

#create df with rugosity quartiles
#explore rugosity distribution
ggplot(quad_build_combined1, aes(x = risk)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Risk Average", x = "Risk Average", y = "Frequency") +
  theme_minimal() # <------ NICE!!!!

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


#plot purple urchin GI as function of rugosity
p1 <- ggplot(gonad_merge, aes(x = risk_avg, y = purple_GI_avg)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ a * exp(b * x), 
              method.args = list(start = list(a = 50, b = -1)),  
              se = FALSE) +
  labs(title = "", 
       x = "Risk index", 
       y = "Purple sea urchin \ngonad index")+
  theme_bw() + base_theme
p1


################################################################################
#Plot figure 2

#plot proportion exposed as function of rugosity
p2 <- ggplot(quad_build_combined1, aes(x = risk, y = prop_exp)) +
  geom_point() +
  geom_smooth(method = "nls", 
              formula = y ~ L / (1 + exp(-k * (x - x0))), 
              method.args = list(start = list(L = 1, k = 1, x0 = 0)),  
              se = FALSE) +
  labs(title = "", 
       x = "Risk index", 
       y = "Proportion exposed") +
  theme_bw() + 
  base_theme
p2


################################################################################
#Plot figure 3


# Add vertical lines for the first and second quartiles
h1 <- ggplot(quad_build_combined1, aes(x = risk)) +
  geom_histogram(binwidth = 1, fill = "navyblue", color = "black", alpha = 0.7) +
  labs(title = "", x = "Risk index", y = "Frequency") +
  theme_bw() + base_theme +
  geom_vline(aes(xintercept = quartiles[1]), color = "indianred", linetype = "solid", size = 1) +  # 1st quartile
  geom_vline(aes(xintercept = quartiles[2]), color = "indianred", linetype = "solid", size = 1)    # 2nd quartile (median)
h1



# Calculate the median size for each tertile
medians_df <- quad_build_combined1 %>% 
  filter(!is.na(risk_tertiles)) %>% 
  # Rename tertiles to descriptive names
  mutate(risk_tertiles = factor(risk_tertiles, 
                                levels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                                labels = c("Low rugosity", "Medium rugosity", "High rugosity"))) %>%
  group_by(risk_tertiles) %>% 
  summarise(median_size = median(size_purple_mean_size_cm, na.rm = TRUE))


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
  labs(title = "", 
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

# Display the plot
h2


# Create the combined boxplot and density plot faceted by rugosity levels
h3 <- ggplot(quad_build_combined1 %>% 
               filter(!is.na(risk_tertiles)) %>% 
               # Rename tertiles to descriptive names
               mutate(risk_tertiles = factor(risk_tertiles, 
                                             levels = c("1st Tertile", "2nd Tertile", "3rd Tertile"), 
                                             labels = c("Low rugosity", "Medium rugosity", "High rugosity"))),
             aes(x = prop_exp, fill = risk_tertiles)) +  # Map fill to risk_tertiles
  # Add horizontal boxplot
  geom_boxplot(aes(group = risk_tertiles), width = 0.15, position = position_nudge(y = 1.9), color = "black", alpha = 0.6) +
  # Add density plot
  geom_density(alpha = 0.7) +  # Use fill based on risk_tertiles
  # Add vertical line for the median per facet
  geom_vline(data = medians_df, aes(xintercept = median_size), color = "black", linetype = "solid", size = 1) +
  # Labels
  labs(title = "", 
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

# Display the plot
h3

h <- ggpubr::ggarrange(h2, h3)
h



