
#Mound Analysis

# This script processes mound activity data collected before treatment and at
# 30 seconds and 5 minutes post-treatment for control and smoke trials.
# For each trial, it calculates both the difference (post - pre) and the
# proportion difference [(post - pre) / pre] relative to the pre-treatment count.
# Bayesian mixed-effects models are then fit separately for the 30-second and
# 5-minute responses to evaluate effects of Population and Treatment, with Colony
# included as a random effect. Final proportion differences are visualized using
# faceted boxplots.

#Load packages
library(tidyverse)   
library(lmerTest)  
library(performance)
library(see)
library(ggh4x)
library(cowplot)
library(brms)
library(glmmTMB)

###Step 1: Read in RDS and calculate proportion differences

#Read in Mound Data 
mound_original <- readRDS("Data/mound_data.RDS") 

mound_differences <- mound_original %>%
  group_by(Name, Treatment) %>%  #allows us to subtract pre count from each other time period
  mutate(
    pre_count  = Count[match("pre", Window)], # creates column to subtract from in following windows of that group (trial). 
    difference = Count - pre_count, #creates the difference column
    rel_diff = difference / pre_count)  %>%
  ungroup() %>% 
  select(-pre_count) 

#Remove pre-counts for the model and plot the proportion differences 
mound_differences <- mound_differences[mound_differences$Window!='pre',] 

#Create df with only 30sec or 5min timeframes to model separately
difference_30only <- mound_differences[mound_differences$Window=="30sec",]

difference_5only <- mound_differences[mound_differences$Window=="5min",]

### STEP 2: Create models, summarize findings, and check models

##relative difference of ants on mound after 30 seconds
model_30 <- brm(rel_diff ~ Population + Treatment + (1|Colony), data=difference_30only)
summary(model_30)
check_model(model_30)

# % of variation explained by the random effect:
m_c_r_2_model_30 = r2_nakagawa(model_30) 
100*(m_c_r_2_model_30$R2_conditional - m_c_r_2_model_30$R2_marginal) 

##relative difference of ants on mound after 5 minutes
model_5 <- brm(rel_diff ~ Population + Treatment + (1|Colony), data=difference_5only)
summary(model_5)
check_model(model_5)

m_c_r_2_model_5 = r2_nakagawa(model_5) 
100*(m_c_r_2_model_5$R2_conditional - m_c_r_2_model_5$R2_marginal) 

#STEP 3: Plot results

#Plotting theme
theme_mine <- function(base_size = 18, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      strip.background = element_blank(),
      strip.text.x = element_text(size = 18),
      strip.text.y = element_text(size = 18, angle = -90),
      axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=14,hjust=1),
      axis.ticks = element_line(colour = "black"),
      axis.title.x= element_text(size=16),
      axis.title.y= element_text(size=16,angle=90),
      panel.background = element_blank(),
      panel.border =element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      axis.line.x = element_line(color="black", size = 1),
      axis.line.y = element_line(color="black", size = 1)
    )
}

#BoxPlot for proportion difference

panel_labs <- data.frame( #creating df for ABCD labels
  Population = c("B",  "B",   "UB", "UB"),
  Window     = c("30sec","5min","30sec","5min"),
  lab        = c("A","C","B","D"))

p_mound <- ggplot(mound_differences,
                  aes(x = Treatment, y = rel_diff,
                      fill = interaction(Population, Treatment, sep = ":"))) +
  geom_boxplot(fatten = 1, outlier.size=1, outlier.shape=1) +
  geom_text(
    data = panel_labs,
    aes(x = -Inf, y = Inf, label = lab),   
    inherit.aes = FALSE,
    hjust = -0.2, vjust = 1.2,
    family = "Helvetica",
    size = 6) +
  facet_grid2(Population ~ Window, scales = "free_y", axes = "all",
              labeller = labeller(
                Population = c(B = "Burned", UB = "Unburned"),
                Window = c(`30sec` = "0.5 minutes", `5min` = "5 minutes"))) +
  labs(x = "Treatment",
       y = "Mound Activity Proportion Difference\n[(Post − Pre) / Pre]") +
  scale_x_discrete(labels = c(C = "Control", S = "Smoke")) +
  scale_fill_manual(
    name = "Population:Treatment",
    values = c("UB:C" = "#a6cee3","UB:S" = "#1f78b4","B:C" = "#fdae6b","B:S" = "#e6550d"),
    labels = c("Burned: Control", "Unburned: Control", "Burned: Smoke", "Unburned: Smoke")) +
  theme_mine() +
  theme(legend.position = "none",
        plot.margin = margin(t = 15, r = 25, b = 10, l = 10)) 

#add more titles and creating plot
ggdraw(p_mound) +
  draw_label("Location",
             x = 0.98, y = 0.5,     
             angle = -90,
             hjust = 0.5, vjust = 0.5,
             fontfamily= "Helvetica",
             size = 18) +
  draw_label("Sampling Time",
             x = 0.5, y = 0.995,
             hjust = 0.5, vjust = 1,
             fontfamily= "Helvetica",
             size = 18)






