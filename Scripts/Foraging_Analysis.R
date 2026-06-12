
#Foraging Analysis

# This script processes foraging count data of Veromessor andrei from Sedwick
# UC Reserve collected before treatment, 30 seconds post-treatment, and 5 minutes
# post-treatment for incoming and outgoing ants. Counts are converted from 
# wide to long format, and both difference (post - pre) and 
# proportional difference [(post - pre) / pre] are calculated for each trial. 
# Bayesian mixed-effects models are then fit separately for incoming and
# outgoing foraging at 30 seconds and 5 minutes to evaluate effects of
# Population and Treatment, with Colony included as a random intercept.
# Final proportional differences are visualized using boxplots.

#load packages
library(tidyverse)
library(lmerTest)
library(performance)
library(see)
library(ggh4x)
library(cowplot)
library(brms)
library(glmmTMB)

###Step 1: Read in RDS and format data

#Read RDS with foraging data
foraging_original <- readRDS("Data/foraging_data.RDS") 

#Add 1 to pre-counts of 0 to prevent dividing by 0
foraging_original <- foraging_original %>%
  mutate(
    F.in.pre = if_else(F.in.pre ==0, F.in.pre + 1, F.in.pre),
    F.out.pre = if_else(F.out.pre ==0, F.out.pre + 1, F.out.pre))

#Change csv from wide to long format
foraging_long <- pivot_longer( 
  data = foraging_original,
  cols = c("F.in.pre", "F.out.pre", "F.in.30sec", "F.out.30sec", "F.in.5min", "F.out.5min"), 
  names_to = "Foraging", 
  values_to = "Count") 

#Mutate dataset so every variable has column to be called upon (inout and timeperiod added)
foraging_long <- foraging_long %>%
  mutate(inout = str_extract(Foraging, pattern = "in|out"), #col for count is for incoming or outgoing ants
         timeperiod = str_remove(Foraging, pattern = "F\\.in\\.|F\\.out\\.")) #col for what timeperiod count was collected at

#Create columns for 'in' and 'out', so each count has a single row per timeframe
inout_wider <- foraging_long %>%
  select(-Foraging) %>% #deleting foraging column as info is in different column
  pivot_wider(names_from = "inout", values_from = "Count") %>%
  mutate(across(c("out", "in"), as.numeric))

###Step 2: Calculate Difference and Proportion Differences

#Adding columns for incoming and outgoing foraging difference at 30 second and 5 min [post - pre]. 
#Adding columns for relative incoming and outgoing foraging differences at 30 second and 5 min [post - pre/pre]
inout_wider <- inout_wider %>% 
  group_by(Name, Treatment) %>%
  mutate(
    pre_in = `in`[match("pre", timeperiod)],
    difference_in = `in` - pre_in, #subtracts pre_in from each other in value seperately
    pre_out = out[match("pre", timeperiod)],
    difference_out = out - pre_out, #subtracts pre_out from each other in value seperately
    relative_diff_in = difference_in / (pre_in), 
    relative_diff_out = difference_out / (pre_out)) %>%
  ungroup() %>%
  select(-pre_in,-pre_out)

#Pre-count is no longer needed, removing to focus on relative differences
foraging_with_differences <- inout_wider[inout_wider$timeperiod!='pre',] 

#Create data frames to analyze 30sec and 5 min separately
foraging_differences_5min <- foraging_with_differences %>%
  filter(timeperiod != "30sec" )

foraging_differences_30sec <- foraging_with_differences %>%
  filter(timeperiod != "5min" )

###Step 3: Create models, Summarize findings, and check models

##proportion difference of incoming ants after 30 seconds
model_30in <- brm(relative_diff_in ~ Population + Treatment + (1|Colony), data=foraging_differences_30sec)
summary(model_30in)
check_model(model_30in)

# % of variation explained by the random effect:
m_c_r_2_model_30in = r2_nakagawa(model_30in) 
100*(m_c_r_2_model_30in$R2_conditional - m_c_r_2_model_30in$R2_marginal) 

##relative difference of incoming ants after 5 minutes 
model_5in <- brm(relative_diff_in ~ Population + Treatment + (1|Colony), data=foraging_differences_5min)
summary(model_5in)
check_model(model_5in)

m_c_r_2_model_5in = r2_nakagawa(model_5in)
100*(m_c_r_2_model_5in$R2_conditional - m_c_r_2_model_5in$R2_marginal) 

##relative difference of outgoing ants after 30 seconds
model_30out <- brm(relative_diff_out ~ Population + Treatment + (1|Colony), data=foraging_differences_30sec)
summary(model_30out)
check_model(model_30out)

m_c_r_2_model_30out = r2_nakagawa(model_30out)
100*(m_c_r_2_model_30out$R2_conditional - m_c_r_2_model_30out$R2_marginal) 

##relative difference of outgoing ants after 5 minutes
model_5out <- brm(relative_diff_out ~ Population + Treatment + (1|Colony), data=foraging_differences_5min)
summary(model_5out)
check_model(model_5out)

m_c_r_2_model_5out = r2_nakagawa(model_5out)
100*(m_c_r_2_model_5out$R2_conditional - m_c_r_2_model_5out$R2_marginal) # very low! ( 1.734459%) 

### Step 4: Plot Results
##change differences into long form so can be called upon in facet
foraging_with_differences <- foraging_with_differences %>%
  pivot_longer(c(relative_diff_in, relative_diff_out), names_to = "rel_metric", values_to = "rel_diff")

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

#inbound relative differences separated by 30sec and 5min

panel_labs <- data.frame( #creating df for ABCD labels
  Population = c("B",  "B",   "UB", "UB"),
  timeperiod = c("30sec","5min","30sec","5min"),
  lab        = c("A","C","B","D"))

p_in <- foraging_with_differences %>%
  filter(rel_metric == "relative_diff_in") %>%
  ggplot(aes(
    x = Treatment, y = rel_diff,
    fill = interaction(Population, Treatment, sep=":"))) +
  geom_boxplot(fatten = 1,outlier.size = 1, outlier.shape=1) +
  geom_text(
    data = panel_labs,
    aes(x = -Inf, y = Inf, label = lab),   
    inherit.aes = FALSE,
    hjust = -0.2, vjust = 1.2,
    family = "Helvetica",
    size = 6) +
  facet_grid2(Population ~ timeperiod, scales = "free_y", axes = "all",
              labeller = labeller(
                Population = c(B = "Burned", UB = "Unburned"),
                timeperiod = c(`30sec` = "0.5 minutes", `5min` = "5 minutes"))) +
  scale_x_discrete(labels = c(C = "Control", S = "Smoke")) +
  labs(x = "Treatment", y = "Returning Foraging Proportion Difference\n[(Post − Pre) / Pre]") +
  scale_fill_manual(
    name = "Population:Treatment",
    values = c("UB:C"="#a6cee3","UB:S"="#1f78b4","B:C"="#fdae6b","B:S"="#e6550d"),
    labels = c("UB:C"="Unburned: Control","UB:S"="Unburned: Smoke",
               "B:C"="Burned: Control","B:S"="Burned: Smoke"))  +
  theme_mine()   +
  theme(legend.position = "none",plot.margin = margin(t = 15, r = 25, b = 10, l = 10)) 

#Adding titles and printing plot
ggdraw(p_in) +
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

#Outbound relative differences seperated by 30sec and 5min
p_out <- foraging_with_differences %>%
  filter(rel_metric == "relative_diff_out") %>%
  ggplot(aes(
    x = Treatment, y = rel_diff,
    fill = interaction(Population, Treatment, sep=":"))) +
  geom_boxplot(fatten = 1, outlier.size = 1, outlier.shape = 1) +
  geom_text(
    data = panel_labs,
    aes(x = -Inf, y = Inf, label = lab),   
    inherit.aes = FALSE,
    hjust = -0.2, vjust = 1.2,
    family = "Helvetica",
    size = 6) +
  geom_text(  # Asterisks above each panel
    data = panel_labs,
    aes(x = 1.5, y = Inf, label = "*"),
    inherit.aes = FALSE,
    vjust = 1.2,
    family = "Helvetica",
    size = 9
  ) +
  facet_grid2(Population ~ timeperiod, scales = "free_y", axes = "all",
              labeller = labeller(
                Population = c(B = "Burned", UB = "Unburned"),
                timeperiod = c(`30sec` = "0.5 minutes", `5min` = "5 minutes"))) +
  scale_x_discrete(labels = c(C = "Control", S = "Smoke")) +
  labs(x = "Treatment", y = "Outgoing Foraging Proportion Difference\n[(Post − Pre) / Pre]") +
  scale_fill_manual(
    name = "Population:Treatment",
    values = c("UB:C"="#a6cee3","UB:S"="#1f78b4","B:C"="#fdae6b","B:S"="#e6550d"),
    labels = c("UB:C"="Unburned: Control","UB:S"="Unburned: Smoke",
               "B:C"="Burned: Control","B:S"="Burned: Smoke"))  +
  theme_mine()   +
  theme(legend.position = "none",plot.margin = margin(t = 15, r = 25, b = 10, l = 10)) 

#Adding titles and printing plot
ggdraw(p_out) +
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