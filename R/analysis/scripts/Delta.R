###########################################################################
#
#  Experiment:  GENUINE
#  Programmer:  QUETTIER THOMAS
#  Date:        28/11/2024
#  Description: Authenticity Emotion Discrimination DELTA
#
#  Update:      28/11/2024
###########################################################################

# Clearing workspace
rm(list=ls()) # Clear the existing workspace to avoid conflicts

# Load dependencies
devtools::load_all() # Load necessary functions and packages

# Data loading
load("data/psychopy_dataset.RData")
sdt_data <- dataset %>%
  GenuineDetection() 

Gain <- sdt_data %>% 
  select(Pt.id, File.emotion, Exp.session, Pt.group, d_prime) %>%
  mutate(d_prime = d_prime + 0.000001) %>%
  spread(Exp.session,d_prime) %>%
  mutate(T0 = (T0 -baseline),
         T20 = (T20 -baseline)) %>%
  select(Pt.id, File.emotion, Pt.group, T0, T20) %>%
  gather(delta, d_prime, 4:5) 


gain_test <- Gain %>%
  group_by(delta, Pt.group) %>%
  summarise(mean = mean(d_prime),
            sd = sd(d_prime),
            t_test = list(t.test(d_prime, mu = 0)),
            .groups = "drop"
  ) %>%
  mutate(
    t_value = map_dbl(t_test, ~ .x$statistic),
    p_value = map_dbl(t_test, ~ .x$p.value),
    conf_low = map_dbl(t_test, ~ .x$conf.int[1]),
    conf_high = map_dbl(t_test, ~ .x$conf.int[2])
  )


plot_data <- Gain %>%
  group_by(Pt.id, delta, Pt.group) %>%
  summarise(mean = mean(d_prime),
            sd = sd(d_prime),
            .groups = "drop") %>%
  mutate(delta = ifelse(delta== "T0", "Δ T0", "Δ T20"))

# Install necessary packages if not installed
if (!require("viridis")) install.packages("viridis")
if (!require("ggsci")) install.packages("ggsci")

# Use the viridis color scale for color-blind-friendly colors
plot_d <- plot_data %>%
  mutate(Pt.group = case_when(Pt.group == "Exp1rpSTS-rIFG" ~ "pSTS-IFG",
                              Pt.group == "Ctrl1rIFG-rpSTS"~ "IFG-pSTS",
                              Pt.group == "Exp2rIFG-rM1"~ "IFG-M1",
                              Pt.group == "Ctrl2rM1-rIFG"~ "M1-IFG")) %>%
  ggplot(aes(x = delta, y = mean, fill = Pt.group, color = Pt.group)) + # Ensure both color and fill are adjusted
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # Add gray line at y = 0
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(position = position_jitter(width = .05), size = 2, alpha = .5) +
  geom_boxplot(width = .3, guides = FALSE, outlier.shape = NA, alpha = 0, notch = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "black") +
  facet_wrap(. ~ Pt.group, ncol = 2) +
  scale_fill_viridis_d(option = "D") + # Viridis palette for fill
  scale_color_manual(values = rep("gray20", length(unique(plot_data$Pt.group)))) + # Dark gray for color
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    panel.grid = element_blank(), # Remove grid lines
    axis.line = element_line(color = "black", size = 0.5), # Add black axis lines
    axis.ticks = element_line(color = "black", size = 0.5), # Add axis ticks
    axis.ticks.length = unit(0.2, "cm") # Set tick length
  ) +
  labs(x = "", y = "d' Δ (T - baseline)")+
  ylim(-1.2, 1.2)  # Set y-axis limits


# Add the significance annotations as before
plot_d <- plot_d +
  ggsignif::geom_signif(
    data = filter(plot_data, Pt.group == "IFG-M1"), # Filter for specific group
    aes(x = delta, y = mean), # Ensure aesthetics align
    map_signif_level = TRUE,
    xmin = 1, xmax = 1, y_position = 1.1, textsize = 4,
    size = 0.4, color = "black", annotations = c("**"), tip_length = 0
  ) +
  ggsignif::geom_signif(
    data = filter(plot_data, Pt.group == "IFG-M1"),
    aes(x = delta, y = mean),
    map_signif_level = TRUE,
    xmin = 2, xmax = 2, y_position = 1.1, textsize = 4,
    size = 0.4, color = "black", annotations = c("**"), tip_length = 0
  ) +
  ggsignif::geom_signif(
    data = filter(plot_data, Pt.group == "pSTS-IFG"),
    aes(x = delta, y = mean),
    map_signif_level = TRUE,
    xmin = 1, xmax = 1, y_position = 1, textsize = 4,
    size = 0.4, color = "black", annotations = c("**"), tip_length = 0
  ) +
  ggsignif::geom_signif(
    data = filter(plot_data, Pt.group == "pSTS-IFG"),
    aes(x = delta, y = mean),
    map_signif_level = TRUE,
    xmin = 2, xmax = 2, y_position = 1, textsize = 4,
    size = 0.4, color = "black", annotations = c("*"), tip_length = 0
  )

# Display the plot
print(plot_d)

print(plot_d)
ggsave("Figure4.png", plot = plot_d, 
       width = 14.41, height = 12.15, units = "cm", 
       dpi = 1200)


