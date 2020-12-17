library(tidyverse)
library(scales)
theme_set(theme_classic())

scientific <- function(x){
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}

simulations_unnest <- read_rds("C://Users/Laboratorio DID/Desktop/simulations_unnest.rds")
predictions <- read_rds("C://Users/Laboratorio DID/Desktop/predictions.rds")

monte_carlo <- predictions %>%
  unnest(predictions) %>% 
  group_by(nsim, id) %>% 
  summarise(period_pred = sum(.pred), .groups = "drop") %>%
  group_by(nsim) %>% 
  summarise(mean_pred = mean(period_pred), .groups = "drop") %>% 
  mutate(overall_mean = mean(mean_pred),
         stand_error = sd(mean_pred),
         li = overall_mean - 1.96 * stand_error,
         ls = overall_mean + 1.96 * stand_error,
         inside_limits = ifelse(mean_pred > li & mean_pred < ls,
                                "inside", "outside"))
# Monte Carlo convergence plot -------------------------------------------------
plot_montecarlo_convergence <- monte_carlo %>% 
  mutate(across(mean_pred:ls, function(x) x / 1e6)) %>% 
  ggplot(aes(x = nsim, y = mean_pred)) +
  geom_ribbon(aes(ymin = li, ymax = ls, 
                  x = seq(from = 0, to = 10500, 
                          length.out = nrow(monte_carlo))),
              fill = "gray") +
  geom_line() +
  geom_point(aes(shape = inside_limits, color = inside_limits), 
             size = 2, stroke = 2) +
  geom_hline(aes(yintercept = overall_mean),
             linetype = 2, color = "firebrick") +
  scale_shape_manual(values = c(16, 4),
                     name = NULL,
                     labels = c("Dentro del intervalo del 95%",
                                "Fuera del intervalo del 95%")) +
  scale_color_manual(values = c("black", "firebrick")) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::number_format(),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 10000)) +
  guides(shape = guide_legend(override.aes = list(color = c("black", "firebrick"))),
         color = FALSE) +
  labs(y = "Predicción media (millones)",
       x = "Cantidad de simulaciones") +
  theme(legend.position = c(0.7, 0.25),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.background = element_rect(fill = "transparent"))

# Monte Carlo density plot -----------------------------------------------------
dens <- density(monte_carlo$mean_pred)
x_sep <- monte_carlo %>% distinct(li, ls)
density_frame <- tibble(x = dens$x, y = dens$y)
plot_montecarlo_density <- density_frame %>% 
  ggplot(aes(x = x / 1e6, y = y)) +
  geom_ribbon(aes(ymin = 0, ymax = y, 
                  fill = x > x_sep$li & x < x_sep$ls)) +  
  geom_line(size = 1) +
  geom_vline(xintercept = x_sep$li / 1e6, color = "white", size = 1) +
  geom_vline(xintercept = x_sep$ls / 1e6, color = "white", size = 1) +
  scale_fill_manual(values = c("firebrick", "gray"),
                    name = NULL,
                    labels = c("Fuera del intervalo", "Dentro del intervalo")) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(label = scientific, expand = c(3e-2, 0)) +
  labs(x = "Predicción media (millones)",
       y = "Densidad") +
  theme(legend.position = c(0.25, 0.6),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.key = element_rect(color = "black"),
        legend.background = element_rect(fill = "transparent"))

# Monte Carlo standard error plot ---------------------------------------------
monte_carlo_sd <- predictions %>% 
  unnest(predictions) %>% 
  group_by(nsim, id) %>% 
  summarise(mean_pred = mean(.pred), .groups = "drop_last") %>% 
  summarise(stand_error = sd(mean_pred), .groups = "drop") %>% 
  mutate(mean_diff = 1.96 * stand_error / sqrt(nsim)) %>% 
  bind_cols(monte_carlo[,"mean_pred"]) %>% 
  mutate(mean_diff = 100 * mean_diff,
         mean_diff_rate = mean_diff / mean_pred) 

plot_montecarlo_stderr <- monte_carlo_sd %>% 
  ggplot(aes(x = nsim, y = mean_diff_rate)) +
  geom_line(size = 1) +
  geom_segment(x = 1400, xend = 1400, y = 0, yend = 0.01, 
               linetype = "dashed", color = "firebrick") +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "firebrick") +
  labs(y = "Error estándar", x = "Cantidad de simulaciones") +
  scale_y_continuous(labels = scales::number_format()) +
  scale_x_continuous(labels = scales::number_format(),
                     breaks = c(1400, seq(0, 10000, 2500))) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14))

# Combine plots
library(patchwork)
plot_montecarlo_combined <- plot_montecarlo_convergence / 
  (plot_montecarlo_density + plot_montecarlo_stderr) +
  plot_annotation(title = "Análisis de convergencia de la simulación Monte Carlo",
                  tag_levels = "A",
                  theme = theme(plot.title = element_text(size = 20, face = "bold",
                                                          margin = margin(0, 0, 15, 0)))) &
  theme(plot.tag = element_text(size = 18, face = "bold"))

ggsave(plot = plot_montecarlo_combined, 
       filename = "C://Users/Laboratorio DID/Desktop/plot_montecarlo_combined.png",
       type = "cairo", dpi = 500,
       height = 12, width = 13)
