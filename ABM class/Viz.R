library(tidyverse)
library(janitor)
library(here)
setwd("~/Documents/Ryan/ABM/HIV model")


data <- here("HIV experiment-table2.csv") %>%
  read_csv(skip = 6) %>%
  clean_names() %>%
  arrange(run_number)

data$average_coupling_tendency <- factor(data$average_coupling_tendency)
data$average_condom_use <- factor(data$average_condom_use)

str(data)

figure <- data %>%
  ggplot(aes(x = step)) +
  geom_line(aes(group = run_number, y = count_turtles_with_infected_true), color = "#fc9272", linewidth = 0.3, alpha = 1) +
  geom_line(aes(group = run_number, y = count_turtles_with_infected_false), color = "#a1d99b", linewidth = 0.3, alpha = 1) +
  geom_line(aes(group = run_number, y = count_turtles_with_known_true), color = "#9ecae1", linewidth = 0.3, alpha = 1) +
  stat_summary(aes(y = count_turtles_with_infected_false, group = 1, colour = "HIV-"), fun = "mean", , geom = "line", group = 1, linewidth = 2) +
  stat_summary(aes(y = count_turtles_with_infected_true, group = 1, colour = "HIV+"), fun = "mean", geom = "line", group = 1, linewidth = 2) +
  stat_summary(aes(y = count_turtles_with_known_true, group = 1, colour = "HIV?"), fun = "mean", geom = "line", group = 1, linewidth = 2) +
  scale_color_manual(name = "Group", values = c("HIV+" = "#de2d26", "HIV-" = "#31a354", "HIV?" = "#3182bd")) +
  labs (color = "Group", x = "Step", y = "Count of the turtles") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.position = "bottom") +
  facet_grid(average_condom_use~average_coupling_tendency) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Coupling tendency", breaks = NULL, labels = NULL)) +
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Condom use", breaks = NULL, labels = NULL))

  
figure
ggsave(plot = figure, "Viz", device = "png", dpi = 600, height=12, width=15)



