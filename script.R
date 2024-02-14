# Script to show how to plot network statistics through time across simulations
# of a diversification model that takes into account mutualistic interactions
# between species on a macroevolutinary time scale.

rm(list = ls())

library(tidyverse)
library(patchwork)

theme_set(theme_classic())

set.seed(42)

# For each combination...
data <- expand_grid(

  # of replicate, parameter set and for different statistics...
  replicate = seq(20),
  set = seq(10),
  variable = c("size", "connectance", "modularity", "nestedness")

) %>%
  group_by(variable, set) %>%
  nest() %>%
  ungroup() %>%
  mutate(slope = rnorm(n())) %>%
  unnest(data) %>%
  mutate(value = map(slope, function(slope) {

    # Simulate some dummy trend through time
    tibble(
      time = seq(10),
      value = slope * time
    ) %>%
      mutate(

        # With noise
        value = value + rnorm(n())

      )

  })) %>%
  unnest(value) %>%
  select(-slope)

# Sprinkle some NAs here and there
data$value[sample(nrow(data), 50)] <- NA

# Rearrange the table into something that looks like Yang's data
data <- data %>% pivot_wider(names_from = "variable", values_from = "value")

# Function to plot a statistic through time across sets and replicates
PLOTFUN <- function(data, y, ylab = NULL, color = "black") {

  # Make sure there is a label
  if (is.null(ylab)) ylab <- y

  # Plot
  data %>%
    mutate(set_lab = paste("Set", set)) %>%
    ggplot(aes(x = time, y = get(y), group = replicate)) +
    geom_line(color = color, alpha = 0.5) +
    facet_wrap(. ~ set_lab) +
    xlab("Time") +
    ylab(ylab)

}

# Make a plot for each statistic
p1 <- PLOTFUN(data, "size", "Size", color = "black")
p2 <- PLOTFUN(data, "connectance", "Connectance", color = "darkgreen")
p3 <- PLOTFUN(data, "modularity", "Modularity", color = "darkblue")
p4 <- PLOTFUN(data, "nestedness", "Nestedness", color = "darkred")

# Assemble the plots
P <- wrap_plots(p1, p2, p3, p4, nrow = 2) +
  plot_annotation(tag_level = "A")

# Save
ggsave("example.png", P, width = 10, height = 7, dpi = 300)
