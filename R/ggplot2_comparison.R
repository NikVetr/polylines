
#### ggplot comparison ####
# Load required libraries
library(ggplot2)
library(dplyr)
library(svglite)

# Create a sample dataset

npts <- 100
x <- seq(0, 2 * pi, length.out = npts)
y <- sin(x)
data <- data.frame(
  x = x,
  y = y
)

# Calculate thickness as a function of distance from zero
data <- data %>%
  mutate(thickness = 0.2 + 0.5 * abs(sin(x))^2)

# Define the color with transparency
line_color <- adjustcolor("black", alpha.f = 0.2) 

# Plot the data with variable thickness
p <- ggplot(data, aes(x = x, y = y)) +
  geom_path(aes(linewidth = thickness), lineend = "round", color = line_color) +
  scale_linewidth_continuous(range = c(0.5, 3)) + 
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "blue", color = NA),
    plot.background = element_rect(fill = "blue", color = NA)
  ) +
  labs(title = "Line with Variable Thickness (ggplot2)",
       x = "X-axis",
       y = "Y-axis",
       linewidth = "Thickness")

# Save the plot as an SVG file
ggsave("~/repos/polylines/images/ggplot_example.svg", plot = p,  width = 5, height = 2, device = "svg")
