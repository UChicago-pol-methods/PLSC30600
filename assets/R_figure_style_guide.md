# R Figure Generation Style Guide

## Core Principles
- **Accessibility First**: Use colorblind-friendly palettes and high contrast themes
- **Publication Ready**: Generate figures at 300 DPI with consistent sizing
- **Clean Design**: Minimize chart junk, use professional typography
- **Reproducibility**: Set global themes and define palettes upfront

## Required Packages

```r
# Core visualization
library(tidyverse)  # includes ggplot2
library(ggplot2)

# Color palettes and themes
library(viridis)      # Colorblind-friendly continuous palettes
library(hrbrthemes)   # Professional typography
library(ggthemes)     # Additional themes
library(RColorBrewer) # Categorical palettes

# Advanced visualization
library(ggdist)       # Gradient intervals for uncertainty
library(corrplot)     # Correlation matrices
```

## Color Palettes

### Primary Colorblind-Friendly Palette
Use this for categorical data (up to 8 categories):

```r
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
```

### Extended Colorblind Palette
For gradient or more categories:

```r
cbPalette_extended <- c('#125A56', '#00767B', '#238F9D', '#42A7C6', 
                       '#60BCE9', '#9DCCEF', '#C6DBED', '#DEE6E7', 
                       '#F0E6B2', '#F9D576', '#FFB954', '#FD9A44', 
                       '#F57634', '#E94C1F', '#D11807', '#A01813')
```

### Continuous Data
Use viridis palettes with adjusted ranges:

```r
scale_fill_viridis(option = "magma", begin = 0.4, end = 0.9)
scale_color_viridis(option = "viridis", begin = 0.2, end = 0.8)
```

### Helper Function
Generate consistent colors across plots:

```r
gg_color_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
```

## Theme Settings

### Global Theme Setup
Set at the beginning of each script:

```r
# Option 1: Clean minimal
theme_set(theme_minimal())
theme_update(
  plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
  text = element_text(size = 12),
  panel.background = element_rect(fill = 'white'),
  plot.background = element_rect(fill = 'white', color = 'white'),
  panel.grid.minor = element_blank()
)

# Option 2: Publication-ready
theme_set(theme_bw())

# Option 3: Professional typography
theme_set(theme_ipsum_rc())
```

### Custom Theme Template
For consistent styling across projects:

```r
custom_theme <- theme(
  # Text elements
  plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 10),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 11, face = "bold"),
  
  # Grid and background
  panel.grid.major = element_line(color = "grey90", size = 0.25),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  
  # Spacing
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
  
  # Axes
  axis.ticks.length = unit(5, "pt"),
  axis.ticks = element_line(size = 0.5),
  
  # Legend
  legend.position = "bottom",
  legend.background = element_rect(fill = "white", color = NA)
)
```

## Standard Plot Types

### Bar/Column Charts
```r
ggplot(data, aes(x = category, y = value, fill = group)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(values = cbPalette) +
  labs(title = "Clear, Descriptive Title",
       x = "Category Label",
       y = "Value Label",
       fill = "Group") +
  custom_theme
```

### Scatter Plots with Confidence Intervals
```r
ggplot(data, aes(x = x_var, y = y_var, color = group)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = cbPalette[c(2,3)]) +
  custom_theme
```

### Uncertainty Visualization
```r
ggplot(data, aes(x = treatment, y = estimate)) +
  stat_gradientinterval(
    aes(ydist = distributional::dist_normal(estimate, std_error)),
    width = 0.6,
    point_size = 2,
    interval_alpha = 0.8
  ) +
  scale_fill_viridis(option = "viridis", begin = 0.3, end = 0.7) +
  custom_theme
```

### Heatmaps
```r
ggplot(data, aes(x = var1, y = var2, fill = correlation)) + 
  geom_tile() + 
  scale_fill_viridis(option = "magma", begin = 0.4, end = 0.9) + 
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  labs(title = "Correlation Matrix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Annotation Best Practices

### Adding Reference Lines
```r
geom_vline(xintercept = mean_value, 
           linetype = "dashed", 
           color = "grey40", 
           alpha = 0.7)
```

### Text Annotations
```r
annotate("text", 
         x = position_x, 
         y = position_y,
         label = "Annotation Text",
         size = 3.5,
         color = "grey30")
```

## Export Settings

### Standard Export
```r
ggsave("figures/figure_name.png", 
       plot = p,
       width = 8, 
       height = 6, 
       units = "in", 
       dpi = 300)
```

### Publication Formats
```r
# High-quality PNG
ggsave("figure.png", width = 10, height = 6, dpi = 300)

# Vector format for publications
ggsave("figure.pdf", width = 10, height = 6)

# For presentations
ggsave("figure_presentation.png", width = 12, height = 7, dpi = 150)
```

## Accessibility Checklist

- [ ] Use colorblind-friendly palette (cbPalette or viridis)
- [ ] Ensure sufficient contrast (test with white/black backgrounds)
- [ ] Include clear, descriptive titles and labels
- [ ] Use shapes/patterns in addition to colors when possible
- [ ] Set appropriate text size (minimum 10pt for axes, 12pt for titles)
- [ ] Add alt-text descriptions in R Markdown: `fig.alt = "Description"`
- [ ] Consider using `ggpattern` for pattern fills when color alone isn't sufficient

## R Markdown Figure Options

```r
{r fig-name, fig.width=8, fig.height=6, fig.cap="Caption here", fig.alt="Alt text here", dpi=300}
# Your plotting code
```

## Quick Reference

```r
# Setup at script start
library(tidyverse)
library(viridis)
library(hrbrthemes)

# Define palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Set theme
theme_set(theme_minimal())

# Create plot
p <- ggplot(data, aes(x, y, color = group)) +
  geom_point() +
  scale_color_manual(values = cbPalette) +
  labs(title = "Title", x = "X Label", y = "Y Label")

# Save
ggsave("figure.png", p, width = 8, height = 6, dpi = 300)
```