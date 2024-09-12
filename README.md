# PCoA-in-R-Studio
# Load necessary libraries
library(vegan)
library(ggplot2)
library(dplyr)

# Generate a random dataset (distance matrix)
set.seed(123)  # For reproducibility
n <- 50  # Number of observations
random_data <- matrix(rnorm(n * 5), nrow = n, ncol = 5)
rownames(random_data) <- paste0("Sample_", 1:n)

# Calculate Euclidean distance matrix
distance_matrix <- dist(random_data)

# Perform Principal Coordinate Analysis (PCoA)
pcoa_result <- cmdscale(distance_matrix, k = 2, eig = TRUE)  # k = 2 for 2D

# Extract coordinates
pcoa_data <- as.data.frame(pcoa_result$points)
colnames(pcoa_data) <- c("PCoA1", "PCoA2")
pcoa_data$Sample <- rownames(pcoa_data)

# Generate random groups (for colorful plotting)
set.seed(123)
pcoa_data$Group <- factor(sample(1:3, n, replace = TRUE))

# Plot the PCoA results with ggplot2
ggplot(pcoa_data, aes(x = PCoA1, y = PCoA2, color = Group, label = Sample)) +
  geom_point(size = 4, alpha = 0.8) +               # Plot points
  geom_text(aes(label = Sample), hjust = 1.5, vjust = 0.5, size = 3) +  # Add labels
  scale_color_manual(values = c("tomato", "steelblue", "forestgreen")) + # Custom colors
  labs(title = "Principal Coordinate Analysis (PCoA)",
       x = "PCoA Axis 1",
       y = "PCoA Axis 2") +
  theme_minimal() +                              # Clean theme
  theme(legend.position = "right",               # Place the legend
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Add horizontal line at y = 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray")    # Add vertical line at x = 0
