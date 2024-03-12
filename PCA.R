# Load required packages
# install.packages("ggplot2")
library(ggplot2)

# Use the iris dataset as an example
data(iris)

# Standardize the data: Important when the variables are on different scales
iris_scaled <- scale(iris[, -5])  # Exclude the Species column

# Perform PCA
pca_result <- prcomp(iris_scaled, center = TRUE, scale. = TRUE)

# Print summary
print(summary(pca_result))

# Plot variance explained
scree_plot <- function(pca) {
  percent_var <- pca$sdev^2 / sum(pca$sdev^2) * 100
  barplot(percent_var, main = "Scree Plot", 
          xlab = "Principal Component",
          ylab = "Percentage of Variance Explained",
          ylim = c(0, max(percent_var) + 10),
          names.arg = 1:length(percent_var))
  abline(h = 0, col = "red")
}
scree_plot(pca_result)

# Plot PC1 vs PC2
iris_pca <- as.data.frame(pca_result$x)

ggplot(iris_pca, aes(x = PC1, y = PC2, color = iris$Species)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "PCA of Iris Dataset: PC1 vs PC2", color = "Species")




###### example 2

library(ggplot2)

# 1. Generate simulated data
set.seed(123) # Setting seed for reproducibility
n <- 100 # Number of data points
study_hours <- rnorm(n, mean=5, sd=1) # Generate random study hours
errors <- rnorm(n, mean=0, sd=0.5) # Generate some noise
test_scores <- 1.5 * study_hours + errors # Linear relation between study hours and test scores

data <- data.frame(study_hours, test_scores)

# 2. Plot original data
ggplot(data, aes(x=study_hours, y=test_scores)) + 
  geom_point() +
  ggtitle("Original Data: Study Hours vs. Test Scores") + theme_classic()

# 3. PCA
pca_result <- prcomp(data, center = TRUE, scale. = TRUE)

# 4. Plot data with principal components
data_with_pcs <- cbind(data, pca_result$x)

ggplot(data_with_pcs, aes(x=study_hours, y=test_scores)) + 
  geom_point() +
  geom_segment(aes(x = -pca_result$rotation[1, 1]*3, 
                   y = -pca_result$rotation[2, 1]*3, 
                   xend = pca_result$rotation[1, 1]*3, 
                   yend = pca_result$rotation[2, 1]*3), 
               arrow = arrow(length = unit(0.5,"cm")), color="red", size=1) +
  geom_segment(aes(x = -pca_result$rotation[1, 2]*3, 
                   y = -pca_result$rotation[2, 2]*3, 
                   xend = pca_result$rotation[1, 2]*3, 
                   yend = pca_result$rotation[2, 2]*3), 
               arrow = arrow(length = unit(0.5,"cm")), color="blue", size=1) +
  ggtitle("Data with Principal Components")
