# Install necessary packages  
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")
install.packages("factoextra")
install.packages("ggplot2")

# Load the required libraries
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualization
library(cluster)     # For clustering methods
library(openxlsx)    # For exporting data to Excel
library(factoextra)  # For visualization of clustering results
library(ggplot2)     # For advanced plotting

################## PREPARATION ###################

# IMPORTING DATA FROM EXCEL 
data <- read_excel(file.choose())

# View the imported dataset
View(data)

# Display column names of the dataset
names(data)

# Display basic summary statistics for each variable
summary(data)

################### DATA PREPROCESSING ###################

# REMOVE DEMOGRAPHIC VARIABLES FOR CLUSTERING
behavioral_data <- data %>% select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)

# STANDARDIZE DATA (Scale the behavioral attributes only)
dfz <- scale(behavioral_data)  # Standardization ensures all variables have equal weight

# Display basic summary statistics for each variable
summary(dfz)
cor(dfz)
################### DETERMINE OPTIMAL NUMBER OF CLUSTERS ###################

# Using the Elbow Method to determine the best number of clusters
elbow_plot <- fviz_nbclust(dfz, kmeans, method = "wss")  # Plot Elbow Method
ggsave("ELBOW_PLOT.png", plot = elbow_plot)  # Save the Elbow plot

# Users should manually inspect the Elbow Plot and choose the optimal number of clusters
k_optimal <- 4  # Replace this with the actual best k from the Elbow Plot

# Compute silhouette scores for k = 2 to k = 8
k_values <- 2:8
sil_scores <- sapply(k_values, function(k) {
  km <- kmeans(dfz, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(dfz))
  mean(ss[, 3])  # Extracting the mean silhouette width
})

# Generate and save Silhouette Method Plot
silhouette_plot <- ggplot(data.frame(k_values, sil_scores), aes(x = k_values, y = sil_scores)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(title = "Silhouette Method for Optimal Clusters",
       x = "Number of Clusters (k)", y = "Average Silhouette Score") +
  theme_minimal()

ggsave("Silhouette_PLOT.png", plot = silhouette_plot)



################### APPLY K-MEANS CLUSTERING ###################

# Set a random seed for reproducibility
set.seed(123)

# Perform K-Means clustering
kmeans_result <- kmeans(dfz, centers = k_optimal, nstart = 25)

# Assign cluster labels to the original dataset
df_final <- data %>% mutate(Cluster = as.factor(kmeans_result$cluster))

################### SEGMENT DESCRIPTION ###################

# CALCULATE SEGMENT SIZES
proportions <- table(df_final$Cluster) / nrow(df_final)
percentages <- proportions * 100
print(percentages)  # Display segment sizes in percentages

# CALCULATE MEAN VALUES OF BEHAVIORAL VARIABLES BY CLUSTER
segments_behavioral <- df_final %>% 
  group_by(Cluster) %>% 
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style), mean, .names = "{col}_mean"))

# CALCULATE DEMOGRAPHIC DISTRIBUTION WITHIN EACH CLUSTER
segments_demographics <- df_final %>%
  group_by(Cluster) %>%
  summarise(Age_mean = mean(Age), 
            Income_mean = mean(Income), 
            Female_Percentage = mean(Female) * 100, 
            AmazonPrime_Percentage = mean(AmznP) * 100,
            Degree_Percentage = mean(Degree == 2) * 100)  # Corrected degree calculation


# MERGE BEHAVIORAL AND DEMOGRAPHIC SUMMARIES
segments_summary <- left_join(segments_behavioral, segments_demographics, by = "Cluster")

# SAVE FINAL SEGMENTATION SUMMARY TO EXCEL
write.xlsx(segments_summary, 'FINALSEGMENTMEANS.xlsx')

################### VALIDATION USING ANOVA ###################

# ANOVA (Analysis of Variance) to verify that clusters are significantly different
# ANOVA checks whether the means of behavioral attributes differ significantly across clusters.

anova_results <- lapply(names(behavioral_data), function(var) {
  aov_formula <- as.formula(paste(var, "~ Cluster"))
  summary(aov(aov_formula, data = df_final))
})

# Print ANOVA results for interpretation
anova_results

# EXPLANATION OF ANOVA:
# - ANOVA tests if the mean values of each feature (e.g., Constant Communication, Wellness Tracking) differ significantly across clusters.
# - A low p-value (< 0.05) suggests that the feature contributes to segmentation.
# - If ANOVA confirms that attributes differ significantly between clusters, this validates the segmentation.

################### VISUALIZATION OF CLUSTERS ###################
# Visualize clusters using PCA plot and assign it to a variable
cluster_plot <- fviz_cluster(kmeans_result, data = dfz, ellipse.type = "norm", geom = "point", ggtheme = theme_minimal())

# Save the plot to a file
ggsave("cluster_plot.png", plot = cluster_plot, width = 8, height = 6)


# PCA for advanced cluster visualization
pca_result <- prcomp(dfz, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x)
pca_data$Cluster <- as.factor(kmeans_result$cluster)

# PCA plot and store it in a variable
pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7, size = 3) +
  labs(title = "Cluster Visualization using PCA",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

# Save the PCA plot
ggsave("pca_data2.png", plot = pca_plot, width = 8, height = 6)


