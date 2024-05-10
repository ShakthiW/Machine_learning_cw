install.packages("tidyverse")
install.packages("readxl")
install.packages("factoextra")
install.packages("NbClust")

# Load libraries
library(tidyverse)
library(factoextra)
library(readxl)
library(NbClust)

# Read the Excel file
wine <- read_xlsx("Whitewine_v6.xlsx")
dim(wine)

# Scale the data 
data <- scale(wine)

# Convert scaled data to a data frame
data_df <- as.data.frame(data)

# Remove the "quality" column from the dataset
new_data <- select(data_df, -quality)

summary(new_data)

# Define a function to remove outliers using IQR method
remove_outliers_IQR <- function(dataframe, columns = names(dataframe)) {
  for (col in columns) {
    Q1 <- quantile(dataframe[[col]], probs = 0.25, na.rm = TRUE)
    Q3 <- quantile(dataframe[[col]], probs = 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.3 * IQR
    upper <- Q3 + 1.3 * IQR
    dataframe <- dataframe[dataframe[[col]] >= lower & dataframe[[col]] <= upper, ]
  }
  return(dataframe)
}

# Apply the function to the new_data dataset
new_data_no_outliers <- remove_outliers_IQR(new_data)

# Check the dimensions
dim(new_data_no_outliers)

# Summary statistics after outlier handling
summary(new_data_no_outliers)

# Boxplot after outlier handling
boxplot(new_data_no_outliers)
boxplot(new_data)

# Check for missing values
if (any(is.na(new_data_no_outliers))) {
  stop("Data contains missing values after outlier handling. Consider alternative methods.")
}

# Convert to data frame
df_no_outliers <- as.data.frame(new_data_no_outliers)

# NbClust method
nb <- NbClust(df_no_outliers, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")

fviz_nbclust(df_no_outliers, kmeans, method = "wss")
# silhouette method
fviz_nbclust(df_no_outliers, kmeans, method = "silhouette")
# Gap Statistics method
fviz_nbclust(df_no_outliers, kmeans, method = "gap_stat")

# Perform K-means clustering
kresult <- kmeans(df_no_outliers, centers = 2, iter.max = 100, nstart = 100)

# Calculate TSS
total_mean <- colMeans(df_no_outliers)
tss <- sum(rowSums((df_no_outliers - total_mean)^2))

# Calculate WSS from kmeans object
wss <- sum(kresult$withinss)

# Calculate BSS
bss <- tss - wss

# Print results
cat("Total Sum of Squares (TSS):", tss, "\n")
cat("Within Sum of Squares (WSS):", wss, "\n")
cat("Between Sum of Squares (BSS):", bss, "\n")

# Visualize clusters
fviz_cluster(kresult, data = df_no_outliers,
             palette = c("#2E9FDF", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

kresult

library(cluster)

# Calculate Silhouette Scores
silhouette_scores <- silhouette(kresult$cluster, dist(df_no_outliers))

# Plot Silhouette Plot
fviz_silhouette(silhouette_scores)

# Calculate Average Silhouette Width
average_silhouette_width <- mean(silhouette_scores[, "sil_width"])

# Display Average Silhouette Width
cat("Average Silhouette Width:", average_silhouette_width, "\n")

