library(fpc)
library(factoextra)
library(readxl)

wine <- read_xlsx("Whitewine_v6.xlsx")
view(wine)

data_df <- as.data.frame(wine)

# Remove the "quality" column from the dataset
new_data <- select(data_df, -quality)
view(new_data)

# Perform PCA analysis
my_pca <- prcomp(new_data, scale = TRUE)
summary(my_pca)

# eigenvalues
print(my_pca$sdev)
# eigenvectors
print(my_pca$rotation)

# calculate total variance explained by each principal component
var_explained = my_pca$sdev^2 / sum(my_pca$sdev^2)
# calculate the cumulative varience explained by each PC
cum_varience <- cumsum(var_explained)
print(cum_varience)

#create scree plot
plot(my_pca, type="l", main="Scree Plot")

# Finding the PCA cutoff 
pca_cutoff <- min(which(cum_varience > 0.85))
pca_cutoff

# Get the relavent PCs to a new dataframe
pca_data <- data.frame(my_pca$x[ , 1:pca_cutoff])
head(pca_data)
view(pca_data)

fviz_nbclust(pca_data, kmeans, method = "wss")
fviz_nbclust(pca_data, kmeans, method = "silhouette")
fviz_nbclust(pca_data, kmeans, method = "gap_stat")

# perfrom k means clustering 
kresult = kmeans(pca_data, centers = 2, iter.max = 100, nstart = 100)

# Calculate TSS
total_mean <- colMeans(pca_data)
tss <- sum(rowSums((pca_data - total_mean)^2))

# Calculate WSS from kmeans object
wss <- sum(kresult$withinss)

# Calculate BSS
bss <- tss - wss

ratio <- bss/tss

# Print results
cat("Total Sum of Squares (TSS):", tss, "\n")
cat("Within Sum of Squares (WSS):", wss, "\n")
cat("Between Sum of Squares (BSS):", bss, "\n")
cat("BSS/TSS ratio:", ratio, "\n")

# plot the clusters 
fviz_pca_ind(my_pca, habillage = kresult$cluster, label = "none", addEllipses = TRUE)

# Calculate Silhouette Scores
silhouette_scores <- silhouette(kresult$cluster, dist(pca_data))

# Plot Silhouette Plot
fviz_silhouette(silhouette_scores)

# Calculate Average Silhouette Width
average_silhouette_width <- mean(silhouette_scores[, "sil_width"])

# Display Average Silhouette Width
cat("Average Silhouette Width:", average_silhouette_width, "\n")

# finding the CH score
round(calinhara(pca_data,kresult$cluster),digits=3)



