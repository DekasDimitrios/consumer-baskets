# Load the necessary libraries
library(ggplot2)   # Run install.packages("ggplot2") if this line does not work properly.
library(arules)    # Run install.packages("arules") if this line does not work properly.
library(ggpubr)    # Run install.packages("ggpubr") if this line does not work properly.

# Load the data that are going to be used for the analyses.
data <- read.csv(file = "Data/GroceriesInitial.csv", header = TRUE, sep = ",")

# The names of the products that are going to be used for the analyses.
prod_names <- c("citrus fruit","tropical fruit", "whole milk", "other vegetables",
                "rolls/buns", "chocolate", "bottled water", "yogurt", "sausage",
                "root vegetables", "pastry", "soda", "cream")

# Apply a function in order to check whether the products are included in a row or not.
products <- as.data.frame(t(apply(data[,4:35], 1,
                                  function(x) (prod_names) %in% as.character(unlist(x)))))

# Name the columns that are produced.
names(products) <- prod_names

# Bind the binary items and the numerical valued features together.
data_ready <- cbind(data[1:3], products)

# Create three intervals of the same length, between the maximum and minimum basket value.
max_value <- max(data$basket_value)
min_value <- min(data$basket_value)
gap <- (max_value - min_value) / 3
intervals <-  c(min_value + gap, min_value + 2*gap, min_value + 3*gap)

# Use the above intervals in order to create the discrete basket value features.
data_ready$low_value_basket <- ifelse(data_ready$basket_value < intervals[1], TRUE, FALSE)
data_ready$medium_value_basket <- ifelse(data_ready$basket_value >= intervals[1] & data_ready$basket_value < intervals[2], TRUE, FALSE)
data_ready$high_value_basket <- ifelse(data_ready$basket_value >= intervals[2] & data_ready$basket_value < intervals[3], TRUE, FALSE)
 
# Use the Apriori method using different minimum support values.
# Values tested: 0.0050, 0.0030, 0.0020, 0.0010
rules <- apriori(data_ready[,4:ncol(data_ready)]
                 , parameter = list(supp = 0.0010)
                 , control = list(verbose=FALSE))

# Sort the produced rule by their support in decreasing order.
rules_sorted <- sort(rules, by="support", decreasing = TRUE)
inspect(rules_sorted[1:20])

# Find redundant rules
subset.matrix <- is.subset(rules_sorted, rules_sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# Remove redundant rules
rules.pruned <- rules_sorted[!redundant]
inspect(rules.pruned[1:20])

# Use the Apriori method in order to produce the association rules for the items.
rules_items <- apriori(data_ready[prod_names]
                       , parameter = list(conf = 1, support = 0.00050)
                       , control = list(verbose=FALSE))

# Sort the produced rule by their support in decreasing order.
rules_items_sorted <- sort(rules_items, by="support", decreasing = TRUE)
inspect(rules_items_sorted[1:20])

# Find redundant rules
subset.matrix <- is.subset(rules_items_sorted, rules_items_sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# Remove redundant rules
rules.pruned <- rules_items_sorted[!redundant]
inspect(rules.pruned[1:20])

# Use the Apriori method in order to produce the association rules for the items and discrete basket values.
rules_items_values <- apriori(data_ready[4:(ncol(data_ready))]
                 , parameter = list(conf = 1, support = 0.0005)
                 , control = list(verbose=FALSE))

# Sort the produced rule by their support in decreasing order.
rules_items_values_sorted <- sort(rules_items_values, by="support", decreasing = TRUE)
inspect(rules_items_values_sorted[1:20])

# Find redundant rules
subset.matrix <- is.subset(rules_items_values_sorted, rules_items_values_sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# Remove redundant rules
rules.pruned <- rules_items_values_sorted[!redundant]
inspect(rules.pruned[1:20])

# Fix the number that will determine the random results produced.
set.seed(1234)

# The data used to plot the final clusters.
plot_data <- data_ready[2:3]

# Scale the data so the k-means algorithm is not affected by the numerical value.
scaled_data <- data_ready[2:3]
scaled_data[,1] <- (scaled_data[,1] - min(scaled_data[,1])) / (max(scaled_data[,1]) - min(scaled_data[,1]))
scaled_data[,2] <- (scaled_data[,2] - min(scaled_data[,2])) / (max(scaled_data[,2]) - min(scaled_data[,2]))

# Fit k-means for 5 centers.
kmeans_fit <- kmeans(scaled_data, centers = 5, nstart = 1000, iter.max = 1000000000)

# Add clusters obtained using the K-means algorithm to the data to be plotted.
plot_data$cluster <- factor(kmeans_fit$cluster)

# Plot the k-means results.
ggscatter(plot_data, x = "recency_days", y = "basket_value",
          color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
          shape = "cluster", size = 1, legend = "right", ggtheme = theme_bw()) +
          stat_mean(aes(color = cluster), size = 4)

# Add the clusters to the data.
data_ready$Cluster <- kmeans_fit$cluster

# The centers need to be up-scaled to their original values.
centers <- as.data.frame(kmeans_fit$centers)
centers[,1] <- max(plot_data[,1]) * centers[,1]
centers[,2] <- max(plot_data[,2]) * centers[,2]

# Calculate mean and sd for each cluster in both axis.
clusters <- split(as.data.frame(plot_data), kmeans_fit$cluster)
cname <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 5")
meanRD <- c()
meanBV <- c()
sdRD <- c()
sdBV <- c()

for (cluster in clusters) {
  meanRD <- c(meanRD, mean(cluster$recency_days))
  meanBV <- c(meanBV, mean(cluster$basket_value))
  sdRD <- c(sdRD, sd(cluster$recency_days))
  sdBV <- c(sdBV, sd(cluster$basket_value))
}

info <- data.frame(Clusters = cname, "Mean Recency Days"=meanRD,
                   "Mean Basket Value"=meanBV, "SD Recency Days"=sdRD,
                   "SD Basket Values"=sdBV)

# Produce binary features for each cluster.
data_ready$Cluster1 <- ifelse(data_ready$Cluster == 1, TRUE, FALSE)
data_ready$Cluster2 <- ifelse(data_ready$Cluster == 2, TRUE, FALSE)
data_ready$Cluster3 <- ifelse(data_ready$Cluster == 3, TRUE, FALSE)
data_ready$Cluster4 <- ifelse(data_ready$Cluster == 4, TRUE, FALSE)
data_ready$Cluster5 <- ifelse(data_ready$Cluster == 5, TRUE, FALSE)

# Bind the item features with the clusters appointed to each basket.
data_items_clusters <- cbind(data_ready[prod_names], data_ready[21:25])

# Use the Apriori method in order to produce the association rules for the items and discrete basket values.
rules_items_clusters <- apriori(data_items_clusters
                                , parameter = list(conf = 1, support = 0.0005)
                                , control = list(verbose=FALSE))

# Sort the produced rule by their support in decreasing order.
rules_items_clusters_sorted <- sort(rules_items_clusters, by="support", decreasing = TRUE)
inspect(rules_items_clusters_sorted[1:20])

# Find redundant rules
subset.matrix <- is.subset(rules_items_clusters_sorted, rules_items_clusters_sorted)
subset.matrix[lower.tri(subset.matrix, diag=TRUE)] <- FALSE
redundant <- colSums(subset.matrix, na.rm=TRUE) >= 1
which(redundant)
# Remove redundant rules
rules.pruned <- rules_items_clusters_sorted[!redundant]
inspect(rules.pruned[1:20])
