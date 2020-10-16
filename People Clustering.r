# Setting the Global Option
options(future.globals.maxSize = 4000 * 1024^5)

# Loading the Libraries
library(tidyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)    # number of clusters identification

# Preparing the People Data
people2 <- people[, c(1,8,15)]
people2 <- na.omit(people2)
people2$Regions <- as.numeric(as.factor(people2$Regions))

# Scaling the Data
people_clust <- scale(people2[, 2:3])

# Function to compute total within-cluster sum of square 
wss <- function(k) {
        kmeans(people_clust, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:5

# Extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

# Plotting WSS
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares",
     main = "WSS Vs Probable Number of Clusters for People Data")

# Cluster Analysis (3 Clusters)
k3_people <- kmeans(people_clust, centers = 3, nstart = 25)

# Visualizing the Clusters
p3_people <- fviz_cluster(k3_people, geom = "point",  data = people_clust) + 
        ggtitle("People Data k-means Clustering") + 
        theme(plot.title = element_text(size = 20, face = "bold"))
p3_people

# Creating new Df with Cluster Label
people3 <- data.frame(cbind(people2, k3_people[1]))

# Box-plot for Regions (V1)
boxplot(people3$Regions ~ people3$cluster,
        xlab = "Cluster",
        ylab = "Region Number",
        main = "People Data Clusters by Region")

# Box-plot for No. of Orgs Founded (V2)
boxplot(people3$Number.of.Founded.Organizations ~ people3$cluster,
        xlab = "Cluster",
        ylab = "Number of Founded Organizations",
        main = "People Data Clusters by No. of Founded Organizations")




# Additional Helpful Codes
# Elbow method
fviz_nbclust(people2[, 2:3], kmeans, method = "wss") +
        geom_vline(xintercept = 4, linetype = 2)+
        labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(people_df_clust[,2:3], kmeans, method = "silhouette")+
        labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(people2[, 2:3], kmeans, nstart = 2,  method = "gap_stat", nboot = 5)+
        labs(subtitle = "Gap statistic method")

# Full Analysis
NbClust(data = df2, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 5, method = "kmenas")

# fviz_dist: for visualizing a distance matrix
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# In case of PCA for multiple clusters
fviz_cluster(k2, data = df)




### Credit goes to https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
### Credit goes to https://uc-r.github.io/kmeans_clustering#:~:text=For%20each%20k%2C%20calculate%20the,the%20appropriate%20number%20of%20clusters.


