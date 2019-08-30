library("dplyr")
library("tidyr")
library("forcats")
library("ggplot2")
computer_ds <- readr::read_csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/computers.csv")
View(computer_ds)
#now K means doesnot work with factor variables . so we will not consider the features cd, milti and premium
final_computer_ds <- subset(computer_ds, select=-c(X1,cd, multi, premium))
View(final_computer_ds)
#From the summary statistics, you can see the data has large values. 
#A good practice with k mean and distance calculation is to rescale the data so that the mean 
#is equal to one and the standard deviation is equal to zero. 
summary(final_computer_ds)
#You rescale the variables with the scale() function of the dplyr library. The transformation reduces 
#the impact of outliers and allows to compare a sole observation against the mean. If a standardized 
#value (or z-score) is high, you can be confident that this observation is indeed above the mean 
#(a large z-score implies that this point is far away from the mean in term of standard deviation. 
#A z-score of two indicates the value is 2 standard deviations away from the mean. 
#Note, the z-score follows a Gaussian distribution and is symmetrical around the mean.
rescale_computer_ds <- final_computer_ds %>%
mutate(price_scal = scale(price),
    speed_scal = scale(speed),
    hd_scal = scale(hd),
    ram_scal = scale(ram),
    screen_scal = scale(screen),
    ads_scal = scale(ads),
    trend_scal = scale(trend)) %>%
select(-c(price, speed, hd, ram, screen, ads, trend))
glimpse(rescale_computer_ds)
summary(rescale_computer_ds)

install.packages("animation")
#You can see each step graphically with the great package build by Yi Hui (also creator of Knit for Rmarkdown)
#After you load the library, you add .ani after kmeans and R will plot all the steps. 
#For illustration purpose, you only run the algorithm with the rescaled variables hd and ram with
#three clusters.

#R base has a function to run the k mean algorithm. The basic function of k mean is:
#kmeans(df, k)
#arguments:
#-df: dataset used to run the algorithm
#-k: Number of clusters

set.seed(2345)
library("animation")
kmeans.ani(rescale_computer_ds, 5)

pc_cluster <- kmeans(rescale_computer_ds,5)
pc_cluster$cluster #indicates the cluster of each observation
pc_cluster$centers #indicates the cluster centroids
pc_cluster$totss #total sum of squares
pc_cluster$size #no of observations within each cluster
pc_cluster$withinss #within sum of squares
pc_cluster$tot.withinss # this is sum of withinss
pc_cluster$betweenss #between clusters sum of squares

#we will see the tot.withinss parameter to compute the optimal number of clusters ie value of K
# we will use the elbow method
# This method uses within-group homogeneity or within-group heterogeneity to evaluate the variability. 
#In other words, you are interested in the percentage of the variance explained by each cluster.
#You can expect the variability to increase with the number of clusters, alternatively, heterogeneity
#decreases. Our challenge is to find the k that is beyond the diminishing returns. 
#Adding a new cluster does not improve the variability in the data because very few information is left to explain.


#   You can construct the elbow graph and find the optimal k as follow:

#    Step 1: Construct a function to compute the total within clusters sum of squares
#    Step 2: Run the algorithm times
#    Step 3: Create a data frame with the results of the algorithm
#    Step 4: Plot the results

kmean_withinss <- function(k) {
    cluster <- kmeans(rescale_computer_ds, k)
    return (cluster$tot.withinss)
}

#Run the algorithm n times
#will use the sapply() function to run the algorithm over a range of k. This technique is faster than creating a loop and store the value.


# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)

# Plot the graph with ggplot
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 20, by = 1))

#we find the optimum value of k as 7 so we run the final k means clustering
pc_cluster <- kmeans(rescale_computer_ds,7,nstart =10)

#               visualizing K means cluster
#It is a good idea to plot the cluster results. These can be used to assess the choice of the number of 
#clusters as well as comparing two different cluster analyses. Now, we want to visualize the data in a
#scatter plot with coloring each data point according to its cluster assignment. The problem is that the 
#data contains more than 2 variables and the question is what variables to choose for the xy scatter plot.
#A solution is to reduce the number of dimensions by applying a dimensionality reduction algorithm, 
#such as Principal Component Analysis (PCA). So if we have a multi-dimensional data set, a solution is to 
#perform Principal Component Analysis (PCA) and to plot data points according to the first two principal 
#components coordinates.

install.packages("factoextra")
library("factoextra")#To create a beautiful graph of the clusters generated with the kmeans() function, will use the factoextra package.
fviz_cluster(pc_cluster,rescale_computer_ds,ellipse.type ="norm", palette = "Set2", ggtheme = theme_minimal())
#Dertermining and Visualizing the Optimal Number of Clusters using factoextra package
fviz_nbclust(rescale_computer_ds, kmeans, method = "wss") +
geom_vline(xintercept = 3, linetype = 2)









