# Collab, Notes & Lecture by Akhona Njeje.
# Date 10 July 2023.
# Topic & Solution : K-Means Clustering, this Algorithm allows us to 
#                    work with unlabled data.
# Documentation : Intro to Stats Learning by Gareth James, Chapter 10.
# Applications : Botany.



# Notes:

# KMC is an UL algorithm that attempt to group similar clusters together in your data.
# KMC type of problems:
### Cluster similar documents.
### Market segmentation.
### Identify similar groups.

# Overall goal is to divide data into distinct groups such that observations,
# within each group are similar.

# The best way to choose thwe K-value is use the elbow method.

install.packages("ISLR")
library(ISLR)

print(head(iris)) # iris is already a built in dataset inside R.

# Visualisations.

library(ggplot2)
pl = ggplot(iris, aes(Petal.Length, Petal.Width, color = Species))
print(pl + geom_point(size=4))


# Implementing KMC.

set.seed(101)
irisCluster = kmeans(iris[,1:4], 3, nstart = 20)
print(irisCluster) # ANALYSIS.


# Evaluate our KMC.

table(irisCluster$cluster, iris$Species)


# Visual Clustering.

library(cluster)
clusplot(iris, irisCluster$cluster, color = T, shade = T, labels = 0, lines = 0)

# More info = help("clusplot").

#######################################################################

# Project KMC.

### Load data.

df1 = read.csv('C:/Users/User/Desktop/AI/Machine Learning/R/KMeans/winequality-red.csv', sep = ";")
df2 = read.csv('C:/Users/User/Desktop/AI/Machine Learning/R/KMeans/winequality-white.csv', sep = ";")

print(head(df1))
print(head(df2))

# Combine df1 & df2 together into a single table.

### df1$label = sapply(df1$pH, function(x){'red'})
wine = rbind(df1, df2, sep = ";")

library(ggplot2)
pl = ggplot(wine, aes(citric.acid)) + geom_histogram(aes(fill = label), 
                                                        color = 'black', bins = 50)
pl + scale_fill_manual(values = c("#ae4554", "#faf7ea")) + theme_bw() # Error in `geom_histogram()`.

# Scatter.
pl = ggplot(wine, aes(citric.acid, residual.sugar)) + geom_point(aes(color=label),
                                                                 alpha=0.2)
pl + scale_color_manual(values = c('white', 'red'))


# Model Development.

