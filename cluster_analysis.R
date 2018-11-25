setwd('G:/Meu Drive/ic_desastres/anual')

library(tidyverse)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

df <- read_csv('df_desastres_anual.csv')
df$code <- as.factor(df$code)
df$disaster_type <- as.factor(df$disaster_type)
df$location <- as.factor(df$location)

df$disaster_state_key = paste0(df$disaster_key,df$code,sep="")

dimensionality_subset <- as.matrix(select(df, total_affected, total_deaths, total_damage_000USD, insured_losses_000USD,
                                population, percentage_of_woman_30_to_54, gdp_cap, poor_sanitation_deaths_per_100thousand,
                                basic_water_access, garbage_collection_access, sewerage_or_tank_access, max_2_per_dorm_percent))
rownames(dimensionality_subset) <- df$disaster_state_key

dimensionality_subset <- na.omit(dimensionality_subset)

# Exploratory data analysis
summary(dimensionality_subset)

correl <- cor(dimensionality_subset, method = 'pearson', use = 'complete.obs')
ggcorrplot(correl)

ggcorrplot_clustered <- ggcorrplot(correl, hc.order = TRUE, type = "lower")

# Principal components - prcomp
subset_pr <- prcomp(dimensionality_subset, scale = TRUE)
biplot(subset_pr, main = 'Principal Components Biplot')

subset_pr_var <- subset_pr$sdev^2
subset_pr_percentual_var <- subset_pr_var/sum(subset_pr_var)

plot(subset_pr_percentual_var, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b", main = 'Proportion of Variance Explained by each Principal Component')

plot(cumsum(subset_pr_percentual_var),xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b", main = 'Cumulative Proportion of Variance')

#another PCA option - FactoMineR
facto_pca <- PCA(scale(dimensionality_subset), graph = FALSE)
summary(facto_pca)

# Variables contributions do Principal Components
fviz_pca_var(facto_pca, col.var = 'contrib',
             gradient.cols = c('#bb2e00', '#002bbb'),
             repel = TRUE)

fviz_pca_var(facto_pca, 
             select.var = list(contrib = 4),
             repel = TRUE)
fviz_contrib(facto_pca,
             choice = 'var',
             axes = 1,
             top = 5)

fviz_contrib(facto_pca,
             choice = 'var',
             axes = 2,
             top = 5)

#hierarchical clustering with 4 principal components (~80% of variance explained)

pr_hclust <- hclust(dist(subset_pr$x[,1:4]), method = 'complete')
plot(pr_hclust)

hc_cut <- cutree(pr_hclust, k = 4)

clust_result <- tibble(pr_hclust$labels, hc_cut)
colnames(clust_result)[1] <- 'disaster_state_key'

clust_df <- merge(df, clust_result, by = 'disaster_state_key', all.y = TRUE)

#clusters:

clust1 <- filter(clust_df, hc_cut == 1)
summary(clust1)

clust2 <- filter(clust_df, hc_cut == 2)
summary(clust2)

clust3 <- filter(clust_df, hc_cut == 3)
summary(clust3)

clust4 <- filter(clust_df, hc_cut == 4)
summary(clust4)







table(clust1$code)
table(clust2$code)
#table(clust1$disaster_type, clust1$code)
#table(clust2$disaster_type, clust2$code)
