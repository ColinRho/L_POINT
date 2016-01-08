### TOPIC 2. EDA

source("TOPIC2/topic2.R", encoding = "UTF-8")



# glimpse into purchase distribution upon age 
ggplot(merged, aes(x = 연령)) + geom_histogram()  # looks like exponential


merged %>% group_by(연령) %>% summarise( n = n() ) %>% arrange(연령) -> age_purchase

# Kolmogorov-Smirnov test for exponential dist'n
# we cannot reject the null : we can say that it follows exponential distribution
ks.test(age_purchase$n / mean(age_purchase$n), "pexp")

# empirical distribution and smoothing 
ggplot(age_purchase) + 
  geom_line(aes(x = 연령, y = n)) +
  geom_smooth(aes(x = 연령, y = n))



###################################################################################################
### Analysis 1. PCA
###################################################################################################

## try to group product categories

num.demo <- demo1 %>% select(-(ID:거주지역))

demo.pc1 <- prcomp(x = num.demo, center = TRUE, scale. = TRUE)

num.demo <- demo2 %>% select(-(ID:거주지역))

demo.pc2 <- prcomp(x = num.demo, center = TRUE, scale. = TRUE)

###################################################################################################
### Analysis 2. k-means 
###################################################################################################


library(cluster)


fit <- kmeans(num.demo, 5)

clusplot(num.demo, fit$cluster, color=TRUE, shade=TRUE,
  labels=2, lines=0)

## '2427' must be 

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(num.demo, fit$cluster) 


demo[2427] # outlier

demo1 %<>% slice(-2427)
demo2 %<>% slice(-2427)



###################################################################################################
### Analysis 3. Comparing group means
###################################################################################################

## t-test & ANOVA

## by gender

lapply(demo1[,-(1:5), with = FALSE], function(x) t.test(x ~ demo1$성별))

lapply(demo2[,-(1:5), with = FALSE], function(x) t.test(x ~ demo2$성별))

## by location

lapply(demo1[,-(1:5), with = FALSE], function(x) summary(aov(x ~ demo1$loc1)))

lapply(demo2[,-(1:5), with = FALSE], function(x) summary(aov(x ~ demo2$loc1)))





