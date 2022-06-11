library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(gridExtra)
library(factoextra)
library(caret)
theme_set(theme_light())

DataSet_Vehicle365 <- read_excel("D:/MY_STUFF/IIT/2nd year/2 sem/Machine Learning/cw/vehicles.xlsx") %>%
  janitor:: clean_names() %>%
  mutate(class=as_factor(class))
summary(DataSet_Vehicle365)



#Data Pre-processing and Outliers Detection 
DataSet_Vehicle365 %>%
  pivot_longer(2:19,names_to="labels") %>%
  filter(class == "van") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill=reorder(labels,value)))+
  geom_boxplot()+
  labs(title="detecting outlier for 'van' class:")

DataSet_Vehicle365 %>%
  pivot_longer(2:19,names_to="labels") %>%
  filter(class == "bus") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill=reorder(labels,value)))+
  geom_boxplot()+
  labs(title="detecting outlier for 'bus' class:")

DataSet_Vehicle365 %>%
  pivot_longer(2:19,names_to="labels") %>%
  filter(class == "saab") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill=reorder(labels,value)))+
  geom_boxplot()+
  labs(title="detecting outlier for 'saab' class:")

DataSet_Vehicle365 %>%
  pivot_longer(2:19,names_to="labels") %>%
  filter(class == "opel") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill=reorder(labels,value)))+
  geom_boxplot()+
  labs(title="detecting outlier for 'opel' class:")

# removing outliers 
Bus_DataSet365=DataSet_Vehicle365 %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x,quantile(.x,c(.05, .95)))))

Van_DataSet365=DataSet_Vehicle365 %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x,quantile(.x,c(.05, .95)))))

Opel_DataSet365=DataSet_Vehicle365 %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x,quantile(.x,c(.05,.95)))))

Saab_DataSet365=DataSet_Vehicle365 %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x,quantile(.x,c(.05,.95)))))

compound365=bind_rows(list(Bus_DataSet365,Opel_DataSet365,Saab_DataSet365,Van_DataSet365))%>%
  arrange(samples)

print(compound365)

compound365%>%
  pivot_longer(2:19,names_to = "labels")%>%
  filter(class=="bus") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill = reorder(labels,value)))+
  geom_boxplot()+
  labs(title="transform outliers for class:bus")

compound365%>%
  pivot_longer(2:19,names_to = "labels")%>%
  filter(class=="saab") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill = reorder(labels,value)))+
  geom_boxplot()+
  labs(title="transform outliers for class:saab")

compound365%>%
  pivot_longer(2:19,names_to = "labels")%>%
  filter(class=="opel") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill = reorder(labels,value)))+
  geom_boxplot()+
  labs(title="transform outliers for class:opel")

compound365%>%
  pivot_longer(2:19,names_to = "labels")%>%
  filter(class=="van") %>%
  mutate(class= fct_reorder(class,value,median)) %>%
  ggplot(aes(class,value,fill = reorder(labels,value)))+
  geom_boxplot()+
  labs(title="transform outliers for class:van")

# Data set scaling
DataSet_VehicleClean365 = compound365 %>%
  select(-samples, - class)
DataSet_VehicleScale365= DataSet_VehicleClean365 %>%
  mutate(across(everything(),scale))

# calculating the clusters automatically 
set.seed(123)
# Euclidean formula
Euclidean_formula365 = NbClust(DataSet_VehicleScale365,distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
# Manhattan formula
Manhattan_formula365 = NbClust(DataSet_VehicleScale365,distance = "manhattan", min.nc = 2, max.nc = 15, method = "kmeans", index = "all")

# showing the clusters manually
manual1_365 <- kmeans(DataSet_VehicleScale365, centers = 2)
manual2_365 <- kmeans(DataSet_VehicleScale365, centers = 3)
manual3_365 <- kmeans(DataSet_VehicleScale365, centers = 4)
manual4_365 <- kmeans(DataSet_VehicleScale365, centers = 5)

# manual1_365 analyzing in K-mean
c1_365 <- fviz_cluster(manual1_365, geom = "point", data = DataSet_VehicleScale365) + ggtitle("k = 2")
manual1_365
#Evaluation of the 19th column's produced result
table(DataSet_Vehicle365$class,manual1_365$cluster)
r2_365 <- c(factor(DataSet_Vehicle365$class))
cluster365 <- c(factor(manual1_365$cluster))
E2_365 <- factor(r2_365)
P2_365 <- factor(cluster365)
sample2_365 <- confusionMatrix(data=P2_365, reference = E2_365)
sample2_365$overall
sample2_365$byClass


# manual2_365 analyzing in K-mean
c2_365 <- fviz_cluster(manual2_365, geom = "point",  data = DataSet_VehicleScale365) + ggtitle("k = 3")
manual2_365
#Evaluation of the 19th column's produced result
table(DataSet_Vehicle365$class,manual2_365$cluster)
class322.3 <- c(factor(DataSet_Vehicle365$class))
cluster322.3 <- c(factor(manual2_365$cluster))
E3_365 <- factor(class322.3)
p3_365 <- factor(cluster322.3)
sample3_365 <- confusionMatrix(data=p3_365, reference = E3_365)
sample3_365$overall
sample3_365$byClass


# manual3_365 analyzing in K-mean
c3_365 <- fviz_cluster(manual3_365, geom = "point",  data = DataSet_VehicleScale365) + ggtitle("k = 4")
manual3_365
#Evaluation of the 19th column's produced result
table(DataSet_Vehicle365$class,manual3_365$cluster)
class322.4 <- c(factor(DataSet_Vehicle365$class))
cluster322.4 <- c(factor(manual3_365$cluster))
E4_365 <- factor(class322.4)
p4_365 <- factor(cluster322.4)
sample4_365 <- confusionMatrix(data=p4_365, reference = E4_365)
sample4_365$overall
sample4_365$byClass


# manual4_365 analyzing in K-mean
c4_365 <- fviz_cluster(manual4_365, geom = "point",  data = DataSet_VehicleScale365) + ggtitle("k = 5")
manual4_365
#Evaluation of the 19th column's produced result
table(DataSet_Vehicle365$class,manual4_365$cluster)


grid.arrange(c1_365, c2_365, c3_365, c4_365, nrow = 2)

# last cluster
Outcome365 <- kmeans(DataSet_VehicleScale365,centers = 2)

# coordinates in result
Outcome365$centers
table(DataSet_Vehicle365$class, Outcome365$cluster)

