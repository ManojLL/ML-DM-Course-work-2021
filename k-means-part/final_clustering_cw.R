library(tidyverse)
library(readxl)
library(NbClust)
library(knitr)
library(tidymodels)
library(flexclust)
library(funtimes)
library(caret)
theme_set(theme_light())
library(factoextra)
library(cluster) 

# read data from xcel file
vehicle_data <- read_excel("vehicles.xlsx")%>%
  janitor::clean_names()%>%
  mutate(class = as_factor(class))
vehicle_data

vehicle.class <- vehicle_data$class
vehicle.class
# get the bird view of the data set
summary(vehicle_data)

#  plot the data set based on clases
# van class
vehicle_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")

#bus class
vehicle_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")

#class opel
vehicle_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'opel'")

#class saab
vehicle_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'saab'")

#
#
#
vehicles_bus_data = vehicle_data %>%
  filter(class == "bus") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_opel_data = vehicle_data %>%
  filter(class == "opel") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_van_data = vehicle_data %>%
  filter(class == "van") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

vehicles_saab_data = vehicle_data %>%
  filter(class == "saab") %>%
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

combine_data = bind_rows(vehicles_bus_data,vehicles_opel_data,vehicles_van_data,vehicles_saab_data)%>%
  arrange(samples)

#######
###
##
combine_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "bus") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'bus'")

combine_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "van") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'van'")

combine_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "opel") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'opel'")

combine_data%>%
  pivot_longer(2:19,names_to = "labels") %>%
  filter(class == "saab") %>%
  mutate(class = fct_reorder(class,value,median)) %>%
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  geom_boxplot() +
  labs(title = "Outlier Detection for class: 'saab'")

#print(combine_data)
# Remove the sample name and the class name. Both of these will be remove so that only n
#numerical data is left for the algorithm.
vehicles_data_points = combine_data%>%
  select(-samples, -class)
# Now that we have the "vehicles_data_points" dataset, scaling is performed

vehicles_scaled_data = vehicles_data_points %>%
  mutate(across(everything(), scale))
vehicles_scaled_data
set.seed(123)

cluster_euclidean = NbClust(vehicles_scaled_data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# use kmeans to clustering part
kc <- kmeans(vehicles_scaled_data,2)
# step 20  - view the data in a table
#par(mfrow=c(2,2), mar=c(5,4,2,2))
table(vehicle.class,kc$cluster)
plot(vehicles_scaled_data[c(2,18)], col=kc$cluster )

fviz_cluster(kc , geom = "point", data = vehicles_scaled_data[c(2,18)]) + ggtitle("Cluster centers for k =2")


# find cluster using elbow method 
k = 2:10
WSS = sapply(k, function(k) {kmeans(vehicles_scaled_data[2:18], centers=k)$tot.withinss})
WSS
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

# according to elbow method I decided k = 3

kc <- kmeans(vehicles_scaled_data,3)
plot(vehicles_scaled_data[c(2,18)], col=kc$cluster )
table(vehicle.class,kc$cluster)


fviz_cluster(kc , geom = "point", data = vehicles_scaled_data[c(2,18)]) + ggtitle("Cluster centers for k =3")
