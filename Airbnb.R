# 2017-06-30
# Author: Deron
# Title: SCU_R_Final


#載入library 
library("dplyr")
library("ggplot2")
library("rpart")
library("rpart.plot")
library("rattle")
library("randomForest")

col <- c("integer","character","numeric","numeric","factor","integer","integer","integer",
         "character","integer","factor","integer","factor","numeric")

airbnb_SF <- read.csv("airbnb_SF.csv", header = TRUE, sep = ",",stringsAsFactors = FALSE,
                   colClasses = col) 

# plot
ggplot(airbnb_SF, aes(neighbourhood)) + geom_bar(fill = "#FF8888") + coord_flip() + theme_bw() +
  labs(title = "2017 April SF Airbnb")
ggplot(airbnb_SF, aes(neighbourhood)) + geom_bar(aes(fill = price_r)) + coord_flip() + theme_bw() +
  labs(title = "2017 April SF Airbnb")                    
ggplot(airbnb_SF, aes(neighbourhood)) + geom_bar(aes(fill = room_type)) + coord_flip() + theme_bw() +
  labs(title = "2017 April SF Airbnb")  
ggplot(airbnb_SF, aes(neighbourhood)) + geom_bar(aes(fill = review)) + coord_flip() + theme_bw() +
  labs(title = "2017 April SF Airbnb") 

# Regression
lm_fit <- lm(price ~ accommodates + bedrooms, data = airbnb_SF)
summary(lm_fit)

# ----------
# 切分訓練與測試資料
n <- nrow(airbnb_SF)  #取得總筆數
set.seed(888)#設定隨機種子
shuffled_data <- airbnb_SF[sample(n), ]

train_indices <- 1:round(0.7 * n) 
train_data <- shuffled_data[train_indices, ] #訓練資料
test_indices <- (round(0.7 * n) + 1):n
test_data <- shuffled_data[test_indices, ] #測試資料



# 建立決策樹分類模型
tree_model <- rpart(review ~ room_type + price_r + reviews_per_month , 
                  data = train_data, method = "class")
test_predicted = predict(tree_model, test_data, type = "class")

fancyRpartPlot(tree_model) 

# 績效
cm <- table(test_data$review, test_predicted, dnn = c("實際", "預測"))
cm

conf_mat <- table(test_data$review , test_predicted)
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
accuracy

# ----------

# 建立隨機森林樹分類模型
# install.packages("randomForest")

forest_fit <- randomForest(review ~ room_type + price_r + reviews_per_month, 
                           data = train_data, importane = T, proximity = T, n_tree = 300)
test_predicted <- predict(forest_fit, test_data)
cm <- table(test_data$review, test_predicted, dnn = c("實際", "預測"))
cm

# 績效
conf_matrix <- table(test_predicted, test_data$review)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# ----------


# install.packages('ggmap')
# map <- get_map(location = 'SF', zoom = 7)
# ggmap(map)


# Heatmap
library(ggmap)
map <- get_map(location = c(-122.443850, lat = 37.754365), 
               zoom = 12, language = "zh-TW", maptype = "roadmap")
ggmap(map) + geom_point(aes(x = longitude, y = latitude),
                        color = "#009FCC", size = 1.2, shape = 19, data = airbnb_SF) +
  stat_density2d(data = airbnb_SF,
                 aes(longitude, latitude, fill = ..level.., alpha = ..level..), size = 0.6, 
                 bins = 12, geom = "polygon") + 
  scale_fill_gradient("Hot_zone", low = "white",high= "#FF5511") +
  scale_alpha(range = c(0, 0.7), guide = FALSE)

# Heatmap-2 
airbnb_SF_H <- airbnb_SF %>% filter(review == 'Very High')

map <- get_map(location = c(-122.443850, lat = 37.754365), 
               zoom = 12, language = "zh-TW", maptype = "roadmap")
ggmap(map) + geom_point(aes(x = longitude, y = latitude),
                        color = "#009FCC", size = 1.2, shape = 19, data = airbnb_SF_H) +
  stat_density2d(data = airbnb_SF_H,
                 aes(longitude, latitude, fill = ..level.., alpha = ..level..), size = 0.6, 
                 bins = 12, geom = "polygon") + 
  scale_fill_gradient("Hot_zone", low = "white",high= "#FF5511") +
  scale_alpha(range = c(0, 0.7), guide = FALSE)
