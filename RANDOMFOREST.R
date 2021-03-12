########################################
library(ggplot2)
library(corrplot)
library(Metrics)
library(dplyr)
library(ranger)
########################################
data(diamonds)
summary(diamonds)
dim(diamonds)
data = diamonds
data$price = as.numeric(data$price)
data$cut = as.numeric(data$cut)
data$color = as.numeric(data$color)
data$clarity = as.numeric(data$clarity)
########################################
set.seed(100)
########################################
percentage = 0.8
########################################
train <- sample(nrow(data), percentage*nrow(data), replace = FALSE)
TrainSet <- data[train,]
ValidSet <- data[-train,]
########################################
numTree = 30
numVar = 4
maxDepth = 20
minNodeSize = 5
########################################
model <- ranger(price~carat+cut+color+clarity+depth+table, data = TrainSet,
                        num.trees = numTree,
                        mtry = numVar,
                        max.depth = maxDepth,
                        min.node.size = minNodeSize,
                importance = "impurity")
model
########################################
pred <- predict(model, ValidSet)$predictions
rmse(ValidSet$price, pred)
########################################
ggplot(  ) + 
  geom_jitter( aes(x = data$carat, y = data$price, color = "#b83b5e", alpha = 0.5)) + 
  geom_jitter( aes(x = ValidSet$carat, y = pred, color = "#f08a5d",  alpha = 0.5)) + 
  labs(x = "Carat", y = "Price", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c("Real", "Predicted"), values = c("#b83b5e","#f08a5d"))
########################################
imps <- data.frame(var = model$forest$independent.variable.names,
                   imps = model$variable.importance/max(model$variable.importance))
imps %>% 
  ggplot(aes(imps, x = reorder(var, imps))) +
  geom_point(size = 3, colour = "#b83b5e") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores") +
  theme_bw(18)
########################################
col1 <- colorRampPalette(c("#f9ed69", "#f08a5d" , "#b83b5e", "#6a2c70"))
C <- cor(data)
corrplot(C, method = "circle", col = col1(100))
########################################
y=c()
i=1
# Change min/max
# num.trees 1:100
# mtry 2:6
# max.depth 1:50
# min.nide.size 1:100
min = 1
max = 100
for (i in min:max) {
  model <- ranger(price ~ carat+cut+color+clarity+depth+table, 
                        # Change for i TrainSet/numVar/maxDepth/minNodeSize
                        data = TrainSet, 
                        num.trees = numTree, 
                        mtry = numVar,
                        max.depth = maxDepth,
                        min.node.size = minNodeSize,
                        importance = "impurity")
  print(i)
  pred <- predict(model, ValidSet)$predictions
  y[i-1] = rmse(ValidSet$price, pred)
}
########################################
# Mtry
x <- c(2,3,4,5,6)
ggplot() + geom_point(aes(x = x, y = y, colour = "#f08a5d", size = 4)) +
  labs(x = "Mtry", y = "RMSE", color = "", size = "") +
  theme(legend.position="none")
########################################
# Numtrees
# y <- y[!is.na(y)]
x <- c(1:99)
ggplot() + 
  geom_point(aes(x = x, y = y, colour = "#f08a5d")) +
  geom_line(aes(x = x, y = y, colour = "#b83b5e")) + 
  labs(x = "Number of trees", y = "RMSE", colour = "") + 
  geom_line(aes(x = x, y = average, colour = "#6a2c70")) + 
  theme(legend.position="none")
########################################
# MaxDepth
x <- c(1:49)
ggplot() + 
  geom_point(aes(x = x, y = y, colour = "#f08a5d")) +
  geom_line(aes(x = x, y = y, colour = "#b83b5e")) + 
  labs(x = "Maximal tree depth", y = "RMSE", colour = "") + 
  geom_line(aes(x = x, y = average, colour = "#6a2c70")) +
  theme(legend.position="none")
########################################
# MinNodeSize
x <- c(1:99)
ggplot() + 
  geom_point(aes(x = x, y = y, colour = "#f08a5d")) +
  geom_line(aes(x = x, y = y, colour = "#b83b5e")) + 
  labs(x = "Min Node Size", y = "RMSE", colour = "") + 
  theme(legend.position="none")
########################################
ggplot(  ) + 
  geom_jitter( aes(x = data$carat, y = data$price, color = "#b83b5e", alpha = 0.5)) + 
  labs(x = "Carat", y = "Price", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c("Real"), values = c("#b83b5e"))
########################################
average <- c()
aux <-  c()
i=1
for (i in 1:100) {
  aux[i] = y[i]
  average[i] = mean(aux)
}
average