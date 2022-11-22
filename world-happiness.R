# Install necessary packages

if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")


# Load necessary libraries 

library(plyr)
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)
library(scales)
library(DescTools)
library(tinytex)
library(caTools)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(reshape2)
library(data.table)
library(tidyr)
library(corrgram)       
library(corrplot)
library(formattable)
library(cowplot)
library(ggpubr)
library(neuralnet)

# Original source file:
# World Happiness Report 2021 (www.kaggle.com)
# Specific file: https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021
#Acquire / load data
library(readr)
urlfile="https://raw.githubusercontent.com/mihovey/cyo_capstone/main/world-happiness-report-2021.csv"
whr21<-read_csv(url(urlfile))
rm(urlfile)


## Review Raw Data Structure

#Review list of column names, row count, and column count
head(whr21) 
nrow(whr21)
ncol(whr21)

#Review list of column names
colnames(whr21)

## Clean the Data 

# Delete unnecessary columns
whr21 <- whr21[, -c(4,5,6,7,8,9,10,11,12,13,20)]

# Change  the name of the remaining  variables (columns)
colnames (whr21) <- c("Nation", "Region", "Happiness",
                          "Prosperity", "Network", "Wellbeing", "Freedom",
                          "Generosity", "Corruption")

# Convert Region to a *factor* from *character*
whr21$Region <- as.factor(whr21$Region)


#Review list of variables and first 6 rows
head(whr21) 


## Exploratory Data Analysis and Visualization

### Orientation to the Dataset

# Create table to show class of variables, structure and final rows
rbind((lapply(whr21, class)), head(whr21))

### Data Visualization

#### Broadest Overview:  Distributions of Happiness

# Visualize distribution of Happiness
whr21 %>% ggplot(aes(x = Happiness, y = ..density..)) +
  geom_histogram(alpha = 0.5, binwidth = 1.0, color = ("grey40"), fill = ("skyblue2")) +
  geom_density(size = 1.0, color = "blue") +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60), labels = c("10", "20", "30", "40", "50", "60")) +
  scale_x_continuous() +
  labs(x = "Happiness Score (0 to 10)", y = "Number of Ratings", caption = "World Happiness Report - 2021 ")

# Visualize distribution of Happiness but more granular...
whr21 %>% ggplot(aes(x = Happiness, y = ..density..)) +
  geom_histogram(alpha = 0.5, binwidth = 0.1, color = ("grey40"), fill = ("skyblue2")) +
  geom_density(size = 1.5, color = "blue") +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9")) +
  scale_x_continuous() +
  labs(x = "Happiness Score (Possible Range:  0 to 10)", y = "Number of Ratings", caption = "World Happiness Report - 2021 ")

#### Broad Overview of Correlations Between Variables

# Correlation between variables

# Finding the correlation between numerical columns
Num.cols <- sapply(whr21, is.numeric)
corr_data <- cor(whr21[, Num.cols])
corrplot.mixed(corr_data, lower = "number", upper = "pie", tl.col = "black", tl.cex = 0.65)

#### Overview of Happiness by Region
# Happiness score summary stats by Region
gg_stats_table_1 <- desc_statby(whr21, measure.var = "Happiness",
                                grps = "Region")
gg_stats_table_1 <- gg_stats_table_1[, c("Region","mean","median", "max", "min")]
names(gg_stats_table_1) <- c("Region", "Mean","Median", "Maximum", "Minimum")

# Summary table
gg_stats_table1.p <- ggtexttable(gg_stats_table_1,rows = NULL, 
                                 theme = ttheme("classic"))
gg_stats_table1.p

# Happiness score plots by Region
# I have left my various experimentations with visual appearances here for your reference.  These were my musings and attempts to achieve a legible graphic.
gg_plot_scatter <- ggplot(whr21,
                          aes(x=Region,
                              y=Happiness,
                              color=Region)) + 
  geom_point() + theme() +
  theme(legend.position = "right") +
  theme(legend.text = element_text(size = 12)) +
  #theme(axis.text.x = element_text(angle=90)) +
  #theme(axis.text.x = element_blank()) +
  #theme(legend.title = element_blank()) +
  #theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = 14)) +
  xlab("Region") + ylab("Happiness") +
  geom_point(stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

gg_plot_box <- ggplot(whr21 , aes(x = Region, y = Happiness)) +
  geom_boxplot(aes(fill=Region)) + theme() +
  theme(legend.position = "right") +
  theme(legend.text = element_text(size = 12)) +
  #theme(axis.text.x = element_text(angle = 90)) +
  #theme(axis.text.x = element_blank()) +
  #theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.title = element_text(size = (14))) + 
  xlab("Region") + ylab("Happiness") +
  #geom_boxplot(stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

# Arrange report grid
ggarrange(gg_plot_scatter, gg_plot_box, ncol = 1, nrow = 2)

#### Composite Comparison All Variables by Region
# Prepare data for composite melt
whr21.region <- whr21 %>%
  select(-0) %>%
  group_by(Region) %>%
  summarize_at(vars(-Nation), funs(mean(., na.rm=TRUE)))

# Melt the "whr21.Region" dataset
whr21.region.melt <- melt(whr21.region)

# Facet melted dataset
ggplot(whr21.region.melt, aes(y=value, x=Region, color=Region, fill=Region)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(strip.text.x = element_text(size = 24)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.title = element_text(size = (24))) + 
  theme(legend.position = "none") +
  theme(legend.text = element_text(size = 18)) +
  labs(y = "Average Score") 

## Development Methods for Multiple Models of Prediction

### Maching Learning Model Development

#### Simple Average Method
# Determine overall average rating for all Nations in train set
mu_hat <- mean(whr21$Happiness)

# Calculate RMSE between each test set rating included and the overall average (mu_hat)
RMSE_simple_average <- RMSE(whr21$Happiness, mu_hat)
MSE_simple_average <- RMSE_simple_average^2

# Determine predicted score by sum method and calculate the corresponding RMSE
simple_average <- whr21 %>% mutate(pred_simple_average = Prosperity +
                                     Network + Wellbeing +
                                     Freedom + Generosity + 
                                     Corruption + 2.430, 
                                   RMSE = RMSE(Happiness, pred_simple_average))

# Plot pred_actual_simple_average
pred_actual_simple_average <- as.data.frame(cbind(Prediction = whr21$Happiness, Actual = simple_average$pred_simple_average))

gg_plot_simple_average <- ggplot(pred_actual_simple_average, aes(Actual, Prediction)) +
  geom_point() + 
  annotate("text", x = 6, y = 8, label = paste("RMSE = ", round(RMSE_simple_average,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Simple Average Method", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_simple_average


#### Simple Sum Method

# Find predicted score by sum method and calculate the corresponding RMSE
simple_sum <- whr21 %>% mutate(pred_simple_sum = Prosperity +
                                 Network + Wellbeing +
                                 Freedom + Generosity + 
                                 Corruption + 2.430, 
                               RMSE = RMSE(Happiness, pred_simple_sum))

# Record RMSE for the simple sum model
RMSE_simple_sum <- RMSE(simple_sum$Happiness, simple_sum$pred_simple_sum)
MSE_simple_sum <- RMSE_simple_sum^2

# Plot pred_actual_simple_sum
pred_actual_simple_sum <- as.data.frame(cbind(Prediction = simple_sum$Happiness, Actual = simple_sum$pred_simple_sum))

gg_plot_simple_sum <- ggplot(pred_actual_simple_sum, aes(Actual, Prediction)) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_simple_sum,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Simple Sum Method", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_simple_sum

#### What is the appropriate dataset split?

# Test for an "best" split of full data set (whr21)
# First, we will create sequence of p values or spread to test
ps <- (seq(from=0.20, to= 0.90, by=.005))

# Calculate RMSEs for each value of p
rmses <- sapply(ps, function(p){
  train_index <- as.numeric(createDataPartition(whr21$Happiness, times=1, p=p, list=FALSE))
  train_split <- whr21[train_index,]
  test_split <- whr21[-train_index,]
  gof <- glm(Happiness ~ Prosperity + Network + Wellbeing + Freedom + Generosity + Corruption, 
             data = train_split)
  test_split <- test_split %>% mutate(pred_score = predict.glm(gof, newdata=test_split))
  RMSE(test_split$Happiness, test_split$pred_score)
})

# Capture the lowest value of RMSE for this split test
low <- min(rmses)

# Capture the split ratio between train and test subsets
# x_values[which.min(rmses)] : 1-x_values[which.min(rmses)]
p_train <- ps[which.min(rmses)]
p_test <- 1-ps[which.min(rmses)]

# Plot optimal RMSE
plot(ps, rmses, main = "Determine Lowest RMSE", xlab = "Values of p", ylab = "RMSE Values") +
  abline(h = low, col = "red") +
  abline(v = p_train, col = "red")

### Prepare Training and Testing Subsests from WHR at (70:30 ratio)

# Split the dataset into subsets: train_set and test_set using *caTools*
set.seed(1234)
regression_subset <- whr21[3:9]
split = sample.split(regression_subset$Happiness, SplitRatio = 0.7)
train_set = subset(regression_subset, split==TRUE)
test_set = subset(regression_subset, split==FALSE)

#### Generalized Linear Regression Model (glm)

# Fit glm to train_set
regressor_glm = glm(formula = Happiness ~ .,
                    data = train_set)

# Predict and plot glm results from test_set
y_pred_glm = predict(regressor_glm, newdata = test_set)

pred_actual_glm <- as.data.frame(cbind(Prediction = y_pred_glm, Actual = test_set$Happiness))

MSE_glm <- sum((test_set$Happiness - y_pred_glm)^2)/nrow(test_set)
RMSE_glm <- sqrt(MSE_glm)

gg_plot_glm <- ggplot(pred_actual_glm, aes(Actual, Prediction)) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_glm,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Generalized Linear Regression", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_glm

#### Multiple Linear Regression Model (lm)

# Fit mlm to train_set
regressor_lm = lm(formula = Happiness ~ .,
               data = train_set)

# Predict and plot mlm results from test_set
y_pred_lm = predict(regressor_lm, newdata = test_set)

pred_actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Happiness))

MSE_lm <- sum((test_set$Happiness - y_pred_lm)^2)/nrow(test_set)
RMSE_lm <- sqrt(MSE_lm)

gg_plot_lm<- ggplot(pred_actual_lm, aes(Actual, Prediction )) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_lm,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_lm

#### Support Vector Regression Model (svm)

# Fit svr/svm to the regression_subset
library(e1071)
regressor_svr = svm(formula = Happiness ~ .,
                data = regression_subset,
                type = 'eps-regression',
                kernel = 'radial')

# Predict and plot svr/svm results from test_set
y_pred_svr = predict(regressor_svr,  newdata = test_set)

pred_actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Happiness))

pred_actual_lm_vs_svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$Happiness)

MSE_svr <- sum((test_set$Happiness - y_pred_svr)^2)/nrow(test_set)
RMSE_svr <- sqrt(MSE_svr)

gg_plot_svr <- ggplot(pred_actual_svr, aes(Actual, Prediction )) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_svr,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Support Vector Regression", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_svr

#### Decision Tree Regression Model (rpart) 

# Fit dt to the regression_subset
library(rpart)
regressor_dt = rpart(formula = Happiness ~ .,
                  data = regression_subset,
                  control = rpart.control(minsplit = 10))

# Predict and plot dt results from test_set
y_pred_dt = predict(regressor_dt, newdata = test_set)

pred_actual_dt <- as.data.frame(cbind(Prediction = y_pred_dt, Actual = test_set$Happiness))

MSE_dt <- sum((test_set$Happiness - y_pred_dt )^2)/nrow(test_set)
RMSE_dt <- sqrt(MSE_dt)

gg_plot_dt <- ggplot(pred_actual_dt, aes(Actual, Prediction )) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_dt,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_dt


# Plot decision tree 
prp(regressor_dt)

#### Random Forest Regression Model (randomForest)

# Fit to regression subset
library(randomForest)
set.seed(1234)
regressor_rf = randomForest(x = regression_subset[-1],
                         y = regression_subset$Happiness,
                         ntree = 250)

# Predict and plot rf results from test_set
y_pred_rf = predict(regressor_rf, newdata = test_set)

pred_actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Happiness))

MSE_rf <- sum((test_set$Happiness - y_pred_rf )^2)/nrow(test_set)
RMSE_rf <- sqrt(MSE_rf)

gg_plot_rf <- ggplot(pred_actual_rf, aes(Actual, Prediction )) +
  geom_point() +
  annotate("text", x = 4.5, y = 7,label = paste("RMSE = ", round(RMSE_rf,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
        axis.title = element_text(size = (10)))
gg_plot_rf

#### Neural Net Model with Zero (0) Hidden Layers (neuralnet)

# Fit nn with 0 hidden layers to the dataset
library(neuralnet)

nn0 <- neuralnet(Happiness ~ Prosperity + Network + Wellbeing + Freedom + Generosity + Corruption, data=train_set,hidden=0, linear.output=TRUE,)


# Plot the neural net (diagram)
plot(nn0, rep = "best")

# Predict and plot nn0
pred_nn0_list <- neuralnet::compute(nn0,test_set[,2:7])

pred_actual_nn0 <- as.data.frame(cbind(Prediction = pred_nn0_list$net.result, Actual = test_set$Happiness))

MSE_nn0 <- sum((test_set$Happiness - pred_nn0_list$net.result)^2)/nrow(test_set)
RMSE_nn0 <- sqrt(MSE_nn0)

gg_plot_nn0 <- ggplot(pred_actual_nn0, aes(Actual, V1 )) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_nn0,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Neural Net - No Hidden Layers", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
      axis.title = element_text(size = (10)))

gg_plot_nn0

#### Neural Net Model with Two (2) Hiddeen Layers (neuralnet) 

# Fit nn with 2 hidden layers to train_set
library(neuralnet)

nn <- neuralnet(Happiness ~ Prosperity + Network + Wellbeing + Freedom + Generosity + Corruption, data=train_set,hidden=2, linear.output=TRUE)

# Plot the neuralnet (2 hidden layers)
plot(nn, rep = "best")

# Predict and plot nn2 from test_set
pred_nn_list <- neuralnet::compute(nn,test_set[,2:7])

pred_actual_nn <- as.data.frame(cbind(Prediction = pred_nn_list$net.result, Actual = test_set$Happiness))

MSE_nn <- sum((test_set$Happiness - pred_nn_list$net.result)^2)/nrow(test_set)
RMSE_nn <- sqrt(MSE_nn)

gg_plot_nn <- ggplot(pred_actual_nn, aes(Actual, V1 )) +
  geom_point() + 
  annotate("text", x = 4.5, y = 7, label = paste("RMSE = ", round(RMSE_nn,4))) +
  geom_smooth (method = "lm", se = TRUE, color = "dodgerblue") +
  theme_bw() + geom_abline() +
  labs(title = "Neural Net - 2 Hidden Layers", subtitle = "Happiness Score (Predicted vs Actual)", x = "Actual",
       y = "Predicted") +
  theme(plot.title = element_text(face = "bold", size = (15)), 
      axis.title = element_text(size = (10)))

gg_plot_nn

# Collect / record RMSE and MSE - just in case...

MSE_glm <- sum((test_set$Happiness - y_pred_glm)^2)/nrow(test_set)
RMSE_glm <- sqrt(MSE_glm)

MSE_lm <- sum((test_set$Happiness - y_pred_lm)^2)/nrow(test_set)
RMSE_lm <- sqrt(MSE_lm)

MSE_nn0 <- sum((test_set$Happiness - pred_nn0_list$net.result)^2)/nrow(test_set)
RMSE_nn0 <- sqrt(MSE_nn0)
  
MSE_nn <- sum((test_set$Happiness - pred_nn_list$net.result)^2)/nrow(test_set)
RMSE_nn <- sqrt(MSE_nn)

MSE_svr <- sum((test_set$Happiness - y_pred_svr)^2)/nrow(test_set)
RMSE_svr <- sqrt(MSE_svr)

MSE_dt <- sum((test_set$Happiness - y_pred_dt )^2)/nrow(test_set)
RMSE_dt <- sqrt(MSE_dt)

MSE_rf <- sum((test_set$Happiness - y_pred_rf )^2)/nrow(test_set)
RMSE_rf <- sqrt(MSE_rf)

# Results

# Create rmse results table including project goal of target RMSE = 1.00000
rmse_target <- 1.000000
rmse_results <- data.frame(Method = "Project Target", RMSE = "1.00000", Gap = "-")
rmse_results %>%
  rbind(c("Simple Average", round(RMSE_simple_average,5), round(RMSE_simple_average-rmse_target,5))) %>%
  rbind(c("Simple Sum", round(RMSE_simple_sum, 5), round(RMSE_simple_sum-rmse_target, 5))) %>%
  rbind(c("Neural Net 0 Hidden Values ", round(RMSE_nn0, 5), round(RMSE_nn0-rmse_target, 5))) %>%
  rbind(c("Generalized Linear Model ", round(RMSE_glm, 5), round(RMSE_glm-rmse_target, 5))) %>%
  rbind(c("Multiple Linear Model", round(RMSE_lm, 5), round(RMSE_lm-rmse_target, 5))) %>%
  rbind(c("Neural Net 2 Hidden Values ", round(RMSE_nn, 5), round(RMSE_nn-rmse_target, 5))) %>%
  rbind(c("Decision Tree", round(RMSE_dt, 5), round(RMSE_dt-rmse_target, 5))) %>%
  rbind(c("Support Vector Regressison", round(RMSE_svr, 5), round(RMSE_svr-rmse_target, 5))) %>%
  rbind(c("Random Forest", round(RMSE_rf, 5), round(RMSE_rf-rmse_target, 5)))

