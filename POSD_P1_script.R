# Implementation of the first and second order optimum linear predictor of AR signal.

# Install and load libraries ----
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, matlib)

# STEP 1: Generation of the AR signal ----
alpha <- 0.3
beta <- -1
gamma <- 1

N <- 100
s <- rep(NA, N)
s[1] <- 0.3
s[2] <- 0.3
set.seed(100)
e <- rnorm(N, 0, 1)

for (n in 3:N){
  s[n] <- alpha * s[n-1] + beta * s[n-2] + gamma * e[n]
}

train_test_split = N/2
train_set <- s[1:train_test_split]
test_set <- s[(train_test_split+1):N]

# STEP 2: Computation of the optimal predictors from the first half of s(n) ----
s_n_1 <- c(0, train_set)
s_n_1 <- s_n_1[-length(s_n_1)]
s_n_2 <- c(c(0,0), train_set)
s_n_2 <- s_n_2[-c(length(s_n_2)-1, length(s_n_2))]
Rss_0 <- mean(train_set^2)
Rss_1 <- mean(train_set*s_n_1)
Rss_2 <- mean(train_set*s_n_2)

h1_first <- Rss_1 / Rss_0

mat1 <- matrix(c(Rss_0, Rss_1, Rss_1, Rss_0), nrow = 2, byrow = TRUE)
mat2 <- matrix(c(Rss_1, Rss_2), nrow = 2)
h_second <- inv(mat1) %*% mat2

h1_second <- h_second[1]
h2_second <- h_second[2]

# STEP 3: Predict the second half and plot the original s(n), the predicted and the error ----
# first order prediction
s_first_pred <- rep(NA, length(test_set)-1)
e_first_pred <- rep(NA, length(test_set)-1)
for (i in 2:length(test_set)){
  s_first_pred[i-1] <- h1_first * test_set[i-1]
  e_first_pred[i-1] <- test_set[i] - s_first_pred[i-1]
}

# second order prediction
s_second_pred <- rep(NA, length(test_set)-2)
e_second_pred <- rep(NA, length(test_set)-2)
for (i in 3:length(test_set)){
  s_second_pred[i-2] <- h1_second * test_set[i-1] + h2_second * test_set[i-2]
  e_second_pred[i-2] <- test_set[i] - s_second_pred[i-2]
}

first_order_MSE <- mean(e_first_pred^2) # 20.97
second_order_MSE <- mean(e_second_pred^2) # 1.667


# Plot results ----
# Original signal
id = 1:N
original_signal = s
df_original_signal = data.frame(id, original_signal)

ggplot(data=df_original_signal, aes(x=id, y=original_signal)) +
  geom_line(size=1) + theme_minimal() + geom_vline(xintercept = N/2, linetype='dashed', color = 'red') +
  ggtitle("Original signal") + xlab('Number of observations') +
  ylab('Original Signal values') + 
  theme(
    plot.title = element_text(size = 20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size = 15, face='bold'),
    axis.title.y = element_text(size = 15, face='bold'),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )


# First order prediction
id = (N/2+2):N
valid_test_set = test_set[-1]
df_first_order = data.frame(id, valid_test_set, s_first_pred, e_first_pred)
colnames(df_first_order) <- c("Number of Observations", "Original", 
                              "Predicted", "Error")
df <- df_first_order %>% 
  gather(key="Legend", value="Values", -`Number of Observations`)

ggplot(data = df, aes(x = `Number of Observations`, y = Values)) +
  geom_line(aes(color = Legend)) + theme_minimal() + ggtitle('First order prediction') +
  theme(
    plot.title = element_text(size = 20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size = 15, face='bold'),
    axis.title.y = element_text(size = 15, face='bold'),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size=13)
  )

# Second order prediction  
id = (N/2+3):N
valid_test_set = test_set[-c(1,2)]
df_second_order = data.frame(id, valid_test_set, s_second_pred, e_second_pred)
colnames(df_second_order) <- c("Number of Observations", "Original", 
                              "Predicted", "Error")
df_2 <- df_second_order %>% 
  gather(key="Legend", value="Values", -`Number of Observations`)

ggplot(data = df_2, aes(x = `Number of Observations`, y = Values)) +
  geom_line(aes(color = Legend)) + theme_minimal() + ggtitle('Second order prediction') +
  theme(
    plot.title = element_text(size = 20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size = 15, face='bold'),
    axis.title.y = element_text(size = 15, face='bold'),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size=13)
  )

