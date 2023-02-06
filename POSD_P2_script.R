# Implementation of the matched filter and the energy detector of a signal
# in a background of independent gaussian noise

# Install libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, flux)

# STEP 1: Generation of both hypothesis ----
set.seed(10)
dim = 10
sigma_2 = 1
a = 3
s = rep(1/sqrt(dim), dim)
n = 1000

H0 = replicate(n, rnorm(dim))
H1 = replicate(n, (a*s + rnorm(dim)))

# STEP 2: Compute stats corresponding to the vectors of the filters ----
lambda = -500:500
matched_filter_H0 = rep(NA, n)
matched_filter_H1 = rep(NA, n)
for (i in 1:n){
  matched_filter_H0[i] = sum(s * H0[,i])
  matched_filter_H1[i] = sum(s * H1[,i])
}

energy_detector_H0 = rep(NA, n)
energy_detector_H1 = rep(NA, n)
for (i in 1:n){
  energy_detector_H0[i] = sum(H0[,i] * H0[,i])
  energy_detector_H1[i] = sum(H1[,i] * H1[,i])
}

comp_matched_filter_H0 = matrix(data=NA, nrow=n, ncol=n)
for (j in 1:n){
  comp_matched_filter_H0[,j] = matched_filter_H0 > lambda[j]
}

comp_matched_filter_H1 = matrix(data=NA, nrow=n, ncol=n)
for (j in 1:n){
  comp_matched_filter_H1[,j] = matched_filter_H1 > lambda[j]
}

comp_energy_detector_H0 = matrix(data=NA, nrow=n, ncol=n)
for (j in 1:n){
  comp_energy_detector_H0[,j] = energy_detector_H0 > lambda[j]
}

comp_energy_detector_H1 = matrix(data=NA, nrow=n, ncol=n)
for (j in 1:n){
  comp_energy_detector_H1[,j] = energy_detector_H1 > lambda[j]
}

# STEP 3: for every lamda_j and every type of detector ----
PFA_matched_filter = rev(colSums(comp_matched_filter_H0) / n)
PD_matched_filter = rev(colSums(comp_matched_filter_H1) / n)

PFA_energy_detector = rev(colSums(comp_energy_detector_H0) / n)
PD_energy_detector = rev(colSums(comp_energy_detector_H1) / n)


# Plot matched filter
PFA_matched_filter_last_0 = sum(PFA_matched_filter == 0)
PFA_matched_filter_first_1 = match(1, PFA_matched_filter)

plot_PFA_matched_filter = PFA_matched_filter[PFA_matched_filter_last_0:PFA_matched_filter_first_1]
plot_PD_matched_filter = PD_matched_filter[PFA_matched_filter_last_0:PFA_matched_filter_first_1]

auc_matched_filter = flux::auc(plot_PFA_matched_filter, plot_PD_matched_filter) #0.97 

df_matched_filter = data.frame(plot_PFA_matched_filter, plot_PD_matched_filter)

df_matched_filter %>% ggplot(aes(x=plot_PFA_matched_filter, y=plot_PD_matched_filter)) +
  geom_line(linewidth = 1.1, color = "blue") + ggtitle("Matched filter. AUC=0.97 ") + 
  xlab('PFA') + ylab('PD') + theme_minimal() + 
  theme(
    plot.title = element_text(size = 20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size = 15, face='bold'),
    axis.title.y = element_text(size = 15, face='bold'),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size=13)
  )

# Plot energy detector
PFA_energy_detector_last_0 = sum(PFA_energy_detector == 0)
PFA_energy_detector_first_1 = match(1, PFA_energy_detector)

plot_PFA_energy_detector = PFA_energy_detector[PFA_energy_detector_last_0:PFA_energy_detector_first_1]
plot_PD_energy_detector = PD_energy_detector[PFA_energy_detector_last_0:PFA_energy_detector_first_1]

auc_energy_detector = flux::auc(plot_PFA_energy_detector, plot_PD_energy_detector) #0.97 

df_energy_detector = data.frame(plot_PFA_energy_detector, plot_PD_energy_detector)

df_energy_detector %>% ggplot(aes(x=plot_PFA_energy_detector, y=plot_PD_energy_detector)) +
  geom_line(linewidth = 1.1, color = "red") + ggtitle("Energy detector. AUC=0.84 ") + 
  xlab('PFA') + ylab('PD') + theme_minimal() + 
  theme(
    plot.title = element_text(size = 20, face="bold.italic", hjust = 0.5),
    axis.title.x = element_text(size = 15, face='bold'),
    axis.title.y = element_text(size = 15, face='bold'),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.text = element_text(size=13)
  )



