# Problem 1: Download the data and import the data set into R .Select the following 
install.packages("haven")
library(haven)
url<-"https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta"
data<-read_dta(url)
write.csv(data, "lmb-data.csv", row.names = FALSE)
View(data)
colnames(data)
head(data)
selected_data<-data[,c("score","democrat","demvoteshare")]
# optional
selected_data<-data %>%
  select(score,democrat,demvoteshare)
head(selected_data)
# Problem 2
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)
# create 100 bins based on demvoteshare
df_binned<-selected_data %>% 
  mutate(bin=ntile(demvoteshare,100)) %>% 
  group_by(bin) %>% 
  summarise(
    avg_score=mean(score,na.rm=TRUE),
    avg_vote_share=mean(demvoteshare,na.rm=TRUE)
  )
# Plot the binned averages 
p_binned <- ggplot(df_binned,aes(x=avg_vote_share,y=avg_score))+
  geom_point(color="steelblue")+
  geom_vline(xintercept = 0.5,linetype="dashed",color="red")+
  labs(
    title="RD Design:Binned Averages of vote share vs Liberal Score",
    x="Democrate Vote Share(Running Variable)",
    y="Liberalism Score(Outcome Variable)"
  )+
  theme_minimal()
ggsave("Rplot.pdf", plot = p_binned, width = 7, height = 5)
# Problem 3: Run a global regression
# Create a binary indicator for whether Democrate vote share>50%
data<-data %>% 
  mutate(democrat_winner=as.integer(demvoteshare > 0.5))
# Run the global regression
model<-lm(score~democrat_winner,data=data)
# View the result
summary(model)
# Problem 4: Center the running variable 
# center running variable
data<-data %>% 
  mutate(demvoteshare_centered=demvoteshare - 0.5)
# Run the RD regression with centered running variable
rd_regression_centered<-lm(score~democrat_winner+demvoteshare_centered,data=data)
# Summarize the results 
summary(rd_regression_centered)
# problem 5: Allow the effect of the running variable to vary on wither side 
# of the discontinuity by including an appropriate interaction
# Run the RD regression with interaction term 
rd_regression_interaction<-lm(score~democrat_winner+demvoteshare_centered+democrat_winner:demvoteshare_centered,data=data)
# summarize the results 
summary(rd_regression_interaction)
# problem 6: Estimate the RD regrassion with a quardratic of the running variable
# Run the RD regression with quadratic terms
rd_regression_quadratic<-lm(
  score~democrat_winner+demvoteshare_centered+ I(demvoteshare_centered^2)+
    democrat_winner:demvoteshare_centered+ democrat_winner:I(demvoteshare_centered^2),
  data=data)
# summarize the results 
summary(rd_regression_quadratic)
# problem 7: Limit the analysis to a smaller window.
# limit the data to the specified window 
windowed_data<-data %>% 
  filter(demvoteshare>0.45& demvoteshare<0.55)
# Estimate the RD regression with a quadratic 
rd_regression_quadratic_windowed<-lm(
  score~democrat_winner+demvoteshare_centered+ I(demvoteshare_centered^2)+
    democrat_winner:demvoteshare_centered+ democrat_winner:I(demvoteshare_centered^2),
  data=windowed_data)
# Summary
summary(rd_regression_quadratic_windowed)
# problem 8: Use a discontinuity sample of observations within 0.02 points at the cutoff.
# The "rdd" package is often used for local regression in RD.
# Install and load the rdd package 
 install.packages("rdd")
 library(rdd)

# Filter data for observations within 0.02 points of the cutoff (0.5)
discontinuity_sample <- data %>%
  filter(demvoteshare >= 0.48 & demvoteshare <= 0.52) # 0.5 - 0.02 = 0.48, 0.5 + 0.02 = 0.52

# Estimate the difference in the variable score using a local regression (simple linear model in this narrow band)
# This is essentially estimating the difference at the cutoff within this very narrow band
local_regression <- lm(score ~ democrat_winner, data = discontinuity_sample)

# Summarize the results
summary(local_regression)

# Alternatively, we can just look at the mean difference within this window
mean_score_democrat_win <- discontinuity_sample %>%
  filter(democrat_winner == 1) %>%
  pull(score) %>%
  mean(na.rm = TRUE)

mean_score_democrat_loss <- discontinuity_sample %>%
  filter(democrat_winner == 0) %>%
  pull(score) %>%
  mean(na.rm = TRUE)

difference_at_cutoff <- mean_score_democrat_win - mean_score_democrat_loss
print(paste("Difference in score at cutoff (within 0.02 window):", round(difference_at_cutoff, 3)))

# problem 9:Carry out a McCrary density test to check whether there was manipulation in the 
#running variable at the cutoff.
## The "rddensity" package is specifically designed for the MaCrary density test.
# Install and load the rddensity package
install.packages("rddensity")
library(rddensity)

# Perform the McCrary density test
mccrary_test <- rddensity(data$demvoteshare, c = 0.5)

# Plot the results
p_density <- ggplot(data, aes(x = demvoteshare)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Density of Democrat Vote Share with Cutoff",
    x = "Democrat Vote Share",
    y = "Density"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0.2, 0.8))
ggsave("Density.pdf", plot = p_density, width = 7, height = 5)

# Print the summary of the test
summary(mccrary_test)

 


#############################################
# Additional robust RD analyses
#############################################

# Helper function for conditional package installation
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Install and load packages for robust RD and local randomization
install_if_missing("rdrobust")
install_if_missing("rdlocrand")

# 1) Bias-corrected local-polynomial RD (rdrobust) and RD plot
rd_est <- rdrobust(y = data$score, x = data$demvoteshare, c = 0.5)
print(summary(rd_est))

# Save the RD binned scatter from rdplot deterministically
pdf("RDplot_rdrobust.pdf", width = 7, height = 5)
rdplot(y = data$score, x = data$demvoteshare, c = 0.5)
dev.off()

# 2) Bandwidth sensitivity (vary h)
bandwidths <- c(0.03, 0.05, 0.08, 0.10)
bw_results <- lapply(bandwidths, function(h) {
  est <- rdrobust(y = data$score, x = data$demvoteshare, c = 0.5, h = h)
  data.frame(
    h = h,
    coef = tryCatch(est$coef[1], error = function(e) NA_real_),
    se = tryCatch(est$se[1], error = function(e) NA_real_),
    p = tryCatch(est$pv[1], error = function(e) NA_real_)
  )
})
bw_df <- do.call(rbind, bw_results)
write.csv(bw_df, "rdrobust_bandwidth_sensitivity.csv", row.names = FALSE)
print(bw_df)

# 3) Donut RD (exclude observations very near the cutoff)
donut_h0 <- 0.01
data_donut <- data %>% 
  filter(abs(demvoteshare - 0.5) >= donut_h0)
rd_donut <- rdrobust(y = data_donut$score, x = data_donut$demvoteshare, c = 0.5)
print(summary(rd_donut))

# 4) Placebo cutoffs (should show no discontinuity away from 0.5)
placebos <- c(0.45, 0.55)
placebo_results <- lapply(placebos, function(cc) {
  est <- rdrobust(y = data$score, x = data$demvoteshare, c = cc)
  data.frame(
    cutoff = cc,
    coef = tryCatch(est$coef[1], error = function(e) NA_real_),
    p = tryCatch(est$pv[1], error = function(e) NA_real_)
  )
})
placebo_df <- do.call(rbind, placebo_results)
print(placebo_df)

# Save donut and placebo summary to CSV
donut_coef <- tryCatch(rd_donut$coef[1], error = function(e) NA_real_)
donut_p <- tryCatch(rd_donut$pv[1], error = function(e) NA_real_)
donut_placebo_df <- rbind(
  data.frame(type = "donut", cutoff = 0.5, note = paste0("exclusion ", donut_h0), coef = donut_coef, p = donut_p),
  cbind(data.frame(type = "placebo"), placebo_df)
)
write.csv(donut_placebo_df, "rdrobust_placebo_donut.csv", row.names = FALSE)

# 5) Local randomization test within a narrow window (±0.02)
lr_test <- rdlocrand::rdrandinf(Y = data$score, R = data$demvoteshare, cutoff = 0.5, wl = 0.48, wr = 0.52)
print(lr_test)
capture.output(print(lr_test), file = "local_randomization_results.txt")

# 6) Covariate-adjusted RD (if suitable pre-treatment covariates are available)
covariate_candidates <- c(
  "incumbent", "incumbency", "open_seat", "lag_score", "lag_demvoteshare",
  "previous_demvoteshare", "turnout", "log_turnout", "population",
  "log_population", "age", "experience"
)
present_covs <- intersect(covariate_candidates, names(data))
if (length(present_covs) > 0) {
  covs_mat <- as.matrix(data[ , present_covs, drop = FALSE])
  rd_covadj <- rdrobust(y = data$score, x = data$demvoteshare, c = 0.5, covs = covs_mat)
  print(summary(rd_covadj))
  capture.output(summary(rd_covadj), file = "rdrobust_covariate_adjusted.txt")
} else {
  message("No predefined pre-treatment covariates found in data; skipping covariate-adjusted RD.")
}

# 7) Cluster-robust inference (if a plausible cluster ID is available)
# Try common clustering variables; prefer state/district if present
cluster_candidates <- c("state", "st", "stfips", "district", "cd", "year")
present_cluster <- intersect(cluster_candidates, names(data))
if (length(present_cluster) > 0) {
  cluster_var <- data[[present_cluster[1]]]
  rd_cluster <- rdrobust(y = data$score, x = data$demvoteshare, c = 0.5, cluster = cluster_var)
  print(summary(rd_cluster))
  capture.output(summary(rd_cluster), file = "rdrobust_clustered.txt")
} else {
  message("No cluster ID found among candidates; skipping clustered RD inference.")
}

# 8) Export all model summaries to a single CSV for quick review
model_summary <- data.frame(
  model = c("global", "centered", "interaction", "quadratic", "windowed_quadratic", 
            "local_narrow", "rdrobust", "donut_rd", "placebo_45", "placebo_55"),
  coef = c(
    coef(model)[2],  # global
    coef(rd_regression_centered)[2],  # centered
    coef(rd_regression_interaction)[2],  # interaction
    coef(rd_regression_quadratic)[2],  # quadratic
    coef(rd_regression_quadratic_windowed)[2],  # windowed
    coef(local_regression)[2],  # local narrow
    rd_est$coef[1],  # rdrobust
    donut_coef,  # donut
    placebo_df$coef[1],  # placebo 0.45
    placebo_df$coef[2]   # placebo 0.55
  ),
  se = c(
    summary(model)$coef[2,2],  # global
    summary(rd_regression_centered)$coef[2,2],  # centered
    summary(rd_regression_interaction)$coef[2,2],  # interaction
    summary(rd_regression_quadratic)$coef[2,2],  # quadratic
    summary(rd_regression_quadratic_windowed)$coef[2,2],  # windowed
    summary(local_regression)$coef[2,2],  # local narrow
    rd_est$se[1],  # rdrobust
    rd_donut$se[1],  # donut
    NA,  # placebo SEs not computed in current loop
    NA
  ),
  p_value = c(
    summary(model)$coef[2,4],  # global
    summary(rd_regression_interaction)$coef[2,4],  # centered
    summary(rd_regression_interaction)$coef[2,4],  # interaction
    summary(rd_regression_quadratic)$coef[2,4],  # quadratic
    summary(rd_regression_quadratic_windowed)$coef[2,4],  # windowed
    summary(local_regression)$coef[2,4],  # local narrow
    rd_est$pv[1],  # rdrobust
    rd_donut$pv[1],  # donut
    placebo_df$p[1],  # placebo 0.45
    placebo_df$p[2]   # placebo 0.55
  ),
  n_obs = c(
    nrow(data),  # global
    nrow(data),  # centered
    nrow(data),  # interaction
    nrow(data),  # quadratic
    nrow(windowed_data),  # windowed
    nrow(discontinuity_sample),  # local narrow
    rd_est$N,  # rdrobust
    rd_donut$N,  # donut
    nrow(data),  # placebo 0.45
    nrow(data)   # placebo 0.55
  ),
  notes = c(
    "Global regression ignoring running variable",
    "RD with centered running variable",
    "RD with different slopes on each side",
    "RD with quadratic terms on each side",
    "RD with quadratic terms, windowed 0.45-0.55",
    "Local comparison within ±0.02 of cutoff",
    "Bias-corrected local polynomial RD",
    "Donut RD excluding |x-0.5|<0.01",
    "Placebo test at cutoff 0.45",
    "Placebo test at cutoff 0.55"
  )
)

write.csv(model_summary, "all_rd_models_summary.csv", row.names = FALSE)
print("Model summary exported to 'all_rd_models_summary.csv'")
print(model_summary)

message("Analysis complete! Check the following outputs:")
message("- Plots: Rplot.pdf, Density.pdf, RDplot_rdrobust.pdf")
message("- Model summaries: all_rd_models_summary.csv")
message("- Robust RD: rdrobust_bandwidth_sensitivity.csv, rdrobust_placebo_donut.csv")
message("- Additional tests: local_randomization_results.txt, rdrobust_covariate_adjusted.txt, rdrobust_clustered.txt")