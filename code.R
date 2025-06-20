# Problem 1: Download the data and import the data set into R .Select the following 
install.packages("haven")
library(haven)
url<-"https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta"
write.csv(data, "lmb-data.csv", row.names = FALSE)

data<-read_dta(url)
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
ggplot(df_binned,aes(x=avg_vote_share,y=avg_score))+
  geom_point(color="steelblue")+
  geom_vline(xintercept = 0.5,linetype="dashed",color="red")+
  labs(
    title="RD Design:Binned Averages of vote share vs Liberal Score",
    x="Democrate Vote Share(Running Variable)",
    y="Liberalism Score(Outcome Variable)"
  )+
  theme_minimal()
# Problem 3: Run a global regression
# Create a binary indicator for whether Democrate vote share>50%
data<-data %>% 
  mutate(democrat_winner=ifelse(demvoteshare>0.5,0,1))
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
ggplot(data, aes(x = demvoteshare)) +
  geom_density(fill = "lightblue", alpha = 0.6) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Density of Democrat Vote Share with Cutoff",
    x = "Democrat Vote Share",
    y = "Density"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0.2, 0.8))

# Print the summary of the test
summary(mccrary_test)

 

