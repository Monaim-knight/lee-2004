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
# Create a binary indicator for whether Democrate vote share>50%
data<-data %>% 
  mutate(democrat_winner=ifelse(demvoteshare>0.5,0,1))
# Run the global regression
model<-lm(score~democrat_winner,data=data)
# View the result
summary(model)
