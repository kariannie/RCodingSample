# Setup -----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggdist)
library(round)
library(readr)
library(RColorBrewer)
library(viridis)
library(paletteer)
library(reshape2)
library(plotly)
library(ggcorrplot)

setwd("D:/MyProfile/Downloads/EXP/fall 2023 projects/1 gsq ess promis correlation")

GSQ <- read.csv("PSS_GSQ_ESS_PROMIS_9.15.23.csv")

## Collapse by record_id
GSQ <- GSQ[ , -which(names(GSQ) %in% c("redcap_event_name"))]
GSQdf <- GSQ[, colSums(is.na(GSQ)) != nrow(GSQ)]
GSQdf <- GSQdf %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
GSQdf <- GSQdf %>% 
  group_by(record_id) %>% 
  fill(colnames(GSQdf), .direction = "downup") %>% 
  distinct(.keep_all = FALSE)


# remove record_id & part_sex & date and birthday columns 
dfRemoved <- subset(GSQdf, select = -c(record_id, gsq_date, promis_date, ess_today, part_dob, record_id))


## Convert Discrete to Continuous
dfRemoved <- mutate_all(dfRemoved, function(x) as.numeric(as.character(x)))

# Regressing out Sex, Age, and Age Squared from data ----- 

# add age squared column 
dfRemoved <- dfRemoved %>% 
  mutate(age_squared = (gsq_age)^2) 
dfRemoved <- dfRemoved %>%
  relocate(age_squared, .before=gsq_visual_hyper)
dfRemoved <- dfRemoved %>% 
  relocate(gsq_age, .before = age_squared)

dfScaled <- scale(dfRemoved)
dfScaled <- as.data.frame(dfScaled)

# create two other dataframes (learned this code from dylan to regress out one variable from ALL columns of a dataframe)
combined_res_notsex <- dfScaled
combined_sig <- dfScaled

# regressed out age, age squared, and sex from columns 
for(i in c(4:23)){
  tmp <- combined_sig
  names(tmp)[i] <- 'Variable'
  tmp_reg <- lm(Variable ~ gsq_age + age_squared + part_sex, tmp)
  tmp_df <- data.frame(Row = names(tmp_reg$residuals), 
                       Value = tmp_reg$residuals)
  combined_res_notsex[,i] <- NA 
  
  for(iter in c(1:nrow(tmp_df))){
    combined_res_notsex[as.numeric(tmp_df$Row[iter]), i] <- tmp_df$Value[iter] 
  }
}

# Scaled Data Setup -> returned to original scale  ----- 
dfScaled <- combined_res_notsex

# function to get statistics 
mean_sd <- function(var){
  group_by(dfRemoved) %>%
    summarise(
      mean = mean({{var}}, na.rm = TRUE),
      sd = sd({{var}}, na.rm = TRUE))
}

# converting to original scale for graphs 
# Sadly during the scope of this project I didn't find a way to write a loop to save these manual calculations 

mean_sd(gsq_visual_hyper)
dfScaled$gsq_visual_hyper <- dfScaled$gsq_visual_hyper * 2.86 + 6.09 
mean_sd(gsq_visual_hypo)
dfScaled$gsq_visual_hypo <- dfScaled$gsq_visual_hypo * 2.33 + 4.52 
mean_sd(gsq_aud_hyper)
dfScaled$gsq_aud_hyper <- dfScaled$gsq_aud_hyper * 2.63 + 9.00 
mean_sd(gsq_aud_hypo)
dfScaled$gsq_aud_hypo <- dfScaled$gsq_aud_hypo * 2.51 + 7.16
mean_sd(gsq_gus_hyper)
dfScaled$gsq_gus_hyper <- dfScaled$gsq_gus_hyper * 2.82 + 5.19 
mean_sd(gsq_gus_hypo)
dfScaled$gsq_gus_hypo <- dfScaled$gsq_gus_hypo * 2.09 + 4.76 
mean_sd(gsq_olf_hyper)
dfScaled$gsq_olf_hyper <- dfScaled$gsq_olf_hyper * 3.11 + 5.73  
mean_sd(gsq_olf_hypo)
dfScaled$gsq_olf_hypo <- dfScaled$gsq_olf_hypo * 2.29 + 3.29 
mean_sd(gsq_tactile_hyper)
dfScaled$gsq_tactile_hyper <- dfScaled$gsq_tactile_hyper * 3.01 + 5.54 
mean_sd(gsq_tactile_hypo)
dfScaled$gsq_tactile_hypo <- dfScaled$gsq_tactile_hypo * 2.64 + 4.76 
mean_sd(gsq_ves_hyper)
dfScaled$gsq_ves_hyper <- dfScaled$gsq_ves_hyper * 3.15 + 5.6 
mean_sd(gsq_ves_hypo) 
dfScaled$gsq_ves_hypo <- dfScaled$gsq_ves_hypo * 2.75 + 3.77 
mean_sd(gsq_pro_hyper) 
dfScaled$gsq_pro_hyper <- dfScaled$gsq_pro_hyper * 2.7 + 3.66 
mean_sd(gsq_pro_hypo)
dfScaled$gsq_pro_hypo <- dfScaled$gsq_pro_hypo * 2.54 + 5.22 
mean_sd(gsq_hyper_total)
dfScaled$gsq_hyper_total <- dfScaled$gsq_hyper_total * 15.6 + 40.8 
mean_sd(gsq_hypo_total) 
dfScaled$gsq_hypo_total <- dfScaled$gsq_hypo_total * 12.6 + 33.5 
mean_sd(gsq_total)
dfScaled$gsq_total <- dfScaled$gsq_total * 26.7 + 74.2 
mean_sd(promis_disturb)
dfScaled$promis_disturb <- dfScaled$promis_disturb * 23.5 + 79.2 
mean_sd(promis_impair)
dfScaled$promis_impair <- dfScaled$promis_impair * 14.2 + 49.4 
mean_sd(ess_score)
dfScaled$ess_score <- dfScaled$ess_score * 4.88 + 8.1 



##### Regressed Plots (NOT MATRIX) -----
# GSQ Total Score (SLIDE 6) -----
sum(!is.na(dfScaled$gsq_total))
median(dfScaled$gsq_total, na.rm = TRUE)
mean(dfScaled$gsq_total, na.rm = TRUE)

# Histogram with density line 
ggplot(dfScaled, aes(x=gsq_total)) + 
  geom_histogram(aes(y=..density..), color = "Black", fill= "steelblue3") + 
  geom_density(alpha=.2, fill="Grey") +
  labs(title = "GSQ Regressed Total Score N = 687, Mean = 74.2", x=" ") +
  theme(plot.title=element_text(hjust=0.5))

##How many greater than X
sum(dfScaled$gsq_total > 57, na.rm=TRUE) 
sum(dfScaled$gsq_total > 74, na.rm=TRUE) 
sum(dfScaled$gsq_total > 84, na.rm=TRUE) 


# Hyper Sensitivity Total (SLIDE 7) -----
sum(!is.na(dfScaled$gsq_hyper_total))
median(dfScaled$gsq_hyper_total, na.rm = TRUE)
mean(dfScaled$gsq_hyper_total, na.rm = TRUE)

ggplot(dfScaled, aes(x=gsq_hyper_total)) + 
  geom_histogram(aes(y=..density..), color = "Black", fill= "steelblue3") + 
  geom_density(alpha=.2, fill="Grey") +
  labs(title = "GSQ Hypersensitivity Total Regressed Score N = 687, Mean = 40.8", x="",) +
  theme(plot.title=element_text(hjust=0.5))

##How many greater than X
sum(dfScaled$gsq_hyper_total > 42, na.rm=TRUE)

# Hypo Sensitivity Total (SLIDE 7) -----
sum(!is.na(dfScaled$gsq_hypo_total))
median(dfScaled$gsq_hypo_total, na.rm = TRUE)
mean(dfScaled$gsq_hypo_total, na.rm = TRUE)

ggplot(dfScaled, aes(x=gsq_hypo_total)) + 
  geom_histogram(aes(y=..density..), color = "Black", fill= "steelblue3") + 
  geom_density(alpha=.2, fill="Grey") +
  labs(title = "GSQ Hyposensitivity Total Regressed Score N = 687, Mean = 33.5", x="",) +
  theme(plot.title=element_text(hjust=0.5))


# Sensory Modalities (SLIDE 9) -----
GSQ_mode <- gather(dfScaled, key="sensory_modality", value = "modality_score", 4:17)

ggplot(GSQ_mode, aes(x=GSQ_mode$sensory_modality, y=GSQ_mode$modality_score, fill=GSQ_mode$sensory_modality)) + 
  geom_violin() + 
  geom_boxplot(width=0.1, color="black") +
  stat_summary(fun.y=mean, geom="point", shape=23, size=1, color="White") +
  theme(legend.position="none", axis.text.x = element_text(angle = 50, vjust = 1, hjust=1)) +
  labs(title = "GSQ Sensory Modality Regressed Scores in SPARK N = 687", x="",y="Sensory Modality Score") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_fill_manual(values=c("red3", "tomato", "dodgerblue", "lightblue", "#FF9900", "#FFCC66",
                             "#339966", "palegreen",  "mediumpurple", "thistle3", "yellow2", "lemonchiffon",
                             "hotpink3", "#FF99CC"))
