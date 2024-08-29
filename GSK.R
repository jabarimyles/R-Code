library("readxl")
library("WebPower")
library(dplyr)
library(DataExplorer)




glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}


OCS_data <- read_excel('/Users/jabarimyles/Documents/GSK/OCSCaseStudySampleData_Update.xlsx')

HCP <- OCS_data[c("HCP_ID", "HCP_Specialty", "HCP_state", "DASHBOARD_PRIORITY", "test_control")]

Pat_HCP <- OCS_data[c("total_pats_pre", "total_pats_post", "total_months_pre", "total_months_post", "pat_activity_pre", "pat_activity_post" )]

Pat_HCP_OCS <- OCS_data[c("OCS_Pats_Pre", "ocs_pats_post", "OCS_Months_Pre", "OCS_Months_Post")]

Useful_OCS <- OCS_data[c("Raw_score_OCS_pre", "raw_score_ocs_post", "rank_osc_pre", "rank_osc_post", "OCS_TOP50", "OCS_RANK")]




# Remove full duplicates rows
OCS_data <- OCS_data %>% distinct()

# Remove partial (on HCP_ID) duplicate rows
OCS_dupes <- OCS_data[duplicated(OCS_data[ , c("HCP_ID")]),]
dupe_ids <- c(993405, 3310845)
OCS_dupes_rows <- OCS_data %>% filter((HCP_ID %in% dupe_ids))
OCS <- OCS_data %>% filter(!(HCP_ID %in% dupe_ids))

# Remove incomplete cases
OCS <- OCS[complete.cases(OCS), ]

# Plot data intro
plot_intro(OCS)

# Plot missing data 
plot_missing(OCS)

# Plot distribution density estimations
plot_density(OCS)

# Plot Correlations
plot_correlation(OCS)

# Check logical nature of data
sum(OCS$OCS_Pats_Pre <= OCS$total_pats_pre)
sum(OCS$ocs_pats_post <= OCS$total_pats_post)


# Calculate cutoff for raw score greater than 50th percentile
pre_cutoffs <- quantile(as.numeric(OCS$Raw_score_OCS_pre), probs = c(0.50))
post_cutoffs <- quantile(as.numeric(OCS$raw_score_ocs_post), probs = c(0.50))

# Create excessive OCS pre and post
OCS <- OCS %>%
  mutate(excessive_OCS_pre = case_when(OCS_TOP50 == 1 & Raw_score_OCS_pre > pre_cutoffs[1] ~ 1,
                             TRUE ~ 0)) %>%
  mutate(excessive_OCS_post = case_when(OCS_TOP50 == 1 & raw_score_ocs_post > pre_cutoffs[1] ~ 1,
                                       TRUE ~ 0))

#Percent of patients on OCS
#This assumes that they followed similar patterns of visiting as non OCS patients
OCS$OCS_pats_perc_pre <- (OCS$OCS_Pats_Pre/OCS$total_pats_pre) #/  
OCS$OCS_pats_perc_post <- (OCS$ocs_pats_post/OCS$total_pats_post) #/    

# Create vars for visits per OCS patient
OCS$OCS_visits_per_pat_pre <-  ((OCS$OCS_Pats_Pre/OCS$total_pats_pre)*OCS$pat_activity_pre)
OCS$OCS_visits_per_pat_post <-((OCS$ocs_pats_post/OCS$total_pats_post)*OCS$pat_activity_post)

# Calculate number of COPD patients not on OCS
OCS$Pats_Non_OCS_pre <- OCS$total_pats_pre - OCS$OCS_Pats_Pre 
OCS$Pats_Non_OCS_post <- OCS$total_pats_post - OCS$ocs_pats_post 

# Split data into test and control sets
test_data <- OCS %>% filter(test_control == 'test') 
control_data <- OCS %>% filter(test_control == 'control') 

# Creates vars for contingency table
total_ocs_pre <- sum(OCS$OCS_Pats_Pre)
total_non_pre <- sum(OCS$Pats_Non_OCS_pre)
total_ocs_post <- sum(OCS$ocs_pats_post)
total_non_post <- sum(OCS$Pats_Non_OCS_post)

# Create a contingency table
contingency_table <- matrix(c(total_ocs_pre, total_non_pre,
                              total_ocs_post, total_non_post), ncol = 2)
print(contingency_table)

table(OCS$DASHBOARD_PRIORITY, OCS$test_control)



# Perform chi-square test for independence
chi_square_result <- chisq.test(contingency_table)

# Print the test result
# The null hypothesis is that the proportion of test before the treatment is the same as after
# Columns are pre/post, Rows are on OCS/Non-OCS
print(chi_square_result)

# Box plots of OCS patient percentage by dashboard priority, pre and post
boxplot(OCS_pats_perc_pre ~ DASHBOARD_PRIORITY, data = OCS)
boxplot(OCS_pats_perc_post ~ DASHBOARD_PRIORITY, data = OCS)

# Box plots by State
boxplot(OCS_pats_perc_post ~ HCP_state, data = OCS)


#cols = c("total_pats_pre" ,"total_pats_post", "total_months_pre" ,"total_months_post", "pat_activity_pre", "pat_activity_post", "OCS_Pats_Pre", "ocs_pats_post", "OCS_Months_Pre","OCS_Months_Post","Raw_score_OCS_pre","raw_score_ocs_post","rank_osc_pre", "rank_osc_post", "OCS_pats_perc_pre","OCS_pats_perc_post","OCS_visits_per_pat_pre","OCS_visits_per_pat_post", "Pats_Non_OCS_pre","Pats_Non_OCS_post"  )

# Create differenced variables
OCS$total_pats_diff <- OCS$total_pats_pre - OCS$total_months_post
OCS$total_months_diff <- OCS$total_months_pre - OCS$total_months_post
OCS$pat_activity_diff <- OCS$pat_activity_pre - OCS$pat_activity_post
OCS$OCS_Pats_diff <- OCS$OCS_Pats_Pre - OCS$ocs_pats_post
OCS$OCS_Months_diff <- OCS$OCS_Months_Pre - OCS$OCS_Months_Post
OCS_data$Raw_score_diff <- as.numeric(OCS_data$Raw_score_OCS_pre) - as.numeric(OCS_data$raw_score_ocs_post)
OCS_data$rank_ocs_diff <- OCS_data$rank_osc_pre - OCS_data$rank_osc_post
OCS$OCS_pats_perc_diff <-  OCS$OCS_pats_perc_pre - OCS$OCS_pats_perc_post
OCS$OCS_visits_per_pat_diff <- OCS$OCS_visits_per_pat_pre - OCS$OCS_visits_per_pat_post
OCS$Pats_Non_OCS_diff <- OCS$Pats_Non_OCS_pre - OCS$Pats_Non_OCS_post
OCS$OCS_pats_pre <- OCS$OCS_Pats_Pre


OCS <- OCS %>% pivot_longer(cols=c(OCS_pats_perc_pre, OCS_pats_perc_post), names_to = "pre_post", values_to="OCS_pats_perc")

OCS_long <- OCS %>%
  mutate(Pre_Post = case_when(
    str_detect(pre_post,pattern = "pre") ~ "pre",
    str_detect(pre_post, pattern = "post") ~"post",
    TRUE ~ NA_character_
  ))

model <- glmer(OCS_pats_perc ~ test_control + Pre_Post+ (test_control*Pre_Post) + (1|HCP_ID), family="poisson", data=OCS_long)

model_wdash <-glmer(OCS_pats_perc ~ test_control + Pre_Post+ (test_control*Pre_Post) + (1|HCP_ID) + DASHBOARD_PRIORITY, family="poisson", data=OCS_long)


# Calculates percent of total patients seen post and pre (exactly)
OCS$percent_not_seen_pre <- (OCS$total_months_pre - OCS$pat_activity_pre)/OCS$total_months_pre
OCS$percent_seen_pre <- OCS$pat_activity_pre/OCS$total_months_pre
OCS$percent_not_seen_post <- (OCS$total_months_post - OCS$pat_activity_post)/OCS$total_months_post
OCS$percent_seen_post <- OCS$pat_activity_post/OCS$total_months_post

########################################################################################
# Estimates how many patients that are on OCS that were actually seen
OCS$OCS_Pats_Pre_seen <- OCS$OCS_Pats_Pre*OCS$percent_seen_pre
#OCS$total_pats_pre_seen <- OCS$total_pats_pre*OCS$percent_seen_pre
OCS$ocs_pats_post_seen <- OCS$ocs_pats_post*OCS$percent_seen_post
#OCS$total_pats_post_seen <- OCS$total_pats_post*OCS$percent_seen_post

# Calculates percent of patients seen each month that are on OCS 
OCS$OCS_Pat_Seen_Monthly_Pre <- OCS$OCS_Pats_Pre_seen/OCS$OCS_Months_Pre #/OCS$pat_activity_pre
OCS$OCS_Pat_Seen_Monthly_Post <- OCS$ocs_pats_post_seen/OCS$OCS_Months_Post #/OCS$pat_activity_post
####################################################################################


OCS$pat_activity_pre_monthly <- OCS$pat_activity_pre/OCS$OCS_Months_Pre
OCS$pat_activity_post_monthly <- OCS$pat_activity_post/OCS$OCS_Months_Post

OCS %>% 
  group_by(test_control) %>%
  summarise(no_rows = length(test_control))



OCS_cont <- OCS[c("total_pats_pre", "total_pats_post", "total_months_pre", "total_months_post")]

OCS_cont <- OCS[c("total_pats_pre", "HS_Grad")]




OCS %>%
table(OCS$HCP_state,OCS$test_control)

res <- wp.poisson(n = 2000, exp0 = 2.798, exp1 = 0.8938,
                  alpha = 0.05, power = NULL, family = "Bernoulli", parameter = 0.53)

model.1 <- glm(cases ~ city + age.range + offset(log(n)), family = poisson(link = "log"), data = nonmel)


