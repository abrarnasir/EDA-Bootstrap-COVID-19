library(tidyverse)
library(opendatatoronto)


df <- search_packages("COVID-19 Cases in Toronto")
df <- df %>% list_package_resources()
df <- df %>% get_resource()


df <- df[!(df$Classification == "PROBABLE"),]

df <- df %>% mutate(group = case_when(
  `Age Group` == "19 and younger" ~ 10,
  `Age Group` == "20 to 29 Years" ~ 24.5,
  `Age Group` == "30 to 39 Years" ~ 34.5,
  `Age Group` == "40 to 49 Years" ~ 44.5,
  `Age Group` == "50 to 59 Years" ~ 54.5,
  `Age Group` == "60 to 69 Years" ~ 64.5,
  `Age Group` == "70 to 79 Years" ~ 74.5,
  `Age Group` == "80 to 89 Years" ~ 84.5,
  `Age Group` == "90 and older" ~ 99
))

df <- select(df, "Client Gender", "group")
df <- df %>% rename(
  Age = group,
  Gender = `Client Gender`
)

df <- df[!(df$Gender == "UNKNOWN"),]
df <- df[!(df$Gender == "TRANSGENDER"),]
df <- df[!(df$Gender == "OTHER"),]
df <- df[!(df$Gender == "NON-BINARY"),]

df <- drop_na(df)

head(df)


df_gender <- select(df, "Gender")

set.seed(644)
df_gender_sample <- df_gender %>% sample_n(200)

set.seed(644)
boot_p <- rep(NA, 10000)

for (i in 1:10000){
  boot_samp <- df_gender_sample %>% sample_n(size = 200, replace = TRUE)
  boot_p[i] <- as.numeric(boot_samp %>% filter(Gender == "FEMALE") %>% summarise(n()))/200
}

boot_p <- tibble(boot_p)

ggplot(boot_p, aes(x = boot_p)) + 
  geom_histogram(binwidth = 0.02, fill = "gray", color = "black") + 
  labs(x = "Proportions from Bootstrap Samples",
       y = "Count",
       title = "Bootstrap Sampling Distribution of Proportion of \n COVID-19 cases where the Patient was Female")

quantile(boot_p$boot_p, c(0.05, 0.95))


df_age <- select(df, "Age")

set.seed(127)
df_age_sample <- df_age %>% sample_n(size = 200)

set.seed(127)
sample_means <- rep(NA, 10000)

for (i in 1:10000){
  df_age_sample <- df_age %>% sample_n(size = 200, replace = FALSE)
  sample_means[i] <- as.numeric(df_age_sample %>% summarise(mean(Age)))
}

sample_means <- tibble(mean_age = sample_means)

ggplot(sample_means, aes(x = mean_age)) +
  geom_histogram(binwidth = 1, fill = "grey", color = "black") +
  labs(x = "Mean Ages from Samples of 200 Patients",
       y = "Count",
       title = "Sampling Distribution for the Mean of Ages of COVID-19 Patients")
