library(tidyverse)
library(opendatatoronto)
library(patchwork)


df <- search_packages("COVID-19 Cases in Toronto")
df <- df %>% list_package_resources()
df <- df %>% get_resource()


df <- df[!(df$Classification == "PROBABLE"),]

df <- select(df, "Client Gender")
colnames(df) <- "Gender"

df <- df[!(df$Gender == "UNKNOWN"),]
df <- df[!(df$Gender == "TRANSGENDER"),]
df <- df[!(df$Gender == "OTHER"),]
df <- df[!(df$Gender == "NON-BINARY"),]

df <- drop_na(df)


set.seed(644)
df_sample <- df %>% sample_n(200)

set.seed(644)
boot_p <- rep(NA, 10000)

for (i in 1:10000){
  boot_samp <- df_sample %>% sample_n(size = 200, replace = TRUE)
  boot_p[i] <- as.numeric(boot_samp %>% filter(Gender == "FEMALE") %>% summarise(n()))/200
}

boot_p <- tibble(boot_p)

ggplot(boot_p, aes(x = boot_p)) + 
  geom_histogram(binwidth = 0.02, fill = "gray", color = "black") + 
  labs(x = "Proportions from bootstrap samples",
       title = "Bootstrap Sampling Distribution of Proportion of \n COVID-19 cases where the Patient was Female")

quantile(boot_p$boot_p, c(0.05, 0.95))