# Load the readr and dplyr packages
library(tidyverse)

# Read datasets kidney_stone_data.csv into dataframe
data <- read_csv('kidney_stones_data.csv')

# Take a look at the first few rows of the dataset
head(data)

# Calculate the number and frequency of success and failure of each treatment 
data %>% 
  group_by(treatment,success)%>%
  summarise(N = n()) %>% 
  mutate(Freq = round(N/sum(N),3))


# Calculate number and frequency of success and failure by stone size for each treatment
sum_data <- 
  data %>% 
  group_by(treatment,stone_size,success) %>%
  summarise(N = n())%>%
  mutate(Freq = round(N/sum(N), 3))

# Print out the data frame we just created
sum_data


# Create a bar plot to show stone size count within each treatment
sum_data %>%
  ggplot(
    aes(treatment, N)
  ) + 
  geom_bar(aes(fill = stone_size),stat = 'identity') 

# Load the broom package 
library(broom)

# Run a Chi-squared test
trt_ss <- chisq.test(data$treatment,data$stone_size)

# Print out the result in tidy format 
tidy(trt_ss)



# Run a multiple logistic regression
m <- glm(data = data, success ~  stone_size + treatment, family = 'binomial')

# Print out model coefficient table in tidy format
tidy(m)


# Save the tidy model output into an object
tidy_m <- tidy(m)

# Plot the coefficient estimates with 95% CI for each term in the model
tidy_m %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 1.96 * std.error, 
                      ymax = estimate + 1.96 * std.error)) +
  geom_hline(yintercept = 0)

tidy_m



# Is small stone more likely to be a success after controlling for treatment option effect?
# Options: Yes, No (as string)
small_high_success <- 'Yes'

# Is treatment A significantly better than B?
# Options: Yes, No (as string)
A_B_sig <- 'No'
