library(tidyverse)

canna_df <- read.csv('/home/sam/Downloads/anonymized data - Sheet1.csv')
canna_df <- read.csv('/home/sam/Classes/Stats/Consulting/EEG_Consulting/MJCohortG3/AllData_MJCohortG3.csv')


summ <- canna_df %>%
  drop_na() %>%
  filter(P != 1, Q != ".") %>%
  select(X, Q) %>%
  group_by(X,Q) %>%
  summarise(n())

sum(summ$`n()`)


canna_df %>%
  drop_na() %>%
  filter(P != 1, Q != ".") %>%
  select(X, Q, sleep_onset) %>%
  mutate(X = factor(X)) %>%
  ggplot(aes(x = X, y = sleep_onset, color = Q)) +
  geom_boxplot() +
  ggtitle('Sleep Onset') +
  theme_classic()
  

canna_df <- canna_df %>%
  rename(
    Hour.0.WakeEvents = Wake.Events.Hour.0,
    Hour.1.WakeEvents = Wake.Events.Hour.1,
    Hour.2.WakeEvents = Wake.Events.Hour.2,
    Hour.3.WakeEvents = Wake.Events.Hour.3
  )

# Step 2: Use pivot_longer to gather the columns
canna_df_long <- canna_df %>%
  drop_na() %>%
  pivot_longer(
    cols = matches("Hour.[0-3]."),
    names_to = c("Hour", ".value"),
    names_pattern = "(Hour.[0-3])\\.(.*)"
  ) %>%
  pivot_longer(
    cols = matches("\\.EEG\\."),
    names_to = c("Band", ".value"),
    names_pattern = "([^.]*).EEG.(.*)"
  ) %>%
  pivot_longer(
    cols = c(W, R, N1, N2, N3),  # Specify the columns to gather
    names_to = "Stage",          # New column for the stage names
    values_to = "SleepTime"           # New column for the corresponding values
  )  %>%
  pivot_longer(
    cols = -c(Stage, SleepTime,WakeEvents, ba_rid, T, V, P, Lightsoffclocktime, J, X, THCMinutesSleep, perTHCSleepMODG, Q, sleep_onset, Band, Hour), # Exclude non-target columns
    names_to = "Location",
    values_to = "EEGpower",
    names_pattern = "([^.]+\\.[^.]+)$" # Regex to capture the format X.Y at the end of the string
  ) %>%
  mutate(
    ID = factor(ba_rid),
    Gender = ifelse(Q=="5", "Male", "Female"),
    Gender = factor(Gender),
    Cannabis = factor(X),
    Hour = as.numeric(sub("Hour.", "", Hour)),  # Remove 'Hour.' prefix and convert to numeric
    Hour = factor(Hour)
  ) %>%
  select(-c(P, Q, V, `T`, J, ba_rid, X)) %>%
  drop_na()

str(canna_df_long)

timePerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= SleepTime, color = Cannabis)) +
  geom_boxplot() +
  facet_wrap(~Stage, nrow=5) +
  ggtitle("Time spent in a given sleep stage per hour based on Cannabis Use") +
  theme_classic()

timePerSleepStage_plot

wakeEvents_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= WakeEvents, color = Cannabis)) +
  geom_boxplot() +
  ggtitle("Wake Eventes per hour based on Cannabis Use") +
  theme_classic()

wakeEvents_plot

powerPerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= EEGpower, color = Cannabis)) +
  geom_boxplot() +
  facet_wrap(~Stage*Location*Band, nrow=5) +
  ggtitle("EEG Power By Location per hour based on Cannabis Use") +
  theme_classic()

powerPerSleepStage_plot

bands <- unique(canna_df_long$Band)

# Loop through each band and create a plot
for (band in bands) {
  # Filter the data for the current band
  band_data <- canna_df_long %>%
    filter(Band == band)
  
  # Create the plot
  powerPerSleepStage_plot <- band_data %>%
    ggplot(aes(x=Hour, y=EEGpower, color=Cannabis)) +
    geom_boxplot() +
    facet_wrap(~Location, nrow=5) +
    ggtitle(paste("EEG Power By Location per hour based on Cannabis Use -", band)) +
    theme_classic()
  
  # Print the plot
  print(powerPerSleepStage_plot)
}


powerPerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y=EEGpower, color=Cannabis)) +
  geom_boxplot() +
  facet_wrap(~Location*Band, nrow=4) +
  ggtitle(paste("EEG Power By Location per hour based on Cannabis Use")) +
  theme_classic()

powerPerSleepStage_plot


# Shapiro-Wilk Test for Normality
library(broom)

# Perform the Shapiro-Wilk test on EEG power for each band and location
shapiro_results_eeg <- canna_df_long %>%
  group_by(Band, Location) %>%
  summarise(shapiro_test = list(shapiro.test(EEGpower)), .groups = 'drop') %>%
  mutate(tidied = map(shapiro_test, tidy)) %>%
  unnest(tidied) %>%
  select(Band, Location, statistic, p.value)

# Print the results
print(shapiro_results)


# Perform the Shapiro-Wilk test on EEG power for each band and location
shapiro_results_stage <- canna_df_long %>%
  mutate(Stage = factor(Stage)) %>%
  group_by(Stage, Hour) %>%
  summarise(shapiro_test = list(shapiro.test(SleepTime)), .groups = 'drop') %>%
  mutate(tidied = map(shapiro_test, tidy)) %>%
  unnest(tidied) %>%
  select(Stage, Hour, statistic, p.value)

# Print the results
print(shapiro_results)

library(stats)

# Assuming canna_df_long is your dataframe and EEGpower is continuous
glm_model <- glm(EEGpower ~ Cannabis + Gender + Band + Location + Hour, data = canna_df_long, family = gaussian(link = "identity"))
summary(glm_model)
