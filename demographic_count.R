library(tidyverse)
library(extrafont)

# font_import()  # This takes a few minutes and only needs to be done once
loadfonts(device = "pdf")  # This loads fonts for the pdf device


canna_df <- read.csv('/home/sam/Downloads/anonymized data - Sheet1.csv')
canna_df <- read.csv('/home/sam/Classes/Stats/Consulting/EEG_Consulting/MJCohortG3/AllData_MJCohortG3.csv')
save_dir <- '/home/sam/Classes/Stats/Consulting/EEG_Consulting/'



canna_df <- canna_df %>%
  drop_na() %>%
  filter(P != 1, Q != ".") %>%
  mutate(Group = case_when(X==0 ~"Non Users",
                           X==1 ~"Before Bed Only",
                           TRUE ~"Multiple Daily Users"),
         Group = factor(Group, levels = c("Non Users", "Before Bed Only", "Multiple Daily Users")),
         Gender = ifelse(Q=="5", "Male", "Female"),
         Gender = factor(Gender))

summ <- canna_df %>%
  group_by(Group, Gender) %>%
  summarise(N=n(),
            `Sleep Onset` = round(median(sleep_onset/60))
            
  )

summ <- canna_df %>%
  group_by(Group) %>%
  summarise(N=n(),
            `Sleep Onset` = round(median(sleep_onset/60)),
            `Hour 1 Wake Events` = round(median(Wake.Events.Hour.0)),
            `Hour 2 Wake Events` = round(median(Wake.Events.Hour.1)),
            `Hour 3 Wake Events` = round(median(Wake.Events.Hour.2)),
            `Hour 4 Wake Events` = round(median(Wake.Events.Hour.3)),
            
  )

sum(summ$`n()`)


fig1<-canna_df %>%
  ggplot(aes(x = Group, y = sleep_onset/60, color = Gender)) +
  geom_boxplot() +
  ggtitle('Sleep Onset') +
  ylab("Sleep onset (minutes after lights off)") +  # Set custom y-axis label
  theme_classic() +
  theme(axis.text.x = element_text(angle = 15, vjust = 1, hjust = 1),
        text = element_text(size = 12))  # Rotate x-axis labels
ggsave(paste0(save_dir,"Fig1.pdf"), plot = fig1, device = "pdf", width = 8, height = 6)
print(fig1)

canna_df <- canna_df %>%
  rename(
    Hour.0.WakeEvents = Wake.Events.Hour.0,
    Hour.1.WakeEvents = Wake.Events.Hour.1,
    Hour.2.WakeEvents = Wake.Events.Hour.2,
    Hour.3.WakeEvents = Wake.Events.Hour.3
  )

# Step 2: Use pivot_longer to gather the columns
canna_df_long <- canna_df %>%
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
    cols = -c(Stage, SleepTime,WakeEvents, ba_rid, T, Q, X, V, P, Lightsoffclocktime, J, Group, THCMinutesSleep, perTHCSleepMODG, Gender, sleep_onset, Band, Hour), # Exclude non-target columns
    names_to = "Location",
    values_to = "EEGpower",
    names_pattern = "([^.]+\\.[^.]+)$" # Regex to capture the format X.Y at the end of the string
  ) %>%
  mutate(
    ID = factor(ba_rid),
    Hour = as.numeric(sub("Hour.", "", Hour)),  # Remove 'Hour.' prefix and convert to numeric
    Hour = factor(Hour)
  ) %>%
  select(-c(P, Q, V, `T`, J, ba_rid, X)) %>%
  drop_na()

str(canna_df_long)

timePerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= SleepTime/60, color = Group)) +
  geom_boxplot() +
  facet_wrap(~Stage, nrow=5) +
  ggtitle("Time spent in a given sleep stage per hour based on Cannabis Use") +
  ylab("Sleep Time (minutes)") +  # Set custom y-axis label
  theme_classic() +
  theme(text = element_text(size = 12))

ggsave(paste0(save_dir,"Fig3.pdf"), plot = timePerSleepStage_plot, device = "pdf", width = 8, height = 11)
print(timePerSleepStage_plot)


wakeEvents_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= WakeEvents, color = Group)) +
  geom_boxplot() +
  ggtitle("Wake Eventes per hour based on Cannabis Use") +
  theme_classic() +
  theme(text = element_text(size = 12))


ggsave(paste0(save_dir,"Fig2.pdf"), plot = wakeEvents_plot, device = "pdf", width = 8, height = 6)
print(wakeEvents_plot)

powerPerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y= EEGpower, color = Group)) +
  geom_boxplot() +
  facet_wrap(~Stage*Location*Band, nrow=5) +
  ggtitle("EEG Power By Location per hour based on Cannabis Use") +
  theme_classic() +
  theme(text = element_text(size = 12))


powerPerSleepStage_plot

bands <- unique(canna_df_long$Band)

j=4
# Loop through each band and create a plot
for (band in bands) {
  # Filter the data for the current band
  band_data <- canna_df_long %>%
    filter(Band == band)
  
  # Create the plot
  powerPerSleepStage_plot <- band_data %>%
    ggplot(aes(x=Hour, y=EEGpower, color=Group)) +
    geom_boxplot() +
    facet_wrap(~Location, nrow=5) +
    ggtitle(paste("EEG Power By Location per hour based on Cannabis Use -", band)) +
    theme_classic()+
    theme(text = element_text(size = 12))
  
  # Print the plot
  ggsave(paste0(save_dir,"Fig",j,".pdf"), plot = powerPerSleepStage_plot, device = "pdf", width = 8, height = 8)
  print(powerPerSleepStage_plot)
  j = j+1
}


powerPerSleepStage_plot <- canna_df_long %>%
  ggplot(aes(x=Hour, y=EEGpower, color=Group)) +
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


write_csv(canna_df_long, file = '/home/sam/Classes/Stats/Consulting/EEG_Consulting/All_EEG_Data_Long.csv')
