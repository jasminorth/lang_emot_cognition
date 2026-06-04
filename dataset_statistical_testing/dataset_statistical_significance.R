# Load packages
library(dplyr)
library(car)
library(ggplot2)

# Read dataset
data <- read.csv("experiment_stimuli_dataframe (final).csv")

### preparation

# Filter for words only
words <- data %>% filter(Condition == "word")

# Convert Emotional_valence_cat to factor
words$Emotional_valence_cat <- factor(words$Emotional_valence_cat, levels = c("positive", "neutral", "negative"))

# List of numeric columns to clean
numeric_cols <- c("Length", "Lg10SUBTLEX_US", "Emotional_valence_cont")

# Convert columns to numeric, change "None" to NA
words <- words %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(as.character(.))))

# Remove rows with NA for each dependent variable before ANOVA
words_length <- words %>% filter(!is.na(Length))
words_freq   <- words %>% filter(!is.na(Lg10SUBTLEX_US))
words_val    <- words %>% filter(!is.na(Emotional_valence_cont))




### tests



# Frequency
freq_aov <- aov(Lg10SUBTLEX_US ~ Emotional_valence_cat, data = words_freq)
summary(freq_aov)
# leveneTest(Lg10SUBTLEX_US ~ Emotional_valence_cat, data = words_freq)
# TukeyHSD(freq_aov)


# Length
length_aov <- aov(Length ~ Emotional_valence_cat, data = words_length)
summary(length_aov) # this is the right one -> look at Pr
leveneTest(Length ~ Emotional_valence_cat, data = words_length)  # use to find out which is longer
TukeyHSD(length_aov)




# Plot



# calculate mean, SDs
length_summary <- words_length %>%
  group_by(Emotional_valence_cat) %>%
  summarise(
    mean_length = mean(Length),
    se_length   = sd(Length)/sqrt(n())
  )

# Plot
ggplot(length_summary, aes(x = Emotional_valence_cat, y = mean_length, fill = Emotional_valence_cat)) +
  geom_col(width = 0.6, color = "black") +           # means
  geom_errorbar(aes(ymin = mean_length - se_length, ymax = mean_length + se_length),
                width = 0.2) +                       # error bars
  labs(x = "Emotional Valence", y = "Mean Word Length") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("negative" = "#E41A1C",
                               "neutral"  = "#377EB8",
                               "positive" = "#4DAF4A")) +
  theme(legend.position = "none")

ggsave(
  "word_length.png",
  plot = last_plot(),
  width = 7,
  height = 5
)



getwd()
setwd("C:/Users/jasmi/OneDrive/Dokumente/Uni/Master/1 WS25_26/Sprachverarbeitung im Kontext individueller Unterschiede")

