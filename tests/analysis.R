library(tidyverse)
library(ggplot2)

data <- read_csv("source.csv")

data$AgeGroup <- as.factor(data$AgeGroup)
data$Year <- as.factor(data$Year)
data$Strain <- as.factor(data$Strain)

head(data)

# one-way anova for effect of age group on vaccine effectiveness
anova_result <- aov(Effectiveness ~ AgeGroup, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)
p_value <- anova_summary[[1]][["Pr(>F)"]][1] #0.0225

# perform tukey's HSD if p-value is significant
if (p_value <= 0.05) {
  # If significant, perform Tukey's HSD post-hoc test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}
else {
  print("not significant")
}

#two-way anova for effect of age group and strain on vaccine effectiveness
anova_result <- aov(Effectiveness ~ AgeGroup * Strain, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)
p_values <- anova_summary[[1]][["Pr(>F)"]] #0.0086

if (any(p_values <= 0.05)) {
  # If significant, perform Tukey's HSD post-hoc test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

anova_result <- aov(Effectiveness ~ Year * AgeGroup, data = data)
anova_summary <- summary(anova_result)
print(anova_summary)
p_values <- anova_summary[[1]][["Pr(>F)"]] #<0.001

if (any(p_values <= 0.05)) {
  # If significant, perform Tukey's HSD post-hoc test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)
}

#print box-plots
plot_age <- ggplot(data, aes(x = AgeGroup, y = Effectiveness) +
  geom_boxplot() +
  labs(title = "Vaccine effectiveness as a function of age group.",
       x = "Age Group",
       y = "Vaccine Effectiveness") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ))

plot_strain <- ggplot(data, aes(x = Strain, y = Effectiveness) +
  geom_boxplot() +
  labs(title = "Vaccine effectiveness as a function of influenza strain.",
       x = "Influenza Type",
       y = "Vaccine Effectiveness)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ))

plot_strain <- ggplot(data, aes(x = Year, y = Effectiveness) +
  geom_boxplot() +
  labs(title = "Vaccine effectiveness as a function of year/season.",
       x = "Year",
       y = "Vaccine Effectiveness") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  ))
