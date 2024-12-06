#Midterm 

# Biological Question: How the hornL is affected
rm(list = ls())
dat = read.table("../ao9018/Desktop/BIOS14/Data/exam2022_part2-1.txt", header=T)
names(dat)

# Basic data checks
str(dat)
summary(dat)

dat <- na.omit(dat) # Remove rows with NA values

library(psych)
pairs.panels(dat) #Explore data using scatter plots, histograms, correlations

#convert categorical vaariables into factors 
dat$sex <- as.factor(dat$sex)
dat$density <- as.factor(dat$density)

#Visualizing the data as they are
library(ggplot2)
ggplot(dat, aes(x = age, y = hornL, color = sex)) + geom_point() + geom_smooth(method = "lm")

cor_matrix <- cor(dat[, c("hornL", "age", "mass", "hornR")])
cor_hornL <- cor_matrix["hornL", ]
print(cor_hornL)

#LM for unfiltered data
model_lm <- lm(hornL ~ age + sex + mass, data = dat)
summary(model_lm) 

#Checking if data are linear
par(mfrow = c(2, 2))
plot(model_lm)

#filtering data from 0-5 years which follow linearity
filtered_data <- dat %>% 
  filter(age >= 0 & age <= 5)

#Attempts to build a good and simple model
lm_model_1 <- lm(hornL ~ age + sex + mass + density, data = filtered_data)
summary(lm_model_1)
anova(lm_model_1)

library(lme4)
lm_model_2 <- lmer(hornL ~ age + sex + mass + density + (1 | cohort), data = filtered_data)
summary(lm_model_2)
anova(lm_model_2)

lm_model_3 <- lmer(hornL ~ age + sex + mass + density + (1 | season), data = filtered_data)
summary(lm_model_3)
anova(lm_model_3)

lm_model_4 <- lmer(hornL ~ age + sex + mass + (1 | season), data = filtered_data)
summary(lm_model_4)
anova(lm_model_4)

lm_model_5 <- lmer(hornL ~ age + sex + mass + (1 | cohort), data = filtered_data)
summary(lm_model_5)
anova(lm_model_5)

AIC(lm_model_1, lm_model_2, lm_model_3, lm_model_4, lm_model_5)

anova(lm_model_5, lm_model_2)

library(ggplot2)
# Boxplot: Horn length (hornL) by density and sex
ggplot(filtered_data, aes(x = density, y = hornL, fill = sex)) +
  geom_boxplot() +
  labs(title = "Boxplot of Horn Length by Density and Sex",
       x = "Population Density",
       y = "Horn Length (mm)",
       fill = "Sex") +
  theme_minimal()

#Plot 1: horn length with age by sex with error bars 
library(ggplot2)
ggplot(filtered_data, aes(x = age, y = hornL, color = sex, group = sex)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Mean points
  stat_summary(fun = mean, geom = "line", size = 1) +   # Line connecting means
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2) +        # Error bars for SD
  labs(
    title = "Horn Length Growth with Age by Sex",
    x = "Age (years)",
    y = "Horn Length (mm)",
    color = "Sex"
  ) +
  theme_minimal()

ggplot(filtered_data, aes(x = mass, y = hornL)) +
  geom_point(alpha = 0.6) +  # Scatter points with transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add a linear regression line with confidence interval
  labs(
    title = "Relationship Between Horn Length and Body Mass",
    x = "Body mass (kg)",
    y = "Horn Length (mm)"
  ) +
  theme_minimal()

# Plot: Horn Length vs Age
ggplot(filtered_data, aes(x = age, y = hornL)) +
  geom_point(alpha = 0.6) +  # Scatter points with transparency
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line
  labs(
    title = "Horn Length vs. Age",
    x = "Age (years)",
    y = "Horn Length (mm)"
  ) +
  theme_minimal()







