# Open packages and data
library(readxl)
library(dplyr)
library(emmeans)
library(ggplot2)
library(plotrix)
library(stringr)
library(gridExtra)
library(psych)
library(lsr)
library(tidyverse)
library(rstatix)
library(lme4)
library(broom.mixed)
library(sjPlot)
library(sjmisc)


# Open pretest data
dat1 <- read_excel("Q:/Lehre/2026 Sommersemester/Masterseminar/04_Paper/papers_to_select/Celiktutan, Klesse & Tuk (2024)/Replication/Study 3/Data_Study3_pretest_osf.xlsx")

# Delete incomplete responses
dat1 <- dat1 %>% filter(Progress==100)

# Gender: 2 is female
table(dat1$gender)
prop.table(table(dat1$gender))

# Age 
summary(dat1$age)

# Categorization of behaviors: 2 is outsourcing
# Behavior 1
table1 <- table(dat1$B1)
round(prop.table(table1) * 100, 2)

# Behavior 2
table2 <- table(dat1$B2)
round(prop.table(table2) * 100, 2)

# Behavior3
table3 <- table(dat1$B3)
round(prop.table(table3) * 100, 2)

# Behavior4
table4 <- table(dat1$B4)
round(prop.table(table4) * 100, 2)

# Behavior5
table5 <- table(dat1$B5)
round(prop.table(table5) * 100, 2)

# Behavior6
table6 <- table(dat1$B6)
round(prop.table(table6) * 100, 2)

# Behavior7
table7 <- table(dat1$B7)
round(prop.table(table7) * 100, 2)

# Behavior8
table8 <- table(dat1$B8)
round(prop.table(table8) * 100, 2)

# Behavior9
table9 <- table(dat1$B9)
round(prop.table(table9) * 100, 2)

# Behavior 10
table10 <- table(dat1$B10)
round(prop.table(table10) * 100, 2)


# Open main study data
dat <- read_excel("Q:/Lehre/2026 Sommersemester/Masterseminar/04_Paper/papers_to_select/Celiktutan, Klesse & Tuk (2024)/Replication/Study 3/Data_Study3_osf.xlsx")

# Delete previews and incomplete responses
dat <- dat %>% filter(Status==0)
dat <- dat %>% filter(Progress==100)

# Gender: 2 is female
table(dat$gender)
prop.table(table(dat$gender))

# Age 
summary(dat$age)

# Remove participants with missing values
dat <- dat %>%
  filter(self != 1 | (!is.na(B1_self) & !is.na(B2_self) & !is.na(B3_self) & !is.na(B4_self)
                      & !is.na(B5_self) & !is.na(B6_self) & !is.na(B7_self) & !is.na(B8_self) 
                      & !is.na(B9_self) & !is.na(B10_self)))

dat <- dat %>%
  filter(self != 0 | (!is.na(B1_other) & !is.na(B2_other) & !is.na(B3_other) & !is.na(B4_other)
                      & !is.na(B5_other) & !is.na(B6_other) & !is.na(B7_other) & !is.na(B8_other) 
                      & !is.na(B9_other) & !is.na(B10_other)))

# Number of participants per condition: 1 is self
table(dat$self)
prop.table(table(dat$self))

# Create usage likelihood
dat$B1 <- -666
dat$B1<- ifelse(dat$self==1 ,dat$B1_self,dat$B1)
dat$B1 <- ifelse(dat$self==0 ,dat$B1_other,dat$B1)

dat$B2 <- -666
dat$B2 <- ifelse(dat$self == 1, dat$B2_self, dat$B2)
dat$B2 <- ifelse(dat$self == 0, dat$B2_other, dat$B2)

dat$B3 <- -666
dat$B3 <- ifelse(dat$self == 1, dat$B3_self, dat$B3)
dat$B3 <- ifelse(dat$self == 0, dat$B3_other, dat$B3)

dat$B4 <- -666
dat$B4 <- ifelse(dat$self == 1, dat$B4_self, dat$B4)
dat$B4 <- ifelse(dat$self == 0, dat$B4_other, dat$B4)

dat$B5 <- -666
dat$B5 <- ifelse(dat$self == 1, dat$B5_self, dat$B5)
dat$B5 <- ifelse(dat$self == 0, dat$B5_other, dat$B5)

dat$B6 <- -666
dat$B6 <- ifelse(dat$self == 1, dat$B6_self, dat$B6)
dat$B6 <- ifelse(dat$self == 0, dat$B6_other, dat$B6)

dat$B7 <- -666
dat$B7 <- ifelse(dat$self == 1, dat$B7_self, dat$B7)
dat$B7 <- ifelse(dat$self == 0, dat$B7_other, dat$B7)

dat$B8 <- -666
dat$B8 <- ifelse(dat$self == 1, dat$B8_self, dat$B8)
dat$B8 <- ifelse(dat$self == 0, dat$B8_other, dat$B8)

dat$B9 <- -666
dat$B9 <- ifelse(dat$self == 1, dat$B9_self, dat$B9)
dat$B9 <- ifelse(dat$self == 0, dat$B9_other, dat$B9)

dat$B10 <- -666
dat$B10 <- ifelse(dat$self == 1, dat$B10_self, dat$B10)
dat$B10 <- ifelse(dat$self == 0, dat$B10_other, dat$B10)


# Prepare data for the main analysis
subset_dat <- dat %>%
  select("ResponseId", "self", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10")

dat_long <- subset_dat %>%
  gather(key = "Behavior", value = "Likelihood", -ResponseId, -self) %>%
  mutate(Behavior = as.numeric(gsub("[^0-9]", "", Behavior)))

# Create outsource
dat_long$outsource <--666
dat_long$outsource <- ifelse(dat_long$Behavior == 1, 8.43, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 2, 57.83, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 3, 20.48, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 4, 68.67, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 5, 33.73, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 6, 21.69, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 7, 81.93, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 8, 90.36, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 9, 90.36, dat_long$outsource)
dat_long$outsource <- ifelse(dat_long$Behavior == 10, 78.31, dat_long$outsource)

# Mean-center outsource
dat_long$outsource_centered <- dat_long$outsource - mean(dat_long$outsource)

# Descriptive statistics
summary(dat_long$outsource_centered)


# Main analysis
mixedreg <- lmer(Likelihood ~ self * outsource_centered + (1|ResponseId), data = dat_long)
summary(mixedreg)

# Perform planned contrasts
probe <- emmeans(mixedreg, ~ self * outsource_centered, at = list(outsource_centered = c(-40, -30, -20, -10, 0, 10, 20, 30)))
contrast(probe, "revpairwise", by = "outsource_centered")


################################################################################
# Plot
################################################################################

# Prepare variable
dat_long$self <- factor(dat_long$self, labels = c("other", "self"))

# Predictions with interaction model
reg <- lm(Likelihood ~ self * outsource_centered, data = dat_long)

# Prepare dataframe for plot   
plotdat <- expand.grid(
  self = levels(dat_long$self),
  outsource_centered = seq(min(dat_long$outsource_centered), max(dat_long$outsource_centered), length.out = 100)
)

plotdat$Likelihood <- predict(reg, newdata = plotdat)

# Include 95% CI
CI <- predict(reg, interval = "confidence", level = 0.95, newdata = plotdat)
plotdat <- cbind(plotdat, CI)

# Make factor IV
plotdat$self <- factor(plotdat$self, levels=c('self', 'other'))

# Plot
plot <- ggplot() +
  geom_line(data = plotdat, aes(x = outsource_centered, y = Likelihood, color = self)) +
  geom_ribbon(data = plotdat, aes(x = outsource_centered, ymin = lwr, ymax = upr, fill = self), alpha = 0.3, color = NA) +
  xlab("Classification of behavior as inspiration (0) to outsourcing (100)") +
  ylab("Usage Likelihood") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.title = element_blank(),
        axis.line = element_line(), 
        panel.background = element_rect(fill = 'white', colour = 'black'),
        text = element_text(size = 14),
        axis.text.x = element_text(colour = 'black'),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 14, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA, size = .2),
        legend.position = "bottom") + 
  coord_cartesian(ylim = c(2.5, 6)) +
  scale_y_continuous(breaks = c(3, 4, 5, 6)) +
  scale_color_manual(values = c("#F08080", "#6495ED"), labels = c("Self", "Other")) +
  scale_fill_manual(values = c("#F08080", "#6495ED"), labels = c("Self", "Other")) +
  guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL))  # This removes the legend for the color aesthetic and fill aesthetic

# Export Plot
svg("Study3_figure.svg",width=6, height=6, pointsize=12)

plot

dev.off()
