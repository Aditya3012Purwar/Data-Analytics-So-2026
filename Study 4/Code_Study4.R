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


# Open data
dat <- read_excel("Q:/Lehre/2026 Sommersemester/Masterseminar/04_Paper/papers_to_select/Celiktutan, Klesse & Tuk (2024)/Replication/Study 4/Data_Study4_osf.xlsx")

# Delete test responses
dat <- dat %>% filter(Q3!="test")
dat <- dat %>% filter(Q3!="TEST")

# Delete previews and incomplete responses
dat <- dat %>% filter(Status==0)
dat <- dat %>% filter(Progress==100)

# Delete failed attention check 
dat <- dat %>% filter(Att_check==1)

# Gender: 2 is female
table(dat$gender)
prop.table(table(dat$gender))

# Age 
summary(dat$age)

# Participants per condition: 1 is self
table(dat$self)
prop.table(table(dat$self))

# Create inferred contribution
dat$cont <- -666
dat$cont <- ifelse(dat$self==1 ,dat$contribute_self_4,dat$cont)
dat$cont <- ifelse(dat$self==0 ,dat$cont_others_4,dat$cont)

# Main Analysis - Inferred contribution
t.test(cont ~ self, data=dat, var.equal=TRUE) 
cohensD(cont ~ self, data=dat)

# Descriptive statistics - Inferred contribution
aggregate(cont ~ self , dat, mean)
aggregate(cont ~ self , dat, sd)


# Create perceptions of usage behavior items
dat$inspire <- -666
dat$inspire <- ifelse(dat$self==1 ,dat$inspire_self,dat$inspire)
dat$inspire <- ifelse(dat$self==0 ,dat$inspire_others,dat$inspire)

dat$copy <- -666
dat$copy <- ifelse(dat$self==1 ,dat$copy_self,dat$copy)
dat$copy <- ifelse(dat$self==0 ,dat$copy_others,dat$copy)

dat$effort <- -666
dat$effort <- ifelse(dat$self==1 ,dat$effort_self,dat$effort)
dat$effort <- ifelse(dat$self==0 ,dat$effort_others,dat$effort)

# Reliability analysis
alpha(dat[, c("inspire", "copy", "effort")])

# Calculating perceptions of usage behavior
dat$usagebeh <- (dat$inspire + dat$copy + dat$effort) / 3

# Main Analysis - Perceptions of usage behavior
t.test(usagebeh ~ self, data=dat, var.equal=TRUE) 
cohensD(usagebeh ~ self, data=dat)

# Descriptive statistics - Perceptions of usage behavior
aggregate(usagebeh~ self, data=dat, mean)
aggregate(usagebeh ~ self, data=dat, sd)

# Mediation Analysis
dat$self <- as.numeric(dat$self)
med <- process (data = dat, y = "cont", x = "self", m ="usagebeh", model = 4, stand =1)


# Exploratory - Experience with ChatGPT (Yes=1)
table(dat$use_chatgpt)
prop.table(table(dat$use_chatgpt))

# Test of difference in experience with ChatGPT between conditions
contingency_table <- table(dat$use_chatgpt, dat$self)
chisq_test <- chisq.test(contingency_table)
print(chisq_test)


# Prepare variables
dat$self <- factor(dat$self)
dat$use_chatgpt <- factor(dat$use_chatgpt)

# Main analysis 
aov <- aov(cont ~ self * use_chatgpt, data = dat)
summary(aov)

# Calculate partial eta-squared
eta_squared <- etaSquared(aov, type = 2)
print(eta_squared)

# Descriptive statistics
aggregate(cont ~ use_chatgpt , dat, mean)
aggregate(cont ~ use_chatgpt , dat, sd)


# Exploratory - Psychological ownership
# Create psychological ownership
dat$psyown <- -666
dat$psyown <- ifelse(dat$self==1 ,dat$psyown_self,dat$psyown)
dat$psyown <- ifelse(dat$self==0 ,dat$psyown_others,dat$psyown)

# Main analysis 
t.test(psyown ~ self, data=dat, var.equal=TRUE) 
cohensD(psyown ~ self, data=dat)

# Descriptive statistics - Psychological ownership
aggregate(psyown ~ self , dat, mean)
aggregate(psyown ~ self , dat, sd)
