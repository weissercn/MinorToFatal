library(dplyr)
library(plyr)
library(caret)
library(VennDiagram)  # must run install.packages("VennDiagram")
library(ggplot2)

# Loading Data
source('data-helpers.R')
df.all <- data.get_df();                      # Cropped data
df.raw <- data.filter(data.get_raw_df());     # Raw data
df.raw.factored <- data.auto_factor(df.raw);  # Raw, but factorized

df.raw$INJ_SEV_FACTOR = factor(df.raw$INJ_SEV)
df.all$INJ_SEV_FACTOR = factor(df.all$INJ_SEV)


######### Add stats below here! #############
# Some examples below

# Total Accident by Injury Severity
sum(dplyr::summarise(group_by(df.raw, df.raw$INJ_SEV), n = n())$n)


# % Plot
ggplot(df.raw, aes(x=V_CONFIG, fill=INJ_SEV_FACTOR)) + geom_bar(position="fill", bins=20) + ylab('% of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + xlim(0,22)

# Quantity Plot
ggplot(df.raw, aes(x=GVWR, fill=INJ_SEV_FACTOR)) + geom_bar(stat="bin", bins=24) + ylab('# of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red"))


# !!Plot Injury Severity as percentage of speed
ggplot(df.raw, aes(x=TRAV_SP, fill=INJ_SEV_FACTOR)) + geom_bar(position="fill",stat="bin", bins=20) + xlim(0,60) +xlab('Vehicle Speed (mph)') + ylab('% of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + ggtitle('Injury Proportion vs. Vehicle Speed') + 
  theme(plot.title = element_text(size=30), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

# !!Plot Injury Severity vs HOUR
ggplot(df.raw, aes(x=HOUR, fill=INJ_SEV_FACTOR)) + geom_bar(stat="bin", bins=24) + xlim(1,24) +xlab('Hour') + ylab('# of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + ggtitle('Injury Count vs. Hour of Day') +
  theme(plot.title = element_text(size=30), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

# !!Plot Injury Severity as a percentage of HOUR
ggplot(df.raw, aes(x=HOUR, fill=INJ_SEV_FACTOR)) + geom_bar(position='fill',stat="bin", bins=24) + xlim(1,24) +xlab('Hour') + ylab('% of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + ggtitle('Injury Proportion vs. Hour of Day') +
  theme(plot.title = element_text(size=30), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

# Plot Injury Severity vs XXX
ggplot(df.raw, aes(x=WEATHER1, fill=INJ_SEV_FACTOR)) + geom_bar(position = 'fill', stat="bin", bins=25) + ylab('# of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + 
  ggtitle('Injury Count vs. Age') +
  xlim(0,12) +
  theme(plot.title = element_text(size=30), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))

# Plot Injury Severity vs % of XXX
ggplot(df.raw, aes(x=VSURCOND, fill=INJ_SEV_FACTOR)) + geom_bar(position = 'fill',stat="bin", bins=25) +xlab('_') + ylab('# of Pedestrians') + scale_fill_manual(name="Injury Level", labels=c('0 No Injury','1 Suspected Injury','2 Minor Injury','3 Serious Injury','4 Fatality'), values=c("black", "dark green", "yellow", "orange", "red")) + 
  ggtitle('Injury Count vs. Age') +
  xlim(0,11) +
  theme(plot.title = element_text(size=30), 
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))


# Data table for PEDS (number of pedestrians in collision)
dplyr::summarise(group_by(df.raw, df.raw$PEDS),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

# Data table for VE_TOTAL (number of vehs in collision)
dplyr::summarise(group_by(df.raw, df.raw$VE_TOTAL),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

# Data table for VAR
dplyr::summarise(group_by(df.raw, df.raw$VSURCOND),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

