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

# Accident by Injury Severity
summarise(group_by(df.raw, df.raw$AGE_IM),
          count = n())

# Plot age and injury severity
ggplot(df.raw, aes(x=AGE_IM, fill=INJ_SEV_FACTOR)) + geom_bar(stat="bin", bins=25)

# Plot Injury Severity as percentage of age
ggplot(df.raw, aes(x=AGE_IM, fill=INJ_SEV_FACTOR)) + geom_bar(position="fill",stat="bin", bins=25)

# Data table for PEDS types
summarise(group_by(df.raw, df.raw$PEDS),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

# Data table for VE_TOTAL
summarise(group_by(df.raw, df.raw$VE_TOTAL),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

# Data table for PER_TYP
summarise(group_by(df.raw, df.raw$PER_TYP),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

# Data table for ALCOHOL
summarise(group_by(df.raw, df.raw$ALCOHOL),
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))


# Helper function to plot 3 way venn diagrams
plot_venn <- function(c1, c1name, c2, c2name, c3, c3name) {
  grid.newpage()
  venn.plot <- draw.triple.venn(
    area1 = sum(c1),
    area2 = sum(c2),
    area3 = sum(c3),
    n12 = sum(c1 & c2),
    n23 = sum(c2 & c3),
    n13 = sum(c1 & c3),
    n123 = sum(c1 & c2 & c3),
    
    category = c(c1name, c2name, c3name),
    fill     = c('red', 'blue', 'green'),
    margin = 0.05,
    scaled = FALSE,
    euler = TRUE
  )
}

# Plot 3 way venn diagram of pedestrian, intersection, and sev
# NOTE: we have filtered out pedestrians already, so diagram is less useful
plot_venn(df.raw$PBPTYPE==5, "Pedestrian",
          df.raw$PEDLOC<=2, "Near Intersection",
          df.raw$INJ_SEV>=3, "INJ_SEV>=3")

# Number of Multivehicle/multi pedestrian accidents
plot_venn(df.raw$PEDS>1, "Multi-Pedestrian",
          df.raw$VE_TOTAL>1, "Multi-Vehicle",
          df.raw$INJ_SEV>=3, "INJ_SEV>=3")
