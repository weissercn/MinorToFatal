library(dplyr)
library(plyr)
library(caret)
library(VennDiagram)  # must run install.packages("VennDiagram")
library(ggplot2)

load_all <- function(file) {
  files = list.files(path="Data", pattern=file, 
             recursive=TRUE, ignore.case = TRUE, full.names=TRUE);
  
  ldply(files, read.csv)
}

# LOADING DATA
acc = load_all('ACCIDENT.CSV');
pers = load_all('PERSON.csv');
pb = load_all('PBTYPE.csv');

pers_proc = pers[which(pers$VEH_NO==0),]
dper <- merge(pers_proc, pb, by=c("CASENUM","VEH_NO","PER_NO"), suffixes=c(".per", ".pb"))
dall <- merge(dper, acc, by=c("CASENUM"), suffixes=c(".per",".acc"))

# Accident by Injury
acc_by_inj_sev <- group_by(dall, dall$INJ_SEV)
summarise(acc_by_inj_sev,
          count = n())


dfAge <- select(dall[which(dall$PBAGE<100),], c("INJ_SEV", "PBAGE"))
dfAge$INJ_SEV = factor(dfAge$INJ_SEV)

sev_by_age <- ggplot(dfAge, aes(x=PBAGE, fill=INJ_SEV)) + geom_bar(stat="bin", bins=25)
sev_by_age
sev_by_age_as_percent <- ggplot(dfAge, aes(x=PBAGE, fill=INJ_SEV)) + geom_bar(position="fill",stat="bin", bins=25)
sev_by_age_as_percent

dfPedctype = dall
dfPedctype$PEDCTYPE = factor(dfPedctype$PEDCTYPE)
dfPedctype$INJ_SEV = factor(dfPedctype$INJ_SEV)

pedctypes <- group_by(dall, dall$PEDCTYPE)
pedcSummary <- summarise(pedctypes,
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))
pedcSummary <- pedcSummary[order(-pedcSummary$count),]
pedcSummary$`dall$PEDCTYPE` = factor(pedcSummary$`dall$PEDCTYPE`, 
                                     levels=pedcSummary$`dall$PEDCTYPE`[order(pedcSummary$count)])

pedctypePlot <- ggplot(pedcSummary, aes(y=count, x=`dall$PEDCTYPE`)) + geom_bar(stat="identity")
pedctypePlot

sev_by_age_as_percent <- ggplot(dfAge, aes(x=PBAGE, fill=INJ_SEV)) + geom_bar(position="fill",stat="bin", bins=25)
sev_by_age_as_percent

acc_by_num_peds <- group_by(dall, dall$PEDS)
summarise(acc_by_num_peds,
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

acc_by_num_vehs <- group_by(dall, dall$VE_TOTAL)
summarise(acc_by_num_vehs,
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

acc_by_per_type <- group_by(dall, dall$PER_TYP)
summarise(acc_by_per_type,
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))

acc_by_ped_loc <- group_by(dall, dall$PEDLOC)
summarise(acc_by_ped_loc,
          count = n(),
          inj_sev = mean(INJ_SEV),
          inj_sev_std = sd(INJ_SEV))


pot_venn <- function(c1, c1name, c2, c2name, c3, c3name) {
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

pot_venn(dall$PBPTYPE==5, "Pedestrian",
         dall$PEDLOC<=2, "Near Intersection",
         dall$INJ_SEV>3, "INJ_SEV>3")
