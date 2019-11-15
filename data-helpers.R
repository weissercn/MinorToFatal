library(dplyr)
library(plyr)
library(ggplot2)

data.factors = c("MONTH","WKDY_IM","HOUR_IM","TYP_INT","REL_ROAD","WRK_ZONE",
                 "LGTCON_IM","WEATHER1","WEATHER2","SCH_BUS","ALCHL_IM",
                 
                 #"MAKE.veh", "MODEL",  # Removed due to > 53 factors
                 "BDYTYP_IM","MDLYR_IM","TOW_VEH.veh","GVWR","V_CONFIG","BUS_USE",
                 "SPEC_USE.veh", "V_ALCH_IM","SPEEDREL","DR_SF1","DR_SF2","DR_SF3","DR_SF4",
                 "VTRAFWAY","VALIGN","VPROFILE","VSURCOND","VTRAFCON",
                 "VTCONT_F","PCRASH1_IM",
                 
                 "PBSEX","PBCWALK","PBSWALK","PBSZONE","PEDPOS","MOTMAN",
                 "PEDLEG","PEDSNR","PEDCGP",
                 
                 "MDRDSTRD",
                 "DRIMPAIR",
                 "MVISOBSC",
                 "MTM_CRSH",
                 "NMIMPAIR",
                 "MPR_ACT");

data.numbers = c("INJ_SEV",
                 "TRAV_SP","VNUM_LAN","VSPD_LIM",
                 "PBAGE");

data.get_df <- function() {
  # Load data from CSVs
  df <- data.get_raw_df()
  
  # Filter data
  df <- data.filter(df)
  
  # Format data according to factors/numbers
  data.format(df);
}

data.format <- function(df, factors = data.factors, numbers = data.numbers) {
  df[, factors] <- lapply(df[, factors], factor)
  df[, numbers] <- lapply(df[, numbers], as.numeric)
  
  # Build data frame with only these columns
  select(df, c(factors, numbers))
}

data.auto_factor <- function(df, max_levels = 25) {
  for(i in names(df)) {
    if(length(unique(df[,i])) < max_levels) {
      df[,i] = factor(df[,i])
    }
  }
  df
}

data.filter <- function(df) {
  # Filter only pedestrians (PBPTYPE is 5 and PEDLOC <= 2)
  #   Also filter out unknown severities
  df[which(df$PBPTYPE==5 & df$PEDLOC<=2 & df$INJ_SEV<=4),]
}

data.get_raw_df <- function() {
  acc = data.load_csvs('ACCIDENT.CSV');
  pers = data.load_csvs('PERSON.csv');
  pb = data.load_csvs('PBTYPE.csv');
  nmcrash = data.load_csvs('NMCRASH.csv');
  nmimpair = data.load_csvs('NMIMPAIR.csv');
  nmprior = data.load_csvs('NMPRIOR.csv');
  
  # Merge data
  df <- merge(pers, pb, by=c("CASENUM","VEH_NO","PER_NO"), suffixes=c(".per", ".pb"));
  df <- merge(df, acc, by=c("CASENUM"), suffixes=c("",".acc"));
  df <- merge(df, nmcrash, by=c("CASENUM","VEH_NO","PER_NO"), suffixes=c("",".nmcrash"));
  df <- merge(df, nmimpair, by=c("CASENUM","VEH_NO","PER_NO"), suffixes=c("",".nmimpair"));
  df <- merge(df, nmprior, by=c("CASENUM","VEH_NO","PER_NO"), suffixes=c("",".nmprior"));
  
  # Only consider FIRST vehicle... what if more than one?
  veh = data.load_csvs('VEHICLE.csv');
  veh1 <- veh[which(veh$VEH_NO==1),];
  df <- merge(df, veh1, by=c("CASENUM"), suffixes=c("",".veh"));
  
  distr = data.load_csvs('DISTRACT.csv');
  distr1 <- distr[which(distr$VEH_NO==1),];
  df <- merge(df, distr1, by=c("CASENUM"), suffixes=c("",".distr"));
  
  drimp = data.load_csvs('DRIMPAIR.csv');
  drimp1 <- drimp[which(drimp$VEH_NO==1),];
  df <- merge(df, drimp1, by=c("CASENUM"), suffixes=c("",".drimp"));
  
  vision = data.load_csvs('VISION.csv');
  vision1 <- vision[which(vision$VEH_NO==1),];
  df <- merge(df, vision1, by=c("CASENUM"), suffixes=c("",".vis"));
  
  df
}

data.load_csvs <- function(file) {
  files = list.files(path="Data", pattern=file, 
                     recursive=TRUE, ignore.case = TRUE, full.names=TRUE);
  
  ldply(files, read.csv)
}
