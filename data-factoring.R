library(dplyr)
library(caret)
library(nnet)
library(e1071) 
library(class)
library(glmnet)
library(car)

# Load Data
source('data-helpers.R')
df.orig <- data.get_df();

# Binary classification
df <- df.orig
df$INJ_SEV <- as.factor(df.orig$INJ_SEV > 2)

#Recode and Reorder
#Variables are binned into larger categories to reduce factor sparsity. Reference the CRSS user guide for what the numbers correspond to for each factor
df$MONTH = recode(df$MONTH,"c('12','1','2')='wint';c('3','4','5')='spring';c('6','7','8')='summ';c('9','10','11')='fall'")
df$WKDY_IM = recode(df$WKDY_IM,"c('2','3','4','5')='wkday';c('6')='fri';c('7')='sat';c('1')='sun'")
df$HOUR_IM = recode(df$HOUR_IM,"c('0','1','2','3','4','5')='nightLate';c('6','7','8','9','10','11')='morn';
                    c('12','13','14','15','16','17')='aftern';c('18','19','20','21','22','23')='nightEarly'")
df$WEATHR_IM = recode(df$WEATHR_IM,"c('1','98')='clear';c('2')='rain';c('3')='sleetHail';c('4')='snow';c('5')='fog';
                      c('10')='cloud'")
df$REL_ROAD = recode(df$REL_ROAD,"c('1','11')='onRoad';c('2','3','4','5','6','7','8')='offRoad';
                     c('98')='unk'");df = within(df, {REL_ROAD = relevel(REL_ROAD,ref="unk")}) 
df$LGTCON_IM = recode(df$LGTCON_IM,"c('1')='day';c('2')='darkNotLgt';c('3')='darkLgt';c('4','5')='dawnDusk';
                      c('6')='darkUnkLgt'"); df = within(df, {LGTCON_IM = relevel(LGTCON_IM,ref="day")})

#Bin age into categorical variables
df$PBAGE = cut(df$PBAGE,c(-1,15,30,45,60,75,130,1000),labels=c('0_15','16_30','31_45','46_60','61_75','75plus','unk')); df$PBAge = factor(df$PBAge, levels = c('unk','0_15','16_30','31_45','46_60','61_75','75plus'))
df$PBSEX = df$PBSEX = recode(df$PBSEX,"c('8','9')='unk';c('1')='male'; c('2')='female'");df = within(df, {PBSEX = relevel(PBSEX,ref="unk")}) 
#Relevel sets the desired variable as the reference for the model. I set "unknown" as the reference to see the contribution from all other possible values
df = within(df, {PBCWALK = relevel(PBCWALK,ref="9")})
df = within(df, {PBSWALK = relevel(PBSWALK,ref="9")})
df$PEDPOS = recode(df$PEDPOS,"c('1')='inters';c('2')='crossWalk';c('3')='lane';c('4','5','6')='shoulder';c('7','8')='nonTraffic';
                     c('9')='unk'");df = within(df, {PEDPOS = relevel(PEDPOS,ref="unk")}) 
df$MOTMAN = recode(df$MOTMAN,"c('8','9')='unk';c('1')='left';c('2')='right';c('3')='straight'");df = within(df, {MOTMAN = relevel(MOTMAN,ref="unk")})
df$PEDCGP = recode(df$PEDCGP,"c('100','600')='990'");df = within(df, {PEDCGP = relevel(PEDCGP,ref="990")})
df$MTM_CRSH = recode(df$MTM_CRSH,"c('21','98')='0'")
df$NMIMPAIR = recode(df$NMIMPAIR,"c('98','99','0')='notImpair';c('1','2','8','9')='tempImpair';c('3','4','5','6','7','10','96')='physImpair'")
df$MPR_ACT = recode(df$MPR_ACT,"c('98')='99'");df = within(df, {MPR_ACT = relevel(MPR_ACT,ref="99")})

# Condense BODYTYP_IM to larger category bins
df$BDYcats = df$BDYTYP_IM
df$BDYcats = cut(df$BDYTYP_IM,c(0,13,19,29,39,49,59,79,90,99),labels=c('auto','utility','vanLight','lightTruck','otherTruck','bus','heavyTruck','motorcycle','unk'))
df$BDYcats = recode(df$BDYcats,"c('unk')='auto'")
df$BDYTYP_IM = as.factor(df$BDYTYP_IM)

df$VSURCOND = recode(df$VSURCOND,"c('0','8','98')='unk';c('1')='dry';c('2')='wet';c('3','4','10')='wetOther'");df = within(df, {VSURCOND = relevel(VSURCOND,ref="unk")})
df$VTRAFCON = recode(df$VTRAFCON,"c('0')='none';c('1','2','3','4','7','8','9')='lightSignal';c('20','21','23','28','29','40','50')='physSign';c('97','98','99')='unk'");df = within(df, {VTRAFCON = relevel(VTRAFCON,ref="unk")})
df$PCRASH1_IM = recode(df$PCRASH1_IM,"c('98','99','0')='unk'");df = within(df, {PCRASH1_IM = relevel(PCRASH1_IM,ref="unk")})
df$VTRAFWAY = recode(df$VTRAFWAY,"c('8','9')='unk';c('0')='nonTraffic';c('4')='oneWay';c('6')='ramp';c('1','5')='twoWayNoDivide';c('2','3')='twoWayDivide'");df = within(df, {VTRAFWAY = relevel(VTRAFWAY,ref="unk")})
df$VNUM_LAN = cut(df$VNUM_LAN,c(-1,0,1,2,3,4,5,6,7,9),labels=c('nonTraffic','1','2','3','4','5','6','7plus','unk')); df$VNUM_LAN = factor(df$VNUM_LAN, levels = c('unk','nonTraffic','1','2','3','4','5','6','7plus'))
# Condense TRAV_SP to larger category bins
df$SPcats = cut(df$TRAV_SP,c(-1,5,10,15,20,25,30,35,40,55,200,1000),labels=c('0_5','5_10','10_15','15_20','20_25','25_30','30_35','35_40','40_55','55_200','unk')); df = within(df, {SPcats = relevel(SPcats,ref="unk")})
df$SPEEDREL = recode(df$SPEEDREL,"c('0')='no';c('2','3','4','5')='yes';c('8','9')='unk'");df = within(df, {SPEEDREL = relevel(SPEEDREL,ref="unk")})
df$LIMcats = cut(df$VSPD_LIM,c(-1,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,95,1000),labels=c('none','5','10','15','20','25','30','35','40','45','50','55','60','65','70','75plus','unk')); df = within(df, {LIMcats = relevel(LIMcats,ref="unk")})

#No driver present so coded MDRDSTRD '16' = NA
df$MDRDSTRD = recode(df$MDRDSTRD,"c('16')=NA;c('0','99','96')='no';c('1','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','92','93','97','98')='yes'")
df$DRIMPAIR = recode(df$DRIMPAIR,"c('0','95','98','99')='no';c('1','2','3','4','5','6','7','8','9','10','96')='yes'")
df$MVISOBSC = recode(df$MVISOBSC,"c('0','95','99')='no';c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','97','98')='yes'")


# !! Logistic regression model (excluding the drop variables that were not helpful)
drops <- c("BDYTYP_IM","V_CONFIG","GVWR","VNUM_LAN","TRAV_SP",
           "TYP_INT","PEDLEG","PEDSNR","VALIGN","VPROFILE","VCONT_F","DR_SF1","DR_SF2",'VSPD_LIM','pcrash_F')
df.small = df[ , !(names(df) %in% drops)]

#Use to check the distribution of INJ_SEV for any variable
dplyr::summarise(group_by(df.orig, df.orig$PBSEX),
                 count = n(),
                 inj_sev = mean(INJ_SEV),
                 inj_sev_std = sd(INJ_SEV))


#You can save/load DFORIG.Rdata to avoid having to regenerate the data each time
save(df.orig,file='DFORIG.Rdata')
#load(file='DFORIG.Rdata')
