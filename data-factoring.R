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
save(df.orig,file='DFORIG.Rdata')

load(df.orig,file='DFORIG.Rdata')
# Binary classification
df <- df.orig
#df$INJ_SEV <- as.factor(df.orig$INJ_SEV > 2)

#Recode and Reorder
#Variables are binned into larger categories to reduce factor sparsity. Reference the CRSS user guide for what the numbers correspond to for each factor
#Relevel sets the desired variable as the reference for the model. I set "Unknownnown" as the reference to see the contribution from all other possible values
df$MONTH = recode(df$MONTH,"c('12','1','2')='Winter';c('3','4','5')='Spring';c('6','7','8')='Summer';c('9','10','11')='Fall'")
df$WKDY_IM = recode(df$WKDY_IM,"c('2','3','4','5')='Mon-Thur';c('6')='Fri';c('7')='Sat';c('1')='Sun'")
df$HOUR_IM = recode(df$HOUR_IM,"c('0','1','2','3','4','5')='0000-0559';c('6','7','8','9','10','11')='0600-1159';
                    c('12','13','14','15','16','17')='1200-1759';c('18','19','20','21','22','23')='1800-2359'")
df$WEATHR_IM = recode(df$WEATHR_IM,"c('1','98')='Clear';c('2')='Rain';c('3')='Sleet/Hail';c('4')='Snow';c('5')='Fog';
                      c('10')='Cloudy'")
df$REL_ROAD = recode(df$REL_ROAD,"c('1','11')='On-Road';c('2','3','4','5','6','7','8')='Off-Road';
                     c('98')='Unknown'");df = within(df, {REL_ROAD = relevel(REL_ROAD,ref="Unknown")}) 
df$LGTCON_IM = recode(df$LGTCON_IM,"c('1')='Day';c('2')='Dark-Unlit';c('3')='Dark-Lit';c('4','5')='Dawn/Dusk';
                      c('6')='Dark-UnknownLgt'"); df = within(df, {LGTCON_IM = relevel(LGTCON_IM,ref="Day")})
df$PBSEX = df$PBSEX = recode(df$PBSEX,"c('8','9')='Unknown';c('1')='male'; c('2')='female'");df = within(df, {PBSEX = relevel(PBSEX,ref="Unknown")}) 
df$PBCWALK = recode(df$PBCWALK,"c('1')='Yes';c('0')='No';c('9')='Unknown'")
df = within(df, {PBCWALK = relevel(PBCWALK,ref="Unknown")})
df$PBSWALK = recode(df$PBSWALK,"c('1')='Yes';c('0')='No';c('9')='Unknown'")
df = within(df, {PBSWALK = relevel(PBSWALK,ref="Unknown")})
df$PEDPOS = recode(df$PEDPOS,"c('1')='Intersection';c('2')='Crosswalk';c('3')='Lane';c('4','5','6')='Shoulder';c('7','8')='Non-Trafficway';c('9')='Unknown'");df = within(df, {PEDPOS = relevel(PEDPOS,ref="Unknown")}) 
df$MOTMAN = recode(df$MOTMAN,"c('8','9')='Unknown';c('1')='Left';c('2')='Right';c('3')='Straight'");df = within(df, {MOTMAN = relevel(MOTMAN,ref="Unknown")})
df$NMIMPAIR = recode(df$NMIMPAIR,"c('98','99','0')='Not-Impaired';c('1','2','8','9')='Temporary-Impaired';c('3','4','5','6','7','10','96')='Physical-Impaired'")
df$VSURCOND = recode(df$VSURCOND,"c('0','8','98')='Unknown';c('1')='Dry';c('2')='Wet';c('3','4','10')='Snow/Slush'");df = within(df, {VSURCOND = relevel(VSURCOND,ref="Unknown")})
df$VTRAFCON = recode(df$VTRAFCON,"c('0')='None';c('1','2','3','4','7','8','9')='Signal';c('20','21','23','28','29','40','50')='Sign';c('97','98','99')='Unknown'");df = within(df, {VTRAFCON = relevel(VTRAFCON,ref="Unknown")})
df$VTRAFWAY = recode(df$VTRAFWAY,"c('8','9')='Unknown';c('0')='Non-Trafficway';c('4')='One-way';c('6')='Ramp';c('1','5')='Twoway-Undivided';c('2','3')='Twoway-Divided'");df = within(df, {VTRAFWAY = relevel(VTRAFWAY,ref="Unknown")})
df$VTCONT_F = recode(df$VTCONT_F,"c('8','9')='Unknown';c('0')='No-Control';c('1','2')='Not-Functional';c('3')='Functional'");df = within(df, {VTCONT_F = relevel(VTCONT_F,ref="Unknown")})
df$SPEEDREL = recode(df$SPEEDREL,"c('0')='No';c('2','3','4','5')='Yes';c('8','9')='Unknown'");df = within(df, {SPEEDREL = relevel(SPEEDREL,ref="Unknown")})
df$DRIMPAIR = recode(df$DRIMPAIR,"c('0','95','98','99')='No';c('1','2','3','4','5','6','7','8','9','10','96')='Yes'")
df$MVISOBSC = recode(df$MVISOBSC,"c('0','95','99')='No';c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','97','98')='Yes'")
df$BDYTYP_IM = recode(df$BDYTYP_IM,"c('1','2','3','4','5','6','7','8','9','10','11','12','13','17','95')='Auto';c('14','15','16','19')='Utility';c('20','21','22','28','29')='Van';c('30','31','32','33','34','39','40','41','45','48','49')='Truck-Light';c('60','61','62','63','64','66','67','71','72','78','79','50','51','52','55','58','59')='Truck-Heavy/Bus';c('80','89','90')='Motorcycle'")
df$MDRDSTRD = recode(df$MDRDSTRD,"c('0','99','96')='No';c('1','3','4','5','6','7','8','9','10','11','12','13','14','15','17','18','19','92','93','97','98')='Yes'") #No driver present so coded MDRDSTRD '16' = not distracted?

#These may need more aggregation (re-binning). They are currently in drops
df$PEDCGP = recode(df$PEDCGP,"c('100','600')='990'");df = within(df, {PEDCGP = relevel(PEDCGP,ref="990")})
df$MTM_CRSH = recode(df$MTM_CRSH,"c('21','98','5')='None';c('1')='Dart-Out';c('11')='Dash';c('12')='Jaywalk';c('19')='Not Visible';c('2')='Fail-RightofWay';c('3')='Fail-ObeySigns';c('4')='In-RoadwayImproper';c('6')='Inattentive'")
df$MPR_ACT = recode(df$MPR_ACT,"c('98')='99'");df = within(df, {MPR_ACT = relevel(MPR_ACT,ref="99")})
#df$PCRASH1_IM = recode(df$PCRASH1_IM,"c('98','99','0')='Unknown';c('1')='Straight';c('2','3,'4','5')='Start/Stop/Acceleration';c('6')='Passing';c('98','99','0')='Unknown'") #incomplete
#df = within(df, {PCRASH1_IM = relevel(PCRASH1_IM,ref="Unknown")})

#All factor dataframe. Bin continuous variables into categorical variables
df.factors = df
df.factors$PBAGE = cut(df.factors$PBAGE,c(-1,15,30,45,60,75,130,1000),labels=c('0-15','16-30','31-45','46-60','61-75','75+','Unknown'))
df.factors$PBAGE = factor(df.factors$PBAGE, levels = c('Unknown','0-15','16-30','31-45','46-60','61-75','75+'))
df.factors$VNUM_LAN = cut(df.factors$VNUM_LAN,c(-1,0,1,2,3,4,5,6,9),labels=c('Non-Trafficway','1','2','3','4','5','6+','Unknown'))
df.factors$VNUM_LAN = factor(df.factors$VNUM_LAN, levels = c('Unknown','Non-Trafficway','1','2','3','4','5','6+'))
df.factors$TRAV_SP = cut(df.factors$TRAV_SP,c(-1,5,10,15,20,25,30,35,40,200,1000),labels=c('0-5','5-10','10-15','15-20','20-25','25-30','30-35','35-40','40+','Unknown'))
df.factors = within(df.factors, {TRAV_SP = relevel(TRAV_SP,ref="Unknown")})
df.factors$VSPD_LIM = cut(df.factors$VSPD_LIM,c(-1,0,20,25,30,35,40,45,95,1000),labels=c('None','<25','25','30','35','40','45','50+','Unknown')); df.factors = within(df.factors, {VSPD_LIM = relevel(VSPD_LIM,ref="Unknown")})

# exclude the drop variables that are not helpful
drops <- c("V_CONFIG","GVWR","TYP_INT","PEDLEG","PEDSNR","VALIGN","VPROFILE","VTCONT_F","DR_SF1","DR_SF2",'pcrash_F','MPR_ACT','MTM_CRSH','PEDCGP','PCRASH1_IM')
df.small = df[ , !(names(df) %in% drops)]
df.factors.small = df.factors[ , !(names(df.factors) %in% drops)]
