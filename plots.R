#Help here: we need better data visualizations. Maybe ggplot? I tried for two hours and this is the best I came up with

sevColors = c("black", "dark green", "yellow", "orange", "red") #define the colors for injury severity levels

#Mosiac plot where the bar widths = number of occurances and y-axis is normalized frequency
plot(df.factors$WKDY_IM, df.factors$INJ_SEV, ylab="Injury Severity",col=sevColors)

#create contigency tables for plotting
contigTable = with(df.factors,table(HOUR_IM,INJ_SEV))
contigTableR = t(contigTable) #transpose the table
contigTable

plot(contigTable, col=sevColors) #mosaic plot
barplot(contigTableR, col=sevColors) #bar plot of frequencies


ncol(df)
colnames(df)[3]
#Compare INJ_SEV for any ind. variable
for(num in 1:ncol(df.factors.small)) {
  print(names(df.factors.small)[num])
  print(dplyr::summarise(group_by(df.factors.small, df.factors.small[,num]),
                   count = n(),
                   inj_sev = mean(INJ_SEV)),n=100)
}



a = dplyr::summarise(group_by(df.factors.small, df.factors.small$BDYTYP_IM),
                 count = n(),
                 inj_sev = mean(INJ_SEV))
print(a, n =100)

head(a,25)
