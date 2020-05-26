#Help here: we need better data visualizations. Maybe ggplot? I tried for two hours and this is the best I came up with

sevColors = c("black", "dark green", "yellow", "orange", "red") #define the colors for injury severity levels

#Mosiac plot where the bar widths = number of occurances and y-axis is normalized frequency
plot(df.order$WKDY_IM, df.order$INJ_SEV, ylab="Injury Severity",col=sevColors)

#create contigency tables for plotting
contigTable = with(df.order,table(HOUR_IM,INJ_SEV))
contigTableR = t(contigTable) #transpose the table
contigTable

plot(contigTable, col=sevColors) #mosaic plot
barplot(contigTableR, col=sevColors) #bar plot of frequencies