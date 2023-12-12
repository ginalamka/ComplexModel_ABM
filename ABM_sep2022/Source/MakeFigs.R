#MakeFigs for Lamka and Willoughby 2023

setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/Output_local/finaldata/sept2023") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

#NOTE: as of 7/5/23, we are using 84% confidence intervals to get an alpha = 0.05
  #therefore, CIs will be between 8 and 92
  #see https://academic.oup.com/jinsectscience/article/3/1/34/2577125 for more info

{
#helpful plotting links
  #line types https://r-charts.com/base-r/line-types/
  #axes labels https://r-charts.com/base-r/axes/#axis-labels
  #pch https://r-charts.com/base-r/pch-symbols/
  #legend https://r-charts.com/base-r/legend/
  #making a figure panel https://r-charts.com/base-r/combining-plots/
  #size of png https://stackoverflow.com/questions/8399100/r-plot-size-and-resolution
  #moving axis label closer to axis https://stackoverflow.com/questions/30265728/in-r-base-plot-move-axis-label-closer-to-axis
}

#1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho[driftSNPS], 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource,
#16=deltaK, 17=propMigSNPs, 18=HoallSNPs, 19=projectname, 20=groupnumb, 21=k, 22=nSNP, 23=miggy, 24=LBhet, 25=LBp, 26=maxage, 27=broodsize, 28=maturity,
#29=years, 30=r0, 31=nSNP.mig, 32=nSNP.cons

{
#  A = 1 mig per gen
#  b= 100 @ y=151
#  c= 25 @y=151,165,181,195
#  d= 1 mig wheny >= edyr + dur+1
#  e= 100 @ y=125
#  f= 25 @ y=125, 140, 155, 170
}

#make panel order by row, with 2 rows and 2 columns
par(mfrow = c(2,2))

par(mar = c(5, 5, 4, 4)+.25)
plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     cex.axis = text.size, cex.lab = text.size)
lines(c(2,4,5,6,7, 8), c(1.3, 1.5, 1.35, 1.52, 1.65,1.7), lwd = 7)
title(ylab = 'Genetic Diversity', line = 1, cex.lab = text.size*3) #1.5
title(xlab = 'Time', line = 2.5, cex.lab = text.size*3)
par(new=TRUE) #merge two plots
lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 7, col = "blue")
axis(4, yaxt = "n")
mtext('Population Divergence', side = 4, line = 2.5, cex = text.size*2.87, col = "blue") #1.2

{
plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     cex.axis = text.size, cex.lab = text.size)
lines(c(2,4,5,6, 8), c(1.1, 1.2, 1.6, 1.65, 1.7), lwd = 5)
title(ylab = 'Proportion Migrant \n Ancestry (%)', line = 1, cex.lab = text.size*1.5)
title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)

plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     cex.axis = text.size, cex.lab = text.size)
lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 5)
title(ylab = 'Original Population \n Divergence (Fst)', line = 1, cex.lab = text.size*1.5)
title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)

plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",
     cex.axis = text.size, cex.lab = text.size)
lines(c(2,4,5,6, 8), c(1.8, 1.7, 1.4, 1.35, 1.3), lwd = 5)
title(ylab = 'Migrant Source Population \n Divergence (Fst)', line = 1, cex.lab = text.size*1.5)
title(xlab = 'Time', line = 1.5, cex.lab = text.size*2)
}

par(mfrow = c(1,1)) #go back to default where only one fig per panel

{plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Proportion Migrant \n Ancestry (%)',
      cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.1, 1.2, 1.6, 1.65, 1.7), lwd = 5)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Original Population \n Divergence (Fst)',
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6,8), c(1.1, 1.15, 1.5, 1.6,1.8), lwd = 5)
  
  plot(c(1, 9), 1:2, type = "n", xaxt = "n", yaxt = "n", xlab = 'Time', ylab = 'Migrant Source Population \n Divergence (Fst)',
       cex.axis = text.size, cex.lab = text.size)
  lines(c(2,4,5,6, 8), c(1.8, 1.7, 1.4, 1.35, 1.3), lwd = 5)
}
  
par(mfrow = c(1,1)) #go back to default where only one fig per panel
  

library(colorspace)
library(scales)
library(FSA)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold") #"springgreen3"

#plot specs
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 350
offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 4

#poly specs
dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
lty = c(3,3,3,3,1,1,1,1)

#plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
#     xaxt = 'n', xlab = 'Year', ylab = varname,
#     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
#legend('center', legend = c('Critically Endangered', 'Endangered','Vulnerable', 'No Population Decline'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.4, horiz = FALSE, x.intersp = 0.3, y.intersp = 0.5)
#legend("center", legend = c('no migrants', '1 migrant / year','burst', 'pulse'), col = "black", pch = pch, lty = lty, pt.lwd = lwd, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, xjust=0, x.intersp = 0.3, y.intersp=0.5, seg.len = .75)

#plots for TWS conference
p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
smry = rbind(p30, p3a)
gt.cols = c("firebrick3", "grey")
par(bg=NA)

LLa = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3a_all_summary
HLa = read.table("end_9.27.23_HL3a_all_summary.csv", header=T, sep=",") #fin_5.23.23_1HL3a_all_summary
LHa = read.table("end_9.27.23_LH3a_all_summary.csv", header=T, sep=",")
HHa = read.table("end_9.18.23_HH3a_all_summary.csv", header=T, sep=",")
smry = rbind(LLa, HLa, HHa, LHa)
gt.cols =  c("cyan3", "maroon2", "purple", "blue")
alf = c(.7, .7, .7, .9)

p7a = read.table("end_9.18.23_LL7a_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL7a_all_summary
p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
p1a = read.table("end_9.18.23_LL1a_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_summary
ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary
smry = rbind(ba, p1a, p3a, p7a) #p50, 
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold") 
lty = c(1,1,1,1)
par(mfrow=c(1,3))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Figure 2
{
p70 = read.table("end_9.18.23_LL70_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL70_all_summary
p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
p10 = read.table("end_comb_LL10_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL10_all_summary
p10[,19] = "LL10"
b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
smry = rbind(b0,p10, p30, p70) #p50, 

#make panel order by row, with 2 rows and 2 columns
par(mfrow = c(2,2))

#1A - population size ~ year, by = IUCN status
  #will want to get total pop size, therefore number adults and babies and numboff
par(mar = c(2,6,2,2))
plot(-20, -20 , xlab="Year", ylab="Population Size", xlim=c(0, 350), ylim=c(0, 1000),
     cex.axis = text.size, cex.lab = text.size) 
text(-5,1000, "A", cex=text.size, family="sans")
#title("A", adj = 0, cex.main = text.size, line = 2)
col=1
for(z in unique(smry[,19])){
  zsmry = smry[smry[,19]==z,,drop=FALSE]
  for(b in 1:length(unique(zsmry[,20]))){
    bsmry = zsmry[zsmry[,20]==b,,drop=FALSE]
    for(p in 1:length(unique(smry[,11]))){
      psmry = bsmry[bsmry[,11]==p,,drop=FALSE]
      lines(psmry[,1], psmry[,2], col=gt.cols[col], pch=16)
    }
  }
  col=col+1
}
#legend('topleft', legend = "A", cex = text.size)
#legend('bottomright', legend = c('critically endangered', 'endangered','vulnerable'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
#legend('bottomright', legend = c('LC', 'CR','EN', 'VU'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)

#1B - delta H ~ year, by = IUCN status [plus no crash]
  #will want CR at bottom 
{
  var = 5
  varname = "Heterozygosity"
  title = "Fig 1B"
  range(smry[,var])
  
  ymin <- 0.1 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.25 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "B", cex=text.size, family="sans")
  #title("B", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
      
    }
    #TBL = rbind(TBL, tbl[,2])
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    print(c(tbl[1,2], tbl[101,2], tbl[151,2], tbl[351,2]))
    col <- col+1
  }
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#1C - fst ~ year, by = IUCN status [plus no crash]
{
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "Fig 1C"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#1D - fst vs source ~ year, by IUCN staus [plus no crash]
{
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "Fig 1C"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 3
{
#2A - prop mig SNPs (ancestry) ~ year, by = IUCN status
LL0 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
LLa = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3a_all_summary
HLa = read.table("end_9.27.23_HL3a_all_summary.csv", header=T, sep=",") #fin_5.23.23_1HL3a_all_summary
LHa = read.table("end_9.27.23_LH3a_all_summary.csv", header=T, sep=",")
HHa = read.table("end_9.18.23_HH3a_all_summary.csv", header=T, sep=",")
HH0 = read.table("end_9.18.23_HH30_all_summary.csv", header=T, sep=",")

#DONTNEED#HL0 = read.table("fin_5.23.23_1HL30_all_summary.csv", header=T, sep=",")
#DONTNEED#HH0a = read.table("comb_fin_5.10.23_2HH30a_all_summary.csv", header=T, sep=",")
#DONTNEED##HH0a >> manually combined fin_5.10.23_2HH30a_all_summary.csv and cpfin_5.10.23_2HH30a_all_summary.csv
#DONTNEED#HH0 = HH0a[HH0a[,23]==0,,drop=FALSE]
#DONTNEED#HH0[,19] = "HH0"
#DONTNEED#HHa = HH0a[HH0a[,23]!=0,,drop=FALSE]
#DONTNEED#HHa[,19] = "HHa"

smry = rbind(LLa, HLa, HHa, LHa)
gt.cols =  c("cyan3", "maroon2", "purple", "blue")

{
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "Fig 2A"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 1 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(2,6,2,2)) #4,6,4,2
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "A", cex=text.size, family="sans")
  #title("A", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  #legend("center", legend = c('low > low', 'high > low','high > high', 'low > high'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, xjust=0, x.intersp = 0.5, y.intersp=0.5) #
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = 1)
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('topleft', legend = c('LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#2B - Ho ~ year, by = high and low allele freq, low = grey, high = black, mig = solid, non-mig = dotted
smry = rbind(LL0, HH0, LLa, HLa, HHa, LHa)
gt.cols =  c("cyan3", "purple", "cyan3", "maroon2", "purple", "blue")

dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7
lty <- c(3,3,1,1,1,1)

{
  var = 5
  varname = "Heterozygosity"
  title = "Fig 2B"
  range(smry[,var])
  
  ymin <- .1 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "B", cex=text.size, family="sans")
  #title("B", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    print(c(tbl[2,2],tbl[20,2],tbl[51,2],tbl[101,2],tbl[151,2],tbl[351,2]))
    col <- col+1
  }
  #legend('left', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#2C - Fst vs orig ~ year, by = starting allele freqs
{
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "Fig 2C"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('topleft', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#2D - Fst vs source ~ year, by = starting allele freqs
{
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "Fig 2D"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('LL0', 'HL0', 'HH0', 'LLa', 'HLa', 'HHa'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 4
{
#3A - Ho ~ year, by = IUCN, mig and no mig
p70 = read.table("end_9.18.23_LL70_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL70_all_summary
p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
p10 = read.table("end_comb_LL10_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL10_all_summary
p10[,19] = "LL10"
b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
p7a = read.table("end_9.18.23_LL7a_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL7a_all_summary
p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
p1a = read.table("end_9.18.23_LL1a_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_summary
ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary

par(mfrow = c(2,2))

#3A - ancestry ~ year, by = IUCN
smry = rbind(ba, p1a, p3a, p7a) #p50, 
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold") 
lty = c(1,1,1,1)
{
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "Fig 3A"
  
  ymin <- 0
  ymax <- 1
  
  par(mar = c(2,6,2,2)) #2,6,2,2
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n', pin=c(1,1))
  text(-5,ymax, "A", cex=text.size, family="sans")
  #title("A", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #legend("center", legend = c('CR', 'EN','VU', 'NO'), col = c(gt.cols[2], gt.cols[3], gt.cols[4], gt.cols[1]), pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, xjust=0, x.intersp = 0.1, y.intersp=0.3) #
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #r=1
  #for(z in unique(smry[,19])){
  #  zsmry = smry[smry[,19]==z,,drop=FALSE]
  #  for(b in 1:length(unique(zsmry[,20]))){
  #    bsmry = zsmry[zsmry[,20]==b,,drop=FALSE]
  #    for(p in 1:length(unique(smry[,11]))){
  #      psmry = bsmry[bsmry[,11]==p,,drop=FALSE]
  #      lines(psmry[,1], psmry[,var], col=gt.cols[r], pch=16)
  #    }
  #  }
  #  r=r+1
  #}
  #annotate(geom="text", x=1, y=ymax, label = "A", family = "sans")
  #legend(1,ymax, legend = 'Ab', bty='n', cex=text.size, xjust=1)
  #legend('topleft', legend = c('CR', 'EN','VU', 'NO'), col = c(gt.cols[2], gt.cols[3], gt.cols[4], gt.cols[1]), pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#3B - H ~ time, IUCN stat
smry = rbind(b0, p10, p30, p70, ba, p1a, p3a, p7a) #p50, 
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold", "grey", "firebrick3", "darkorange1", "gold") # "springgreen3"
lty = c(3,3,3,3,1,1,1,1)
{
  var = 5
  varname = "Heterozygosity"
  title = "Fig 3B"
  range(smry[,var])
  
  ymin <- .05 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .25 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty <- c(3,3,3,3, 1,1,1,1)
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "B", cex=text.size, family="sans")
  #title("B", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#3C - orig fst ~ year, by = IUCN
{
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "Fig 3C"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.4 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty <- c(3,3,3,3, 1,1,1,1)
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    print(c(tbl[2,2],tbl[20,2],tbl[51,2],tbl[101,2],tbl[151,2],tbl[351,2]))
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#3D - source fst ~ year, by = IUCN
{
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "Fig 3D"
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty <- c(3,3,3,3, 1,1,1,1)
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    print(c(tbl[2,2],tbl[20,2],tbl[51,2],tbl[101,2],tbl[151,2],tbl[351,2]))
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('center', legend = c('Critically Endangered', 'Endangered','Vulnerable', 'No Population Decline'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.4, horiz = FALSE, x.intersp = 0.3, y.intersp = 0.5)
}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 5 #CONSIDER ADDING POP GROWTH
{
  par(mfcol = c(4,3))
  text.size <- 1.75*.75
  pt.cex <- 1.25/2
  lwd <- 3
  pt.size <- 1.5
  
  #4A, 4D, 4G, 4J
  p1a = read.table("end_9.18.23_LL1a_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_summary
  p10 = read.table("end_comb_LL10_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL10_all_summary  #has 42 pops that crashed!!
  p10[,19] = "LL10"
  p1b = read.table("end_comb_LL1b_all_summary.csv", header=T, sep=",") #comb_fin_6.1.23_LL1b_all_summary #has 35 pops that crashed!!!
  p1b[,19] = "p1b"
  p1c = read.table("end_comb_LL1c_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL1c_all_summary #has 37 pops that crashed!!!
  p1c[,19] = "p1c"
  b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
  ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary
  smry = rbind(b0, ba, p10, p1a, p1b, p1c)
  #gt.cols = c("hotpink","orchid3", "springgreen", "blue") #c("black", "chartreuse3", "chocolate3", "goldenrod3")
  
  gt.cols = c("grey", "grey", "firebrick3", "firebrick3", "firebrick3", "firebrick3")
  lty = c(3, 1, 3, 1, 2, 4)
  pch = c(18, 19, 18, 19, 15, 17)
  
  #A
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "A"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(1,2,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y5[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #D
  var = 5
  varname = "Heterozygosity"
  title = "D"
  ymin <- .05
  ymax <- .25
  {
    par(mar = c(1,2,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y5[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #G
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "G"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(1,2,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #J
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "J"
  ymin <- 0
  ymax <- .5
  {
    par(mar = c(2,2,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
  p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
  p3b = read.table("end_9.18.23_LL3b_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3b_all_summary
  p3c = read.table("end_9.18.23_LL3c_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3c_all_summary
  b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
  ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary
  smry = rbind(b0, ba, p30, p3a, p3b, p3c)
  
  
  gt.cols = c("grey", "grey","darkorange1", "darkorange1", "darkorange1", "darkorange1")
  lty = c(3, 1, 3, 1, 2, 4)
  pch = c(18, 19, 18, 19, 15, 17)
  
  #B
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "B"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #E
  var = 5
  varname = "Heterozygosity"
  title = "E"
  ymin <- .05
  ymax <- .25
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #H
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "H"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #K
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "K"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(2,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size) 
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  p7a = read.table("end_9.18.23_LL7a_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL7a_all_summary
  p70 = read.table("end_9.18.23_LL70_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL70_all_summary
  p7b = read.table("end_9.18.23_LL7b_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL7b_all_summary
  p7c = read.table("end_9.18.23_LL7c_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL7c_all_summary
  b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
  ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary
  smry = rbind(b0, ba, p70, p7a, p7b, p7c)
  
  gt.cols = c("grey","grey","goldenrod1", "goldenrod1", "goldenrod1", "goldenrod1")
  lty = c(3, 1, 3, 1, 2, 4)
  pch = c(18, 19, 18, 19, 15, 17)
  
  #C
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "C"
  ymin <- 0
  ymax <- 1
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #F
  var = 5
  varname = "Heterozygosity"
  title = "F"
  ymin <- .05
  ymax <- .25
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #I
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "I"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(1,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = FALSE, cex.axis = text.size) #c('0','50', '100','150','200', '250', '300','350')
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  #L
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "L"
  ymin <- 0
  ymax <- .5
  
  {
    par(mar = c(2,1,1,1))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(0,ymax, title, cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), labels = FALSE, cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size) 
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = alpha(gt.cols[col], ln.alph), code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = pch[col], cex = pt.size) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}
#OLD vv
{
par(mfcol = c(4,4))
text.size <- 1.75*.75
pt.cex <- 1.25/2
lwd <- 3
pt.size <- 1.5

#4A, 4D, 4G, 4J
p1a = read.table("comb_fin_5.11.23_1LL1a_all_summary.csv", header=T, sep=",") 
p10 = read.table("comb_fin_5.11.23_1LL10_all_summary.csv", header=T, sep=",")
p1b = read.table("comb_fin_6.1.23_LL1b_all_summary.csv", header=T, sep=",")  #has 14 pops that crashed!!!
p1b[,19] = "p1b"
p1c = read.table("fin_6.1.23_LL1c_all_summary.csv", header=T, sep=",")  #has 3 pops that crashed!!!
smry = rbind(p10, p1a, p1b, p1c)
#gt.cols = c("hotpink","orchid3", "springgreen", "blue") #c("black", "chartreuse3", "chocolate3", "goldenrod3")

gt.cols = "firebrick3"
lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)

#A
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "A"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#E
var = 5
varname = "Heterozygosity"
title = "E"
ymin <- .1
ymax <- .3
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#I
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "I"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#M
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "M"
ymin <- 0
ymax <- .5
{
  par(mar = c(4,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

p30 = read.table("fin_5.10.23_1LL30_all_summary.csv", header=T, sep=",")
p3a = read.table("fin_5.10.23_1LL3a_all_summary.csv", header=T, sep=",")  
p3b = read.table("fin_5.10.23_1LL3b_all_summary.csv", header=T, sep=",")
p3c = read.table("fin_5.10.23_1LL3c_all_summary.csv", header=T, sep=",")
smry = rbind(p30, p3a, p3b, p3c)


gt.cols = "darkorange1"
lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)

#B
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "B"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#F
var = 5
varname = "Heterozygosity"
title = "F"
ymin <- .1
ymax <- .3
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#J
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "J"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#N
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "N"
ymin <- 0
ymax <- .5

{
  par(mar = c(4,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

p7a = read.table("fin_5.11.23_1LL7a_all_summary.csv", header=T, sep=",")
p70 = read.table("fin_5.11.23_1LL70_all_summary.csv", header=T, sep=",")
p7b = read.table("fin_6.1.23_LL7b_all_summary.csv", header=T, sep=",")
p7c = read.table("fin_6.1.23_LL7c_all_summary.csv", header=T, sep=",")
smry = rbind(p70, p7a, p7b, p7c)

gt.cols = c("goldenrod1")
lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)

#C
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "C"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#G
var = 5
varname = "Heterozygosity"
title = "G"
ymin <- .1
ymax <- .3
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#K
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "K"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#O
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "O"
ymin <- 0
ymax <- .5

{
  par(mar = c(4,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen','burst mig', 'pulse mig'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

b0 = read.table("fin_6.1.23_nbtl30_all_summary.csv", header=T, sep=",")
ba = read.table("fin_6.1.23_nbtl3a_all_summary.csv", header=T, sep=",")
smry = rbind(b0, ba)

gt.cols = "grey"
lty = c(3, 1)
pch = c(18, 19)

#first
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "D"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#second
var = 5
varname = "Heterozygosity"
title = "H"
ymin <- .1
ymax <- .3
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#third
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "L"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#fourth
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "P"
ymin <- 0
ymax <- .5

{
  par(mar = c(4,2,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(0,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = pt.size) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Figure 6 
{
#5A - timing of burst mig rates (B&E) ~ year 
par(mfcol = c(4,2))

p3b = read.table("end_9.18.23_LL3b_all_summary.csv", header=T, sep=",") #read.table("fin_5.10.23_1LL3b_all_summary.csv", header=T, sep=",")
p3e = read.table("end_9.18.23_LL3e_all_summary.csv", header=T, sep=",") #read.table("cpfin_5.10.23_1LL3e_all_summary.csv", header=T, sep=",")
smry = rbind(p3b, p3e)
gt.cols <- c("black", "grey")
lty = c(1,1)

#A
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "A"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,6,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  #legend("topleft", legend = c('after restoration', 'during restoration'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.75, horiz = FALSE, xjust=0, x.intersp = 0.2, y.intersp=0.4, seg.len = .75)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#C
var = 5
varname = "Heterozygosity"
title = "C"
ymin <- .05
ymax <- .25
{
  par(mar = c(2,6,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#E
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "E"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,6,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#G
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "G"
ymin <- 0
ymax <- .5

{
  par(mar = c(4,6,1,1))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

p3c = read.table("end_9.18.23_LL3c_all_summary.csv", header=T, sep=",") #read.table("fin_5.10.23_1LL3c_all_summary.csv", header=T, sep=",")
p3f = read.table("end_9.18.23_LL3f_all_summary.csv", header=T, sep=",") #read.table("fin_5.10.23_1LL3f_all_summary.csv", header=T, sep=",")
smry = rbind(p3c, p3f)

#B
var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "B"
ymin <- 0
ymax <- 1
{
  par(mar = c(2,2,1,6))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    print(tbl[351,2])
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#D
var = 5
varname = "Heterozygosity"
title = "D"
ymin <- .05
ymax <- .25
{
  par(mar = c(2,2,1,6))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))

    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}


#F
var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "F"
ymin <- 0
ymax <- .4
{
  par(mar = c(2,2,1,6))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels=FALSE, cex.axis = text.size) #labels = c('0','50', '100','150','200', '250', '300','350')
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

#H
var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "H"
ymin <- 0
ymax <- .5

{
  par(mar = c(4,2,1,6))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5,ymax, title, cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###SUPPLEMENTAL FIGURES
#Supp Figure 1
#data for when migrants arent preferentially mated ~ migration rate
{
p30. = read.table("end_9.18.23_LL30._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL30._all_summary.csv", header=T, sep=",")
p3a. = read.table("end_9.18.23_LL3a._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3a._all_summary.csv", header=T, sep=",")  
p3b. = read.table("end_9.18.23_LL3b._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3b._all_summary.csv", header=T, sep=",")
p3c. = read.table("end_9.18.23_LL3c._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3c._all_summary.csv", header=T, sep=",")
smry = rbind(p30., p3a., p3b., p3c.)

gt.cols = "darkorange1"
lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)

par(mfrow = c(2,2))

var = 17
varname = "Proportion Migrant\n Ancestry (%)"
title = "A"
ymin <- 0
ymax <- 1

{
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "A", cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #legend("center", legend = c('no migrants', '1 migrant / year','burst', 'pulse'), col = gt.cols, pch = pch, lty = lty, pt.lwd = lwd, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, xjust=0, x.intersp = 0.3, y.intersp=0.5, seg.len = .75)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

var = 5
varname = "Heterozygosity"
title = "B"
ymin <- .05
ymax <- .25

{
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "B", cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

var = 10
varname = "Recipient Population \n Divergence (Fst)"
title = "C"
ymin <- 0
ymax <- .4
{
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "C", cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

var = 14
varname = "Migrant Source Population \n Divergence (Fst)"
title = "D"
ymin <- 0
ymax <- .5
{
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "D", cex=text.size, family="sans")
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    y1<-temp[temp[,1] == orig.xs[1],,]
    y2<-temp[temp[,1] == orig.xs[2],,]
    y3<-temp[temp[,1] == orig.xs[3],,]
    y4<-temp[temp[,1] == orig.xs[4],,]
    y5<-temp[temp[,1] == orig.xs[5],,]
    y6<-temp[temp[,1] == orig.xs[6],,]
    y7<-temp[temp[,1] == orig.xs[7],,]
    y8<-temp[temp[,1] == orig.xs[8],,]
    
    xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
           y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
           lwd = 1, col = alpha(gt.cols, ln.alph), code=3, angle=90, length=0.1)
    lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, ln.alph), lwd = lwd, lty = lty[col])
    points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols, pt.alph), pch = pch[col], cex = 2) #pt.cex
    
    print(mean(y1[,var]))
    print(mean(y7[,var]))
    col <- col+1
  }
  #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}
#version 2
{
  p30. = read.table("end_9.18.23_LL30._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL30._all_summary.csv", header=T, sep=",")
  p3a. = read.table("end_9.18.23_LL3a._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3a._all_summary.csv", header=T, sep=",")  
  p3b. = read.table("end_9.18.23_LL3b._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3b._all_summary.csv", header=T, sep=",")
  p3c. = read.table("end_9.18.23_LL3c._all_summary.csv", header=T, sep=",") #read.table("fin_6.1.23_1LL3c._all_summary.csv", header=T, sep=",")
  p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
  p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
  p3b = read.table("end_9.18.23_LL3b_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3b_all_summary
  p3c = read.table("end_9.18.23_LL3c_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3c_all_summary
  
  gt.cols = c("grey", "grey", "grey", "grey","darkorange1", "darkorange1", "darkorange1", "darkorange1")
  lty = c(3, 1, 2, 4, 3, 1, 2, 4)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  smry = rbind(p30., p3a., p3b., p3c., p30, p3a, p3b, p3c)

  
  par(mfrow = c(2,2))
  
  var = 17
  varname = "Proportion Migrant\n Ancestry (%)"
  title = "A"
  ymin <- 0
  ymax <- 1
  
  {
    par(mar = c(2,6,2,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(-5, ymax, "A", cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #legend("center", legend = c('no mig', '1 mig/yr','burst', 'pulse'), col = "black", pch = pch, lty = lty, pt.lwd = lwd, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, xjust=0, x.intersp = 0.1, y.intersp=0.3, seg.len = .75)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = gt.cols[col], code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 5
  varname = "Heterozygosity"
  title = "B"
  ymin <- .05
  ymax <- .25
  
  {
    par(mar = c(2,6,2,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(-5, ymax, "B", cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = gt.cols[col], code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 10
  varname = "Recipient Population \n Divergence (Fst)"
  title = "C"
  ymin <- 0
  ymax <- .4
  {
    par(mar = c(4,6,2,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(-5, ymax, "C", cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = gt.cols[col], code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
  
  var = 14
  varname = "Migrant Source Population \n Divergence (Fst)"
  title = "D"
  ymin <- 0
  ymax <- .5
  {
    par(mar = c(4,6,2,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', xlab = 'Year', ylab = varname,
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(-5, ymax, "D", cex=text.size, family="sans")
    #title(title, adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    col <- 1
    for(c in unique(smry[,19])){
      print(c)
      temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      y1<-temp[temp[,1] == orig.xs[1],,]
      y2<-temp[temp[,1] == orig.xs[2],,]
      y3<-temp[temp[,1] == orig.xs[3],,]
      y4<-temp[temp[,1] == orig.xs[4],,]
      y5<-temp[temp[,1] == orig.xs[5],,]
      y6<-temp[temp[,1] == orig.xs[6],,]
      y7<-temp[temp[,1] == orig.xs[7],,]
      y8<-temp[temp[,1] == orig.xs[8],,]
      
      xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
      arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
             y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
             lwd = 1, col = gt.cols[col], code=3, angle=90, length=0.1)
      lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], lwd = lwd, lty = lty[col])
      points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = gt.cols[col], pch = pch[col], cex = 2) #pt.cex
      
      print(mean(y1[,var]))
      print(mean(y7[,var]))
      col <- col+1
    }
    #legend('bottomleft', legend = c('no mig', '1 mig/gen'), col = gt.cols, pch = pch, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }
}

#Supp Figure 2
#data for the proportion of populations that crashed
{
p1a = read.table("end_9.18.23_LL1a_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_summary
p10 = read.table("end_comb_LL10_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL10_all_summary
p10[,19] = "LL10"
p1b = read.table("end_comb_LL1b_all_summary.csv", header=T, sep=",") #comb_fin_6.1.23_LL1b_all_summary #has 35 pops that crashed!!!
p1b[,19] = "p1b"
p1c = read.table("end_comb_LL1c_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL1c_all_summary #has 37 pops that crashed!!!
p1c[,19] = "p1c"
smry = rbind(p10, p1a, p1b, p1c)

par(mfrow = c(1,1)) #go back to default where only one fig per panel

gt.cols = "firebrick3"
lty = c(3, 1, 2, 4)
pch = c(18, 19, 15, 17)
alf = c(1, .8, .6, .4) 

A = table(p10[,1])
A = as.numeric(A)

B = table(p1a[,1])
B = as.numeric(B)

C = table(p1b[,1])
C = as.numeric(C)

D = table(p1c[,1])
D = as.numeric(D)

var = rbind(A, B, C, D)
varname = "Proportion of Viable\n Populations (%)"
title = ""
ymin <- 0
ymax <- 1

{
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(0, 350), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  #title(title, adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(0, .2, .4, .6, .8, 1), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = 151, lty = 2, col = "grey")
  
  col <- 1
  for(m in 1:nrow(var)){
    plt.y = var[m,]/100  #turn into a proportion
    plt.x = c(0:350)
    lines(plt.x, plt.y, col = alpha(gt.cols, alf[col]), lwd = lwd, lty = lty[col])
    col = col+1
  }
  #legend("left", legend = c('no mig', '1 mig/yr','burst', 'pulse'), col = alpha(gt.cols, alf), lty = lty, lwd = lwd, bty = 'n', cex = (text.size-.5), horiz = FALSE, xjust=0, x.intersp = 0.1, y.intersp=0.3, seg.len = .75)
  #legend('bottom', legend = c('no mig', '1 mig/gen', "burst", "pulse"), col = alpha(gt.cols, alf), lty = lty, lwd = lwd, bty = 'n', cex = text.size, text.width = text.size, ncol = 2) #pt.cex = text.size
}
}

#Supp Figure 3
#Fis, sex ratio, LRS?

p70 = read.table("end_9.18.23_LL70_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL70_all_summary
p30 = read.table("end_9.18.23_LL30_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_summary
p10 = read.table("end_comb_LL10_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL10_all_summary
p10[,19] = "LL10"
b0 = read.table("end_9.18.23_nbtl30_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_summary
p7a = read.table("end_9.18.23_LL7a_all_summary.csv", header=T, sep=",") #fin_5.11.23_1LL7a_all_summary
p3a = read.table("end_9.18.23_LL3a_all_summary.csv", header=T, sep=",")  #fin_5.10.23_1LL3a_all_summary
p1a = read.table("end_9.18.23_LL1a_all_summary.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_summary
ba = read.table("end_9.18.23_nbtl3a_all_summary.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_summary
p7b = read.table("end_9.18.23_LL7b_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL7b_all_summary
p7c = read.table("end_9.18.23_LL7c_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL7c_all_summary
p3b = read.table("end_9.18.23_LL3b_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3b_all_summary
p3c = read.table("end_9.18.23_LL3c_all_summary.csv", header=T, sep=",") #fin_5.10.23_1LL3c_all_summary
p1b = read.table("end_comb_LL1b_all_summary.csv", header=T, sep=",") #comb_fin_6.1.23_LL1b_all_summary #has 35 pops that crashed!!!
p1b[,19] = "p1b"
p1c = read.table("end_comb_LL1c_all_summary.csv", header=T, sep=",") #fin_6.1.23_LL1c_all_summary #has 37 pops that crashed!!!
p1c[,19] = "p1c"

par(mfrow = c(2,2)) #lty = c(3, 1, 2, 4)
#Fis
{
smry = rbind(b0, p10, p30, p70)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
lty = c(3,3,3,3)
{
  var = 6
  varname = "Inbreeding (Fis)"
  title = ""
  range(smry[,var])
  
  ymin <- -.005 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .005 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "A", cex=text.size, family="sans")
  #title("A", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0, lty = 2, col = "black")
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}


smry = rbind(ba, p1a, p3a, p7a)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
lty  = c(1,1,1,1)
{
var = 6
varname = "Inbreeding (Fis)"
title = ""
range(smry[,var])

ymin <- -.005 #round(min(smry[,var]), digits = 2)#-.1
ymax <- .005 #round(max(smry[,var]), digits = 2)#+.1

par(mar = c(2,6,2,2))
## make plot
plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', xlab = '', ylab = '',
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
text(-5, ymax, "B", cex=text.size, family="sans")
#title("B", adj = 0, cex.main = text.size, line = 2)
axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
abline(v = c(100,151), lty = 2, col = "grey")
abline(h = 0, lty = 2, col="black") #alpha("black", 0.3)

col <- 1
for(c in unique(smry[,19])){
  print(c)
  temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
  
  #y1<-temp[temp[,1] == orig.xs[1],,]
  #y2<-temp[temp[,1] == orig.xs[2],,]
  #y3<-temp[temp[,1] == orig.xs[3],,]
  #y4<-temp[temp[,1] == orig.xs[4],,]
  #y5<-temp[temp[,1] == orig.xs[5],,]
  #y6<-temp[temp[,1] == orig.xs[6],,]
  #y7<-temp[temp[,1] == orig.xs[7],,]
  #y8<-temp[temp[,1] == orig.xs[8],,]
  
  tbl = NULL
  tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
  
  for(d in unique(temp[,1])){
    dat = temp[temp[,1]==d,,drop=FALSE]
    tbl[(d+1),1] = as.numeric(d)
    tbl[(d+1),2] = as.numeric(mean(dat[,var]))
    tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
    tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
    tbl[(d+1),5] = as.character(dat[1,19])
  }
  #TBL = rbind(TBL, tbl)
  #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
  polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
          col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
  lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
  
  #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
  #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
  #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
  #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  
  #print(mean(y1[,var]))
  #print(mean(y7[,var]))
  col <- col+1
}

#legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

smry = rbind(p1b, p3b, p7b)
gt.cols <- c("firebrick3", "darkorange1", "gold")
lty = c(2, 2, 2)
{
  var = 6
  varname = "Inbreeding (Fis)"
  title = ""
  range(smry[,var])
  
  ymin <- -.01 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .04 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0, lty = 2, col = "black")
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}


smry = rbind(p1c, p3c, p7c)
gt.cols <- c("firebrick3", "darkorange1", "gold")
lty  = c(4, 4, 4)
{
  var = 6
  varname = "Inbreeding (Fis)"
  title = ""
  range(smry[,var])
  
  ymin <- -.01 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- .02 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0, lty = 2, col="black") #alpha("black", 0.3)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}

###Supp Figure 4
#Sex Ratio
{
smry = rbind(b0, p10, p30, p70)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
lty = c(3,3,3,3)
{
  var = 8
  varname = "Sex Ratio"
  title = ""
  range(smry[,var])
  
  ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "A", cex=text.size, family="sans")
  #title("A", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0.5, lty = 2, col = "black")
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

smry = rbind(ba, p1a, p3a, p7a)
gt.cols <- c("grey", "firebrick3", "darkorange1", "gold")
lty  = c(1,1,1,1)
{
  var = 8
  varname = "Sex Ratio"
  title = ""
  range(smry[,var])
  
  ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(2,4,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = '', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "B", cex=text.size, family="sans")
  #title("B", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0.5, lty = 2, col="black") #alpha("black", 0.3)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

smry = rbind(p1b, p3b, p7b)
gt.cols <- c("firebrick3", "darkorange1", "gold")
lty = c(2, 2, 2)
{
  var = 8
  varname = "Sex Ratio"
  title = ""
  range(smry[,var])
  
  ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0.5, lty = 2, col = "black")
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

smry = rbind(p1c, p3c, p7c)
gt.cols <- c("firebrick3", "darkorange1", "gold")
lty  = c(4, 4, 4)
{
  var = 8
  varname = "Sex Ratio"
  title = ""
  range(smry[,var])
  
  ymin <- 0.3 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 0.7 #round(max(smry[,var]), digits = 2)#+.1
  
  par(mar = c(4,4,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  abline(v = c(100,151), lty = 2, col = "grey")
  abline(h = 0.5, lty = 2, col="black") #alpha("black", 0.3)
  
  col <- 1
  for(c in unique(smry[,19])){
    print(c)
    temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    #y1<-temp[temp[,1] == orig.xs[1],,]
    #y2<-temp[temp[,1] == orig.xs[2],,]
    #y3<-temp[temp[,1] == orig.xs[3],,]
    #y4<-temp[temp[,1] == orig.xs[4],,]
    #y5<-temp[temp[,1] == orig.xs[5],,]
    #y6<-temp[temp[,1] == orig.xs[6],,]
    #y7<-temp[temp[,1] == orig.xs[7],,]
    #y8<-temp[temp[,1] == orig.xs[8],,]
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1])), ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      tbl[(d+1),5] = as.character(dat[1,19])
    }
    #TBL = rbind(TBL, tbl)
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    #xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
    #lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd, lty = lty[col])
    #points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
    #arrows(x0 = xs, y0 = c(quantile(y1[,var], probs=0.08), quantile(y2[,var], probs=c(0.08)), quantile(y3[,var], probs=c(0.08)), quantile(y4[,var], probs=c(0.08)), quantile(y5[,var], probs=c(0.08)), quantile(y6[,var], probs=c(0.08)), quantile(y7[,var], probs=c(0.08)), quantile(y8[,var], probs=c(0.08))), 
    #       y1 = c(quantile(y1[,var], probs=0.92), quantile(y2[,var], probs=0.92), quantile(y3[,var], probs=0.92), quantile(y4[,var], probs=0.92), quantile(y5[,var], probs=0.92), quantile(y6[,var], probs=0.92), quantile(y7[,var], probs=0.92), quantile(y8[,var], probs=0.92)), 
    #       lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
    
    #print(mean(y1[,var]))
    #print(mean(y7[,var]))
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 19, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}

###Supp Figure 5
#LRS
{
r30 = read.table("end_9.18.23_LL30_all_repsuc.csv", header=T, sep=",") #fin_5.10.23_1LL30_all_repsuc
r3a = read.table("end_9.18.23_LL3a_all_repsuc.csv", header=T, sep=",") #fin_5.10.23_1LL3a_all_repsuc
r3b = read.table("end_9.18.23_LL3b_all_repsuc.csv", header=T, sep=",") #fin_5.10.23_1LL3b_all_repsuc
r3c = read.table("end_9.18.23_LL3c_all_repsuc.csv", header=T, sep=",") #fin_5.10.23_1LL3c_all_repsuc

r10 = read.table("end_LL10_comb_repsuc.csv", header=T, sep=",") #fin_5.11.23_1LL10_all_repsuc
r10[,13] = "r10"
r1a = read.table("end_9.18.23_LL1a_all_repsuc.csv", header=T, sep=",") #comb_fin_5.11.23_1LL1a_all_repsuc
r70 = read.table("end_9.18.23_LL70_all_repsuc.csv", header=T, sep=",") #fin_5.11.23_1LL70_all_repsuc
r7a = read.table("end_9.18.23_LL7a_all_repsuc.csv", header=T, sep=",") #fin_5.11.23_1LL7a_all_repsuc

r1b = read.table("end_LL1b_comb_repsuc.csv", header=T, sep=",")  #comb_fin_6.1.23_LL1b_all_repsuc #has 35 pops that crashed!!!
r1b[,13] = "r1b"
r1c = read.table("end_LL1c_comb_repsuc.csv", header=T, sep=",")  #fin_6.1.23_LL1c_all_repsuc #has 37 pops that crashed!!!
r1c[,13] = "r1c"
r7b = read.table("end_9.18.23_LL7b_all_repsuc.csv", header=T, sep=",") #fin_6.1.23_LL7b_all_repsuc
r7c = read.table("end_9.18.23_LL7c_all_repsuc.csv", header=T, sep=",") #fin_6.1.23_LL7c_all_repsuc

ra = read.table("end_9.18.23_nbtl3a_all_repsuc.csv", header=T, sep=",") #fin_6.1.23_nbtl3a_all_repsuc
r0 = read.table("end_9.18.23_nbtl30_all_repsuc.csv", header=T, sep=",") #fin_6.1.23_nbtl30_all_repsuc

smry = rbind(r0, r10, r30, r70)
gt.cols = c("grey","firebrick3", "darkorange1", "gold")
#smry_new = matrix(nrow = 1, ncol = ncol(smry))
#colnames(smry_new) <- colnames(smry)
#for(u in 1:nrow(smry)){
#  if(is.na(smry[u,3])==FALSE){
#    rbind(smry_new,smry[u,])
#  }
#}

smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
smry_new = smry_new[smry_new[,1]<=348,,drop=FALSE]
smry = smry_new



dens = NULL #c(100, 100, 100, 100, NULL, NA, NULL, NA)
ang = 45
bo = NA #c(NULL, NULL, NULL, NULL, NA, NA, NA, NA) #alpha(gt.cols[col], .8)
alf = c(.7, .7, .7, .7, .7, .7, .7, .7) #.7, .7, .7, .7

{
  var = 3
  varname = "Lifetime Reproductive \n Success"
  title = ""
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 3.5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty = c(3,3,3,3)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  
  par(mar = c(2,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = title, xlab = '', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "A", cex=text.size, family="sans")
  #title("A", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  TBL = NULL
  col <- 1
  for(c in unique(smry[,13])){
    print(c)
    temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1]))+1, ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      #tbl[(d+1),5] = as.character(dat[1,19])
      
      
    }
    TBL = rbind(TBL, tbl)
    tbl = tbl[!is.na(tbl[,1]),,drop=FALSE]
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}


smry = rbind(ra, r1a, r3a, r7a)
gt.cols = c("grey","firebrick3", "darkorange1", "gold")
smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
smry_new = smry_new[smry_new[,1]<=348,,drop=FALSE]
smry = smry_new

{
    var = 3
    varname = "Lifetime Reproductive \n Success"
    title = ""
    range(smry[,var])
    
    ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
    ymax <- 3.5 #round(max(smry[,var]), digits = 2)#+.1
    ln.alph <- 0.5
    pt.alph <- 1.25
    diff <- 0.15
    xmin <- 0
    xmax <- 350
    offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
    orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
    text.size <- 1.75
    pt.cex <- 1.25
    lwd <- 4
    lty = c(1,1,1,1)
    pch = c(18, 19, 15, 17, 18, 19, 15, 17)
    
    par(mar = c(2,4,2,2))
    ## make plot
    plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
         xaxt = 'n', main = title, xlab = '', ylab = '',
         cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
    text(-5, ymax, "B", cex=text.size, family="sans")
    #title("B", adj = 0, cex.main = text.size, line = 2)
    axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
    axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
    #abline(h = 0, lty = 2)
    
    TBL = NULL
    col <- 1
    for(c in unique(smry[,13])){
      print(c)
      temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
      
      tbl = NULL
      tbl = matrix(nrow = length(unique(temp[,1]))+1, ncol = 5)
      
      for(d in unique(temp[,1])){
        dat = temp[temp[,1]==d,,drop=FALSE]
        tbl[(d+1),1] = as.numeric(d)
        tbl[(d+1),2] = as.numeric(mean(dat[,var]))
        tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
        tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
        #tbl[(d+1),5] = as.character(dat[1,19])
        
        
      }
      tbl = tbl[!is.na(tbl[,1]),,drop=FALSE]
      TBL = rbind(TBL, tbl)
      
      #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
      polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
              col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
      lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
      
      col <- col+1
    }
    
    #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
  }


smry = rbind(r1b, r3b, r7b)
gt.cols = c("firebrick3", "darkorange1", "gold")
smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
smry_new = smry_new[smry_new[,1]<=348,,drop=FALSE]
smry = smry_new
{
  var = 3
  varname = "Lifetime Reproductive \n Success"
  title = ""
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 3.5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty = c(2, 2, 2)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  
  par(mar = c(4,6,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = title, xlab = 'Year', ylab = varname,
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "C", cex=text.size, family="sans")
  #title("C", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  TBL = NULL
  col <- 1
  for(c in unique(smry[,13])){
    print(c)
    temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1]))+1, ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      #tbl[(d+1),5] = as.character(dat[1,19])
      
      
    }
    TBL = rbind(TBL, tbl)
    tbl = tbl[!is.na(tbl[,1]),,drop=FALSE]
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}

smry = rbind(r1c, r3c, r7c)
gt.cols = c("firebrick3", "darkorange1", "gold")
smry_new = smry[!is.na(smry[,2]),,drop=FALSE]
smry_new = smry_new[smry_new[,1]<=348,,drop=FALSE]
smry = smry_new
{
  var = 3
  varname = "Lifetime Reproductive \n Success"
  title = ""
  range(smry[,var])
  
  ymin <- 0 #round(min(smry[,var]), digits = 2)#-.1
  ymax <- 3.5 #round(max(smry[,var]), digits = 2)#+.1
  ln.alph <- 0.5
  pt.alph <- 1.25
  diff <- 0.15
  xmin <- 0
  xmax <- 350
  offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
  orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
  text.size <- 1.75
  pt.cex <- 1.25
  lwd <- 4
  lty = c(4, 4, 4)
  pch = c(18, 19, 15, 17, 18, 19, 15, 17)
  
  par(mar = c(4,4,2,2))
  ## make plot
  plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
       xaxt = 'n', main = title, xlab = 'Year', ylab = '',
       cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
  text(-5, ymax, "D", cex=text.size, family="sans")
  #title("D", adj = 0, cex.main = text.size, line = 2)
  axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
  axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
  #abline(h = 0, lty = 2)
  
  TBL = NULL
  col <- 1
  for(c in unique(smry[,13])){
    print(c)
    temp <- smry[smry[,13] == c,, drop=FALSE] #separate by parameter set/aka project name
    
    tbl = NULL
    tbl = matrix(nrow = length(unique(temp[,1]))+1, ncol = 5)
    
    for(d in unique(temp[,1])){
      dat = temp[temp[,1]==d,,drop=FALSE]
      tbl[(d+1),1] = as.numeric(d)
      tbl[(d+1),2] = as.numeric(mean(dat[,var]))
      tbl[(d+1),3] = as.numeric(quantile(dat[,var], probs=0.08))
      tbl[(d+1),4] = as.numeric(quantile(dat[,var], probs=0.92))
      #tbl[(d+1),5] = as.character(dat[1,19])
      
      
    }
    TBL = rbind(TBL, tbl)
    tbl = tbl[!is.na(tbl[,1]),,drop=FALSE]
    #xs <- tbl[,1] + offsets[col]  #dont forget you're in a loop, dummy
    polygon(x = c(tbl[,1],rev(tbl[,1])) , y = c(tbl[,3],rev(tbl[,4])), density = dens[col], angle = ang, border = bo[col],
            col = adjustcolor(alpha(gt.cols[col], .7), alpha.f=alf[col]))  #border = alpha(gt.cols[col], .8)
    lines(tbl[,1], tbl[,2], col = gt.cols[col], lwd = lwd, lty = lty[col])
    
    col <- col+1
  }
  
  #legend('topleft', legend = c('CR', 'EN','VU', 'LC'), col = gt.cols, pch = 15, bty = 'n', cex = (text.size-.5), pt.cex = pt.cex+.5, horiz = FALSE, x.intersp = 0.2)
}
}



##### Generic plotting code
#1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho[driftSNPS], 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource,
#16=deltaK, 17=propMigSNPs, 18=HoallSNPs, 19=projectname, 20=groupnumb, 21=k, 22=nSNP, 23=miggy, 24=LBhet, 25=LBp, 26=maxage, 27=broodsize, 28=maturity,
#29=years, 30=r0, 31=nSNP.mig, 32=nSNP.cons
{
var = 5
varname = "observed heterozygosity"
title = "various starting allele freqs"
range(smry[,var])
#if(anyNA(smry[,var]==TRUE)){
#  hold<- na.omit(smry[,var],drop=FALSE)
#  smry <- as.matrix(hold)
#}

ymin <- round(min(smry[,var]), digits = 2)#-.1
ymax <- round(max(smry[,var]), digits = 2)#+.1
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 350
offsets <- c(-0.1, -0.5, 0, 0.5, 0.1, 0.15, 0.2, 0.25) #c(-0.2, -0.1, 0, 0.1, 0.2) #must have the same number of parameter sets
orig.xs <- c(1, 50, 100, 151, 201, 250, 300, 350) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 4

## make plot
plot(-1,-1, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', main = title, xlab = 'Generation Time', ylab = varname,
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
axis(2, at = c(ymin, ymax-(ymax-ymin)/2, ymax), cex.axis = text.size)
axis(1, at = c(0, 50, 100, 150, 200, 250, 300, 350), labels = c('0','50', '100','150','200', '250', '300','350'), cex.axis = text.size)
#abline(h = 0, lty = 2)

col <- 1
for(c in unique(smry[,19])){
  print(c)
  temp <- smry[smry[,19] == c,, drop=FALSE] #separate by parameter set/aka project name
  
  y1<-temp[temp[,1] == orig.xs[1],,]
  y2<-temp[temp[,1] == orig.xs[2],,]
  y3<-temp[temp[,1] == orig.xs[3],,]
  y4<-temp[temp[,1] == orig.xs[4],,]
  y5<-temp[temp[,1] == orig.xs[5],,]
  y6<-temp[temp[,1] == orig.xs[6],,]
  y7<-temp[temp[,1] == orig.xs[7],,]
  y8<-temp[temp[,1] == orig.xs[8],,]
  
  xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #columns <- c(18, 19, 20, 21)
  lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
  points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var]), mean(y8[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var]), mean(y6[,var])-sd(y6[,var]), mean(y7[,var])-sd(y7[,var]), mean(y8[,var])-sd(y8[,var])), 
         y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var]), mean(y6[,var])+sd(y6[,var]), mean(y7[,var])+sd(y7[,var]), mean(y8[,var])+sd(y8[,var])), 
         lwd = lwd, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  for(l in unique(orig.xs)){
  #    column <- columns[l]
  #    ## 95% CIs (inappropriate for large sample sizes)
  #    # arrows(x0 = xs[l], x1 = xs[l], y0 = (mean(temp[,column], na.rm = TRUE) - (sd(temp[,column], na.rm = TRUE)/10*1.96)),
  #    #        y1 = (mean(temp[,column], na.rm = TRUE) + (sd(temp[,column], na.rm = TRUE)/10*1.96)),
  #    #        lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  arrows(x0 = OTime[,1], y0 = (OTime[,2])-(sd(OTime[,7])), y1 = (OTime[,2])+(sd(OTime[,7])), lwd = .5, col = "black", code=3, angle=90, length=0.1)
  
  #    
  #    arrows(x0 = xs[l], x1 = xs[l], y0 = quantile(temp[,column], probs = c(0.025,0.975))[1],   #will need to do this for each year of interest
  #           y1 = quantile(temp[,column], probs = c(0.025,0.975))[2],
  #           lwd = 2, col = alpha(gt.cols[col], pt.alph), code=3, angle=90, length=0.1)
  #  }    
  print(mean(y1[,var]))
  print(mean(y3[,var]))
  print(mean(y4[,var]))
  print(mean(y8[,var]))
  col <- col+1
}

}
