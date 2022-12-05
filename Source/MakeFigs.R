#MakeFigs

setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/Output/holding") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

tab0 = read.table("summary_ABM_run.10.12.22_d_all.csv", header=T, sep=",")
tab1 = read.table("ABM_run.11.8.22_1a_all_summary.csv", header=T, sep=",")  
tab2 = read.table("ABM_run.11.7.22_2a_all_summary.csv", header=T, sep=",")
tab3 = read.table("ABM_run.11.7.22_3a_all_summary.csv", header=T, sep=",")
tab4 = read.table("ABM_run.11.8.22_4a_all_summary.csv", header=T, sep=",")
tab5 = read.table("ABM_run.11.8.22_5a_all_summary.csv", header=T, sep=",")
tab6 = read.table("ABM_run.11.14.22_6a_all_summary.csv", header=T, sep=",")
tab7 = read.table("ABM_run.11.14.22_7a_all_summary.csv", header=T, sep=",")
tab8 = read.table("ABM_run.11.14.22_8a_all_summary.csv", header=T, sep=",")
tab9 = read.table("ABM_run.11.14.22_9a_all_summary.csv", header=T, sep=",")

#Data
{
#summary_ABM_run.10.12.22_d_all.csv  >> zero migration
#ABM_run.11.8.22_1a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .4-.6
#ABM_run.11.7.22_2a_all_summary.csv  >> one migration of 50 indv at year 175, starting heterozy in source pop = .4-.6
#ABM_run.11.7.22_3a_all_summary.csv  >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .4-.6
#ABM_run.11.8.22_4a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .8-.9
#ABM_run.11.8.22_5a_all_summary.csv  >> 1 mig per generation, starting heterozy in source pop = .1-.2
#ABM_run.11.14.22_6a_all_summary.csv >> one migration of 50 indv at year 175, starting heterozy in source pop = .1-.2
#ABM_run.11.14.22_7a_all_summary.csv >> one migration of 50 indv at year 175, starting heterozy in source pop = .8-.9
#ABM_run.11.14.22_8a_all_summary.csv >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .8-.9
#ABM_run.11.14.22_9a_all_summary.csv >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .1-.2
}

#give each parameter set a unique identifier
tab0[,12] <- "z"
tab1[,12] <- "a"
tab2[,12] <- "b"
tab3[,12] <- "c"
tab4[,12] <- "d"
tab5[,12] <- "e"
tab6[,12] <- "f"
tab7[,12] <- "g"
tab8[,12] <- "h"
tab9[,12] <- "i"

#note that as written, figs are for FOUR (4) parameter sets -- no mig plus the 3 varieties
comp1 = rbind(tab0,tab1,tab2,tab3) #Change migration intensities @ .4-.6 het in source pop
comp2 = rbind(tab0,tab1,tab4,tab5) #change fitness of source pop @ 1 mig per gen
comp3 = rbind(tab0,tab2,tab6,tab7) #change fitness of source pop @ 1x pulse mig
comp4 = rbind(tab0,tab3,tab8,tab9) #change fitness of source pop @ 3x pulse mig 
comp5 = rbind(tab0,tab4,tab7,tab8) #change migration intensities @ .8-.9 het in source pop
comp6 = rbind(tab0,tab5,tab6,tab9) #change migration intensities @ .1-.2 het in source pop

smry <- comp6   #compX
  
#~~~Avril's Code
### set colors
library(ghibli)
library(scales)
gt.cols <- ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- ghibli_palette('PonyoLight')[4]

ymin <- 0.2
ymax <- round(max(smry[,5]), digits = 2)+0.1
ln.alph <- 0.5
pt.alph <- 1
diff <- 0.15
xmin <- 0
xmax <- 250
offsets <- c(-0.5, 0, 0.5) #c(-0.2, -0.1, 0, 0.1, 0.2)
orig.xs <- c(1:250) #c(1, 100, 150, 200, 250)
text.size <- 1.75
pt.cex <- 1.25
lwd <- 2

#tab3[tab3[,1] == 1,,]

## Observed Het over Time
plot(0,0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', main = 'Observed Heterozygosity Over Time', xlab = 'Generation Time', ylab = 'Observed Heterozygosity',
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
#axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
axis(1, at = c(0, 100, 150, 200, 250), labels = c('0','100','150','200', '250'), cex.axis = text.size)
#abline(h = 0, lty = 2)

col <- 1
for(c in unique(smry[,12])){
  print(c)
  temp <- smry[smry[,12] == c,, drop=FALSE] #separate by parameter set

  y1<-temp[temp[,1] == orig.xs[1],,]
  y2<-temp[temp[,1] == orig.xs[2],,]
  y3<-temp[temp[,1] == orig.xs[3],,]
  y4<-temp[temp[,1] == orig.xs[4],,]
  y5<-temp[temp[,1] == orig.xs[5],,]
  
  xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #columns <- c(18, 19, 20, 21)
  lines(xs, c(mean(y1[,5]), mean(y2[,5]), mean(y3[,5]), mean(y4[,5]), mean(y5[,5])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
  points(xs, c(mean(y1[,5]), mean(y2[,5]), mean(y3[,5]), mean(y4[,5]), mean(y5[,5])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  arrows(x0 = xs, y0 = c(mean(y1[,5])-sd(y1[,5]), mean(y2[,5])-sd(y2[,5]), mean(y3[,5])-sd(y3[,5]), mean(y4[,5])-sd(y4[,5]), mean(y5[,5])-sd(y5[,5])), 
         y1 = c(mean(y1[,5])+sd(y1[,5]), mean(y2[,5])+sd(y2[,5]), mean(y3[,5])+sd(y3[,5]), mean(y4[,5])+sd(y4[,5]), mean(y5[,5])+sd(y5[,5])), 
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
  col <- col+1
}

legend('top', legend = c('tab1', 'tab2','tab3','tab4'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.7)


##### Generic plotting code
#1=yr, 2=pop size, 3=propmig, 4=He, 5=Ho, 6=fis, 7=nadult, 8=sxratio, 9=nmig, 10=fst, 11=replicate, 12=paramset, 13=noffspring, 14=fstvsource, 15=fisvsource
var = 10
smry = rbind(tab1,tab2,tab3,tab4)
smry = rbind(tab1,tab2,tab5,tab6)
range(smry[,var])
if(anyNA(smry[,var]==TRUE)){
  hold<- na.omit(smry)
  smry <- hold
}


ymin <- round(min(smry[,var]), digits = 2)#-.1
ymax <- round(max(smry[,var]), digits = 2)#+.1
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 250
offsets <- c(-0.5, 0, 0.5, 0.1) #c(-0.2, -0.1, 0, 0.1, 0.2)
orig.xs <- c(1, 100, 150, 201, 250) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 2

## make plot
plot(0,0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', main = 'fst @comp4', xlab = 'Generation Time', ylab = 'Variable of Interest',
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
#axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
axis(1, at = c(0, 100, 150, 200, 250), labels = c('0','100','150','200', '250'), cex.axis = text.size)
#abline(h = 0, lty = 2)

col <- 1
for(c in unique(smry[,12])){
  print(c)
  temp <- smry[smry[,12] == c,, drop=FALSE] #separate by parameter set
  
  y1<-temp[temp[,1] == orig.xs[1],,]
  y2<-temp[temp[,1] == orig.xs[2],,]
  y3<-temp[temp[,1] == orig.xs[3],,]
  y4<-temp[temp[,1] == orig.xs[4],,]
  y5<-temp[temp[,1] == orig.xs[5],,]
  
  xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #columns <- c(18, 19, 20, 21)
  lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
  points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var])), 
         y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var])), 
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
  col <- col+1
}

legend('topleft', legend = c('mig=0', 'mig=3x,25indv@y=175','high source het','low source het'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.2)

##NEED TO FIGURE OUT A WAY TO DO A POLYGON/GO ACROSS ALL YEARS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#create names for data so taht it is easier to call and change when the data change
yr       = smry[,1]   #year
n        = smry[,2]   #population size
propmig  = smry[,3]   #proportion of pop that are migrants
He       = smry[,4]   #He
Ho       = smry[,5]   #Ho
fis      = smry[,6]   #fis vs initialized pop
nadult   = smry[,7]   #number of indv that have reached maturity
sx       = smry[,8]   #sex ratio
nmig     = smry[,9]   #number of migrants
fst      = smry[,10]  #fst vs initialized pop
rep      = smry[,11]  #replicate number
noff     = smry[,13]  #number of offspring produced that year
fstvs    = smry[,14]  #fst vs the source pop
fisvs    = smry[,15]  #fis vs the source pop

#What is the best way to plot?? do I put all data in one dataset and then do a loop?
#or keep all data seperate so that it is easier to do the colors, etc?


png("popsize_overtime.png")
plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, max(yr)), ylim=c(0, max(n))) 
points(yr, n, col="firebrick")
dev.off()

png("Ho_overtime.png")
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)+.1)))
points(yr,Ho,col="orchid")
points(tab5[,1],tab5[,5],col="dodgerblue")
points(tab0[,1],tab0[,5],col="firebrick")
points(tab6[,1],tab6[,5],col="gold")
points(tab9[,1],tab9[,5],col="springgreen")
legend('top', legend = c('mig=1', 'mig=0','mig=3x','mig=1x'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.5)
legend('bottom', legend = c('mig=1', 'mig=0','mig=1x','mig=3x ALL AT .1-.2'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.5)

dev.off()

png("fis_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)-.1), (max(fis)+.1)))
points(yr,fis,col="dodgerblue")
points(tab5[,1],tab5[,6],col="dodgerblue")
points(tab0[,1],tab0[,6],col="firebrick")
points(tab6[,1],tab6[,6],col="gold")
points(tab9[,1],tab9[,6],col="springgreen")
dev.off()

png("sexratio_overtime.png")
plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
points(yr,sx,col="black")
points(tab5[,1],tab5[,8],col="dodgerblue")
points(tab0[,1],tab0[,8],col="firebrick")
points(tab6[,1],tab6[,8],col="gold")
points(tab9[,1],tab9[,8],col="springgreen")
dev.off()

png("fst_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.1), (max(fst)+.1)))
points(yr,fst,col="pink")
points(tab5[,1],tab5[,10],col="dodgerblue")
points(tab0[,1],tab0[,10],col="firebrick")
points(tab6[,1],tab6[,10],col="gold")
points(tab9[,1],tab9[,10],col="springgreen")
dev.off()

png("noffspring_overtime.png")
plot(-100, -100 , xlab="year", ylab="number of offpsring produced", xlim=c(0, max(yr)), ylim=c(0, (max(noff))))
points(yr,noff,col="yellow")
points(tab5[,1],tab5[,13],col="dodgerblue")
points(tab0[,1],tab0[,13],col="firebrick")
points(tab6[,1],tab6[,13],col="gold")
points(tab9[,1],tab9[,13],col="springgreen")
dev.off()

png("fstVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0, 0.25))
points(yr,fstvs,col="goldenrod")
points(tab5[,1],tab5[,14],col="dodgerblue")
points(tab0[,1],tab0[,14],col="firebrick")
points(tab6[,1],tab6[,14],col="gold")
points(tab9[,1],tab9[,14],col="springgreen")
dev.off()

png("fisVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis vs source", xlim=c(0, max(yr)), ylim=c(-.5, 1))
points(yr,fisvs,col="green")
points(tab5[,1],tab5[,15],col="dodgerblue")
points(tab0[,1],tab0[,15],col="firebrick")
points(tab6[,1],tab6[,15],col="gold")
points(tab9[,1],tab9[,15],col="springgreen")
dev.off()




########################################################################
########################################################################
#REPRODUCTIVE SUCCESS
########################################################################

rep7 = read.table("ABM_run.11.14.22_7a_all_repsuc.csv", header=T, sep=",")
rep8 = read.table("ABM_run.11.14.22_8a_all_repsuc.csv", header=T, sep=",")
rep9 = read.table("ABM_run.11.14.22_9a_all_repsuc.csv", header=T, sep=",")


#give each parameter set a unique identifier
rep7[,10] <- "g"
rep8[,10] <- "h"
rep9[,10] <- "i"

> plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(0, 250), ylim=c(0, 2.3))
> points(rep7[,1],rep7[,3])
> points(rep8[,1],rep8[,3], col="dodgerblue")
> points(rep9[,1],rep9[,3], col="firebrick")
> all<- rbind(rep7,rep8,rep9)

> plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(0, 250), ylim=c(0, 475))
> points(rep7[,1],rep7[,2])
> points(rep8[,1],rep8[,2], col="dodgerblue")
> points(rep9[,1],rep9[,2], col="firebrick")

> plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(0, 250), ylim=c(0, .25))
> points(rep7[,1],rep7[,7])
> points(rep8[,1],rep8[,7], col="dodgerblue")
> points(rep9[,1],rep9[,7], col="firebrick")
