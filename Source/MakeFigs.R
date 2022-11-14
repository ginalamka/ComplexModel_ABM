#MakeFigs

setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/Output/holding") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

tab1 = read.table("ABM_run.11.8.22_1a_all_summary.csv", header=T, sep=",")  
tab2 = read.table("summary_ABM_run.10.12.22_d_all.csv", header=T, sep=",")
tab3 = read.table("ABM_run.11.7.22_3a_all_summary.csv", header=T, sep=",")
tab4 = read.table("ABM_run.11.7.22_2a_all_summary.csv", header=T, sep=",")
tab5 = read.table("ABM_run.11.8.22_4a_all_summary.csv", header=T, sep=",")
tab6 = read.table("ABM_run.11.8.22_5a_all_summary.csv", header=T, sep=",")

#Data
{
#summary_ABM_run.10.12.22_d_all.csv >> zero migration
#ABM_run.11.8.22_1a_all_summary.csv >> 1 mig per generation, starting heterozy in source pop = .4-.6
#ABM_run.11.7.22_2a_all_summary.csv >> one migration of 50 indv at year 175, starting heterozy in source pop = .4-.6
#ABM_run.11.7.22_3a_all_summary.csv >> three migrations of 25 indv at years 175, 201, 225, starting heterozy in source pop = .4-.6
#ABM_run.11.8.22_4a_all_summary.csv >> 1 mig per generation, starting heterozy in source pop = .8-.9
#ABM_run.11.8.22_5a_all_summary.csv >> 1 mig per generation, starting heterozy in source pop = .1-.2
}

#give each parameter set a unique identifier
tab1[,12] <- "a"
tab2[,12] <- "b"
tab3[,12] <- "c"
tab4[,12] <- "d"
tab5[,12] <- "e"
tab6[,12] <- "f"
smry = rbind(tab1,tab2,tab3, tab4)
smry = rbind(tab1,tab2,tab5, tab6)

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
var = 5
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
     xaxt = 'n', main = 'Ho with different migration intensities', xlab = 'Generation Time', ylab = 'Variable of Interest',
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

legend('topleft', legend = c('mig=1', 'mig=0','mig=3x','mig=1x'), col = gt.cols, pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.2)

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
points(tab1[,1],tab1[,5],col="dodgerblue")
points(tab2[,1],tab2[,5],col="firebrick")
points(tab5[,1],tab5[,5],col="gold")
points(tab6[,1],tab6[,5],col="springgreen")
legend('top', legend = c('mig=1', 'mig=0','mig=3x','mig=1x'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.5)
legend('bottom', legend = c('mig=1', 'mig=0','high source het','low source het'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.5)

dev.off()

png("fis_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)-.1), (max(fis)+.1)))
points(yr,fis,col="dodgerblue")
points(tab1[,1],tab1[,6],col="dodgerblue")
points(tab2[,1],tab2[,6],col="firebrick")
points(tab5[,1],tab5[,6],col="gold")
points(tab6[,1],tab6[,6],col="springgreen")
dev.off()

png("sexratio_overtime.png")
plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
points(yr,sx,col="black")
points(tab1[,1],tab1[,8],col="dodgerblue")
points(tab2[,1],tab2[,8],col="firebrick")
points(tab5[,1],tab5[,8],col="gold")
points(tab6[,1],tab6[,8],col="springgreen")
dev.off()

png("fst_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.1), (max(fst)+.1)))
points(yr,fst,col="pink")
points(tab1[,1],tab1[,10],col="dodgerblue")
points(tab2[,1],tab2[,10],col="firebrick")
points(tab5[,1],tab5[,10],col="gold")
points(tab6[,1],tab6[,10],col="springgreen")
dev.off()

png("noffspring_overtime.png")
plot(-100, -100 , xlab="year", ylab="number of offpsring produced", xlim=c(0, max(yr)), ylim=c(0, (max(noff))))
points(yr,noff,col="yellow")
points(tab1[,1],tab1[,13],col="dodgerblue")
points(tab2[,1],tab2[,13],col="firebrick")
points(tab5[,1],tab5[,13],col="gold")
points(tab6[,1],tab6[,13],col="springgreen")
dev.off()

png("fstVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0, 0.25))
points(yr,fstvs,col="goldenrod")
points(tab1[,1],tab1[,14],col="dodgerblue")
points(tab2[,1],tab2[,14],col="firebrick")
points(tab5[,1],tab5[,14],col="gold")
points(tab6[,1],tab6[,14],col="springgreen")
dev.off()

png("fisVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis vs source", xlim=c(0, max(yr)), ylim=c(-.5, 1))
points(yr,fisvs,col="green")
points(tab1[,1],tab1[,15],col="dodgerblue")
points(tab2[,1],tab2[,15],col="firebrick")
points(tab5[,1],tab5[,15],col="gold")
points(tab6[,1],tab6[,15],col="springgreen")
dev.off()
