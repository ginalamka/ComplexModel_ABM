#MakeFigs

setwd("C:/Users/ginab/Box/New Computer/Auburn/Data/ComplexModel_ABM/Output_local/holding") #setwd("C:/Users/Gina/Desktop/2022/ComplexModel_ABM") 
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
tab = read.table("ABM_run.12.9.22_0c_all_summary.csv", header=T, sep=",")
tab = read.table("ABM_run.1.5.23_A_all_summary.csv", header=T, sep=",")
tab = read.table("ABM_run.12.14.22_1a_all_summary.csv", header=T, sep=",")
tab = read.table("ABM_run.12.10.22_1A_all_summary.csv", header=T, sep=",") #this one has crashes
tab = read.table("ABM_run.1.11.23_C_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.9.23_D_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.11.23_B_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.11.23_E_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.18.23_c_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.18.23_d_all_summary.csv", header=T, sep=",") 
tab = read.table("ABM_run.1.18.23_a_all_summary.csv", header=T, sep=",") 
tab = read.table("run_d_quickie_summary.csv", header=T, sep=",") 
tab = read.table("run_e_quickie_summary.csv", header=T, sep=",") 
tab = read.table("run_a_quickie_summary.csv", header=T, sep=",") 
tab = read.table("run_f_quickie_summary.csv", header=T, sep=",")
ted = read.table("ABMrun_3.15.23_e_quickie_summary.csv", header=T, sep=",")
tat = read.table("ABMrun_3.1.23_e_quickie_summary.csv", header=T, sep=",")
tab = read.table("ABMrun_3.15.23_emany2_all_summary.csv", header=T, sep=",")
smry = read.table("ABMrun_3.3.23_e_quickie_summary.csv", header=T, sep=",")

#ABG, CDH, EFI
#tat = 1,2,3
#tab = 4,5,6
#smry = 7,8

ted[,12] = ted[,12]+3
table(ted[,12])

tat[,12] = tat[,12]+6
table(tat[,12])

smry = rbind(tat, tab, smry)

library(colorspace)
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(5, "Pastel1") #ghibli_palette('PonyoLight')[4]
"#C87A8A""#6B9D59""#5F96C2""#909646" "#00A396" "#9189C7"
gt.cols<- c("#C87A8A","#00A396","#9189C7","lightgreen","orange" )
gt.cols<- c("#C87A8A","#00A396","#6B9D59","#AC8C4E")
"orange" "lightgreen""#C87A8A" "#BF826A" "#AC8C4E" "#909646" "#6B9D59" "#39A277" "#00A396" "#169FB1" "#5F96C2" "#9189C7" "#B37EBE" "#C578A8"
"#FFC5D0" "#E2D4A8" "#A8E1BF" "#A4DDEF" "#E4CBF9"
v = matrix(nrow=1755,ncol=1) 
v[1:350+1,1] = 1
v[351:700+2,1]=2
v[701:1050+3,1]=3
v[1051:1400+4,1]=4
v[1401:1750+5]=5

grp1 <- tab[tab[,12]<=8,,drop=FALSE]
grp2 <- tab[(tab[,12]>8)&(tab[,12]<17),,drop=FALSE]
grp3 <- tab[tab[,12]>=17,,drop=FALSE]

grp1a <- grp1[grp1[,12]<=4,,drop=FALSE]
grp1b <- grp1[grp1[,12]>4,,drop=FALSE]

grp1 <- tab[tab$maxage==3,,drop=FALSE]
grp1a <- grp1[grp1$broodsize==4,,drop=FALSE]
grp1b <- grp1[grp1$broodsize==6,,drop=FALSE]

grp1 <- tab[tab$r0==0.1,,drop=FALSE]

smry = tab

{
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

smry <- tab   #compX
  
#~~~Avril's Code
### set colors
library(ghibli)
library(scales)
library(colorspace)
gt.cols <- qualitative_hcl(4, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(4, "Pastel1") #ghibli_palette('PonyoLight')[4]
demoplot(gt.cols, "bar")
demoplot(lt.gt.col, "bar")

ymin <- 0.2
ymax <- round(max(smry[,5]), digits = 2)+0.1
ln.alph <- 0.5
pt.alph <- 1
diff <- 0.15
xmin <- 0
xmax <- 350
offsets <- c(-0.5, 0, 0.5) #c(-0.2, -0.1, 0, 0.1, 0.2)
orig.xs <- c(1, 100, 150, 200, 250) #c(1:250) #c(1, 100, 150, 200, 250)
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
var = 3
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
}
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
para     = smry[,12]
noff     = smry[,13]  #number of offspring produced that year
fstvs    = smry[,14]  #fst vs the source pop
fisvs    = smry[,15]  #fis vs the source pop
col      = smry[,12] #para  #c("#E16A86", "#909800", "#00AD9A", "#9183E6")#smry[,12] #-4
#What is the best way to plot?? do I put all data in one dataset and then do a loop?
#or keep all data seperate so that it is easier to do the colors, etc

############Generic Code

#variable
plt = smry[,6]
plt_name = "FIS"

plot(-100, -100 , xlab="year", ylab=plt_name, xlim=c(0, max(yr)), ylim=c((min(plt)), (max(plt)))) 
for(d in unique(para)){
  da = smry[para==d,,drop=FALSE]
  for(y in unique(da[,20])){ #group
    dat = da[da[,20]==y,,drop=FALSE]
    if(d <= 9){
      lines(dat[,1],dat[,6],col=alpha(gt.cols[dat[,12]],0.4),lwd=3.5)
    }
    
  }
}
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,6], by = list(dat[,1]), FUN = "mean")
  if(d<=9){lines(avg[,1], avg[,2], col=alpha(gt.cols[d],0.99), lwd = 5)} #gt.cols[d]
}

############Averages
plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, max(yr)), ylim=c(0, max(n))) 
for(p in unique(para)){
  dat = smry[smry[,12]==p,,drop=FALSE]
  avg = aggregate(x = dat[,2], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col="black", lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) #max(yr)
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d<8){lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 5)} #gt.cols[d]
}
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d == 1|d==2){
    lines(avg[,1], avg[,2], col="black", lty=2, lwd = 6) 
  }else if(d > 3){
    lines(avg[,1], avg[,2], col="black", lwd = 4) #gt.cols[d]
  }else{next}
  
}
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d == 1|d==2){
    lines(avg[,1], avg[,2], col="grey", lty=2, lwd = 6) 
  }else if(d > 3){
    lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 4) #gt.cols[d]
  }else{next}
  
}
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d > 6){
    lines(avg[,1], avg[,2], col="black", lwd = 4) #gt.cols[d]
  }else{next}
  
}


plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.01), (max(fst))))
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,10], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 8)
}

plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0, max(fstvs)))
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,14], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col="black", lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="prop migrant SNPs", xlim=c(0, max(yr)), ylim=c(0, max(smry[,17])))
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,17], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col="black", lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)), (max(fis)))) 
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,6], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,8], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}

######LEGEND
plot(-100, -100 , xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
#miggy.V       = c(0,"a","b","c")  #"a"=one mig per gen, "b"=1xof50@175, "c"=3xpf25@175|201|225  #migration parameter type
legend('center', legend = c('mig=0',
                            'mig=1migpergen_random mates',
                            'mig=1migpergen y>151_randommates', 
                            'mig=0',
                            'mig=1migpergen_migrantspreffered',
                            'mig=1migpergen y>151_migrantspreffered'), 
       col = gt.cols, pch = 19, bty = 'n', cex = 1.5, pt.cex = 3, horiz = FALSE, x.intersp = 0.5)



######Points
{
pm = smry[smry$propmig!=0,,drop=FALSE]
plot(pm$propmig~pm$Ho,col=gt.cols[col], pch=16)
#how does the prop or migrants influence the heterozygosity?
y<-lmer(Ho~propmig + (1|parameterset), data=smry)
y<-lmer(Ho~propmig+yr*propmig + (1|parameterset), data=smry)
summary(y)
#http://127.0.0.1:19921/graphics/plot_zoom_png?width=1147&height=900
#think about propmig~LRS also -- will need to merge the datasets

png("popsize_overtime.png")
plot(-100, -100 , xlab="year", ylab="population size", xlim=c(0, max(yr)), ylim=c(0, max(n))) 
points(yr, n, col=gt.cols[col], pch=16) #"firebrick"
dev.off()

png("Ho_overtime.png")
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) 
points(yr,smry[,18],col=gt.cols[col])
points(tab5[,1],tab5[,5],col="dodgerblue")
points(tab0[,1],tab0[,5],col="firebrick")
points(tab6[,1],tab6[,5],col="gold")
points(tab9[,1],tab9[,5],col="springgreen")
legend('top', legend = c('mig=1', 'mig=0','mig=3x','mig=1x'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = TRUE, x.intersp = 0.5)
legend('bottom', legend = c('mig=1', 'mig=0','mig=1x','mig=3x ALL AT .1-.2'), col = c("dodgerblue", "firebrick", "gold", "springgreen"), pch = 19, bty = 'n', cex = text.size, pt.cex = pt.cex, horiz = FALSE, x.intersp = 0.5)

dev.off()

for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  #avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d > 3){
    points(dat[,1], dat[,5], col=gt.cols[d]) #gt.cols[d]
  }else{next}
  
}

#install.packages("scales")
library(scales)
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(0, max(yr)), ylim=c((min(Ho)), (max(Ho)))) 
for(d in unique(smry[,12])){
  da = smry[para==d,,drop=FALSE]
  for(y in unique(da[,20])){
    dat = da[da[,20]==y,,drop=FALSE]
    if(d <= 9){
      lines(dat[,1],dat[,18],col=alpha(gt.cols[dat[,12]],0.4),lwd=3.5)
    }
    
  }
}
for(d in unique(para)){
  dat = smry[smry[,12]==d,,drop=FALSE]
  avg = aggregate(x = dat[,5], by = list(dat[,1]), FUN = "mean")
  if(d<=9){lines(avg[,1], avg[,2], col=alpha(gt.cols[d],0.99), lwd = 5)} #gt.cols[d]
}
for(t in unique(tat[,12])){
  tata = tat[tat[,12]==t,,drop=FALSE]
  avg = aggregate(x = tata[,5], by = list(tata[,1]), FUN = "mean")
  if(t==3){lines(avg[,1], avg[,2], col=alpha(ex.cols[tata[,12]],0.99), lwd = 5)}
  #for(p in unique(tata[,11])){
  #  ta = tata[tata[,11]==p,,drop=FALSE]
  #  if(t<4){lines(ta[,1],ta[,18],col=alpha(ex.cols[ta[,12]],0.6),lwd=3)}
  #}
}
for(t in unique(ted[,12])){
  teda = ted[ted[,12]==t,,drop=FALSE]
  avg = aggregate(x = teda[,5], by = list(teda[,1]), FUN = "mean")
  if(t<9){lines(avg[,1], avg[,2], col=ad.cols[teda[,12]], lwd = 5)}
  #for(p in unique(tata[,11])){
  #  ta = tata[tata[,11]==p,,drop=FALSE]
  #  if(t<4){lines(ta[,1],ta[,18],col=alpha(ex.cols[ta[,12]],0.6),lwd=3)}
  #}
}

ex.cols = c("#9189C7","#FFC5D0","#E2D4A8")
ad.cols = c("purple","hotpink")
#"#C87A8A","#00A396",

png("fis_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, max(yr)), ylim=c((min(fis)), (max(fis)))) 
points(yr,fis,col=gt.cols[col])
points(tab5[,1],tab5[,6],col="dodgerblue")
points(tab0[,1],tab0[,6],col="firebrick")
points(tab6[,1],tab6[,6],col="gold")
points(tab9[,1],tab9[,6],col="springgreen")
dev.off()


png("sexratio_overtime.png")
plot(-100, -100 , xlab="year", ylab="sex ratio", xlim=c(0, max(yr)), ylim=c((min(sx)-.1), (max(sx)+.1)))
points(yr,sx,col=gt.cols[col])
points(tab5[,1],tab5[,8],col="dodgerblue")
points(tab0[,1],tab0[,8],col="firebrick")
points(tab6[,1],tab6[,8],col="gold")
points(tab9[,1],tab9[,8],col="springgreen")
dev.off()

png("fst_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, max(yr)), ylim=c((min(fst)-.01), (max(fst))))
points(yr,fst,col=gt.cols[col], pch=16)
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
plot(-100, -100 , xlab="year", ylab="Fst vs source", xlim=c(0, max(yr)), ylim=c(0.05, max(fstvs)))
points(yr,fstvs,col=gt.cols[col])
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

plot(-100, -100 , xlab="year", ylab="prop migrant SNPs", xlim=c(0, max(yr)), ylim=c(0, 1))
points(yr,smry[,17],col=gt.cols[col],pch=16)

plot(-100, -100 , xlab="Ho drift SNPs", ylab="Ho all SNPS", xlim=c(0, 1), ylim=c(0, 1))
points(Ho,smry[,18],col=gt.cols[col])
abline(a=0,b=1)
}

########################################################################
########################################################################
#REPRODUCTIVE SUCCESS
########################################################################

#NOTE--- as of 1.13.23, migrants ARE included in LRS values 

rep7 = read.table("ABM_run.11.14.22_7a_all_repsuc.csv", header=T, sep=",")
rep8 = read.table("ABM_run.11.14.22_8a_all_repsuc.csv", header=T, sep=",")
rep9 = read.table("ABM_run.11.14.22_9a_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.5.23_A_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.9.23_D_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.11.23_C_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.11.23_B_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.11.23_E_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.18.23_c_all_repsuc.csv", header=T, sep=",")
rep = read.table("ABM_run.1.18.23_d_all_repsuc_cut.csv", header=T, sep=",")
rep_ = read.table("run_d_quickie_repsuc.csv", header=T, sep=",")
rep = read.table("run_e_quickie_repsuc.csv", header=T, sep=",")
rep = read.table("run_a_quickie_repsuc.csv", header=T, sep=",")
rep = read.table("run_f_quickie_repsuc.csv", header=T, sep=",")
red = read.table("ABMrun_3.3.23_f_quickie_repsuc.csv", header=T, sep=",")
rev = read.table("ABMrun_3.1.23_e_quickie_repsuc.csv", header=T, sep=",")
rep = read.table("ABMrun_3.15.23_e_quickie_repsuc.csv", header=T, sep=",")
rep[is.na(rep)] <- 0
red[is.na(red)] <- 0
rev[is.na(rev)] <- 0

#ABG, CDH, EFI
#rep = 1,2,3
#red = 4,5,6
#rev = 7,8

#give each parameter set a unique identifier
rep[,10] = rep[,10]+3
table(red[,10])

rev[,10] = rev[,10]+6
table(rev[,10])

rep = rbind(rep, red, rev)

library(colorspace)
gt.cols <- qualitative_hcl(3, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(3, "Pastel1") #ghibli_palette('PonyoLight')[4]


#het1<- rep[rep[,10]<=4,,drop=FALSE]
#het2<- rep[(rep[,10]>4)&(rep[,10]<9),,drop=FALSE]
#het2<- rep[rep[,10]>=5,,drop=FALSE]

library(ghibli)
library(scales)
gt.cols <- ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- ghibli_palette('PonyoLight')[4]
library(colorspace)
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(6, "Pastel1") #ghibli_palette('PonyoLight')[4]

col = rep[,10] #-4  #color by parameter
para = rep[,10]
yr  = rep[,1]   #year
n   = rep[,2]   #n born
LRS = rep[,3]   #mean LRS
SD  = rep[,4]   #SD of LRS
LRSf= rep[,5]   #female LRS
LRSm= rep[,6]   #male LRS
RRS = rep[,7]   #mean RRS
SDR = rep[,8]   #SD of RRS
replic = rep[,9]   #replicate
LRSmig = rep[,11] #LRS of migrants
LRSnat = rep[,12] #LRS of natives

#Averages
plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, 4)) #
for(d in unique(para)){
  dat = rep[rep[,10]==d,,drop=FALSE]
  avg = aggregate(x = dat[,3], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}

#Relative repro success
rel = matrix(nrow=nrow(rep),ncol=1)
rel = rep[,12]/rep[,11]
rep = cbind(rep,rel)

plot(-100, -100 , xlab="year", ylab="LRSnatives/LRSmigrants", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #
for(d in unique(para)){
  dat = rep[rep[,10]==d,,drop=FALSE]
  avg = aggregate(x = dat[,15], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(RRS)))
for(d in unique(para)){
  dat = rep[rep[,10]==d,,drop=FALSE]
  avg = aggregate(x = dat[,7], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}

plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
for(d in unique(para)){
  dat = rep[rep[,10]==d,,drop=FALSE]
  avg = aggregate(x = dat[,2], by = list(dat[,1]), FUN = "mean")
  lines(avg[,1], avg[,2], col=gt.cols[d], lwd = 3)
}
 
plot(-100, -100 , xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
legend('top', legend = c('mig=0',
                              'mig=1migpergen',
                              'mig=1migpergen y<100 & >151', 
                              'mig=100@yr151',
                              'mig=25@yr151,165,181,195',
                              'mig=1migpergen y>151',
                         'mig=100@yr125',
                         'mig=25@yr125,140,155,170',
                         'another'), 
       col = ("black"), pch = 19, bty = 'n', cex = 1.75, pt.cex = 5, horiz = FALSE, x.intersp = 0.5)
#miggy.V       = c(0,"a","b","c")  #"a"=one mig per gen, "b"=1xof50@175, "c"=3xpf25@175|201|225  #migration parameter type
legend('center', legend = c('mig=1migpergen_NOAllee', 'mig=1migpergen_WAllee','mig=1migpergen_NOAllee', 'mig=0_WAllee','mig=1migpergen_WAllee','mig=0', 'mig=100@yr 175,201,225','mig=1mpg y <100 & >150','mig=2migpergen y>150'), col = gt.cols[1:6], pch = 19, bty = 'n', cex = 1.75, pt.cex = 2, horiz = FALSE, x.intersp = 0.5)

####POints
{
plot(-100, -100 , xlab="year", ylab="LRS", xlim=c(min(yr), max(yr)), ylim=c(0, 3))#max(LRS)
points(yr, LRS, col=gt.cols[col], pch=19)
points(rep8[,1],rep8[,3], col="dodgerblue")
points(rep9[,1],rep9[,3], col="firebrick")

plot(-100, -100 , xlab="year", ylab="nborn", xlim=c(min(yr), max(yr)), ylim=c(0, max(n)))
points(yr, n, col=gt.cols[col])
points(rep8[,1],rep8[,2], col="dodgerblue")
points(rep9[,1],rep9[,2], col="firebrick")

plot(-100, -100 , xlab="year", ylab="RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(RRS)))
points(yr, RRS, col=gt.cols[col], pch=19)
points(rep8[,1],rep8[,7], col="dodgerblue")
points(rep9[,1],rep9[,7], col="firebrick")

plot(-100, -100 , xlab="year", ylab="LRS of females", xlim=c(min(yr), max(yr)), ylim=c(0, max(LRSf)))
points(yr, LRSf, col=gt.cols[col])

plot(-100, -100 , xlab="year", ylab="LRS of males", xlim=c(min(yr), max(yr)), ylim=c(0, max(LRSm)))
points(yr, LRSm, col=gt.cols[col], pch=16)

plot(-100, -100 , xlab="year", ylab="SD of LRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(SD)))
points(yr, SD, col=gt.cols[col])

plot(-100, -100 , xlab="year", ylab="SD of RRS", xlim=c(min(yr), max(yr)), ylim=c(0, max(SDR)))
points(yr, SDR, col=gt.cols[col])

plot(-100, -100 , xlab="year", ylab="LRS of migrants", xlim=c(min(yr), max(yr)), ylim=c(0, 10)) #max(LRSmig)
points(yr, LRSmig, col=gt.cols[col])

plot(-100, -100 , xlab="year", ylab="LRS of natives", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #max(LRSnat)
points(yr, LRSnat, col=gt.cols[col])

plot(-100, -100 , xlab="year", ylab="LRS of migrants/natives", xlim=c(min(yr), max(yr)), ylim=c(0, 5)) #max(LRSnat)
points(yr, LRSmig/LRSmig, col=gt.cols[col], pch=16)
}

##### Generic plotting code
{
#1=yr, 2=nborn, 3=meanLRS, 4=SD, 5=LRSFemale, 6=LRSmale, 7=meanRRS, 8=SDRRS, 9=replicate, 10=parameterset, 11=LRSmigrants, 12=LRSnatives, 13=project, 14=group
var = 7

range(rep[,var])
if(anyNA(rep[,var]==TRUE)){
  hold<- na.omit(rep)
  rep <- hold
}

gt.cols <- qualitative_hcl(8, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(8, "Pastel1")


ymin <- round(min(rep[,var]), digits = 2)#-.1
ymax <- round(max(rep[,var]), digits = 2)#+.1
ln.alph <- 0.5
pt.alph <- 1.25
diff <- 0.15
xmin <- 0
xmax <- 350
offsets <- c(-0.5, 0, 0.5, 0.1) #c(-0.2, -0.1, 0, 0.1, 0.2)
orig.xs <- c(1, 100, 150, 201, 250, 300, 345) #years of interest 
text.size <- 1.75
pt.cex <- 1.25
lwd <- 2

## make plot
plot(0,0, xlim = c(xmin, xmax), ylim = c(ymin, ymax), 
     xaxt = 'n', main = 'RRS with maxage=3', xlab = 'Generation Time', ylab = 'RRS',
     cex.axis = text.size, cex.lab = text.size, yaxt = 'n')
#axis(2, at = c(-0.1, 0, 0.1, 0.2), cex.axis = text.size)
axis(1, at = c(0, 100, 150, 200, 250, 300, 350), labels = c('0','100','150','200', '250', '300', '350'), cex.axis = text.size)
#abline(h = 0, lty = 2)

col <- 1
for(c in unique(rep[,10])){
  #print(c)
  temp <- rep[rep[,10] == c,, drop=FALSE] #separate by parameter set
  
  y1<-temp[temp[,1] == orig.xs[1],,]
  y2<-temp[temp[,1] == orig.xs[2],,]
  y3<-temp[temp[,1] == orig.xs[3],,]
  y4<-temp[temp[,1] == orig.xs[4],,]
  y5<-temp[temp[,1] == orig.xs[5],,]
  y6<-temp[temp[,1] == orig.xs[6],,]
  y7<-temp[temp[,1] == orig.xs[7],,]
  
  xs <- orig.xs + offsets[col]  #dont forget you're in a loop, dummy
  #columns <- c(18, 19, 20, 21)
  lines(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var])), col = alpha(gt.cols[col], ln.alph), lwd = lwd)
  points(xs, c(mean(y1[,var]), mean(y2[,var]), mean(y3[,var]), mean(y4[,var]), mean(y5[,var]), mean(y6[,var]), mean(y7[,var])), col = alpha(gt.cols[col], pt.alph), pch = 19, cex = pt.cex)
  arrows(x0 = xs, y0 = c(mean(y1[,var])-sd(y1[,var]), mean(y2[,var])-sd(y2[,var]), mean(y3[,var])-sd(y3[,var]), mean(y4[,var])-sd(y4[,var]), mean(y5[,var])-sd(y5[,var]), mean(y6[,var])-sd(y6[,var]), mean(y7[,var])-sd(y7[,var])), 
         y1 = c(mean(y1[,var])+sd(y1[,var]), mean(y2[,var])+sd(y2[,var]), mean(y3[,var])+sd(y3[,var]), mean(y4[,var])+sd(y4[,var]), mean(y5[,var])+sd(y5[,var]), mean(y6[,var])+sd(y6[,var]), mean(y7[,var])+sd(y7[,var])), 
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
}

#############################################################################3
#############################################################################
ne = read.table("run_aa_quickie_necounts.csv", header=T, sep=",")

library(colorspace)
gt.cols <- qualitative_hcl(6, "Dark2") #ghibli_palette('PonyoMedium')#[4]
lt.gt.col <- qualitative_hcl(6, "Pastel1") #ghibli_palette('PonyoLight')[4]

col = ne[,9] 


library(lme4)
y<-lmer(Ho~propmig + (1|parameterset), data=smry)

max(ne[,3])

plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 200))
points(ne[,1], ne[,2], col="pink")  #effective moms
points(ne[,1], ne[,3], col="blue")  #effective dads

plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 600))
points(ne[,1], ne[,6], col="pink")  #possible moms
points(ne[,1], ne[,7], col="blue")  #possible dads

plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 30))
points(ne[,1], ne[,8], col="pink")  #effective migrants

for(i in 1:nrow(ne)){
  NE <- (4*ne[i,2]*ne[i,3])/(ne[i,2]+ne[i,3])           #Ne = 4*number males*number females/number males+number females
  ne[i,10] <- NE
}
plot(-100, -100 , xlab="year", ylab="Ne", xlim=c(0, 350), ylim=c(0, 300))
points(ne[,1], ne[,10], col="pink")
points(ne[,1], ne[,8], col="purple")

for(j in 1:nrow(ne)){
  FST <- 1/(1+(4*ne[j,10]*ne[j,8]))                     #1/(1+4*effective pop size*[effective] migrants) 
  ne[j,11] <- FST
}
plot(-100, -100 , xlab="year", ylab="1/1+(4*Ne*Nm)", xlim=c(0, 350), ylim=c(0, .03))
points(ne[,1], ne[,11], col= gt.cols[col])



smry2 <- smry[smry[,12]==2,,drop=FALSE]
smry2<-smry2[smry2[,1]!=0,,drop=FALSE]
ne2 <- ne[ne[,9]==2,,drop=FALSE]
plot(-100, -100 , xlab="year", ylab="FST", xlim=c(0, 350), ylim=c(0, .02))
points(ne2[,1], ne2[,11], col= "orange")


for(e in 1:nrow(ne)){
  RAT <- ne[e,10]/ne[e,5]                                #ratio Ne/Nc == small Ne relative to Nc (that is, small Ne/Nc ratio) will lose gene diversity more quickly 
  ne[e,12] <- RAT
}
plot(-100, -100 , xlab="year", ylab="ratio Ne/Nc", xlim=c(0, 350), ylim=c(0, 1))
points(ne[,1], ne[,12], col= gt.cols[col])



bb[,12]=2
cc[,12]=3
smry = rbind(aa,bb,cc)
smry = smry[smry[,1]!=0,,drop=FALSE]
data = cbind(ne, smry)
head(data)

plot(-100, -100 , xlab="Fst", ylab="Ne = 4*number males*number females/number males+number females", xlim=c(0, 275), ylim=c(0, 1))
plot(data[,10]~data[,22])  #Ne = 4*number males*number females/number males+number females   ~  Fst (calc in sim)
plot(data[,11]~data[,22])  #1/(1+4*effective pop size*[effective] migrants) 
plot(data[,12]~data[,22])    #ratio Ne/Nc    <-- interestinggg
