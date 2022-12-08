#this is for the complex model for aBM class
#this is taken somewhat from Janna's plot functions

#notes from Janna 11/19
#Do you remember the figures I showed in class where I plotted output varying only one parameter at a time? That's what I'd suggest. But, you def don't need ALL combinations. Probably you'll want like a control (no migrants?) and then you can use different colors to show output for levels of a particular variable.
#But, you'll still have to explore your output to know which variables are imporatn.
#(check Janna's figures from her paper to reference)

#info for error bars here
#https://www.benjaminbell.co.uk/2019/04/how-to-add-error-bars-in-r.html#:~:text=Many%20people%20would%20suggest%20that%20R%20does%20not,a%20vertical%20and%20horizontal%20arrow%20on%20a%20plot%3A


#cat theEND_6.20_p0_01.csv theEND_6.20_p0_02.csv theEND_6.20_p0_03.csv theEND_6.20_p0_04.csv theEND_6.20_p0_05.csv theEND_6.20_p0_06.csv theEND_6.20_p0_07.csv theEND_6.20_p0_08.csv theEND_6.20_p0_09.csv theEND_6.20_p0_10.csv theEND_6.20_p0_11.csv theEND_6.20_p0_12.csv theEND_6.20_p0_13.csv theEND_6.20_p0_14.csv theEND_6.20_p0_15.csv theEND_6.20_p0_16.csv theEND_6.20_p0_17.csv theEND_6.20_p0_18.csv theEND_6.20_p0_19.csv theEND_6.20_p0_20.csv > theEND_p0c.csv
#cat summary_repEND_d_01.csv summary_repEND_d_02.csv summary_repEND_d_03.csv summary_repEND_d_04.csv summary_repEND_d_05.csv summary_repEND_d_06.csv summary_repEND_d_07.csv summary_repEND_d_07.csv summary_repEND_d_09.csv summary_repEND_d_10.csv summary_repEND_d_11.csv summary_repEND_d_12.csv summary_repEND_d_13.csv summary_repEND_d_14.csv summary_repEND_d_15.csv summary_repEND_d_16.csv summary_repEND_d_17.csv summary_repEND_d_18.csv summary_repEND_d_19.csv summary_repEND_d_20.csv > repEND_7.5_d.csv

setwd("C:/Users/HP/Box/New Computer/Auburn/Data/ComplexModel_ABM/CSVs")
directory = getwd()
outdir = paste(directory, "/figs/", sep = "")

theEND1 = read.table("theEND_p1.csv", header=T, sep=",")
theEND2 = read.table("theEND_p2.csv", header=T, sep=",")
theEND3 = read.table("theEND_p3.csv", header=T, sep=",")
theEND4 = read.table("theEND_p4.csv", header=T, sep=",")
theEND5 = read.table("theEND_p5.csv", header=T, sep=",")
theEND6 = read.table("theEND_p6.csv", header=T, sep=",")
theEND7 = read.table("theEND_p7.csv", header=T, sep=",")
theEND8 = read.table("theEND_p8.csv", header=T, sep=",")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theEND1 = read.table("theEND_p1b.csv", header=T, sep=",")
theEND2 = read.table("theEND_p2b.csv", header=T, sep=",")
theEND3 = read.table("theEND_p3b.csv", header=T, sep=",")
theEND4 = read.table("theEND_p4b.csv", header=T, sep=",")
theEND5 = read.table("theEND_p5b.csv", header=T, sep=",")
theEND6 = read.table("theEND_p6b.csv", header=T, sep=",")
theEND7 = read.table("theEND_p7b.csv", header=T, sep=",")
theEND8 = read.table("theEND_p8b.csv", header=T, sep=",")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theEND1 = read.table("theEND_p1c.csv", header=T, sep=",")
theEND2 = read.table("theEND_p2c.csv", header=T, sep=",")
theEND3 = read.table("theEND_p3c.csv", header=T, sep=",")
theEND4 = read.table("theEND_p4c.csv", header=T, sep=",")
theEND5 = read.table("theEND_p5c.csv", header=T, sep=",")
theEND6 = read.table("theEND_p6c.csv", header=T, sep=",")
theEND7 = read.table("theEND_p7c.csv", header=T, sep=",")
theEND8 = read.table("theEND_p0c.csv", header=T, sep=",") #notice 0c is p8

tableA= read.table("tableforcertainyears_p1-8.csv", header=T, sep=",")
tableB= read.table("tableforcertainyears_p1b-8b.csv", header=T, sep=",")

fig1 <- tableA[tableA[,1] == 2 | tableA[,1] == 5 | tableA[,1] == 6,, drop=FALSE]
fig2 <- tableA[tableA[,1] == 1 | tableA[,1] == 3 | tableA[,1] == 4,, drop=FALSE]
fig3 <- tableB[tableB[,1] == 2 | tableB[,1] == 5 | tableB[,1] == 6,, drop=FALSE]
fig4 <- tableB[tableB[,1] == 1 | tableB[,1] == 3 | tableB[,1] == 4,, drop=FALSE]
write.table(fig1, paste(directory, "/tableforfig1.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(fig2, paste(directory, "/tableforfig2.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(fig3, paste(directory, "/tableforfig3.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)
write.table(fig4, paste(directory, "/tableforfig4.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)


f1 <- table[table[,1] == 4,,drop=FALSE]
fig1 <- read.table("tableforfig1.csv", header=T, sep=",")
fig1 <- rbind(fig1, f1)

f2 <- table[table[,1] == 5,,drop=FALSE]
fig2 <- read.table("tableforfig2.csv", header=T, sep=",")
fig2 <- rbind(fig2, f2)

f3 <- table[table[,1] == 3 |table[,1] == 7 ,,drop=FALSE]
fig3 <- read.table("tableforfig3.csv", header=T, sep=",")
fig3 <- rbind(fig3, f3)

f4 <- table[table[,1] == 2 |table[,1] == 6 ,,drop=FALSE]
fig4 <- read.table("tableforfig4.csv", header=T, sep=",")
fig4 <- rbind(fig4, f4)


#REMOVED#theEND = read.table("all_temp_param.csv", header=T, sep=",")



#THINK ABOUT REMOVING less than one from before since it is really 1 or 2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
theEND[,5] = as.numeric(theEND[,5])

theEND1[,12] <-1
theEND2[,12] <-2
theEND3[,12] <-3
theEND4[,12] <-4
theEND5[,12] <-5
theEND6[,12] <-6
theEND7[,12] <-7
theEND8[,12] <-8
theEND <- rbind(theEND1, theEND2, theEND3, theEND4, theEND5, theEND6, theEND7, theEND8)

theEND <- rbind(theEND2, theEND3, theEND4, theEND5, theEND6, theEND7)
#remove year 0
theEND <- theEND[theEND[,1] != 0, , drop=FALSE]  

#for certain years
theEND <- theEND[theEND[,1] == 100, , drop=FALSE]
theEND <- theEND[theEND[,1] == 150, , drop=FALSE]
theEND <- theEND[theEND[,1] == 175, , drop=FALSE]
theEND <- theEND[theEND[,1] == 200, , drop=FALSE]
theEND <- theEND[theEND[,1] == 250, , drop=FALSE]


a <- theEND[theEND[,12]==1,,drop=FALSE]
b <- theEND[theEND[,12]==2,,drop=FALSE]
c <- theEND[theEND[,12]==3,,drop=FALSE]
d <- theEND[theEND[,12]==4,,drop=FALSE]
e <- theEND[theEND[,12]==5,,drop=FALSE]
f <- theEND[theEND[,12]==6,,drop=FALSE]
g <- theEND[theEND[,12]==7,,drop=FALSE]
h <- theEND[theEND[,12]==8,,drop=FALSE]

a <- a[!is.na(a[,1]),]
b <- b[!is.na(b[,1]),]
c <- c[!is.na(c[,1]),]
d <- d[!is.na(d[,1]),]
e <- e[!is.na(e[,1]),]
f <- f[!is.na(f[,1]),]
g <- g[!is.na(g[,1]),]
h <- h[!is.na(h[,1]),]

{
table = matrix(nrow=8, ncol=21)            #each individual gets its own row.. matrix > dataframe -- "ncol = 7 + (nloci)*2
colnames(table) <- c("param", "mig", "HO", "HOLL", "HOUL", "Fis", "FisLL", "FisUL","numboff", "numboffLL", "numboffUL", "FST", "FSTLL", "FSTUL", "fstsource", "fstsourceLL", "fstsourceUL", "fissource", "fissourceLL", "fissourceUL", "year")

#param
table[,1] = seq(1,8,1) 

#year
#table[1,21] = mean(a[,1])
table[2,21] = mean(b[,1])
table[3,21] = mean(c[,1])
table[4,21] = mean(d[,1])
table[5,21] = mean(e[,1])
table[6,21] = mean(f[,1])
table[7,21] = mean(g[,1])
table[8,21] = mean(h[,1])

#mig
#table[1,2] = mean(a[,9])
table[2,2] = mean(b[,9])
table[3,2] = mean(c[,9])
table[4,2] = mean(d[,9])
table[5,2] = mean(e[,9])
table[6,2] = mean(f[,9])
table[7,2] = mean(g[,9])
table[8,2] = mean(h[,9])

#Ho
#table[1,3] = mean(a[,5])
table[2,3] = mean(b[,5])
table[3,3] = mean(c[,5])
table[4,3] = mean(d[,5])
table[5,3] = mean(e[,5])
table[6,3] = mean(f[,5])
table[7,3] = mean(g[,5])
table[8,3] = mean(h[,5])
#H0 Lower limit
#table[1,4] = quantile(a[,5], prob=0.025)
table[2,4] = quantile(b[,5], prob=0.025)
table[3,4] = quantile(c[,5], prob=0.025)
table[4,4] = quantile(d[,5], prob=0.025)
table[5,4] = quantile(e[,5], prob=0.025)
table[6,4] = quantile(f[,5], prob=0.025)
table[7,4] = quantile(g[,5], prob=0.025)
table[8,4] = quantile(h[,5], prob=0.025)
#Ho Upper Limit
#table[1,5] = quantile(a[,5], prob=0.975)
table[2,5] = quantile(b[,5], prob=0.975)
table[3,5] = quantile(c[,5], prob=0.975)
table[4,5] = quantile(d[,5], prob=0.975)
table[5,5] = quantile(e[,5], prob=0.975)
table[6,5] = quantile(f[,5], prob=0.975)
table[7,5] = quantile(g[,5], prob=0.975)
table[8,5] = quantile(h[,5], prob=0.975)

#Fis
#table[1,6] = mean(a[,6])
table[2,6] = mean(b[,6])
table[3,6] = mean(c[,6])
table[4,6] = mean(d[,6])
table[5,6] = mean(e[,6])
table[6,6] = mean(f[,6])
table[7,6] = mean(g[,6])
table[8,6] = mean(h[,6])
#Fis Lower limit
#table[1,7] = quantile(a[,6], prob=0.025)
table[2,7] = quantile(b[,6], prob=0.025)
table[3,7] = quantile(c[,6], prob=0.025)
table[4,7] = quantile(d[,6], prob=0.025)
table[5,7] = quantile(e[,6], prob=0.025)
table[6,7] = quantile(f[,6], prob=0.025)
table[7,7] = quantile(g[,6], prob=0.025)
table[8,7] = quantile(h[,6], prob=0.025)
#Fis Upper Limit
#table[1,8] = quantile(a[,6], prob=0.975)
table[2,8] = quantile(b[,6], prob=0.975)
table[3,8] = quantile(c[,6], prob=0.975)
table[4,8] = quantile(d[,6], prob=0.975)
table[5,8] = quantile(e[,6], prob=0.975)
table[6,8] = quantile(f[,6], prob=0.975)
table[7,8] = quantile(g[,6], prob=0.975)
table[8,8] = quantile(h[,6], prob=0.975)

#fst
#table[1,12] = mean(a[,10])
table[2,12] = mean(b[,10])
table[3,12] = mean(c[,10])
table[4,12] = mean(d[,10])
table[5,12] = mean(e[,10])
table[6,12] = mean(f[,10])
table[7,12] = mean(g[,10])
table[8,12] = mean(h[,10])
#fst Lower limit
#table[1,13] = quantile(a[,10], prob=0.025)
table[2,13] = quantile(b[,10], prob=0.025)
table[3,13] = quantile(c[,10], prob=0.025)
table[4,13] = quantile(d[,10], prob=0.025)
table[5,13] = quantile(e[,10], prob=0.025)
table[6,13] = quantile(f[,10], prob=0.025)
table[7,13] = quantile(g[,10], prob=0.025)
table[8,13] = quantile(h[,10], prob=0.025)
#fst Upper Limit
#table[1,14] = quantile(a[,10], prob=0.975)
table[2,14] = quantile(b[,10], prob=0.975)
table[3,14] = quantile(c[,10], prob=0.975)
table[4,14] = quantile(d[,10], prob=0.975)
table[5,14] = quantile(e[,10], prob=0.975)
table[6,14] = quantile(f[,10], prob=0.975)
table[7,14] = quantile(g[,10], prob=0.975)
table[8,14] = quantile(h[,10], prob=0.975)

}
if(u = 3){
  #numboff
  #table[1,9] = mean(a[,13])
  table[2,9] = mean(b[,13])
  table[3,9] = mean(c[,13])
  table[4,9] = mean(d[,13])
  table[5,9] = mean(e[,13])
  table[6,9] = mean(f[,13])
  table[7,9] = mean(g[,13])
  #table[8,9] = mean(h[,13])
  #numboff Lower limit
  #table[1,10] = quantile(a[,13], prob=0.025)
  table[2,10] = quantile(b[,13], prob=0.025)
  table[3,10] = quantile(c[,13], prob=0.025)
  table[4,10] = quantile(d[,13], prob=0.025)
  table[5,10] = quantile(e[,13], prob=0.025)
  table[6,10] = quantile(f[,13], prob=0.025)
  table[7,10] = quantile(g[,13], prob=0.025)
  #table[8,10] = quantile(h[,13], prob=0.025)
  #numboff Upper Limit
  #table[1,11] = quantile(a[,13], prob=0.975)
  table[2,11] = quantile(b[,13], prob=0.975)
  table[3,11] = quantile(c[,13], prob=0.975)
  table[4,11] = quantile(d[,13], prob=0.975)
  table[5,11] = quantile(e[,13], prob=0.975)
  table[6,11] = quantile(f[,13], prob=0.975)
  table[7,11] = quantile(g[,13], prob=0.975)
  #table[8,11] = quantile(h[,13], prob=0.975)
  
  #FSTsource
  #table[1,15] = mean(a[,14])
  table[2,15] = mean(b[,14])
  table[3,15] = mean(c[,14])
  table[4,15] = mean(d[,14])
  table[5,15] = mean(e[,14])
  table[6,15] = mean(f[,14])
  table[7,15] = mean(g[,14])
  #table[8,15] = mean(h[,14])
  #numboff Lower limit
  #table[1,16] = quantile(a[,14], prob=0.025)
  table[2,16] = quantile(b[,14], prob=0.025)
  table[3,16] = quantile(c[,14], prob=0.025)
  table[4,16] = quantile(d[,14], prob=0.025)
  table[5,16] = quantile(e[,14], prob=0.025)
  table[6,16] = quantile(f[,14], prob=0.025)
  table[7,16] = quantile(g[,14], prob=0.025)
  #table[8,16] = quantile(h[,14], prob=0.025)
  #numboff Upper Limit
  #table[1,17] = quantile(a[,14], prob=0.975)
  table[2,17] = quantile(b[,14], prob=0.975)
  table[3,17] = quantile(c[,14], prob=0.975)
  table[4,17] = quantile(d[,14], prob=0.975)
  table[5,17] = quantile(e[,14], prob=0.975)
  table[6,17] = quantile(f[,14], prob=0.975)
  table[7,17] = quantile(g[,14], prob=0.975)
  #table[8,17] = quantile(h[,14], prob=0.975)
  
  #fissource
  #table[1,18] = mean(a[,15])
  table[2,18] = mean(b[,15])
  table[3,18] = mean(c[,15])
  table[4,18] = mean(d[,15])
  table[5,18] = mean(e[,15])
  table[6,18] = mean(f[,15])
  table[7,18] = mean(g[,15])
  #table[8,18] = mean(h[,15])
  #fissource Lower limit
  #table[1,19] = quantile(a[,15], prob=0.025)
  table[2,19] = quantile(b[,15], prob=0.025)
  table[3,19] = quantile(c[,15], prob=0.025)
  table[4,19] = quantile(d[,15], prob=0.025)
  table[5,19] = quantile(e[,15], prob=0.025)
  table[6,19] = quantile(f[,15], prob=0.025)
  table[7,19] = quantile(g[,15], prob=0.025)
  #table[8,19] = quantile(h[,15], prob=0.025)
  #fissource Upper Limit
  #table[1,20] = quantile(a[,15], prob=0.975)
  table[2,20] = quantile(b[,15], prob=0.975)
  table[3,20] = quantile(c[,15], prob=0.975)
  table[4,20] = quantile(d[,15], prob=0.975)
  table[5,20] = quantile(e[,15], prob=0.975)
  table[6,20] = quantile(f[,15], prob=0.975)
  table[7,20] = quantile(g[,15], prob=0.975)
  #table[8,20] = quantile(h[,15], prob=0.975)
} #if have V source values

t100 <- table
t150 <- table
t175 <- table
t200 <- table
t250 <- table


table <- rbind(t100, t150, t175, t200, t250)
write.table(table, paste(directory, "/tableforcertainyears_p0c-7c.csv", sep=""), sep=",", col.names=TRUE, append=FALSE, quote=FALSE, row.names=FALSE)



Ho       = table[,3]                  #grabs observed heterozygosity
nmig     = table[,2]                  #grabs the number of migrants in the population
fst      = table[,12]                 #grabs Fst 
fstS     = table[,15]
fis      = table[,6]                  #grabs Fis
fisS     = table[,18]
para     = table[,1]
yr       = table[,21]

legend("topleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 2", "m = 5"),
       col=c("firebrick", "darkgoldenrod3", "springgreen", "dodgerblue", "purple"), pch = 19, cex=1.3)
legend("topleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 2", "m = 5"),
       col=c("black", "grey57", "red", "darkgoldenrod1", "maroon4"), pch = 19, cex=1.3)

png("fst_at_certain_times_c250.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(95, max(yr)), ylim=c(.001, .06)) 
for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,12], col = "red", lwd = 6)
    lines(d1[,21], d1[,12], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,14]
      yb = d1[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,14]
      yb = d3[c,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,12], col = "gold", lwd = 6)
    lines(d4[,21], d4[,12], col = "gold", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,14]
      yb = d4[e,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="gold", lwd=3)
    }
  }else if(k == 8){
    d8 = temp[temp[,1] == 8, , drop=FALSE]
    points(d8[,21], d8[,12], col = "grey57", lwd = 6)
    lines(d8[,21], d8[,12], col = "grey57", lwd = 4)
    for(f in 1:nrow(d8)){
      xa = d8[f,21]
      xb = d8[f,21]
      ya = d8[f,14]
      yb = d8[f,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("grey57", "red", "darkgoldenrod1", "gold"), pch = 19, cex=1)
#dev.copy(png, file = "/figs/fst_at_certain_times_c250.png")
dev.off()



png("fst_at_certain_times_c100.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(95, max(yr)), ylim=c(.001, .06)) 
for(l in 1:length(unique(para))){
  temp <- table[table[,1] == l,,drop=FALSE]
  if(l == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,12], col = "deeppink1", lwd = 6)
    lines(d2[,21], d2[,12], col = "deeppink1", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,14]
      yb = d2[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="deeppink1", lwd=3)
    }
  }else if(l == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,21], d5[,12], col = "blue", lwd = 6)
    lines(d5[,21], d5[,12], col = "blue", lwd = 4)
    for(d in 1:nrow(d5)){
      xa = d5[d,21]
      xb = d5[d,21]
      ya = d5[d,14]
      yb = d5[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(l == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,12], col = "purple", lwd = 6)
    lines(d6[,21], d6[,12], col = "purple", lwd = 4)
    for(d in 1:nrow(d6)){
      xa = d6[d,21]
      xb = d6[d,21]
      ya = d6[d,14]
      yb = d6[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="purple", lwd=3)
    }
  }else if(l == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,21], d7[,12], col = "springgreen", lwd = 6)
    lines(d7[,21], d7[,12], col = "springgreen", lwd = 4)
    for(d in 1:nrow(d7)){
      xa = d7[d,21]
      xb = d7[d,21]
      ya = d7[d,14]
      yb = d7[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="springgreen", lwd=3)
    }
  }else{next}
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("springgreen", "deeppink1", "blue", "purple"), pch = 19, cex=1)
dev.off()


png("FINALHo_Fig2.png")   ###******************************************************
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(95, max(yr)), ylim=c(.35, .5))
for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,3], col = "red", lwd = 6)
    lines(d1[,21], d1[,3], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,4]
      yb = d1[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 8){
    d2 = temp[temp[,1] == 8, , drop=FALSE]
    points(d2[,21], d2[,3], col = "black", lwd = 6)
    lines(d2[,21], d2[,3], col = "black", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,4]
      yb = d2[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,3], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,3], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,4]
      yb = d3[c,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,3], col = "maroon4", lwd = 6)
    lines(d4[,21], d4[,3], col = "maroon4", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,4]
      yb = d4[e,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="maroon4", lwd=3)
    }
  }else if(k == 5){
    d6 = temp[temp[,1] == 5, , drop=FALSE]
    points(d6[,21], d6[,3], col = "grey57", lwd = 6)
    lines(d6[,21], d6[,3], col = "grey57", lwd = 4)
    for(f in 1:nrow(d6)){
      xa = d6[f,21]
      xb = d6[f,21]
      ya = d6[f,4]
      yb = d6[f,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("bottomleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("grey57", "red", "darkgoldenrod1", "mistyrose2"), pch = 19, cex=1)
dev.off()


png("FINALHo_Fig3.png")   ###******************************************************
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(95, max(yr)), ylim=c(.35, .5))
for(l in 1:length(unique(para))){
  temp <- table[table[,1] == l,,drop=FALSE]
  if(l == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,3], col = "deeppink1", lwd = 6)
    lines(d2[,21], d2[,3], col = "deeppink1", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,4]
      yb = d2[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="deeppink1", lwd=3)
    }
  }else if(l == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,21], d5[,3], col = "blue", lwd = 6)
    lines(d5[,21], d5[,3], col = "blue", lwd = 4)
    for(d in 1:nrow(d5)){
      xa = d5[d,21]
      xb = d5[d,21]
      ya = d5[d,4]
      yb = d5[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(l == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,3], col = "purple", lwd = 6)
    lines(d6[,21], d6[,3], col = "purple", lwd = 4)
    for(d in 1:nrow(d6)){
      xa = d6[d,21]
      xb = d6[d,21]
      ya = d6[d,4]
      yb = d6[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="purple", lwd=3)
    }
  }else if(l == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,21], d7[,3], col = "springgreen", lwd = 6)
    lines(d7[,21], d7[,3], col = "springgreen", lwd = 4)
    for(d in 1:nrow(d7)){
      xa = d7[d,21]
      xb = d7[d,21]
      ya = d7[d,4]
      yb = d7[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="springgreen", lwd=3)
    }
  }else if(l == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,3], col = "black", lwd = 6)
    lines(d3[,21], d3[,3], col = "black", lwd = 4)
    for(d in 1:nrow(d3)){
      xa = d3[d,21]
      xb = d3[d,21]
      ya = d3[d,4]
      yb = d3[d,5]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else{next}
}
legend("bottomleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 2", "m = 5"),
       col=c("black", "springgreen", "deeppink1", "blue", "purple"), pch = 19, cex=1)
dev.off()


png("FINALFis_Fig2.png")   ###******************************************************
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(95, max(yr)), ylim=c(.2, .4))
for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,6], col = "red", lwd = 6)
    lines(d1[,21], d1[,6], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,7]
      yb = d1[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 8){
    d2 = temp[temp[,1] == 8, , drop=FALSE]
    points(d2[,21], d2[,6], col = "black", lwd = 6)
    lines(d2[,21], d2[,6], col = "black", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,7]
      yb = d2[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,6], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,6], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,7]
      yb = d3[c,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,6], col = "maroon4", lwd = 6)
    lines(d4[,21], d4[,6], col = "maroon4", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,7]
      yb = d4[e,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="maroon4", lwd=3)
    }
  }else if(k == 5){
    d6 = temp[temp[,1] == 5, , drop=FALSE]
    points(d6[,21], d6[,6], col = "grey57", lwd = 6)
    lines(d6[,21], d6[,6], col = "grey57", lwd = 4)
    for(f in 1:nrow(d6)){
      xa = d6[f,21]
      xb = d6[f,21]
      ya = d6[f,7]
      yb = d6[f,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("topright", legend=c("m = 0", "m = 0.5", "m = 1", "m = 2", "m = 5"),
       col=c("black", "red", "darkgoldenrod1", "grey57", "mistyrose2"), pch = 19, cex=1)
dev.off()


png("Fis_Fig3.png") ###******************************************************
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(95, max(yr)), ylim=c(.2, .4))
for(l in 1:length(unique(para))){
  temp <- table[table[,1] == l,,drop=FALSE]
  if(l == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,6], col = "deeppink1", lwd = 6)
    lines(d2[,21], d2[,6], col = "deeppink1", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,7]
      yb = d2[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="deeppink1", lwd=3)
    }
  }else if(l == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,6], col = "black", lwd = 6)
    lines(d3[,21], d3[,6], col = "black", lwd = 4)
    for(d in 1:nrow(d3)){
      xa = d3[d,21]
      xb = d3[d,21]
      ya = d3[d,7]
      yb = d3[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(l == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,21], d5[,6], col = "blue", lwd = 6)
    lines(d5[,21], d5[,6], col = "blue", lwd = 4)
    for(d in 1:nrow(d5)){
      xa = d5[d,21]
      xb = d5[d,21]
      ya = d5[d,7]
      yb = d5[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(l == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,6], col = "purple", lwd = 6)
    lines(d6[,21], d6[,6], col = "purple", lwd = 4)
    for(d in 1:nrow(d6)){
      xa = d6[d,21]
      xb = d6[d,21]
      ya = d6[d,7]
      yb = d6[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="purple", lwd=3)
    }
  }else if(l == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,21], d7[,6], col = "springgreen", lwd = 6)
    lines(d7[,21], d7[,6], col = "springgreen", lwd = 4)
    for(d in 1:nrow(d7)){
      xa = d7[d,21]
      xb = d7[d,21]
      ya = d7[d,7]
      yb = d7[d,8]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="springgreen", lwd=3)
    }
  }else{next}
}
legend("topright", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("springgreen", "deeppink1", "blue", "purple"), pch = 19, cex=1)
dev.off()

png("fst_Fig4.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(95, max(yr)), ylim=c(.001, .1)) 
for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,12], col = "red", lwd = 6)
    lines(d1[,21], d1[,12], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,14]
      yb = d1[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,12], col = "black", lwd = 6)
    lines(d2[,21], d2[,12], col = "black", lwd = 4)
    for(c in 1:nrow(d2)){
      xa = d2[c,21]
      xb = d2[c,21]
      ya = d2[c,13]
      yb = d2[c,14]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,12], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,13]
      yb = d3[c,14]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,12], col = "mistyrose2", lwd = 6)
    lines(d4[,21], d4[,12], col = "mistyrose2", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,13]
      yb = d4[e,14]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="mistyrose2", lwd=3)
    }
  }else if(k == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,12], col = "grey57", lwd = 6)
    lines(d6[,21], d6[,12], col = "grey57", lwd = 4)
    for(f in 1:nrow(d6)){
      xa = d6[f,21]
      xb = d6[f,21]
      ya = d6[f,13]
      yb = d6[f,14]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("grey57", "red", "darkgoldenrod1", "gold"), pch = 19, cex=1)
#dev.copy(png, file = "/figs/fst_at_certain_times_c250.png")
dev.off()



png("fstVSource_at_certain_times_c100.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(95, max(yr)), ylim=c(.001, .03)) 
for(l in 1:length(unique(para))){
  temp <- table[table[,1] == l,,drop=FALSE]
  if(l == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,15], col = "deeppink1", lwd = 6)
    lines(d2[,21], d2[,15], col = "deeppink1", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,17]
      yb = d2[d,16]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="deeppink1", lwd=3)
    }
  }else if(l == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,21], d5[,15], col = "blue", lwd = 6)
    lines(d5[,21], d5[,15], col = "blue", lwd = 4)
    for(d in 1:nrow(d5)){
      xa = d5[d,21]
      xb = d5[d,21]
      ya = d5[d,17]
      yb = d5[d,16]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(l == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,15], col = "purple", lwd = 6)
    lines(d6[,21], d6[,15], col = "purple", lwd = 4)
    for(d in 1:nrow(d6)){
      xa = d6[d,21]
      xb = d6[d,21]
      ya = d6[d,17]
      yb = d6[d,16]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="purple", lwd=3)
    }
  }else if(l == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,21], d7[,15], col = "springgreen", lwd = 6)
    lines(d7[,21], d7[,15], col = "springgreen", lwd = 4)
    for(d in 1:nrow(d7)){
      xa = d7[d,21]
      xb = d7[d,21]
      ya = d7[d,17]
      yb = d7[d,16]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="springgreen", lwd=3)
    }
  }else{next}
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("springgreen", "deeppink1", "blue", "purple"), pch = 19, cex=1)
dev.off()


png("numboff_at_certain_times_c250.png")
plot(-100, -100 , xlab="year", ylab="numboff", xlim=c(95, max(yr)), ylim=c(20, 71)) 
for(k in 1:length(unique(para))){
  temp <- table[table[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,21], d1[,9], col = "red", lwd = 6)
    lines(d1[,21], d1[,9], col = "red", lwd = 4)
    for(d in 1:nrow(d1)){
      xa = d1[d,21]
      xb = d1[d,21]
      ya = d1[d,10]
      yb = d1[d,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="red", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,21], d3[,9], col = "darkgoldenrod1", lwd = 6)
    lines(d3[,21], d3[,9], col = "darkgoldenrod1", lwd = 4)
    for(c in 1:nrow(d3)){
      xa = d3[c,21]
      xb = d3[c,21]
      ya = d3[c,10]
      yb = d3[c,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="darkgoldenrod1", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,21], d4[,9], col = "gold", lwd = 6)
    lines(d4[,21], d4[,9], col = "gold", lwd = 4)
    for(e in 1:nrow(d4)){
      xa = d4[e,21]
      xb = d4[e,21]
      ya = d4[e,10]
      yb = d4[e,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="gold", lwd=3)
    }
  }else if(k == 8){
    d8 = temp[temp[,1] == 8, , drop=FALSE]
    points(d8[,21], d8[,9], col = "grey57", lwd = 6)
    lines(d8[,21], d8[,9], col = "grey57", lwd = 4)
    for(f in 1:nrow(d8)){
      xa = d8[f,21]
      xb = d8[f,21]
      ya = d8[f,10]
      yb = d8[f,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="grey57", lwd=3)
    }
  }
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("grey57", "red", "darkgoldenrod1", "gold"), pch = 19, cex=1)
#dev.copy(png, file = "/figs/fst_at_certain_times_c250.png")
dev.off()


png("numboff_at_certain_times_c100.png")
plot(-100, -100 , xlab="year", ylab="numboff", xlim=c(95, max(yr)), ylim=c(20, 71)) 
for(l in 1:length(unique(para))){
  temp <- table[table[,1] == l,,drop=FALSE]
  if(l == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,21], d2[,9], col = "deeppink1", lwd = 6)
    lines(d2[,21], d2[,9], col = "deeppink1", lwd = 4)
    for(d in 1:nrow(d2)){
      xa = d2[d,21]
      xb = d2[d,21]
      ya = d2[d,10]
      yb = d2[d,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="deeppink1", lwd=3)
    }
  }else if(l == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,21], d5[,9], col = "blue", lwd = 6)
    lines(d5[,21], d5[,9], col = "blue", lwd = 4)
    for(d in 1:nrow(d5)){
      xa = d5[d,21]
      xb = d5[d,21]
      ya = d5[d,10]
      yb = d5[d,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(l == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,21], d6[,9], col = "purple", lwd = 6)
    lines(d6[,21], d6[,9], col = "purple", lwd = 4)
    for(d in 1:nrow(d6)){
      xa = d6[d,21]
      xb = d6[d,21]
      ya = d6[d,10]
      yb = d6[d,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="purple", lwd=3)
    }
  }else if(l == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,21], d7[,9], col = "springgreen", lwd = 6)
    lines(d7[,21], d7[,9], col = "springgreen", lwd = 4)
    for(d in 1:nrow(d7)){
      xa = d7[d,21]
      xb = d7[d,21]
      ya = d7[d,10]
      yb = d7[d,11]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="springgreen", lwd=3)
    }
  }else{next}
}
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("springgreen", "deeppink1", "blue", "purple"), pch = 19, cex=1)
dev.off()


##############################################################################################
png("fst_for_each_migrationrate.png")
plot(-100, -100 , xlab="migration rate", ylab="Fst", xlim=c(0, 5), ylim=c(.001, .06)) 
for(k in 1:length(unique(para))){
  temp <- t250[t250[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,2], d1[,12], col = "black", lwd = 6)
    for(d in 1:nrow(d1)){
      xa = d1[d,2]
      xb = d1[d,2]
      ya = d1[d,14]
      yb = d1[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,2], d2[,12], col = "blue", lwd = 6)
    for(d in 1:nrow(d2)){
      xa = d2[d,2]
      xb = d2[d,2]
      ya = d2[d,14]
      yb = d2[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,2], d3[,12], col = "black", lwd = 6)
    for(c in 1:nrow(d3)){
      xa = d3[c,2]
      xb = d3[c,2]
      ya = d3[c,14]
      yb = d3[c,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,2], d4[,12], col = "black", lwd = 6)
    for(e in 1:nrow(d4)){
      xa = d4[e,2]
      xb = d4[e,2]
      ya = d4[e,14]
      yb = d4[e,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,2], d5[,12], col = "blue", lwd = 6)
    for(d in 1:nrow(d5)){
      xa = d5[d,2]
      xb = d5[d,2]
      ya = d5[d,14]
      yb = d5[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,2], d6[,12], col = "blue", lwd = 6)
    for(d in 1:nrow(d6)){
      xa = d6[d,2]
      xb = d6[d,2]
      ya = d6[d,14]
      yb = d6[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,2], d7[,12], col = "blue", lwd = 6)
    for(d in 1:nrow(d7)){
      xa = d7[d,2]
      xb = d7[d,2]
      ya = d7[d,14]
      yb = d7[d,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 8){
    d8 = temp[temp[,1] == 8, , drop=FALSE]
    points(d8[,2], d8[,12], col = "black", lwd = 6)
    for(f in 1:nrow(d8)){
      xa = d8[f,2]
      xb = d8[f,2]
      ya = d8[f,14]
      yb = d8[f,13]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="black", lwd=3)
    }
  }
}
legend("topright", legend=c("k = 250", "k = 100"),
       col=c("black", "blue"), pch = 19, cex=1)
dev.off()


png("Ho_for_each_migrationrate.png")
plot(-100, -100 , xlab="migration rate", ylab="Ho", xlim=c(0, 5), ylim=c(.41, .46)) 
for(k in 1:length(unique(para))){
  temp <- t250[t250[,1] == k,,drop=FALSE]
  
  if(k == 1){
    d1 = temp[temp[,1] == 1, , drop=FALSE]
    points(d1[,2], d1[,3], col = "black", lwd = 6)
    for(d in 1:nrow(d1)){
      xa = d1[d,2]
      xb = d1[d,2]
      ya = d1[d,5]
      yb = d1[d,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 2){
    d2 = temp[temp[,1] == 2, , drop=FALSE]
    points(d2[,2], d2[,3], col = "blue", lwd = 6)
    for(d in 1:nrow(d2)){
      xa = d2[d,2]
      xb = d2[d,2]
      ya = d2[d,5]
      yb = d2[d,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 3){
    d3 = temp[temp[,1] == 3, , drop=FALSE]
    points(d3[,2], d3[,3], col = "black", lwd = 6)
    for(c in 1:nrow(d3)){
      xa = d3[c,2]
      xb = d3[c,2]
      ya = d3[c,5]
      yb = d3[c,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 4){
    d4 = temp[temp[,1] == 4, , drop=FALSE]
    points(d4[,2], d4[,3], col = "black", lwd = 6)
    for(e in 1:nrow(d4)){
      xa = d4[e,2]
      xb = d4[e,2]
      ya = d4[e,5]
      yb = d4[e,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="black", lwd=3)
    }
  }else if(k == 5){
    d5 = temp[temp[,1] == 5, , drop=FALSE]
    points(d5[,2], d5[,3], col = "blue", lwd = 6)
    for(d in 1:nrow(d5)){
      xa = d5[d,2]
      xb = d5[d,2]
      ya = d5[d,5]
      yb = d5[d,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 6){
    d6 = temp[temp[,1] == 6, , drop=FALSE]
    points(d6[,2], d6[,3], col = "blue", lwd = 6)
    for(d in 1:nrow(d6)){
      xa = d6[d,2]
      xb = d6[d,2]
      ya = d6[d,5]
      yb = d6[d,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 7){
    d7 = temp[temp[,1] == 7, , drop=FALSE]
    points(d7[,2], d7[,3], col = "blue", lwd = 6)
    for(d in 1:nrow(d7)){
      xa = d7[d,2]
      xb = d7[d,2]
      ya = d7[d,5]
      yb = d7[d,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.2, col="blue", lwd=3)
    }
  }else if(k == 8){
    d8 = temp[temp[,1] == 8, , drop=FALSE]
    points(d8[,2], d8[,3], col = "black", lwd = 6)
    for(f in 1:nrow(d8)){
      xa = d8[f,2]
      xb = d8[f,2]
      ya = d8[f,5]
      yb = d8[f,4]
      arrows(x0=xa, y0=ya, x1=xb, y1=yb, code = 3, angle = 90, length = 0.1, col="black", lwd=3)
    }
  }
}
legend("topleft", legend=c("k = 250", "k = 100"),
       col=c("black", "blue"), pch = 19, cex=1)
dev.off()


############################################################################################################################################3
############################################################################################################################################3
############################################################################################################################################3

theEND <- rbind(theEND1, theEND2, theEND3, theEND4, theEND5, theEND6, theEND7, theEND8)
#remove year 0
theEND <- theEND[theEND[,1] != 0, , drop=FALSE]  

p1 = theEND[theEND[,12] == 1, , drop=FALSE]
p2 = theEND[theEND[,12] == 2, , drop=FALSE]
p3 = theEND[theEND[,12] == 3, , drop=FALSE]
p4 = theEND[theEND[,12] == 4, , drop=FALSE]
p5 = theEND[theEND[,12] == 5, , drop=FALSE]
p6 = theEND[theEND[,12] == 6, , drop=FALSE]
p7 = theEND[theEND[,12] == 7, , drop=FALSE]
p8 = theEND[theEND[,12] == 8, , drop=FALSE]

p1 <- theEND[theEND[,12]=="1b",,drop=FALSE]
p2 <- theEND[theEND[,12]=="2b",,drop=FALSE]
p3 <- theEND[theEND[,12]=="3b",,drop=FALSE]
p4 <- theEND[theEND[,12]=="4b",,drop=FALSE]
p5 <- theEND[theEND[,12]=="5b",,drop=FALSE]
p6 <- theEND[theEND[,12]=="6b",,drop=FALSE]
p7 <- theEND[theEND[,12]=="7b",,drop=FALSE]
p8 <- theEND[theEND[,12]=="8b",,drop=FALSE]



y250 <- theEND[theEND[,1] == 250, , drop=FALSE] 
y175 <- theEND[theEND[,1] == 175, , drop=FALSE] 
y200 <- theEND[theEND[,1] == 200, , drop=FALSE] 
y150 <- theEND[theEND[,1] == 150, , drop=FALSE] 
theEND <- y150
theEND <- y175
theEND <- y250
theEND <- y200


png("FINALfst_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, 250), ylim=c(0, .15)) 
points(p1[,1], p1[,10], col="firebrick")
points(p2[,1], p2[,10], col="purple")
points(p4[,1], p4[,10], col="dodgerblue")
points(p5[,1], p5[,10], col="springgreen")
points(p6[,1], p6[,10], col="dodgerblue")
points(p7[,1], p7[,10], col="darkgoldenrod3")
points(p8[,1], p8[,10], col="darkgoldenrod3")
points(p3[,1], p3[,10], col="springgreen")
legend("topleft", legend=c("m = 0", "m = 0.5", "m = 1", "m = 2", "m = 5"),
       col=c("firebrick", "darkgoldenrod3", "springgreen", "dodgerblue", "purple"), pch = 19, cex=1.3)
dev.off()

png("fis_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(100, 250), ylim=c(0.2, .45)) 
points(p1[,1], p1[,6], col="firebrick")
points(p2[,1], p2[,6], col="firebrick")
points(p4[,1], p4[,6], col="dodgerblue")
points(p5[,1], p5[,6], col="springgreen")
points(p6[,1], p6[,6], col="dodgerblue")
points(p7[,1], p7[,6], col="darkgoldenrod3")
points(p8[,1], p8[,6], col="darkgoldenrod3")
points(p3[,1], p3[,6], col="springgreen")
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()

png("fis_overtime_twocolors.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(100, 250), ylim=c(0.2, .45)) 
points(p1[,1], p1[,6], col="black")
points(p2[,1], p2[,6], col="blue")
points(p4[,1], p4[,6], col="black")
points(p5[,1], p5[,6], col="blue")
points(p6[,1], p6[,6], col="blue")
points(p7[,1], p7[,6], col="blue")
points(p8[,1], p8[,6], col="black")
points(p3[,1], p3[,6], col="black")
legend("topleft", legend=c("drop to 250", "drop to 100"),
       col=c("black", "blue"), pch = 19, cex=1.3)
dev.off()



png("HO_overtime.png")
plot(-100, -100 , xlab="year", ylab="Ho", xlim=c(100, 250), ylim=c(0.4, .50)) 
points(p1[,1], p1[,5], col="firebrick")
points(p2[,1], p2[,5], col="firebrick")
points(p4[,1], p4[,5], col="dodgerblue")
points(p5[,1], p5[,5], col="springgreen")
points(p6[,1], p6[,5], col="dodgerblue")
points(p7[,1], p7[,5], col="darkgoldenrod3")
points(p8[,1], p8[,5], col="darkgoldenrod3")
points(p3[,1], p3[,5], col="springgreen")
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()


png("numboff_overtime.png")
plot(-100, -100 , xlab="year", ylab="numboff", xlim=c(0, 250), ylim=c(85, 485)) 
points(p1[,1], p1[,7], col="firebrick")
points(p2[,1], p2[,7], col="firebrick")
points(p4[,1], p4[,7], col="dodgerblue")
points(p5[,1], p5[,7], col="springgreen")
points(p6[,1], p6[,7], col="dodgerblue")
points(p7[,1], p7[,7], col="darkgoldenrod3")
points(p8[,1], p8[,7], col="darkgoldenrod3")
points(p3[,1], p3[,7], col="springgreen")
legend("bottomleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

theENDlow <- theEND[theEND[,1] <= 100, , drop=FALSE]
theENDdrop <- theEND[theEND[,1] >100 & theEND[,1] <=150,,drop=FALSE]
theENDdur <- theEND[theEND[,1] >150 & theEND[,1] <=175,,drop=FALSE]
theENDup <- theEND[theEND[,1] > 175, , drop=FALSE]

png("FINALpopsize_overtime_colorbyyear.png")
plot(-100, -100 , xlab="year", ylab="Population Size", xlim=c(0, 250), ylim=c(75, 505)) 
points(theENDlow[,1], theENDlow[,2], col = "black")
points(theENDdrop[,1], theENDdrop[,2], col = "blue")
points(theENDdur[,1], theENDdur[,2], col = "red")
points(theENDup[,1], theENDup[,2], col = "yellow")
legend("bottomleft", legend=c("years 100 - 150", "years 150 - 200", "years > 200"),
       col=c("blue", "red", "yellow"), pch = 19, cex=1)
dev.off()


png("popsize_overtime.png")
plot(-100, -100 , xlab="year", ylab="Population Size", xlim=c(0, 250), ylim=c(75, 505)) 
points(p1[,1], p1[,2], col="black")
points(p2[,1], p2[,2], col="blue")
points(p4[,1], p4[,2], col="black")
points(p5[,1], p5[,2], col="blue")
points(p6[,1], p6[,2], col="blue")
points(p7[,1], p7[,2], col="blue")
points(p8[,1], p8[,2], col="black")
points(p3[,1], p3[,2], col="black")
points(p1[,1], p1[,7], col="red")
points(p2[,1], p2[,7], col="darkgoldenrod3")
points(p4[,1], p4[,7], col="red")
points(p5[,1], p5[,7], col="red")
points(p6[,1], p6[,7], col="darkgoldenrod3")
points(p7[,1], p7[,7], col="darkgoldenrod3")
points(p8[,1], p8[,7], col="red")
points(p3[,1], p3[,7], col="red")
legend("bottomleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()


png("HEvsHO.png")
plot(-100, -100 , xlab="Ho", ylab="He", xlim=c(.25, .48), ylim=c(.25, .48)) 
points(p1[,5], p1[,4], col="black")
points(p2[,5], p2[,4], col="blue")
points(p4[,5], p4[,4], col="black")
points(p5[,5], p5[,4], col="blue")
points(p6[,5], p6[,4], col="blue")
points(p7[,5], p7[,4], col="blue")
points(p8[,5], p8[,4], col="black")
points(p3[,5], p3[,4], col="black")
legend("bottomleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()



png("HEvsHO.png")
plot(-100, -100 , xlab="Ho", ylab="He", xlim=c(.4, .48), ylim=c(.4, .48)) 
points(theENDlow[,5], theENDlow[,4], col = "black")
points(theENDdrop[,5], theENDdrop[,4], col = "blue")
points(theENDdur[,5], theENDdur[,4], col = "red")
points(theENDup[,5], theENDup[,4], col = "yellow")
abline(coef = c(0,1), col = "black")
legend("bottomright", legend=c("years 100 - 150", "years 150 - 175", "years > 175"),
       col=c("blue", "red", "yellow"), pch = 19, cex=1.3)
dev.off()


png("fstVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(0, 250), ylim=c(0, .04)) 
points(p1[,1], p1[,14], col="firebrick")
points(p2[,1], p2[,14], col="firebrick")
points(p4[,1], p4[,14], col="dodgerblue")
points(p5[,1], p5[,14], col="springgreen")
points(p6[,1], p6[,14], col="dodgerblue")
points(p7[,1], p7[,14], col="darkgoldenrod3")
points(p8[,1], p8[,14], col="darkgoldenrod3")
points(p3[,1], p3[,14], col="springgreen")
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()

png("fisVSource_overtime.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, 250), ylim=c(0.4, .48)) 
points(p1[,1], p1[,15], col="firebrick")
points(p2[,1], p2[,15], col="firebrick")
points(p4[,1], p4[,15], col="dodgerblue")
points(p5[,1], p5[,15], col="springgreen")
points(p6[,1], p6[,15], col="dodgerblue")
points(p7[,1], p7[,15], col="darkgoldenrod3")
points(p8[,1], p8[,15], col="darkgoldenrod3")
points(p3[,1], p3[,15], col="springgreen")
legend("topleft", legend=c("m ≤ 1", "m = 1", "m = 2", "m = 5"),
       col=c("darkgoldenrod3", "firebrick", "springgreen", "dodgerblue"), pch = 19, cex=1.3)
dev.off()

png("fisVSource_overtime_twocolors.png")
plot(-100, -100 , xlab="year", ylab="Fis", xlim=c(0, 250), ylim=c(0.4, .48)) 
points(p1[,1], p1[,15], col="black")
points(p2[,1], p2[,15], col="blue")
points(p3[,1], p3[,15], col="black")
points(p4[,1], p4[,15], col="black")
points(p5[,1], p5[,15], col="blue")
points(p6[,1], p6[,15], col="blue")
points(p7[,1], p7[,15], col="blue")
points(p8[,1], p8[,15], col="black")
legend("topleft", legend=c("drop to 250", "drop to 100"),
       col=c("black", "blue"), pch = 19, cex=1.3)
dev.off()


###############################################################################################
###############################################################################################

l <- matrix(nrow= nrow(theEND), ncol = 1)
l[,1] = seq(1,nrow(theEND),1) 
theEND<- cbind(theEND,l)

plot(-100, -100 , xlab="year", ylab="Observed heterozygosity", xlim=c(125, max(theEND[,1])), ylim=c(.3, .5)) 
for(k in unique(theEND[,26])){
  temp <- theEND[theEND[,26] == k,,drop=FALSE]
  points(temp[,1], temp[,5])
  points(temp[,1], temp[,4], col = "blue")
}


theEND <- theEND[theEND[,1] >= 125, , drop=FALSE] 
plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(125, max(theEND[,1])), ylim=c(.01, .06)) 
for(k in unique(theEND[,26])){
  temp <- theEND[theEND[,26] == k,,drop=FALSE]
  
  d1 = temp[temp[,12] == 1, , drop=FALSE]
  d3 = temp[temp[,12] == 3, , drop=FALSE]
  d4 = temp[temp[,12] == 4, , drop=FALSE]
  d8 = temp[temp[,12] == 8, , drop=FALSE]
  
  points(d1[,1], d1[,10], col = "red")
  points(d3[,1], d3[,10], col = "yellow")
  points(d4[,1], d4[,10], col = "green")
  points(d8[,1], d8[,10], col = "brown")
}
legend("topleft", legend=c("m = 1", "m = 0", "m = 5", "m = 0 or 1"),
       col=c("red", "yellow", "green", "brown"), lty=1:1, cex=0.8)
dev.copy(png, "/fst_over_time_c250.png")
dev.off()


plot(-100, -100 , xlab="year", ylab="Fst VERSES SOURCE", xlim=c(125, max(theEND[,1])), ylim=c(0, .03)) 
for(k in unique(theEND[,26])){
  temp <- theEND[theEND[,26] == k,,drop=FALSE]
  
  d1 = temp[temp[,12] == 1, , drop=FALSE]
  d3 = temp[temp[,12] == 3, , drop=FALSE]
  d4 = temp[temp[,12] == 4, , drop=FALSE]
  d8 = temp[temp[,12] == 8, , drop=FALSE]
  
  points(d1[,1], d1[,14], col = "red")
  points(d3[,1], d3[,14], col = "yellow")
  points(d4[,1], d4[,14], col = "green")
  points(d8[,1], d8[,14], col = "brown")
}
legend("topleft", legend=c("m = 1", "m = 0", "m = 5", "m = 0 or 1"),
       col=c("red", "yellow", "green", "brown"), lty=1:1, cex=0.8)
dev.copy(png, "~/fst_over_time_c250.png")
dev.off()




plot(-100, -100 , xlab="year", ylab="Fst", xlim=c(125, max(theEND[,1])), ylim=c(0, .06)) 
for(k in unique(theEND[,26])){
  temp <- theEND[theEND[,26] == k,,drop=FALSE]
  
  d2 = temp[temp[,12] == 2, , drop=FALSE]
  d5 = temp[temp[,12] == 5, , drop=FALSE]
  d6 = temp[temp[,12] == 6, , drop=FALSE]
  d7 = temp[temp[,12] == 7, , drop=FALSE]
  
  points(d2[,1], d2[,10], col = "orange")
  points(d5[,1], d5[,10], col = "blue")
  points(d6[,1], d6[,10], col = "purple")
  points(d7[,1], d7[,10], col = "black")
  
}
dev.copy(png, "/fst_over_time_c100.png")
dev.off()

plot(-100, -100 , xlab="year", ylab="Fst VERSES SOURCE", xlim=c(125, max(theEND[,1])), ylim=c(0, .06)) 
for(k in unique(theEND[,26])){
  temp <- theEND[theEND[,26] == k,,drop=FALSE]
  
  d2 = temp[temp[,12] == 2, , drop=FALSE]
  d5 = temp[temp[,12] == 5, , drop=FALSE]
  d6 = temp[temp[,12] == 6, , drop=FALSE]
  d7 = temp[temp[,12] == 7, , drop=FALSE]
  
  points(d2[,1], d2[,14], col = "orange")
  points(d5[,1], d5[,14], col = "blue")
  points(d6[,1], d6[,14], col = "purple")
  points(d7[,1], d7[,14], col = "black")
  
}
legend("topleft", legend=c("m = 1", "m = 0", "m = 5", "m = 0 or 1"),
       col=c("orange", "blue", "purple", "black"), lty=1:1, cex=0.8)
dev.copy(png, "/fstVSOURCE_over_time_c100.png")
dev.off()



plot(-100, -100 , xlab="Parameter", ylab="Observed heterozygosity", xlim=c(0, max(para)), ylim=c(.3, .5)) 
points(p1[,12], p1[,5], col="firebrick")
points(p2[,12], p2[,5], col="firebrick")
points(p3[,12], p3[,5], col="springgreen")
points(p4[,12], p4[,5], col="dodgerblue")
points(p5[,12], p5[,5], col="springgreen")
points(p6[,12], p6[,5], col="dodgerblue")
points(p7[,12], p7[,5], col="darkgoldenrod3")
points(p8[,12], p8[,5], col="darkgoldenrod3")
points(p1[,12], p1[,5], col="firebrick", pch=4)
points(p2[,12], p2[,5], col="firebrick", pch=4)
points(p3[,12], p3[,5], col="springgreen", pch=4)
points(p4[,12], p4[,5], col="dodgerblue", pch=4)
points(p5[,12], p5[,5], col="springgreen", pch=4)
points(p6[,12], p6[,5], col="dodgerblue", pch=4)
points(p7[,12], p7[,5], col="darkgoldenrod3", pch=4)
points(p8[,12], p8[,5], col="darkgoldenrod3", pch=4)


plot(-100, -100 , xlab="year", ylab="Observed heterozygosity", xlim=c(100, max(theEND[,1])), ylim=c(.3, .5)) 
points(p1[,1], p1[,5], col="firebrick", pch=4)
points(p2[,1], p2[,5], col="firebrick", pch=4)
points(p3[,1], p3[,5], col="springgreen", pch=4)
points(p4[,1], p4[,5], col="dodgerblue", pch=4)
points(p5[,1], p5[,5], col="springgreen", pch=4)
points(p6[,1], p6[,5], col="dodgerblue", pch=4)
points(p7[,1], p7[,5], col="darkgoldenrod3", pch=4)
points(p8[,1], p8[,5], col="darkgoldenrod3", pch=4)


plot(-100, -100 , xlab="Parameter", ylab="Fst", xlim=c(0, max(para)), ylim=c(.01, .05)) 
points(mean(p1[,12]), mean(p1[,10]), col="firebrick")
points(p2[,12], p2[,10], col="firebrick")
points(p3[,12], p3[,10], col="springgreen")
points(p4[,12], p4[,10], col="dodgerblue")
points(p5[,12], p5[,10], col="springgreen")
points(p6[,12], p6[,10], col="dodgerblue")
points(p7[,12], p7[,10], col="darkgoldenrod3")
points(p8[,12], p8[,10], col="darkgoldenrod3")





plot(-100, -100 , xlab="Parameter", ylab="Fis", xlim=c(0, max(para)), ylim=c(.23, .4)) 
points(p1[,12], p1[,6], col="firebrick")
points(p2[,12], p2[,6], col="firebrick")
points(p3[,12], p3[,6], col="springgreen")
points(p4[,12], p4[,6], col="dodgerblue")
points(p5[,12], p5[,6], col="springgreen")
points(p6[,12], p6[,6], col="dodgerblue")
points(p7[,12], p7[,6], col="darkgoldenrod3")
points(p8[,12], p8[,6], col="darkgoldenrod3")

plot(-100, -100 , xlab="Parameter", ylab="numboff", xlim=c(0, max(para)), ylim=c(20, 65)) 
points(mean(p1[,12]), mean(p1[,13]), col="firebrick")
points(mean(p2[,12]), mean(p2[,13]), col="firebrick")
points(mean(p3[,12]), mean(p3[,13]), col="springgreen")
points(mean(p4[,12]), mean(p4[,13]), col="dodgerblue")
points(mean(p5[,12]), mean(p5[,13]), col="springgreen")
points(mean(p6[,12]), mean(p6[,13]), col="dodgerblue")
points(mean(p7[,12]), mean(p7[,13]), col="darkgoldenrod3")
points(mean(p8[,12]), mean(p8[,13]), col="darkgoldenrod3")



pcolors  = c("firebrick",  "darkgoldenrod3", "springgreen4", "dodgerblue3") #



#if running all
theEND <- rbind(theEND1, theEND2, theEND3, theEND4, theEND5, theEND6, theEND7, theEND8)
#if running change in k
theEND <- rbind(theEND1, theEND4)
#if running change in r
theEND <- rbind(theEND1, theEND2)
#if running change in duration
theEND <- rbind(theEND1, theEND8)
#if running change in edyr
theEND <- rbind(theEND1, theEND6)


library(ggplot2)
data <- data.frame(x = table[,1],                    # Create data frame 
                   y = table[,3],
                   line = c(rep(1, 5),
                            rep(2, 5),
                            rep(3, 5),
                            rep(4, 5),
                            rep(5, 5),
                            rep(6, 5),
                            rep(7, 5),
                            rep(8, 5)))

ggplot(data, aes(x = x, y = y, col = line)) +           # Draw line plot with ggplot2
  geom_line()



