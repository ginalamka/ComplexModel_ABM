#Analyze 2 to seperate out the SNPs

######FROM ORIGINAL SCRIPT##################################

#He and Ho - neutral (?)
SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)
genotype = data[, -c(ncol(data)-SNPS:ncol(data))]
snps = rep(c(1,2),ncol(genotype)/2)

HE = NULL
HO = NULL

loc.pos = seq(1, SNPS, 2)
for(lp in loc.pos){
  #per locus heterozygostiy
  locus <- genotype[, c(lp, lp+1), drop=FALSE]
  geno  <- length(locus[,1])
  het   <- length(which(locus[,1] != locus[,2]))
  het.observed <- het/geno
  HO = c(HO, het.observed)
  
  freqs <- table(locus)
  homozygous = NULL
  for(v in 1:length(freqs)){
    homozygous = c(homozygous, (freqs[v]/sum(freqs)*freqs[v]/sum(freqs)))
  }
  het.expected <- 1 - sum(homozygous)
  HE = c(HE, het.expected)
}
FIN[f,4] <- mean(HE)
FIN[f,5] <- mean(HO)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#prepare the genotypes for hierfstat
SNPS = (nSNP*2) + (nSNP.mig*2) + (nSNP.cons*2)                    #find number of SNPs
fstdata <- data[, -c(ncol(data)-(SNPS):ncol(data))]               #grab SNPs

#change 0s to 2s for hierfstat to read
fstdata[fstdata[,]==0] <-2

head(fstdata)


#allele 1 positions, aka odd values
pos1 = seq(1, SNPS, 2)
pos2 = pos1+1

#merge pos1 and pos2 into pos1, then remove pos2
fstdata[,pos1] <- as.numeric(paste(fstdata[,pos1], fstdata[,pos2], sep=""))
fstdata <- fstdata[,-c(pos2)]

#add pop identifier for calculations
popident <- matrix(nrow=nrow(fstdata), ncol=1)
popident[,1] = y
fstdata <- cbind(popident,fstdata)

if(y != 0){
  #do the same to the initialized focal pop -- for comparison
  fstinit <- focalpop[, -c(ncol(focalpop)-(SNPS):ncol(focalpop))]               #grab SNPs
  fstinit[fstinit[,]==0] <-2                                                    #change 0s to 2s
  fstinit[,pos1] <- as.numeric(paste(fstinit[,pos1], fstinit[,pos2], sep=""))                     #merge SNPs
  fstinit <- fstinit[,-c(pos2)]                                                 #remove single pos2 SNPs
  initident <- matrix(nrow=nrow(fstinit), ncol=1)                               #add pop identifier
  initident[,1] = 0
  fstinit <- cbind(initident,fstinit)                                           #merge identifier and genotypes
  
  fstdata <- rbind(fstdata, fstinit)                                            #merge current year and initialized year to one matrix for calculations
}
if(y == 0){
  #do the same to the initialized source pop -- for comparison
  fstsource <- source1[, -c(ncol(source1)-(SNPS):ncol(source1))]                    #grab SNPs
  fstsource[fstsource[,]==0] <-2                                                    #change 0s to 2s
  fstsource[,pos1] <- as.numeric(paste(fstsource[,pos1], fstsource[,pos2], sep=""))                     #merge SNPs
  fstsource <- fstsource[,-c(pos2)]                                                 #remove single pos2 SNPs
  sourceident <- matrix(nrow=nrow(fstsource), ncol=1)                               #add pop identifier
  sourceident[,1] = -1
  fstsource <- cbind(sourceident,fstsource)                                         #merge identifier and genotypes
  
  fstdata <- rbind(fstdata, fstsource)                                              #merge current year and initialized year to one matrix for calculations
}

fstdata <- as.data.frame(fstdata)                                               #turn into a dataframe
calc <-wc(fstdata, diploid=TRUE, pol=0)                                         #calc FST and FIS
#calc <- pairwise.WCfst(fstdata,diploid=TRUE)                                   #calculate FST

FIN[f,10] <- calc$FST
FIN[f, 6] <- calc$FIS

##############################################################
dSNPS = nSNP*2
mSNPS = nSNP.mig*2
cSNPS = nSNP.cons*2

###For drift alleles####################

dgenotype = data[, -c(ncol(data)-(mSNPS+cSNPS+dSNPS):ncol(data)-(cSNPS+mSNPS))]
dfstdata = dgenotype
dsnps = rep(c(1,2),ncol(dgenotype)/2)
dHE = NULL
dHO = NULL

dloc.pos = seq(1, dSNPS, 2)
for(dlp in dloc.pos){
  #per locus heterozygostiy
  dlocus <- dgenotype[, c(dlp, dlp+1), drop=FALSE]
  dgeno  <- length(dlocus[,1])
  dhet   <- length(which(dlocus[,1] != dlocus[,2]))
  dhet.observed <- dhet/dgeno
  dHO = c(dHO, dhet.observed)
  
  dfreqs <- table(dlocus)
  dhomozygous = NULL
  for(dv in 1:length(dfreqs)){
    dhomozygous = c(dhomozygous, (dfreqs[dv]/sum(dfreqs)*dfreqs[dv]/sum(dfreqs)))
  }
  dhet.expected <- 1 - sum(dhomozygous)
  dHE = c(dHE, dhet.expected)
}
FIN[f,14] <- mean(dHE)
FIN[f,15] <- mean(dHO)

##############################################################
###For migration alleles####################

mgenotype = data[, -c(ncol(data)-(mSNPS+cSNPS):ncol(data)-cSNPS)]
mfstdata = mgenotype
msnps = rep(c(1,2),ncol(mgenotype)/2)
mHE = NULL
mHO = NULL

mloc.pos = seq(1, mSNPS, 2)
for(mlp in mloc.pos){
  #per locus heterozygostiy
  mlocus <- mgenotype[, c(mlp, mlp+1), drop=FALSE]
  mgeno  <- length(mlocus[,1])
  mhet   <- length(which(mlocus[,1] != mlocus[,2]))
  mhet.observed <- mhet/mgeno
  mHO = c(mHO, mhet.observed)
  
  mfreqs <- table(mlocus)
  mhomozygous = NULL
  for(mv in 1:length(mfreqs)){
    mhomozygous = c(mhomozygous, (mfreqs[mv]/sum(mfreqs)*mfreqs[mv]/sum(mfreqs)))
  }
  mhet.expected <- 1 - sum(mhomozygous)
  mHE = c(mHE, mhet.expected)
}
FIN[f,16] <- mean(mHE)
FIN[f,17] <- mean(mHO)

##############################################################
###For mutation/conserved alleles####################

cgenotype = data[, -c(ncol(data)-cSNPS:ncol(data))]
cfstdata = cgenotype
csnps = rep(c(1,2),ncol(cgenotype)/2)
cHE = NULL
cHO = NULL

cloc.pos = seq(1, cSNPS, 2)
for(clp in cloc.pos){
  #per locus heterozygostiy
  clocus <- cgenotype[, c(clp, clp+1), drop=FALSE]
  cgeno  <- length(clocus[,1])
  chet   <- length(which(clocus[,1] != clocus[,2]))
  chet.observed <- chet/cgeno
  cHO = c(cHO, chet.observed)
  
  cfreqs <- table(clocus)
  chomozygous = NULL
  for(cv in 1:length(cfreqs)){
    chomozygous = c(chomozygous, (cfreqs[cv]/sum(cfreqs)*cfreqs[cv]/sum(cfreqs)))
  }
  chet.expected <- 1 - sum(chomozygous)
  cHE = c(cHE, chet.expected)
}
FIN[f,18] <- mean(cHE)
FIN[f,19] <- mean(cHO)

###########################################################################3
#/////////////////////////////////////////////////////////////////////////////
############################################################################
dfstdata[dfstdata[,]==0] <-2
mfstdata[mfstdata[,]==0] <-2
cfstdata[cfstdata[,]==0] <-2

#############################
dpos1 = seq(1, dSNPS, 2)
dpos2 = dpos1+1

#merge pos1 and pos2 into pos1, then remove pos2
dfstdata[,dpos1] <- as.numeric(paste(dfstdata[,dpos1], dfstdata[,dpos2], sep=""))
dfstdata <- dfstdata[,-c(dpos2)]

#add pop identifier for calculations
dpopident <- matrix(nrow=nrow(dfstdata), ncol=1)
dpopident[,1] = y
dfstdata <- cbind(dpopident,dfstdata)

if(y != 0){
  #do the same to the initialized focal pop -- for comparison
  fstinit <- focalpop[, -c(ncol(focalpop)-(SNPS):ncol(focalpop))]               #grab SNPs
  fstinit[fstinit[,]==0] <-2                                                    #change 0s to 2s
  fstinit[,pos1] <- as.numeric(paste(fstinit[,pos1], fstinit[,pos2], sep=""))                     #merge SNPs
  fstinit <- fstinit[,-c(pos2)]                                                 #remove single pos2 SNPs
  initident <- matrix(nrow=nrow(fstinit), ncol=1)                               #add pop identifier
  initident[,1] = 0
  fstinit <- cbind(initident,fstinit)                                           #merge identifier and genotypes
  
  fstdata <- rbind(fstdata, fstinit)                                            #merge current year and initialized year to one matrix for calculations
}
if(y == 0){
  #do the same to the initialized source pop -- for comparison
  fstsource <- source1[, -c(ncol(source1)-(SNPS):ncol(source1))]                    #grab SNPs
  fstsource[fstsource[,]==0] <-2                                                    #change 0s to 2s
  fstsource[,pos1] <- as.numeric(paste(fstsource[,pos1], fstsource[,pos2], sep=""))                     #merge SNPs
  fstsource <- fstsource[,-c(pos2)]                                                 #remove single pos2 SNPs
  sourceident <- matrix(nrow=nrow(fstsource), ncol=1)                               #add pop identifier
  sourceident[,1] = -1
  fstsource <- cbind(sourceident,fstsource)                                         #merge identifier and genotypes
  
  fstdata <- rbind(fstdata, fstsource)                                              #merge current year and initialized year to one matrix for calculations
}

fstdata <- as.data.frame(fstdata)                                               #turn into a dataframe
calc <-wc(fstdata, diploid=TRUE, pol=0)                                         #calc FST and FIS
#calc <- pairwise.WCfst(fstdata,diploid=TRUE)                                   #calculate FST

FIN[f,10] <- calc$FST
FIN[f, 6] <- calc$FIS