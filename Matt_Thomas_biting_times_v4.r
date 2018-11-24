#######################################
##
## Matt Thomas
## Theoretical output were timing of biting to reduce the infective cohort of mosquitoes
##
##
########################################################################################


##
## Using BM model - ITNs, IRS, 2013 model LLIN resistance_IRS resistance_Time of bites.mmd 

##
dat = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table prevalence_with Phi_v4.txt",header = TRUE)

##
## Using BM model - ITNs, IRS, 2013 model LLIN resistance_IRS resistance_Time of bites.mmd 
##

dat_incv = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table rate infective_with Phi_v4.txt",header = TRUE)


par(mar=c(6,5,5,5))
par(mfrow=c(2,2))
plot(dat[1:1461,2] ~ dat[1:1461,1], ylab="Prevalence in 2 to 10 years (%)",yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.8),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.8,0.2),labels=seq(0,80,20),cex.axis=1.6,cex.lab=1.6)
cols = c("black","darkblue","royalblue","black","darkblue","royalblue")
ltys = c(1,1,1,4,4,4)
for(i in 2:7){
  lines(dat[1:1461,i] ~ dat[1:1461,1],lty=ltys[i-1],lwd=1,col=cols[i-1])
}

plot(dat[1:1461,2] ~ dat[1:1461,1], ylab="Prevalence in 2 to 10 years (%)",yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.8),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.8,0.2),labels=seq(0,80,20),cex.axis=1.6,cex.lab=1.6)
cols2 = c("darkred","red","orange","darkred","red","orange")
for(i in 8:13){
  lines(dat[1:1461,i] ~ dat[1:1461,1],lty=ltys[i-7],lwd=1,col=cols2[i-7])
}

plot(dat_incv[1:1461,2] ~ dat_incv[1:1461,1], ylab=expression("Inc"[V]),yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.1),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.1,0.02),labels=seq(0,0.1,0.02),cex.axis=1.6,cex.lab=1.6)

for(i in 2:7){
  lines(dat_incv[1:1461,i] ~ dat_incv[1:1461,1],lty=ltys[i-1],lwd=1,col=cols[i-1])
}
#for(i in 8:13){
#  lines(dat_incv[1:1461,i] ~ dat_ince[1:1461,1],lty=i-7,lwd=2,col="orange")
#}

prev_time_2555 = c(0.707,0.584,0.098,0.593,0.682,0.394)#dat[2555,2:7]
prev_time_2555nets = c(0.22,0.11,0,0.117,0.194,0.018)#dat[2555,8:13]

#Denominator is now the estimate for infective mosquitoes
# i.e. proportion EE * 2.32 + proportion NT * 1 + prportion EM * 0.06
proportionEE = c(1,0,0,0.15,0.7,0)
proportionNT = c(0,1,0,0.7,0.3,0.3)
proportionEM = c(0,0,1,0.15,0,0.7)

prop_EE =c(proportionEE * 2.32 + proportionNT * 1 + proportionEM * 0.06)

plot(prev_time_2555 ~ prop_EE, ylab="Prevalence in 2 to 10 years (%)",
     yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab=expression(paste("(Mosq"[EE], "+ Mosq"[NT], "+ Mosq"[EM],")")),xlim=c(0,2.5),ylim=c(0,1))
axis(1,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.6,cex.lab=1.6)
points(prev_time_2555 ~ prop_EE,col = "blue",lty=1,lwd=2)
points(prev_time_2555nets ~ prop_EE,col = "orange",lty=1,lwd=2)

par(xpd=NA,cex = 1)
text(x = -4.5, y = 2.9,"(A)",cex=1.2)
text(x = -4.5, y = 1.34,"(C)",cex=1.2)
text(x = -0.8, y = 2.90,"(B)",cex=1.2)
text(x = -0.8, y = 1.34,"(D)",cex=1.2)




## Relative Efficacy
par(mfrow=c(1,1))
rel_eff = 100 * (prev_time_2555 - prev_time_2555nets)/prev_time_2555
plot(rel_eff ~ prop_EE, ylab="Relative efficacy of nets (%)",
     yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab=expression(paste("(Mosq"[EE], "+ Mosq"[NT], "+ Mosq"[EM],")")),
     xlim=c(0,2.5),ylim=c(0,100),pch="")
axis(1,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,100,20),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
points(rel_eff ~ prop_EE,col = "darkgreen",lty=1,lwd=2)
lines(rev(sort(rel_eff)) ~ sort(prop_EE),col = "darkgreen",lty=1)



###############################################################
##
## Figure for Matt 6 lines (with and without the inclusion of net exposure differences)
##
##
#################################################################

## without phi changes
dat = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table prevalence_v3.txt",header = TRUE)
dat_incv = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table rate infective_V3.txt",header = TRUE)


## with phi changes
dat2 = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table prevalence_with Phi_v4.txt",header = TRUE)
dat_incv2 = read.table("H:\\Ellie\\Matt Thomas Eaves tubes timing of bites\\Time of bites table rate infective_with Phi_v4.txt",header = TRUE)


par(mar=c(6,5,5,5))
par(mfrow=c(2,2))
plot(dat[1:1461,2] ~ dat[1:1461,1], ylab="Prevalence in 2 to 10 years (%)",yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.8),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.8,0.2),labels=seq(0,80,20),cex.axis=1.6,cex.lab=1.6)
cols = c("black","darkblue","royalblue","black","darkblue","royalblue")
ltys = c(1,1,1,4,4,4)
for(i in 5:7){
  lines(dat[1:1461,i] ~ dat[1:1461,1],lty=1,lwd=1,col=cols[i-1])
  lines(dat2[1:1461,i] ~ dat2[1:1461,1],lty=1,lwd=1,col=cols[i-1])
}


plot(dat[1:1461,2] ~ dat[1:1461,1], ylab="Prevalence in 2 to 10 years (%)",yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.8),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.8,0.2),labels=seq(0,80,20),cex.axis=1.6,cex.lab=1.6)
cols2 = c("darkred","red","orange")
for(i in 11:13){
  lines(dat[1:1461,i] ~ dat[1:1461,1],lty=1,lwd=1,col=cols2[i-10])
  lines(dat2[1:1461,i] ~ dat2[1:1461,1],lty=2,lwd=1,col=cols2[i-10])
}

plot(dat_incv[1:1461,2] ~ dat_incv[1:1461,1], ylab=expression("Inc"[V]),yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab="Time in years",xlim=c(0,1461),ylim=c(0,0.1),pch="",bty="n")
axis(1,at=seq(0,1460,by=365),labels=seq(0,4,1),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,0.1,0.02),labels=seq(0,0.1,0.02),cex.axis=1.6,cex.lab=1.6)


for(i in 11:13){
  lines(dat_incv2[1:1461,i] ~ dat_incv2[1:1461,1],lty=2,lwd=1,col=cols2[i-10])
}
for(i in 11:13){
  lines(dat_incv[1:1461,i] ~ dat_incv[1:1461,1],lty=1,lwd=1,col=cols2[i-10])
}


prev_time1 = c(0.59,0.68,0.39)#dat[2555,2:7]
prev_time_nets1 = c(0.116,0.192,0.018)#dat[2555,11:13]

prev_time2 = c(0.593,0.682,0.394)#dat2[2555,2:7]
prev_time_nets2 = c(0.181,0.375,0.096)#dat2[2555,11:13]


#Denominator is now the estimate for infective mosquitoes
# i.e. proportion EE * 2.32 + proportion NT * 1 + prportion EM * 0.06
proportionEE = c(0.15, 0.7,  0)
proportionNT = c(0.7,  0.3,  0.3)
proportionEM = c(0.15, 0,    0.7)

prop_EE =c(proportionEE * 2.32 + proportionNT * 1 + proportionEM * 0.06)


rel_eff1 = 100 * (prev_time1 - prev_time_nets1)/prev_time1
rel_eff2 = 100 * (prev_time2 - prev_time_nets2)/prev_time2
plot(rel_eff1 ~ prop_EE, ylab="Relative efficacy of nets (%)",
     yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
     xlab=expression(paste("(Mosq"[EE], "+ Mosq"[NT], "+ Mosq"[EM],")")),
     xlim=c(0,2.5),ylim=c(0,100),pch="")
axis(1,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,100,20),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)

points(rel_eff1 ~ prop_EE,col = "darkgreen",lty=1,lwd=2)
lines(rev(sort(rel_eff1)) ~ sort(prop_EE),col = "darkgreen",lty=1)

points(rel_eff2 ~ prop_EE,col = "darkgreen",lty=1,lwd=2)
lines(rev(sort(rel_eff2)) ~ sort(prop_EE),col = "darkgreen",lty=2)

par(xpd=NA,cex = 1)
text(x = -4.5, y = 290,"(A)",cex=1.2)
text(x = -4.5, y = 120,"(C)",cex=1.2)
text(x = -0.5, y = 290,"(B)",cex=1.2)
text(x = -0.5, y = 120,"(D)",cex=1.2)


#plot(prev_time1 ~ prop_EE, ylab="Prevalence in 2 to 10 years (%)",
#     yaxt="n",xaxt="n",cex.axis=1.6,cex.lab=1.6,
#     xlab=expression(paste("(Mosq"[EE], "+ Mosq"[NT], "+ Mosq"[EM],")")),xlim=c(0,2.5),ylim=c(0,1))
#axis(1,at=seq(0,2.5,0.5),labels=seq(0,2.5,0.5),cex.axis=1.6,cex.lab=1.6)
#axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,1,0.2),cex.axis=1.6,cex.lab=1.6)
#points(prev_time1 ~ prop_EE,col = "blue",lty=1,lwd=2)
#points(prev_time_nets1 ~ prop_EE,col = "orange",lty=1,lwd=2,pch=15)
#points(prev_time_nets2 ~ prop_EE,col = "orange",lty=1,lwd=2,pch=17)

#par(xpd=NA,cex = 1)
#text(x = -4.5, y = 2.9,"(A)",cex=1.2)
#text(x = -4.5, y = 1.34,"(C)",cex=1.2)
#text(x = -0.8, y = 2.90,"(B)",cex=1.2)
#text(x = -0.8, y = 1.34,"(D)",cex=1.2)


  
