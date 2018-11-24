require(lme4)
require(RColorBrewer)
require(scales)

printed = "PBO"
is.pbo = 2 #says whether pbo net (0 = standard, 1= PBO, 2=G2_nets)
species =  1#1 = gambiae ss, 2=arabiensis , 3=funestus
metric = 1 #1 = best guess, 2= lower 95% confidence interval 3 upper
metric_change = 1
#Assay to hut mortality conversion		
alpha1=	array(c(rep(0.63445,3),rep(0.012,3),rep(1.294,3)),c(3,3))
alpha2=	array(c(rep(3.997,3),rep(3.171,3),rep(5.119,3)),c(3,3))

#Benefit of PBO in assay		
beta1=	array(c(rep(3.407,2),2.527,rep(4.331,2),3.547,rep(2.666,2),1.528),c(3,3))
beta2=	array(c(rep(5.88,2),0.891,rep(6.956,2),1.882,rep(4.754,2),0.128),c(3,3))
beta3=	array(c(rep(0.783,2),0,rep(1.038,2),0,rep(0.543,2),0),c(3,3))

#Deterency from mortality		
delta1=	array(c(rep(0.071,3),rep(0.17,3),rep(0.255,3)),c(3,3))
delta2=	array(c(rep(1.257,3),rep(0.627,3),rep(2.073,3)),c(3,3))
delta3=	array(c(rep(-1.517,3),rep(4.03,3),rep(0.646,3)),c(3,3))

#Success from mortality		
theta1=	array(c(rep(0.025,3),rep(0.007,3),rep(0.034,3)),c(3,3))
theta2=	array(c(rep(3.317,3),rep(2.919,3),rep(4.899,3)),c(3,3))

#Decay in insecticide non-PBO net		
mup=	array(c(rep(-2.36,3),rep(2.948,3),rep(1.821,3)),c(3,3))
rhop=	array(c(rep(-3.05,3),rep(3.762,3),rep(2.322,3)),c(3,3))


kp0=0.699
net_halflife=2.64
#1-0.796 Bioko bradley study
##1-0.11 Kagera West study
#c(0.922,0.455)#

surv_bioassay=seq(0,1,0.2)		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}

pbo_benefit_a<-beta1[species,metric]+beta2[species,metric]*((1-surv_bioassay)-0.5)/(1+beta3[species,metric]*((1-surv_bioassay)-0.5))   
pbo_benefit<-exp(pbo_benefit_a)/(1+exp(pbo_benefit_a))

#Benefit of PBO in assay		
#Using log binomial fit
## output is summary(glm_2)$coeff[2,1] from IVCC_New_net_comparison
# I think 0.63 and 4 comes from Churcher et al. 2016 and determins the relationship for standard LLIN
#from IVCC_New_net_comparisons.r 
#summary(glm_2)$coeff[2,1] =4.024976
#summary(glm_2)$coeff[1,1] =0.08905463

G2_benefit = array(dim=c(length(surv_bioassay),3))
G2_benefit[,1] = 1 / (1 + exp(-4.024976 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - 0.08905463 ))
G2_benefit[,2] = 1 / (1 + exp(-3.613 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - -0.032 ))
G2_benefit[,3] = 1 / (1 + exp(-4.437 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - 0.210 ))

mort_assay=if(is.pbo==0) 1-surv_bioassay else if(is.pbo==1) pbo_benefit else G2_benefit[,metric_change]
mort_hut_a = alpha1[species,metric_change] + alpha2[species,metric]*(mort_assay-0.5)			              	#relationship mortality in bioassay -> hut trial, logit scale}
mort_hut   = exp(mort_hut_a)/(1+exp(mort_hut_a))

#plot(mort_hut,ylim=c(0,1))
#lines(mort_hut,lty=2,col="darkred")


det_hut_a = delta1[species,metric]+delta2[species,metric]*(mort_hut-0.5)+delta3[species,metric]*(mort_hut-0.5)^2	#relationship hut trial mortality -> deterrence}
det_hut   = ifelse(det_hut_a<0,0,det_hut_a)			                  #censored to stop becoming negative}
suc_hut   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_hut))				              #relationship hut trial mortality -> success}
rep_hut   = 1-suc_hut-mort_hut

n1n0 = 1-det_hut
kp1  = n1n0*suc_hut
jp1  = n1n0*rep_hut+(1-n1n0)
lp1  = n1n0*mort_hut

r_ITN0  = (1-kp1/kp0)*(jp1/(lp1+jp1))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0  = (1-kp1/kp0)*(lp1/(lp1+jp1))		          	#probability of repeating behaviour (max)}
s_ITN0  = 1-d_ITN0-r_ITN0   


#max tail to variable notation indicates maximum net efficacy (no resistance)
#min tail to variable notation indicates mainimum net efficacy (100% resistance)

mort_max_a = alpha1[species,metric] + alpha2[species,metric]*(1-0.5)				          #maximum mortality seen in huts, used to adjust}
mort_max   = exp(mort_max_a)/(1+exp(mort_max_a))

mort_min_a = alpha1[species,metric] + alpha2[species,metric]*(0-0.5)				          #maximum mortality seen in huts, used to adjust}
mort_min   = exp(mort_min_a)/(1+exp(mort_min_a))

det_max_a = delta1[species,metric]+delta2[species,metric]*(mort_max-0.5)+delta3[species,metric]*(mort_max-0.5)^2	#relationship hut trial mortality -> deterrence}
det_max   = ifelse(det_max_a<0,0,det_max_a)			                  #censored to stop becoming negative}
suc_max   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_max))				              #relationship hut trial mortality -> success}
rep_max   = 1-suc_max-mort_max

n1n0_max = 1-det_max
kp1_max  = n1n0_max*suc_max
jp1_max  = n1n0_max*rep_max+(1-n1n0_max)
lp1_max  = n1n0_max*mort_max

r_ITN0_max  = (1-kp1_max/kp0)*(jp1_max/(lp1_max+jp1_max))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0_max  = (1-kp1_max/kp0)*(lp1_max/(lp1_max+jp1_max))		          	#probability of repeating behaviour (max)}
s_ITN0_max  = 1-d_ITN0_max-r_ITN0_max


det_min_a = delta1[species,metric]+delta2[species,metric]*(mort_min-0.5)+delta3[species,metric]*(mort_min-0.5)^2	#relationship hut trial mortality -> deterrence}
det_min   = ifelse(det_min_a<0,0,det_min_a)			                  #censored to stop becoming negative}
suc_min   = theta1[species,metric] *exp(theta2[species,metric] *(1-mort_min))				              #relationship hut trial mortality -> success}
rep_min   = 1-suc_min-mort_min

n1n0_min= 1-det_min
kp1_min  = n1n0_min*suc_min
jp1_min  = n1n0_min*rep_min+(1-n1n0_min)
lp1_min  = n1n0_min*mort_min

r_ITN0_min  = (1-kp1_min/kp0)*(jp1_min/(lp1_min+jp1_min))		          	#probability of dying with an encounter with ITN (max)}
d_ITN0_min  = (1-kp1_min/kp0)*(lp1_min/(lp1_min+jp1_min))		          	#probability of repeating behaviour (max)}
s_ITN0_min  = 1-d_ITN0_min-r_ITN0_min



#{halflife}
my_max_washes_a = mup[species,metric] +rhop[species,metric]*(mort_max-0.5)		
my_max_washes   = log(2)/(exp(my_max_washes_a)/(1+exp(my_max_washes_a)))

wash_decay_rate_a = mup[species,metric] +rhop[species,metric]*(mort_hut-0.5)
wash_decay_rate   = log(2)/(exp(wash_decay_rate_a)/(1+exp(wash_decay_rate_a)))
itn_half_life     = wash_decay_rate/my_max_washes*net_halflife





## adjusted to match Griffin et al 2015 Natt Comms
Griff_d_ITN0<-0.51
Griff_r_ITN0<-0.31  ###THINK THIS NEEDS TO BE CHECKED
Griff_s_ITN0<-1-Griff_d_ITN0-Griff_r_ITN0




##ERG parameterisations
##mortality parameters modified to match Jamies paper 
##success paramater scaled to start at jamies paper values and go to elife parameters
ERG_d_ITN0 <- d_ITN0/d_ITN0_max*Griff_d_ITN0
ERG_s_ITN0 <- (Griff_s_ITN0)+(s_ITN0-s_ITN0_max)/(s_ITN0_min-s_ITN0_max)*(s_ITN0_min-Griff_s_ITN0)
ERG_r_ITN0 <- 1-ERG_d_ITN0-ERG_s_ITN0


r_ITN0_min = 0.24
deaths_over_time = repeats_over_time = succ_fed_over_time = array(dim=c(365,length(ERG_d_ITN0)))

for(i in 1:length(ERG_d_ITN0)){
  time = 1:365
  deaths_over_time[,i] = ERG_d_ITN0[i] * exp(-(1/365*itn_half_life[i])*time)
  repeats_over_time[,i] = ERG_r_ITN0[i] - (ERG_r_ITN0[i] - r_ITN0_min) * exp(-(1/365*itn_half_life[i])*time) + r_ITN0_min
  succ_fed_over_time[,i] = 1 - deaths_over_time[,i] - repeats_over_time[,i]
}

checks = seq(1,101,by=10)

plot(deaths_over_time[,1] ~ time,ylim=c(0,1),pch="")
  for(i in 1:ncol(deaths_over_time)){
  lines(deaths_over_time[,i] ~ time,col="blue")
  lines(repeats_over_time[,i] ~ time,col="orange")
  lines(succ_fed_over_time[,i]~ time,col="darkred")
  
}

##check probabilities always sum to 1 for any set of params
for (i in 1:365){ch[i] = sum(deaths_over_time[i,1],repeats_over_time[i,1],succ_fed_over_time[i,1])}

is.pbo #says whether pbo net (0 = standard, 1= PBO)
species #1 = gambiae ss, 2=arabiensis , 3=funestus
metric #1 = best guess, 2= lower 95% confidence interval 3 upper
surv_bioassay[1]
## 65 resistance
#ERG_r_ITN0[14];ERG_d_ITN0[14];itn_half_life[14]

## 0 resistance
ERG_r_ITN0[1];ERG_d_ITN0[1];itn_half_life[1]

## 20 resistance
ERG_r_ITN0[5];ERG_d_ITN0[5];itn_half_life[5]
## 40 resistance
ERG_r_ITN0[9];ERG_d_ITN0[9];itn_half_life[9]
## 50 resistance
ERG_r_ITN0[11];ERG_d_ITN0[11];itn_half_life[11]
## 60 resistance
ERG_r_ITN0[13];ERG_d_ITN0[13];itn_half_life[13]
## 80 resistance
ERG_r_ITN0[17];ERG_d_ITN0[17];itn_half_life[17]
## 100 resistance
ERG_r_ITN0[21];ERG_d_ITN0[21];itn_half_life[21]

## 50 resistance
ERG_r_ITN0[11];ERG_d_ITN0[11];itn_half_life[11]

data.frame(ERG_r_ITN0,ERG_r_ITN0,ERG_r_ITN0,ERG_d_ITN0,ERG_d_ITN0,ERG_d_ITN0,itn_half_life)

##IVCC G2 runs

##mortality 30% = [71]
##mortality 35% = [66]
##mortality 11% =[90]
##mortality 41% =[60]
##mortality 6% =[95]
##mortality 19% =[82]
##mortality 43% =[58]
##mortality 25% =[76]
##mortality 100% =[1]

##mortality 55% = [46]
##mortality 24% = [77] #GAMB AND FUN
##mortality 60% =[41] #ARAB
##mortality 78% =[23]
##mortality 82% =[19]
##mortality 81% =[20]#GAMB AND FUN
##mortality 95% =[6]#ARAB
##mortality 54% =[47]#GAMB AND FUN
##mortality 51% =[50]#ARAB
##mortality 62% =[39]#ARAB

val_ref =67

ERG_r_ITN0[val_ref]
ERG_d_ITN0[val_ref] 
#ERG_s_ITN0[val_ref]
itn_half_life[val_ref]
D

g2_anopheles = data.frame(surv_bioassay,ERG_r_ITN0,ERG_d_ITN0,itn_half_life)
#write.csv(g2_anopheles,"H:\\Ellie\\IRS and resistance\\IVCC\\Data_IVCC_newLLINsG2Interceptor\\Analysis for country predictions\\G2_anopheles.csv")



#r_ITN_min=0.24 
#r_ITN_min + (r_ITN0- r_ITN_min)*itn_half_life
##check with plots in the style of elife paper

my.cols<-c(brewer.pal(12, "Paired"),"white",brewer.pal(8, "Dark2"))

my.success.inside.a<-suc_hut*100
my.det.a<-det_hut*100
my.det.trunc.a<-ifelse(my.det.a<0,0,my.det.a)
my.success.a<-my.success.inside.a*(1-my.det.trunc.a/100)

my.death<-mort_assay*100*(1-my.det.trunc.a/100)
my.success.kill<-100-my.death

a.det<-my.det.trunc.a
my.kill.det<-100-my.death-a.det

my.x100<-(1-surv_bioassay)*100

plot(mort_assay,my.success.kill,type="n",main="",cex.lab=1.6,cex.axis=1.6,cex.main=1.6,
     xlim=c(0,100),ylim=c(0,100),las=1,xlab="Resistance test (% survival)",ylab="Probability (%)")
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),rep(100,length(my.x100))),col=my.cols[1],border=my.cols[1])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.success.kill),col=my.cols[3],border=my.cols[3])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.kill.det),col=my.cols[7],border=my.cols[7])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.success.a),col=my.cols[5],border=my.cols[5])
#text(30,80,"Killed",col=my.cols[2],cex=1.4)
#text(30,40,"Deterred",col=my.cols[4],cex=1.4)
#text(80,40,"Exited",col=my.cols[8],cex=1.4)
#text(80,10,"Successfully blood fed",col=my.cols[6],cex=1.4)