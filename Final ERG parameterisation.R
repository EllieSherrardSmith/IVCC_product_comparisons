#Final erg parameters

rm(list = ls())
require(lme4)
require(RColorBrewer)
require(scales)

is.pbo = 0 #says whether pbo net (0 = standard, 1= PBO, 2= G2 interceptor)
species = 1 #1 = gambiae ss, 2=arabiensis , 3=funestus
metric = 1 #1 = best guess, 2= lower 95% confidence interval 3 upper

#Assay to hut mortality conversion		
alpha1=	array(c(rep(0.63445,3),rep(0.012,3),rep(1.294,3)),c(3,3))
alpha2=	array(c(rep(3.997,3),rep(3.171,3),rep(5.119,3)),c(3,3))

#Benefit of PBO in assay		
beta1=	array(c(rep(3.407,2),2.527,rep(2.666,2),1.528,rep(4.331,2),3.547),c(3,3))
beta2=	array(c(rep(5.88,2),0.891,rep(4.754,2),(0.128),rep(6.956,2),1.882),c(3,3))
beta3=	array(c(rep(0.783,2),0,rep(0.543,2),0,rep(1.038,2),0),c(3,3))

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

surv_bioassay=seq(0,1,0.05)		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}

pbo_benefit_a<-beta1[species,metric]+beta2[species,metric]*((1-surv_bioassay)-0.5)/(1+beta3[species,metric]*((1-surv_bioassay)-0.5))   
pbo_benefit<-exp(pbo_benefit_a)/(1+exp(pbo_benefit_a))

#Benefit of PBO in assay		
#Using log binomial fit
g2_benefit = 1 / (1 + exp(-1.19 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - 0.45 ))

#g2_benefit_a<-g2beta1+g2beta2*((1-surv_bioassay)-0.5)/(1+g2beta3*((1-surv_bioassay)-0.5))   
#g2_benefit<-exp(g2_benefit_a)/(1+exp(g2_benefit_a))


mort_assay=if(is.pbo==0) 1-surv_bioassay else if(is.pbo==1) pbo_benefit else g2_benefit
mort_hut_a = alpha1[species,metric] + alpha2[species,metric]*(mort_assay-0.5)			              	#relationship mortality in bioassay -> hut trial, logit scale}
mort_hut   = exp(mort_hut_a)/(1+exp(mort_hut_a))

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

r_ITN0[14];d_ITN0[14];s_ITN0[14]
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

r_ITN0;d_ITN0;s_ITN0

itn_half_life[14]
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

ERG_d_ITN0[1]
ERG_r_ITN0[1]
ERG_s_ITN0[1]
itn_half_life[1]
ERG_d_ITN0[11]
ERG_r_ITN0[11]
ERG_s_ITN0[11]
itn_half_life[11]
ERG_d_ITN0[21]
ERG_r_ITN0[21]
ERG_s_ITN0[21]
itn_half_life[21]
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

plot(mort_assay,my.success.kill,type="n",
     xlim=c(0,100),ylim=c(0,100),las=1,xlab="Resistance test (% survival)",ylab="Probability (%)")
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),rep(100,length(my.x100))),col=my.cols[1],border=my.cols[1])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.success.kill),col=my.cols[3],border=my.cols[3])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.kill.det),col=my.cols[7],border=my.cols[7])
polygon(c(my.x100,rev(my.x100)),c(rep(0,length(my.x100)),my.success.a),col=my.cols[5],border=my.cols[5])

#text(30,80,"Killed",col=my.cols[2])
#text(30,40,"Deterred",col=my.cols[4])
#text(80,40,"Exited",col=my.cols[8])
#text(80,15,"Blood fed",col=my.cols[6])
abline(v=0,lwd=1)
abline(v=20,lwd=1.5)
abline(v=40,lwd=2)
abline(v=60,lwd=2.5)
abline(v=80,lwd=3)
abline(v=100,lwd=3.5)