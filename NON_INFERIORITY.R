#################################
##
## Non-inferioirity
##

#parameters for 

rm(list = ls())
require(lme4)
require(RColorBrewer)
require(scales)

is.pbo = 0 #says whether pbo net (0 = standard, 1= PBO)     ##KEEP ZERO FOR NON-INFERIORITY
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

surv_bioassay=seq(0,1,0.02)		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}

pbo_benefit_a<-beta1[species,metric]+beta2[species,metric]*((1-surv_bioassay)-0.5)/(1+beta3[species,metric]*((1-surv_bioassay)-0.5))   
pbo_benefit<-exp(pbo_benefit_a)/(1+exp(pbo_benefit_a))

mort_assay=if(is.pbo==0) 1-surv_bioassay else pbo_benefit
mort_hut_a = alpha1[species,metric] + alpha2[species,metric]*(mort_assay-0.5)			              	#relationship mortality in bioassay -> hut trial, logit scale}
mort_hut_max   = max(exp(mort_hut_a)/(1+exp(mort_hut_a)))

mort_hut = mort_hut_max-seq(0,0.30,0.05)

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


##OUTPUT FOR ELLIE

pc_reduction<-seq(0,0.3,0.05) # % reduction
itn_kill<-ERG_d_ITN0
itn_repel<-ERG_r_ITN0

rbind(pc_reduction,itn_kill,itn_repel)

## KEEP HALF-LIFE CONSTANT

##For the country and admin level runs these are in the following files (H:\ is the shared drive)
##standard nets (H:\Ellie\Rprojects\Malaria\non_inferiority_1.csv)
##standard nets - 5% (H:\Ellie\Rprojects\Malaria\non_inferiority_1.csv)
##standard nets - 10%  (H:\Ellie\Rprojects\Malaria\non_inferiority_2.csv)
##standard nets - 15%  (H:\Ellie\Rprojects\Malaria\non_inferiority_3.csv)
##standard nets - 20%  (H:\Ellie\Rprojects\Malaria\non_inferiority_4.csv)
##standard nets - 25%  (H:\Ellie\Rprojects\Malaria\non_inferiority_5.csv)
##standard nets - 30%  (H:\Ellie\Rprojects\Malaria\non_inferiority_6.csv)

##Running all through cluster using the following function
## non-inferiority
##
library(MalariaLaunchR)
non_inferiority_ff<-function(site,
                             covITN,covIRS,
                             
                             itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                             itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                             itn_halflife_1,
                             
                             run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_Copy/Africa_sites_0/site_GF_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn_irs_corr', 1,
                    'output_type 0 itn 1 itn_coverage', covITN, 'irs 1 irs_coverage', covIRS,
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun 0.31 itn_repel_gamb_ss 0.31 itn_repel_arab 0.31',
                    'itn_kill_fun 0.51 itn_kill_gamb_ss 0.51 itn_kill_arab 0.51',
                    'itn_half_life 2.64',
                    
                    'change_itn_2 1 change_itn_time_2 0',
                    'itn_repel_fun_2', itn_repel_fun_1, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_1, 'itn_repel_arab_2', itn_repel_arab_1,
                    'itn_kill_fun_2', itn_kill_fun_1, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_1, 'itn_kill_arab_2', itn_kill_arab_1,
                    'itn_half_life_2', itn_halflife_1)
  ##the csv file defined level of resistance is used
  
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                 OutputRoot = paste0("P:/Ellies_output_folder/non_inferiority/", Run_name, '/draw_', draw),
                 Options=Options,
                 Exe = "P:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="P:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('/Africa_Sites_Ellie_Copy/Africa_sites_0/site_GF_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}

#e.g. to run on laptop
stat = read.csv("F:/non_inferiority_1.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = stat[i,1],
                    covITN = stat[i,2],covIRS = stat[i,3],
                    itn_repel_fun_1 = stat[i,4], itn_repel_gamb_ss_1 = stat[i,5], itn_repel_arab_1 = stat[i,6], 
                    itn_kill_fun_1 =  stat[i,7], itn_kill_gamb_ss_1 =  stat[i,8], itn_kill_arab_1 = stat[i,9],
                    itn_halflife_1 = stat[i,10],
                    run_name = "status_quo")
  
}
minus05 = read.csv("F:/non_inferiority_2.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus05[i,1],
                     covITN = minus05[i,2],covIRS = minus05[i,3],
                     itn_repel_fun_1 = minus05[i,4], itn_repel_gamb_ss_1 = minus05[i,5], itn_repel_arab_1 = minus05[i,6], 
                     itn_kill_fun_1 =  minus05[i,7], itn_kill_gamb_ss_1 =  minus05[i,8], itn_kill_arab_1 = minus05[i,9],
                     itn_halflife_1 = minus05[i,10],
                     run_name = "minus05")
  
}
minus10 = read.csv("F:/non_inferiority_3.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus10[i,1],
                     covITN = minus10[i,2],covIRS = minus10[i,3],
                     itn_repel_fun_1 = minus10[i,4], itn_repel_gamb_ss_1 = minus10[i,5], itn_repel_arab_1 = minus10[i,6], 
                     itn_kill_fun_1 =  minus10[i,7], itn_kill_gamb_ss_1 =  minus10[i,8], itn_kill_arab_1 = minus10[i,9],
                     itn_halflife_1 = minus10[i,10],
                     run_name = "minus10")
  
}
minus15 = read.csv("F:/non_inferiority_4.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus15[i,1],
                     covITN = minus15[i,2],covIRS = minus15[i,3],
                     itn_repel_fun_1 = minus15[i,4], itn_repel_gamb_ss_1 = minus15[i,5], itn_repel_arab_1 = minus15[i,6], 
                     itn_kill_fun_1 =  minus15[i,7], itn_kill_gamb_ss_1 =  minus15[i,8], itn_kill_arab_1 = minus15[i,9],
                     itn_halflife_1 = minus15[i,10],
                     run_name = "minus15")
  
}
minus20 = read.csv("F:/non_inferiority_5.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus20[i,1],
                     covITN = minus20[i,2],covIRS = minus20[i,3],
                     itn_repel_fun_1 = minus20[i,4], itn_repel_gamb_ss_1 = minus20[i,5], itn_repel_arab_1 = minus20[i,6], 
                     itn_kill_fun_1 =  minus20[i,7], itn_kill_gamb_ss_1 =  minus20[i,8], itn_kill_arab_1 = minus20[i,9],
                     itn_halflife_1 = minus20[i,10],
                     run_name = "minus20")
  
}
minus25 = read.csv("F:/non_inferiority_6.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus25[i,1],
                     covITN = minus25[i,2],covIRS = minus25[i,3],
                     itn_repel_fun_1 = minus25[i,4], itn_repel_gamb_ss_1 = minus25[i,5], itn_repel_arab_1 = minus25[i,6], 
                     itn_kill_fun_1 =  minus25[i,7], itn_kill_gamb_ss_1 =  minus25[i,8], itn_kill_arab_1 = minus25[i,9],
                     itn_halflife_1 = minus25[i,10],
                     run_name = "minus25")
  
}
minus30 = read.csv("F:/non_inferiority_7.csv",header=TRUE)
for(i in 291){
  non_inferiority_ff(site = minus30[i,1],
                     covITN = minus30[i,2],covIRS = minus30[i,3],
                     itn_repel_fun_1 = minus30[i,4], itn_repel_gamb_ss_1 = minus30[i,5], itn_repel_arab_1 = minus30[i,6], 
                     itn_kill_fun_1 =  minus30[i,7], itn_kill_gamb_ss_1 =  minus30[i,8], itn_kill_arab_1 = minus30[i,9],
                     itn_halflife_1 = minus30[i,10],
                     run_name = "minus30")
  
}
##Confirm the model does what you hope it does:
data1 = read.table("F:\\Ellies_output_folder\\non_inferiority\\status_quo\\draw_0\\status_quo_406_0.txt",header=TRUE)#the status quo nets
data2 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus05\\draw_0\\minus05_406_0.txt",header=TRUE)#minus 5% killing and decay
data3 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus10\\draw_0\\minus10_406_0.txt",header=TRUE)#minus 5% killing and decay
data4 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus15\\draw_0\\minus15_406_0.txt",header=TRUE)#minus 5% killing and decay
data5 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus20\\draw_0\\minus20_406_0.txt",header=TRUE)#minus 5% killing and decay
data6 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus25\\draw_0\\minus25_406_0.txt",header=TRUE)#minus 5% killing and decay
data7 = read.table("F:\\Ellies_output_folder\\non_inferiority\\minus30\\draw_0\\minus30_406_0.txt",header=TRUE)#minus 5% killing and decay

par(mfrow=c(1,2))
par(mar=c(5,5,5,5))

plot(data1$prev_2_10 ~ data1$year,pch="",ylim=c(0,0.4),bty="n",yaxt="n",
     ylab="Prevalence in 2 - 10 years (%)",xlim = c(-1,4),
     xlab="Time in years",cex.lab=1.2,cex.axis=1.2)
axis(2,las=2,at=c(0,0.1,0.2,0.3,0.4),labels=c(0,10,20,30,40),cex.axis=1.2,cex.lab=1.2)
lines(data2$prev_2_10 ~ data2$year,lwd=2,col="gray16")
lines(data3$prev_2_10 ~ data3$year,lwd=2,col="gray36")
lines(data4$prev_2_10 ~ data4$year,lwd=2,col="gray56")
lines(data5$prev_2_10 ~ data5$year,lwd=2,col="gray76")
lines(data6$prev_2_10 ~ data6$year,lwd=2,col="gray86")
lines(data7$prev_2_10 ~ data7$year,lwd=2,col="gray96")
lines(data1$prev_2_10 ~ data1$year,lwd=2,col="black")

legend(2,20,bty="n",main = "Lost efficacy (%)",
       legend = c("5","10","20","25","30"),lwd=2,col=c("black","gray16","gray36","gray56","gray76","gray86","gray96"))

plot(data1$clin_inc_0_5 ~ data1$year,pch="",ylim=c(0,3),bty="n",yaxt="n",
     ylab="Clinical cases per child under 5 per year",xlim = c(-1,4),
     xlab="Time in years",cex.lab=1.2,cex.axis=1.2)
axis(2,las=2,at=seq(0,3,1),cex.axis=1.2,cex.lab=1.2)
lines(data2$clin_inc_0_5 ~ data2$year,lwd=2,col="gray16")
lines(data3$clin_inc_0_5 ~ data3$year,lwd=2,col="gray36")
lines(data4$clin_inc_0_5 ~ data4$year,lwd=2,col="gray56")
lines(data5$clin_inc_0_5 ~ data5$year,lwd=2,col="gray76")
lines(data6$clin_inc_0_5 ~ data6$year,lwd=2,col="gray86")
lines(data7$clin_inc_0_5 ~ data7$year,lwd=2,col="gray96")
lines(data1$clin_inc_0_5 ~ data1$year,lwd=2,col="black")

#percentage_increase_cases_across_3yrs = (sum(data2$clin_inc_all_smooth[262:417]) - sum(data1$clin_inc_all_smooth[262:417]) )/sum(data2$clin_inc_all_smooth[262:417])


####################################################
## Population estimates
## Read in the raw data

# Load packages
library(tidyverse)

## Populations for Admin units over time
popns = read.csv("F:\\populations2.csv")
pop = popns$X2017

## Subset the data by just subsaharan countries
data_resource = read.csv("F:\\Copy of Intervention_coverage.csv")
vec_count = c(levels(data_resource$ISO[data_resource$CONTINENT == "Africa"]))
vec_count_val = c(data_resource$DIDE_CODE[data_resource$CONTINENT == "Africa"])

count_names = as.factor(data_resource$ISO[data_resource$CONTINENT == "Africa"])

## Population adjustments
adj_pop_temp = read.csv("H:\\Ellie\\IVCC_Resistance_Runs\\PopData_renormalised_2015.csv",header=TRUE)
adj_pop = subset(adj_pop_temp,adj_pop_temp$Year > 1999 & adj_pop_temp$Year < 2035)
#adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[1]]

N_admins = numeric(length(vec_count))
for(i in 1:length(vec_count)) N_admins[i] = length(data_resource$DIDE_CODE[data_resource$CONTINENT == "Africa" & data_resource$ISO == vec_count[i]])

years = 2000:2033

popul_2000_2035 = array(dim=c(34,601))
DIDE_CODE = popns$DIDE_CODE[popns$DIDE_CODE > 48 & popns$DIDE_CODE < 861]

j=1

##this is the popn in 2015, 
aa = (popns$X2017[popns$DIDE_CODE == DIDE_CODE[j]]/adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[j]][18])
##need to create a vector by multiplying the  renorm by this
aa * adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[j]]
## and store this in an array

##Create estimates for each country for each variable required 
vec_count2 = c(rep(vec_count[1],N_admins[1]),
               rep(vec_count[2],N_admins[2]),
               rep(vec_count[3],N_admins[3]),
               rep(vec_count[4],N_admins[4]),
               rep(vec_count[5],N_admins[5]),
               rep(vec_count[6],N_admins[6]),
               rep(vec_count[7],N_admins[7]),
               rep(vec_count[8],N_admins[8]),
               rep(vec_count[9],N_admins[9]),
               rep(vec_count[10],N_admins[10]),
               rep(vec_count[11],N_admins[11]),
               rep(vec_count[12],N_admins[12]),
               rep(vec_count[13],N_admins[13]),
               rep(vec_count[14],N_admins[14]),
               rep(vec_count[15],N_admins[15]),
               rep(vec_count[16],N_admins[16]),
               rep(vec_count[17],N_admins[17]),
               rep(vec_count[18],N_admins[18]),
               rep(vec_count[19],N_admins[19]),
               rep(vec_count[20],N_admins[20]),
               rep(vec_count[21],N_admins[21]),
               rep(vec_count[22],N_admins[22]),
               rep(vec_count[23],N_admins[23]),
               rep(vec_count[24],N_admins[24]),
               rep(vec_count[25],N_admins[25]),
               rep(vec_count[26],N_admins[26]),
               rep(vec_count[27],N_admins[27]),
               rep(vec_count[28],N_admins[28]),
               rep(vec_count[29],N_admins[29]),
               rep(vec_count[30],N_admins[30]),
               rep(vec_count[31],N_admins[31]),
               rep(vec_count[32],N_admins[32]),
               rep(vec_count[33],N_admins[33]),
               rep(vec_count[34],N_admins[34]),
               rep(vec_count[35],N_admins[35]),
               rep(vec_count[36],N_admins[36]),
               rep(vec_count[37],N_admins[37]),
               rep(vec_count[38],N_admins[38]),
               rep(vec_count[39],N_admins[39]),
               rep(vec_count[40],N_admins[40]),
               rep(vec_count[41],N_admins[41]),
               rep(vec_count[42],N_admins[42]),
               rep(vec_count[43],N_admins[43]))



##Confirm that ISO are in the same order in the popn file and the norm file
for(j in 1:601){
  for(i in 1:34){
    popul_2000_2035[i,j] = (popns$X2017[popns$DIDE_CODE == DIDE_CODE[j]]/
                              adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count2[j]][18]) * 
      adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count2[j]][i]  ##2017 populations (dont have 2015 so go with these...)
  }
  
}

#CHECK ALL MATCHES UP SO FAR  
checks = data.frame(DIDE_CODE,vec_count2,popn_2015 = popul_2000_2035[16,],popn_2016 = popul_2000_2035[17,],popn_2017 = popul_2000_2035[18,])
#write.csv(checks,"H:\\Ellie\\IVCC_Resistance_Runs\\checker1.csv")
##sort by DIDE_CODE
colnames(checks) = c("DIDE_CODE","ISO","popn_2015","popn_2016","popn_2017")
data1 = data2 = data3 = data4 = data5 = data6 = data7 = array(dim=c(601,4))
data1[,1] = checks[,1]
data2[,1] = checks[,1]
data3[,1] = checks[,1]
data4[,1] = checks[,1]
data5[,1] = checks[,1]
data6[,1] = checks[,1]
data7[,1] = checks[,1]
for(d in 1:601){
  data1[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\status_quoYR\\draw_0\\status_quoYR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the status quo nets
  data2[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus05YR\\draw_0\\minus05YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 5% 
  data3[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus10YR\\draw_0\\minus10YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 10%
  data4[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus15YR\\draw_0\\minus15YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 15%
  data5[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus20YR\\draw_0\\minus20YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 20%
  data6[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus25YR\\draw_0\\minus25YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 15%
  data7[d,2:4] = c(read.table(paste0('F:\\Ellies_output_folder\\non_inferiority\\minus30YR\\draw_0\\minus30YR_',checks[d,1],'_0.txt'),header=TRUE)$clin_inc_all[10:12]) #the lost 20%
  
}
colnames(data1) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data2) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data3) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data4) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data5) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data6) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")
colnames(data7) = c("DIDE_CODE","cases_2015","cases_2016","cases_2017")

baseline = merge(checks,data1,by="DIDE_CODE")
non_inf05 = merge(checks,data2,by="DIDE_CODE")
non_inf10 = merge(checks,data3,by="DIDE_CODE")
non_inf15 = merge(checks,data4,by="DIDE_CODE")
non_inf20 = merge(checks,data5,by="DIDE_CODE")
non_inf25 = merge(checks,data6,by="DIDE_CODE")
non_inf30 = merge(checks,data7,by="DIDE_CODE")

baseline$N_cases2015 = baseline$popn_2015*baseline$cases_2015  
baseline$N_cases2016 = baseline$popn_2016*baseline$cases_2016  
baseline$N_cases2017 = baseline$popn_2017*baseline$cases_2017  

non_inf05$N_cases2015 = non_inf05$popn_2015*non_inf05$cases_2015  
non_inf05$N_cases2016 = non_inf05$popn_2016*non_inf05$cases_2016  
non_inf05$N_cases2017 = non_inf05$popn_2017*non_inf05$cases_2017  

non_inf10$N_cases2015 = non_inf10$popn_2015*non_inf10$cases_2015  
non_inf10$N_cases2016 = non_inf10$popn_2016*non_inf10$cases_2016  
non_inf10$N_cases2017 = non_inf10$popn_2017*non_inf10$cases_2017  

non_inf15$N_cases2015 = non_inf15$popn_2015*non_inf15$cases_2015  
non_inf15$N_cases2016 = non_inf15$popn_2016*non_inf15$cases_2016  
non_inf15$N_cases2017 = non_inf15$popn_2017*non_inf15$cases_2017  

non_inf20$N_cases2015 = non_inf20$popn_2015*non_inf20$cases_2015  
non_inf20$N_cases2016 = non_inf20$popn_2016*non_inf20$cases_2016  
non_inf20$N_cases2017 = non_inf20$popn_2017*non_inf20$cases_2017  

non_inf25$N_cases2015 = non_inf25$popn_2015*non_inf25$cases_2015  
non_inf25$N_cases2016 = non_inf25$popn_2016*non_inf25$cases_2016  
non_inf25$N_cases2017 = non_inf25$popn_2017*non_inf25$cases_2017  

non_inf30$N_cases2015 = non_inf30$popn_2015*non_inf30$cases_2015  
non_inf30$N_cases2016 = non_inf30$popn_2016*non_inf30$cases_2016  
non_inf30$N_cases2017 = non_inf30$popn_2017*non_inf30$cases_2017  

for(i in 1:601){
  baseline$non_inf05_extra_cases[i] = sum(non_inf05[i,9] - baseline[i,9],non_inf05[i,10] - baseline[i,10],non_inf05[i,11] - baseline[i,11])
  baseline$non_inf10_extra_cases[i] = sum(non_inf10[i,9] - baseline[i,9],non_inf10[i,10] - baseline[i,10],non_inf10[i,11] - baseline[i,11])
  baseline$non_inf15_extra_cases[i] = sum(non_inf15[i,9] - baseline[i,9],non_inf15[i,10] - baseline[i,10],non_inf15[i,11] - baseline[i,11])
  baseline$non_inf20_extra_cases[i] = sum(non_inf20[i,9] - baseline[i,9],non_inf20[i,10] - baseline[i,10],non_inf20[i,11] - baseline[i,11])
  baseline$non_inf25_extra_cases[i] = sum(non_inf25[i,9] - baseline[i,9],non_inf25[i,10] - baseline[i,10],non_inf25[i,11] - baseline[i,11])
  baseline$non_inf30_extra_cases[i] = sum(non_inf30[i,9] - baseline[i,9],non_inf30[i,10] - baseline[i,10],non_inf30[i,11] - baseline[i,11])
  
}

for(i in 1:601){
  baseline$non_inf05_pc_extra_cases[i] = (sum(non_inf05[i,9],non_inf05[i,10],non_inf05[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf05[i,9],non_inf05[i,10],non_inf05[i,11])
  baseline$non_inf10_pc_extra_cases[i] = (sum(non_inf10[i,9],non_inf10[i,10],non_inf10[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf10[i,9],non_inf10[i,10],non_inf10[i,11])
  baseline$non_inf15_pc_extra_cases[i] = (sum(non_inf15[i,9],non_inf15[i,10],non_inf15[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf15[i,9],non_inf15[i,10],non_inf15[i,11])
  baseline$non_inf20_pc_extra_cases[i] = (sum(non_inf20[i,9],non_inf20[i,10],non_inf20[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf20[i,9],non_inf20[i,10],non_inf20[i,11])
  baseline$non_inf25_pc_extra_cases[i] = (sum(non_inf25[i,9],non_inf25[i,10],non_inf25[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf25[i,9],non_inf25[i,10],non_inf25[i,11])
  baseline$non_inf30_pc_extra_cases[i] = (sum(non_inf30[i,9],non_inf30[i,10],non_inf30[i,11]) - sum(baseline[i,9],baseline[i,10],baseline[i,11]))/sum(non_inf30[i,9],non_inf30[i,10],non_inf30[i,11])
  
}
head(baseline)

###########################################
##
## Maps

## load the relevent libraries
library(adegenet)
library(MalariaMap)
library(latticeExtra)
library(sp)
require(maptools)

simple_map2= function (data, z, breaks = seq(0, 1, length.out = 11), labs, ...) 
{
  M1 <- subset_world(...)
  M2 <- sp::merge(M1, data, by = "DIDE_CODE")
  sp::spplot(M2[z], at = breaks, col.regions = brewer.pal(9,"YlOrRd"), labels=labs)#YlGnBu
}

library(ggplot2)
#M1 <- subset_world(level = "Admin", Continent = "Africa")
#M2 <- sp::merge(M1, tr, by = "DIDE_CODE")
#M3 = subset_world(level = "Country", Continent = "Africa")
#M4 <- sp::merge(M3, tr_U, by = "ISO")
#b = spplot(M4["With80covIRSandRES50"], at = seq(0, 70, length.out = 9),fill=FALSE, col="black",lwd=1.5)
#spplot(M2["With80covIRSandRES50"], at = seq(0, 70, length.out = 9), col="grey",col.regions = brewer.pal(9,"YlOrRd")) + b
simple_map3= function (data, data_U, z, labs, seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9,...) 
{
  M1 <- subset_world(level = "Admin", Continent = "Africa")
  M2 <- sp::merge(M1, data, by = "DIDE_CODE")
  
  M3 = subset_world(level = "Country", Continent = "Africa")
  M4 <- sp::merge(M3, data_U, by = "ISO")
  
  b = spplot(M4[z], at = seq(seq1, seq9, length.out = 9),fill=FALSE, col="black",lwd=1.5,names.attr = labs,
             main = "Additional cases")
  spplot(M2[z], names.attr = c("Lost effect 20%",
                               "Lost effect 25%",
                               "Lost effect 30%",
                               "Lost effect 05%",
                               "Lost effect 10%",
                               "Lost effect 15%"),main = "Additional cases", at = c(seq1, seq2, seq3,  seq4,  seq5,  seq6,  seq7,  seq8,  seq9), col="grey",col.regions = brewer.pal(9,"YlOrRd"),
         colorkey =  list(space = "right", height = 0.8)) + b
  
  
}



##Work out how best to split the data
quantile(c(baseline$non_inf05_extra_cases,
           baseline$non_inf10_extra_cases,
           baseline$non_inf15_extra_cases,
           baseline$non_inf20_extra_cases,
           baseline$non_inf25_extra_cases,
           baseline$non_inf30_extra_cases),c(0.001,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.999))

quantile(c(baseline$non_inf05_pc_extra_cases,
           baseline$non_inf10_pc_extra_cases,
           baseline$non_inf15_pc_extra_cases,
           baseline$non_inf20_pc_extra_cases,
           baseline$non_inf25_pc_extra_cases,
           baseline$non_inf30_pc_extra_cases),c(0.001,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.999),na.rm=TRUE)

##maybe correct for the negatives? Depending on what is there
baseline$non_inf05_pc_extra_cases = ifelse(baseline$non_inf05_pc_extra_cases < 0,0,baseline$non_inf05_pc_extra_cases)
baseline$non_inf10_pc_extra_cases = ifelse(baseline$non_inf10_pc_extra_cases < 0,0,baseline$non_inf10_pc_extra_cases)
baseline$non_inf15_pc_extra_cases = ifelse(baseline$non_inf15_pc_extra_cases < 0,0,baseline$non_inf15_pc_extra_cases)
baseline$non_inf20_pc_extra_cases = ifelse(baseline$non_inf20_pc_extra_cases < 0,0,baseline$non_inf20_pc_extra_cases)
baseline$non_inf25_pc_extra_cases = ifelse(baseline$non_inf25_pc_extra_cases < 0,0,baseline$non_inf25_pc_extra_cases)
baseline$non_inf30_pc_extra_cases = ifelse(baseline$non_inf30_pc_extra_cases < 0,0,baseline$non_inf30_pc_extra_cases)

baseline$non_inf05_extra_cases = ifelse(baseline$non_inf05_extra_cases < 0,0,baseline$non_inf05_extra_cases)
baseline$non_inf10_extra_cases = ifelse(baseline$non_inf10_extra_cases < 0,0,baseline$non_inf10_extra_cases)
baseline$non_inf15_extra_cases = ifelse(baseline$non_inf15_extra_cases < 0,0,baseline$non_inf15_extra_cases)
baseline$non_inf20_extra_cases = ifelse(baseline$non_inf20_extra_cases < 0,0,baseline$non_inf20_extra_cases)
baseline$non_inf25_extra_cases = ifelse(baseline$non_inf25_extra_cases < 0,0,baseline$non_inf25_extra_cases)
baseline$non_inf30_extra_cases = ifelse(baseline$non_inf30_extra_cases < 0,0,baseline$non_inf30_extra_cases)

trRes_U =  baseline[!duplicated(baseline[,c('ISO')]),]

plot.new()
par(new = "TRUE",
    plt = c(0.1,0.9,0.1,0.3),
    las = 1,
    cex.axis = 1)

obj1 = simple_map3(data=baseline, data_U = trRes_U, 
                   z=c("non_inf20_extra_cases", #make sure these are the right way around.. tricky to tell but confirm with csv outputs
                       "non_inf25_extra_cases",
                       "non_inf30_extra_cases",
                       "non_inf05_extra_cases",
                       "non_inf10_extra_cases",
                       "non_inf15_extra_cases"),
                   seq1=0, seq2=2, seq3=1384,  seq4=10959,  seq5=36806,  seq6=126797,  seq7=361687,  seq8=598482,  seq9 = 9487494,
                   as.table = TRUE,
                   main = "Additional cases") ##Fig 3 panel G 
obj1


obj2 = simple_map3(data=baseline, data_U = trRes_U, 
                   z=c("non_inf20_pc_extra_cases",
                       "non_inf25_pc_extra_cases",
                       "non_inf30_pc_extra_cases",
                       "non_inf05_pc_extra_cases",
                       "non_inf10_pc_extra_cases",
                       "non_inf15_pc_extra_cases"),
                   seq1=0, seq2=0.0007, seq3=0.0274,  seq4=0.0572,  seq5=0.1044,  seq6=0.15466,  seq7=0.224,  seq8=0.342,  seq9 = 0.983,
                   as.table = TRUE,
                   main = "Additional cases (%)") ##Fig 3 panel G 
obj2

#write.csv(baseline,"H:\\Ellie\\non inferiority\\cases_non_inferiority.csv")
