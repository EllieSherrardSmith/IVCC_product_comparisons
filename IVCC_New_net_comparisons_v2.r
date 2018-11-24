
#########################################################
##
## Confirming new products fit the predictions of 'all LLINs' 
##
##
library(adegenet)
library(rstan)
options(mc.cores = parallel::detectCores())
library(MalariaLaunchR)


##Data list in "Original_DATA_cHURCHER20166.R"
exit_from_hut = rep_u/caught_u
mosq_survival_hut = 1 - dead_u/caught_u
successful = (mosq_survival_hut-exit_from_hut)

data_1 = data.frame(rep_u,caught_u,dead_u,is_treat,mosq_survival_hut)

deterred = (caught_u[data_1$is_treat == 0] - caught_u[data_1$is_treat == 1])/
                              caught_u[data_1$is_treat == 0] 


##Unwashed_hut_trial_data
G2_dat = read.csv("H:/Ellie/IRS and resistance/IVCC/Data_IVCC_newLLINsG2Interceptor/INTERCEPTOR_G2_DATA.R2.csv",header=TRUE)
G2_dat = subset(G2_dat,G2_dat$Study != "2")
names(G2_dat)

G2_dat$G2_deterred  = (G2_dat$N_mosq_C - G2_dat$N_mosq)/G2_dat$N_mosq_C
#G2_dat$G2_72hr_dead = ( (G2_dat$N_dead_72/G2_dat$N_mosq) *(1-(G2_dat$N_dead_72_C/G2_dat$N_mosq_C)) )
#G2_dat$G2_mosq_survival = 1 - G2_dat$G2_72hr_dead
#G2_dat$G2_exiting   = (G2_dat$N_exited/G2_dat$N_mosq) * (1 - G2_dat$G2_72hr_dead)
#G2_dat$G2_successful = 1 - G2_dat$G2_72hr_dead - G2_dat$G2_exiting

G2_dat$G2_72hr_dead = (G2_dat$N_dead_72/G2_dat$N_mosq)
G2_dat$G2_mosq_survival = 1 - G2_dat$G2_72hr_dead
G2_dat$G2_exiting   = (G2_dat$N_exited/G2_dat$N_mosq) * (1 - G2_dat$G2_72hr_dead)
G2_dat$G2_successful = 1 - G2_dat$G2_72hr_dead - G2_dat$G2_exiting

G2_dat$x = G2_dat$bioassay_mort_PERMETHRIN
G2_dat$survival = 1 - G2_dat$bioassay_mort_PERMETHRIN

plot(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"] ~
       G2_dat$survival[G2_dat$Net_type == "interceptor_LN"],ylim=c(0,1),xlim=c(0,1))
######################################################
## confirmation figures to continue as per Churcher et al 2016

par(mfrow=c(1,3))
plot(deterred ~ mosq_survival_hut[data_1$is_treat == 1],
     ylim = c(0,1),xlim=c(0,1),
     ylab="Deterred from entering hut with LLIN (%)",yaxt="n",
     xlab="Mosquito survival in hut trial (%)",xaxt="n",
     cex.lab=1.6,cex.axis = 1.6)
points(G2_dat$G2_deterred[G2_dat$Net_type == "interceptor_G2"] ~ 
         G2_dat$G2_mosq_survival[G2_dat$Net_type == "interceptor_G2"], pch=15)
axis(1,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)

plot(exit_from_hut[data_1$is_treat == 1] ~ mosq_survival_hut[data_1$is_treat == 1],
     ylim = c(0,1),xlim=c(0,1),
     ylab="Exiting hut without feeding (%)",yaxt="n",
     xlab="Mosquito survival in hut trial (%)",xaxt="n",
     cex.lab=1.6,cex.axis = 1.6)
points(G2_dat$G2_exiting[G2_dat$Net_type == "interceptor_G2"] ~ 
         G2_dat$G2_mosq_survival[G2_dat$Net_type == "interceptor_G2"], pch=15)
axis(1,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)

plot(successful[data_1$is_treat == 1] ~ mosq_survival_hut[data_1$is_treat == 1],
     ylim = c(0,1),xlim=c(0,1),
     ylab="Successful blood feeding (%)",yaxt="n",
     xlab="Mosquito survival in hut trial (%)",xaxt="n",
     cex.lab=1.6,cex.axis = 1.6)
points(G2_dat$G2_successful[G2_dat$Net_type == "interceptor_G2"] ~ 
         G2_dat$G2_mosq_survival[G2_dat$Net_type == "interceptor_G2"], pch=15)
axis(1,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)




###########################################
##
## How will Interceptor G2 retain function?
## 
par(mfrow=c(1,2))
par(mar=c(8,6,2,2))
plot(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2" & G2_dat$N_washes == 0] ~ 
       G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN" & G2_dat$N_washes == 0],ylim=c(0,1),xlim=c(0,1),
     ylab="",yaxt="n",cex=1.6,
     xlab="",xaxt="n",cex.axis=1.6,cex.lab=1.6)
axis(1,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
mtext(side = 2,line = 4, "Mosquito mortality in Interceptor G2 hut trial (%)",cex=1.6)
mtext(side = 1, line = 5, "Mosquito mortality in Interceptor LN hut trial (%)",cex=1.6)
points(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2" & G2_dat$N_washes == 0] ~ 
         G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN" & G2_dat$N_washes == 0],pch=20,cex=2)
abline(0,1,lty=2)

##Relationship 1 Normal LN and bioassay
surv_bioassay=seq(0,1,0.05)		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}
mort = 1 - surv_bioassay

ln_mort = 1 / (1 + exp(0.63 + 4*(surv_bioassay))) 


##Relationship 2 G2 benefit on top of Normal LN 
##LINEAR BENEFIT
#load libraries
library(rstan)
library(coda)
set.seed(20151204)
options(mc.cores = parallel::detectCores())
#the explanatory variables
dat<-data.frame(x1 = c(1,1,1,1,1,G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN" & G2_dat$N_washes == 0]))

#the model
X<-model.matrix(~x1,dat)

#the regression slopes
betas<-runif(2,-1,1)

#the standard deviation for the simulated data
sigma<-1

#the simulated data
y_norm<-c(1,1,1,1,1,G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2"  & G2_dat$N_washes == 0])

#a matrix to get the predicted y values
new_X<-model.matrix(~x1,expand.grid(x1=seq(0,max(dat$x1),length=100)))

#lines(sort(y_norm) ~ as.numeric(new_X[,2]))

#the model
m_norm<-stan(file="Q:/RProjects/IVCC_product_comparisons/biomortality_G2fit_linear.stan",
             data = list(N=12,N2=100,K=2,y=y_norm,X=X,new_X=new_X),
             pars = c("beta","sigma","y_pred"))
parm <- extract(m_norm)
names(parm)
mean(parm$beta)
#getting regression curves plus 95% credible intervals
new_x<-data.frame(x1=new_X)
new_y<-extract(m_norm,pars="y_pred")
pred<-apply(new_y[[1]],2,quantile,probs=c(0.025,0.5,0.975)) #the median line with 95% credible intervals

#plot
points(dat$x1,y_norm,pch=16)
lines(new_x$x1.x1[14:100],pred[2,14:100],col="red",lwd=3)
lines(new_x$x1.x1[14:100],pred[1,14:100],col="red",lwd=1,lty=2)
lines(new_x$x1.x1[14:100],pred[3,14:100],col="red",lwd=1,lty=2)


##Relationship 2 G2 benefit on top of Normal LN 
##LOGISTIC BENEFIT
d_t = c(100,100,100,100,100,G2_dat$N_dead_72[G2_dat$Net_type == "interceptor_G2" & G2_dat$N_washes == 0]) # number of mosquitoes dying IRS HUTS
n_t = c(100,100,100,100,100,G2_dat$N_mosq[G2_dat$Net_type == "interceptor_G2"  & G2_dat$N_washes == 0]) # Number of mosquitoes entering IRS huts
x1 = c(1,1,1,1,1,G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"  & G2_dat$N_washes == 0])
x = seq(0,1,length=100)
fm2 <- cbind(d_t,n_t-d_t) ~ x1 
glm_2 <- glm(fm2, family = binomial())
role.fitted2 <- predict(glm_2, se.fit = TRUE, type = "response")
summary(glm_2)$coeff[2,1]
summary(glm_2)$coeff[1,1]

g2_option2 = 1 / (1 + exp(-summary(glm_2)$coeff[2,1]*x - summary(glm_2)$coeff[1,1]))
g2_option2low = 1 / (1 + exp(-(summary(glm_2)$coeff[2,1]-summary(glm_2)$coeff[2,2])*x - (summary(glm_2)$coeff[1,1]-summary(glm_2)$coeff[1,2])))
g2_option2upp = 1 / (1 + exp(-(summary(glm_2)$coeff[2,1]+summary(glm_2)$coeff[2,2])*x - (summary(glm_2)$coeff[1,1]+summary(glm_2)$coeff[1,2])))

lines(g2_option2[14:100] ~ x[14:100], col="blue",lwd=3)
lines(g2_option2low[14:100] ~ x[14:100], col="blue",lwd=1, lty=2)
lines(g2_option2upp[14:100] ~ x[14:100], col="blue",lwd=1, lty=2)


#plot(ln_mort ~ surv_bioassay)

##linear
G2_benefit_lin = median(parm$beta) * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) + pred[2,1]
plot(G2_benefit_lin ~ surv_bioassay,ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",pch="",
     ylab="Mortlaity hut trial (%)",xlab="Survival at bioassay (%)",cex.lab=1.6)
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
#lines(G2_benefit_lin ~ surv_bioassay,col="red")
surv_bioassay2=seq(0,1,length=21)
lines(ln_mort ~ surv_bioassay2,lty=2)

##logistic
G2_benefit_log = 1 / (1 + exp(-summary(glm_2)$coeff[2,1] * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - summary(glm_2)$coeff[1,1] ))
lines(G2_benefit_log ~ surv_bioassay,col="blue",lwd=2)

G2_dat$bioassay_mort_PERMETHRIN[G2_dat$Net_type == "interceptor_LN"]
#Benefit of PBO in assay		
#g2beta1=	2
#g2beta2=	5
#g2beta3=	0.5

#g2_a  = 2*(1/(1+exp(beta1 * mort + beta2)))

#g2_benefit_a<-g2beta1+g2beta2*((1-surv_bioassay)-0.5)/(1+g2beta3*((1-surv_bioassay)-0.5))   
#g2_benefit<-exp(g2_benefit_a)/(1+exp(g2_benefit_a))

#lines(g2_benefit ~ surv_bioassay,lty=2,lwd=2)
lines(pbo_benefit ~ surv_bioassay,lty=2,lwd=2)

legend(0.1,0.30,legend=c("standard LN","PBO","G2"),
       col=c("black","black","blue"),
       lwd=c(1,2,2),lty=c(2,2,1))

#polygon(c(surv_bioassay[11],surv_bioassay[21],
#          surv_bioassay[21],surv_bioassay[11]),
 #       c(0,0,1,1),border=NA,
#        col=transp("grey",0.4))


###{Now running the model to see how good G2 looks}


fitM_function<-function(targ, site, 
                        
                        ITN, 
                        
                        itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                        itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                        itn_halflife_1,
                        
                        itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                        itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                        itn_halflife_2){
  
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('p:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/Site_G2RCT_', site, '.txt'))
  pop_size<- 1000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 1 overwrite 1 num_people', pop_size, 'itn 1 itn_coverage', ITN, 
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    
                    'irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## 
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="p:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_',site,'.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 1, interval = c(2, 50),
        maxiter = 10)
  
}
inp <- read.csv("p:/G2_interceptor_RCT_site_options_input_MODEL1.csv",header=TRUE)
head(inp)
targs = c(0.614,	0.614,	0.367,	0.367,	0.348,
          0.348,	0.0922,	0.1718,	0.1357,	
          0.502,	0.264, 0.484,	0.389,	0.349,	
          0.349,	0.349,	0.349,	0.0567)
ITNs = c(0.596,0.596,0.581,0.581,0.66,0.66,0.42,0.265,0.265,0.759,0.9821,0.9821,0.9821,0.7038,0.7038,0.7038,0.7038,0.78)

##these have 2 to 10 prev
for(i in 1:9){
  fitM_function(targ = targs[i], site = inp[i,1],
                ITN = ITNs[i], 
                itn_repel_fun_1 = inp[i,4], itn_repel_gamb_ss_1 = inp[i,5], itn_repel_arab_1 = inp[i,6],
                itn_kill_fun_1 = inp[i,7], itn_kill_gamb_ss_1 = inp[i,8], itn_kill_arab_1 = inp[i,9],
                itn_halflife_1 = inp[i,10],
                itn_repel_fun_2 = inp[i,11], itn_repel_gamb_ss_2 = inp[i,12], itn_repel_arab_2 = inp[i,13],
                itn_kill_fun_2 = inp[i,14], itn_kill_gamb_ss_2 = inp[i,15], itn_kill_arab_2 = inp[i,16],
                itn_halflife_2 = inp[i,17])
}
for(i in 7:9){ ####Change the interval to lower...
  fitM_function(targ = targs[i], site = inp[i,1],
                ITN = ITNs[i], 
                itn_repel_fun_1 = inp[i,4], itn_repel_gamb_ss_1 = inp[i,5], itn_repel_arab_1 = inp[i,6],
                itn_kill_fun_1 = inp[i,7], itn_kill_gamb_ss_1 = inp[i,8], itn_kill_arab_1 = inp[i,9],
                itn_halflife_1 = inp[i,10],
                itn_repel_fun_2 = inp[i,11], itn_repel_gamb_ss_2 = inp[i,12], itn_repel_arab_2 = inp[i,13],
                itn_kill_fun_2 = inp[i,14], itn_kill_gamb_ss_2 = inp[i,15], itn_kill_arab_2 = inp[i,16],
                itn_halflife_2 = inp[i,17])
}
##and i = 18

##these have 0.5 to 10
for(i in 10:13){
  
}

##these have 0 to 5
for (i in 14:17){
  
}

#######################################
##
## Once fitting is done can use this function for the actual runs
Run_seas<-function(site, 
                   
                   ITN, 
                   itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                   itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                   itn_halflife_1,
                   
                   itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                   itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                   itn_halflife_2,
                   
                   run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('p:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/Site_G2RCT_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 0 num_people', pop_size, 'itn 1 itn_coverage', ITN, 
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    
                    'irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("p:/Ellies_output_folder/G2_v2/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "p:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="p:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/Site_G2RCT_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}

inpG2 <- read.csv("p:/G2_interceptor_RCT_site_options_input_MODEL1_v2.csv",header=TRUE)
inpPBO <- read.csv("p:/G2_interceptor_RCT_site_options_input_MODEL1PBO_v2.csv",header=TRUE)
inpstandard <- read.csv("p:/G2_interceptor_RCT_site_options_input_noChangeNetType_v2.csv",header=TRUE)


###RUNS

for(i in 1:18){
  Run_seas(site = inpG2[i,1],
           ITN = inpG2[i,2], 
           
           itn_repel_fun_1 = inpG2[i,4], itn_repel_gamb_ss_1 = inpG2[i,5], itn_repel_arab_1 = inpG2[i,6],
           itn_kill_fun_1 = inpG2[i,7], itn_kill_gamb_ss_1 = inpG2[i,8], itn_kill_arab_1 = inpG2[i,9],
           itn_halflife_1 = inpG2[i,10],
           
           itn_repel_fun_2 = inpG2[i,11], itn_repel_gamb_ss_2 = inpG2[i,12], itn_repel_arab_2 = inpG2[i,13],
           itn_kill_fun_2 = inpG2[i,14], itn_kill_gamb_ss_2 = inpG2[i,15], itn_kill_arab_2 = inpG2[i,16],
           itn_halflife_2 = inpG2[i,17],
           
           run_name="Net_runs_G2")
  
}

for(i in 1:18){
  Run_seas(site = inpPBO[i,1],
           ITN = inpPBO[i,2], 
           
           itn_repel_fun_1 = inpPBO[i,4], itn_repel_gamb_ss_1 = inpPBO[i,5], itn_repel_arab_1 = inpPBO[i,6],
           itn_kill_fun_1 = inpPBO[i,7], itn_kill_gamb_ss_1 = inpPBO[i,8], itn_kill_arab_1 = inpPBO[i,9],
           itn_halflife_1 = inpPBO[i,10],
           
           itn_repel_fun_2 = inpPBO[i,11], itn_repel_gamb_ss_2 = inpPBO[i,12], itn_repel_arab_2 = inpPBO[i,13],
           itn_kill_fun_2 = inpPBO[i,14], itn_kill_gamb_ss_2 = inpPBO[i,15], itn_kill_arab_2 = inpPBO[i,16],
           itn_halflife_2 = inpPBO[i,17],
           
           run_name="Net_runs_PBO")
  
}

for(i in 1:18){
  Run_seas(site = inpstandard[i,1],
           ITN = inpstandard[i,2], 
           
           itn_repel_fun_1 = inpstandard[i,4], itn_repel_gamb_ss_1 = inpstandard[i,5], itn_repel_arab_1 = inpstandard[i,6],
           itn_kill_fun_1 = inpstandard[i,7], itn_kill_gamb_ss_1 = inpstandard[i,8], itn_kill_arab_1 = inpstandard[i,9],
           itn_halflife_1 = inpstandard[i,10],
           
           itn_repel_fun_2 = inpstandard[i,11], itn_repel_gamb_ss_2 = inpstandard[i,12], itn_repel_arab_2 = inpstandard[i,13],
           itn_kill_fun_2 = inpstandard[i,14], itn_kill_gamb_ss_2 = inpstandard[i,15], itn_kill_arab_2 = inpstandard[i,16],
           itn_halflife_2 = inpstandard[i,17],
           
           run_name="Net_runs_standard")
  
}

##Glabat fitted separately

##############################
##
##
## Plotting these out
loc1 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_1_0.txt",header = TRUE) #BF = Diebougou
loc2 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_2_0.txt",header = TRUE) #BF = Danu
loc3 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_3_0.txt",header = TRUE) #Mali = Baroueli
loc4 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_4_0.txt",header = TRUE) #Mali = Bla
loc5 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_5_0.txt",header = TRUE) #Mali = Kati
loc6 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_6_0.txt",header = TRUE) #Mali = Koulikoro
loc7 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_7_0.txt",header = TRUE) #Malawi = Phalombe
loc8 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_8_0.txt",header = TRUE) #Malawi = Lilongwe 
loc9 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_9_0.txt",header = TRUE) #Malawi = Chitipa

loc10 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_10_0.txt",header = TRUE) #Benin = Plateau 
loc11 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_11_0.txt",header = TRUE) #Uganda = Walukuba
loc12 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_12_0.txt",header = TRUE) #Uganda = Kihihi
loc13 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_13_0.txt",header = TRUE) #Uganda = Nagongera
loc14 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_14_0.txt",header = TRUE) #Kenya = Bondo
loc15 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_15_0.txt",header = TRUE) #Kenya = Rachuonyo
loc16 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_16_0.txt",header = TRUE) #Kenya = Nyando 
loc17 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_17_0.txt",header = TRUE) #Kenya = Teso
loc18 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_0\\Net_runs_G2_18_0.txt",header = TRUE) #Senegal = Kedougou
loc19 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2_v2\\draw_0\\Net_runs_G2_v2_19_0.txt",header = TRUE) #Sudan = Galabat nets only
loc20 =  read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2_v2\\draw_0\\Net_runs_G2_v2_20_0.txt",header = TRUE) #Sudan = Galabat nets + sprays

loc1n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_1_0.txt",header = TRUE)
loc2n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_2_0.txt",header = TRUE)
loc3n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_3_0.txt",header = TRUE)
loc4n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_4_0.txt",header = TRUE)
loc5n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_5_0.txt",header = TRUE)
loc6n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_6_0.txt",header = TRUE)
loc7n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_7_0.txt",header = TRUE)
loc8n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_8_0.txt",header = TRUE)
loc9n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_9_0.txt",header = TRUE)

loc10n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_10_0.txt",header = TRUE)
loc11n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_11_0.txt",header = TRUE)
loc12n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_12_0.txt",header = TRUE)
loc13n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_13_0.txt",header = TRUE)
loc14n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_14_0.txt",header = TRUE)
loc15n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_15_0.txt",header = TRUE)
loc16n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_16_0.txt",header = TRUE)
loc17n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_17_0.txt",header = TRUE)
loc18n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_0\\Net_runs_standard_18_0.txt",header = TRUE)
loc19n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard_v2\\draw_0\\Net_runs_standard_v2_19_0.txt",header = TRUE)
loc20n = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard_v2\\draw_0\\Net_runs_standard_v2_20_0.txt",header = TRUE)

loc1p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_1_0.txt",header = TRUE)
loc2p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_2_0.txt",header = TRUE)
loc3p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_3_0.txt",header = TRUE)
loc4p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_4_0.txt",header = TRUE)
loc5p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_5_0.txt",header = TRUE)
loc6p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_6_0.txt",header = TRUE)
loc7p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_7_0.txt",header = TRUE)
loc8p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_8_0.txt",header = TRUE)
loc9p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_9_0.txt",header = TRUE)

loc10p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_10_0.txt",header = TRUE)
loc11p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_11_0.txt",header = TRUE)
loc12p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_12_0.txt",header = TRUE)
loc13p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_13_0.txt",header = TRUE)
loc14p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_14_0.txt",header = TRUE)
loc15p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_15_0.txt",header = TRUE)
loc16p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_16_0.txt",header = TRUE)
loc17p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_17_0.txt",header = TRUE)
loc18p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_0\\Net_runs_PBO_18_0.txt",header = TRUE)
loc19p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO_v2\\draw_0\\Net_runs_PBO_v2_19_0.txt",header = TRUE)
loc20p = read.table("F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO_v2\\draw_0\\Net_runs_PBO_v2_20_0.txt",header = TRUE)

plots_fun = function(data1, data2,outcome,cols,location,measure,targsval){
  plot(data1[,measure] ~ data1$year,pch="",ylim=c(0,ifelse(measure == 2,1,3)),
       xlim=c(-1,3),ylab=outcome,yaxt="n",
       xlab="Time years)",cex.lab=1.6,cex.axis=1.6,xaxt="n")
  axis(1,at=c(-1,0,1,2,3),cex.lab=1.6,cex.axis=1.6)
  axis(2,las=2,at=seq(0,3,0.2),cex.lab=1.6,cex.axis=1.6)
  
  abline(v=0,lty=2)
  #abline(v=3,lty=2)
  #abline(v=6,lty=2)
  abline(v=3,lty=3,col="grey20")
  
  lines(data1[,measure] ~ data1$year,lwd=2,col=cols,lty=1)
  lines(data2[,measure] ~ data2$year,lwd=2,col=cols,lty=2)
  
  #text(0,ifelse(measure==2,0.98,1.44),"Intervention (80% net cover)",cex=1.6)
  #text(3,ifelse(measure==3,0.92,1.35),"Introduce increasing resistance (0 - 100%)",cex=1.2)
  #text(6,ifelse(measure==3,0.84,1.26),"Switch to G2 nets",cex=1.2)
  #text(9,ifelse(measure==3,0.78,1.17),"G2 vs standard nets comparison",cex=1.2)
  
#  if(measure == 2){
#    legend(-0.3,0.4,
#           title = "Net type",bty="n",
#           legend=c("Standard net","Interceptor G2"),
#           lty=c(2,1),cex=1.6)
#  } else {}
  
  text(0,ifelse(measure == 2,1,3),location,cex=1.5)
  text(2.5,ifelse(measure == 2,1,3),targsval,cex=1.5)
}

targs = c("A","B","C","D","E","F",
          "G","H","I","J","K","L",
          "M","N","O","P","Q","R")

plots_fun(loc1,loc1n,"Prevalence in 2 to 10 year olds",
          "blue","Burkina Faso: (Diebougou)",2,targsval=targs[1])
plots_fun(loc1,loc1n,"Clinical cases (per person per year)",
          "blue","Burkina Faso: (Diebougou)",10,targsval=targs[1])
plots_fun(loc2,loc2n,"Prevalence in 2 to 10 year olds",
          "blue","Burkina Faso: (Danu)",2,targsval=targs[2])
plots_fun(loc2,loc2n,"Clinical cases (per person per year)",
          "blue","Burkina Faso: (Danu)",10,targsval=targs[2])

plots_fun(loc3,loc3n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Segou (Bareouli)",2,targsval=targs[3])
plots_fun(loc3,loc3n,"Clinical cases (per person per year)",
          "blue","Mali: Segou (Bareouli)",10,targsval=targs[3])
plots_fun(loc4,loc4n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Segou (Bla)",2,targsval=targs[4])
plots_fun(loc4,loc4n,"Clinical cases (per person per year)",
          "blue","Mali: Segou (Bla)",10,targsval=targs[4])

plots_fun(loc5,loc5n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Koulikoro (Kati)",2,targsval=targs[5])
plots_fun(loc5,loc5n,"Clinical cases (per person per year)",
          "blue","Mali: Koulikoro (Kati)",10,targsval=targs[5])
plots_fun(loc6,loc6n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Koulikoro (Koulikoro)",2,targsval=targs[6])
plots_fun(loc6,loc6n,"Clinical cases (per person per year)",
          "blue","Mali: Koulikoro (Koulikoro)",10,targsval=targs[6])

plots_fun(loc8,loc8n,"Prevalence in 2 to 10 year olds",
          "blue","Malawi: Southern region (Phalombe)",2,targsval=targs[8])
plots_fun(loc8,loc8n,"Clinical cases (per person per year)",
          "blue","Malawi: Southern region (Phalombe)",10,targsval=targs[8])
plots_fun(loc7,loc7n,"Prevalence in 2 to 10 year olds",
          "blue","Malawi: Central region (Lilongwe)",2,targsval=targs[7])
plots_fun(loc7,loc7n,"Clinical cases (per person per year)",
          "blue","Malawi: Central region (Lilongwe)",10,targsval=targs[7])
plots_fun(loc9,loc9n,"Prevalence in 2 to 10 year olds",
          "blue","Malawi: Northern region (Chitipa)",2,targsval=targs[9])
plots_fun(loc9,loc9n,"Clinical cases (per person per year)",
          "blue","Malawi: Northern region (Chitipa)",10,targsval=targs[9])

plots_fun(loc10,loc10n,"Prevalence in 2 to 10 year olds",
          "blue","Benin: (Plateau)",2,targsval=targs[10])
plots_fun(loc10,loc10n,"Clinical cases (per person per year)",
          "blue","Benin: (Plateau)",10,targsval=targs[10])

plots_fun(loc11,loc11n,"Prevalence in 2 to 10 year olds",
          "blue","Uganda: (Walukuba, Jinja)",2,targsval=targs[11])
plots_fun(loc11,loc11n,"Clinical cases (per person per year)",
          "blue","Uganda: (Walukuba, Jinja)",10,targsval=targs[11])
plots_fun(loc12,loc12n,"Prevalence in 2 to 10 year olds",
          "blue","Uganda: (Kihihi)",2,targsval=targs[12])
plots_fun(loc12,loc12n,"Clinical cases (per person per year)",
          "blue","Uganda: (Kihihi)",10,targsval=targs[12])
plots_fun(loc13,loc13n,"Prevalence in 2 to 10 year olds",
          "blue","Uganda: (Nagongera, Jinja)",2,targsval=targs[13])
plots_fun(loc13,loc13n,"Clinical cases (per person per year)",
          "blue","Uganda: (Nagongera, Jinja)",10,targsval=targs[13])

plots_fun(loc14,loc14n,"Prevalence in 2 to 10 year olds",
          "blue","Kenya: (Bondo, Western)",2,targsval=targs[14])
plots_fun(loc14,loc14n,"Clinical cases (per person per year)",
          "blue","Kenya: (Bondo, Western)",10,targsval=targs[14])
plots_fun(loc15,loc15n,"Prevalence in 2 to 10 year olds",
          "blue","Kenya: (Rachuonyo, Western)",2,targsval=targs[15])
plots_fun(loc15,loc15n,"Clinical cases (per person per year)",
          "blue","Kenya: (Rachuonyo, Western)",10,targsval=targs[15])
plots_fun(loc16,loc16n,"Prevalence in 2 to 10 year olds",
          "blue","Kenya: (Nyando, Western)",2,targsval=targs[16])
plots_fun(loc16,loc16n,"Clinical cases (per person per year)",
          "blue","Kenya: (Nyando, Western)",10,targsval=targs[16])
plots_fun(loc17,loc17n,"Prevalence in 2 to 10 year olds",
          "blue","Kenya: (Teso, Western)",2,targsval=targs[17])
plots_fun(loc17,loc17n,"Clinical cases (per person per year)",
          "blue","Kenya: (Teso, Western)",10,targsval=targs[17])

plots_fun(loc18,loc18n,"Prevalence in 2 to 10 year olds",
          "blue","Senegal: (Kedougou District)",2,targsval=targs[18])
plots_fun(loc18,loc18n,"Clinical cases (per person per year)",
          "blue","Sudan: (Galabat, Gedarif State)",10,targsval=targs[18])

##For Galabat


trialg2nets = trialg2nets_irs = trialnets = trialnets_irs = trialpbonets = trialpbonets_irs = expand.grid(time = seq(-5,31,length=nrow(loc20)))

trialg2nets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_1\\Net_runs_G2_19_1.txt'),header=TRUE)[,1]
trialg2nets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_1\\Net_runs_G2_20_1.txt'),header=TRUE)[,1]
trialnets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_1\\Net_runs_standard_19_1.txt'),header=TRUE)[,1]
trialnets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_1\\Net_runs_standard_19_1.txt'),header=TRUE)[,1]
trialpbonets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_1\\Net_runs_PBO_19_1.txt'),header=TRUE)[,1]
trialpbonets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_1\\Net_runs_PBO_19_1.txt'),header=TRUE)[,1]

draw = 1:50
for(j in 1:50){
  trialg2nets[,j+1]       =     read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_', draw[j], '\\Net_runs_G2_19_', draw[j], '.txt'),header=TRUE)[,2]
  trialg2nets_irs[,j+1]       = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_', draw[j], '\\Net_runs_G2_20_', draw[j], '.txt'),header=TRUE)[,2]
  trialnets[,j+1] =       read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_', draw[j], '\\Net_runs_standard_19_', draw[j], '.txt'),header=TRUE)[,2]
  trialnets_irs[,j+1] =   read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_', draw[j], '\\Net_runs_standard_20_', draw[j], '.txt'),header=TRUE)[,2]
  trialpbonets[,j+1] =         read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_', draw[j], '\\Net_runs_PBO_19_', draw[j], '.txt'),header=TRUE)[,2]
  trialpbonets_irs[,j+1]      =      read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_', draw[j], '\\Net_runs_PBO_20_', draw[j], '.txt'),header=TRUE)[,2]
  
    }

upper = lower = medians = array(dim=c(1301,6))
for(i in 1:1301){
  upper[i,1] =  quantile(as.numeric(trialg2nets[i,2:50]),0.1)
  lower[i,1] =  quantile(as.numeric(trialg2nets[i,2:50]),0.9)
  upper[i,2] =  quantile(as.numeric(trialg2nets_irs[i,2:50]),0.1)
  lower[i,2] =  quantile(as.numeric(trialg2nets_irs[i,2:50]),0.9)
  upper[i,3] =  quantile(as.numeric(trialnets[i,2:50]),0.1)
  lower[i,3] =  quantile(as.numeric(trialnets[i,2:50]),0.9)
  upper[i,4] =  quantile(as.numeric(trialnets_irs[i,2:50]),0.1)
  lower[i,4] =  quantile(as.numeric(trialnets_irs[i,2:50]),0.9)
  upper[i,5] =  quantile(as.numeric(trialpbonets[i,2:50]),0.1)
  lower[i,5] =  quantile(as.numeric(trialpbonets[i,2:50]),0.9)
  upper[i,6] =  quantile(as.numeric(trialpbonets_irs[i,2:50]),0.1)
  lower[i,6] =  quantile(as.numeric(trialpbonets_irs[i,2:50]),0.9)
  
  medians[i,1] = median(as.numeric(trialg2nets[i,2:50]))
  medians[i,2] = median(as.numeric(trialg2nets_irs[i,2:50]))
  medians[i,3] = median(as.numeric(trialnets[i,2:50]))
  medians[i,4] = median(as.numeric(trialnets_irs[i,2:50]))
  medians[i,5] = median(as.numeric(trialpbonets[i,2:50]))
  medians[i,6] = median(as.numeric(trialpbonets_irs[i,2:50]))
}
par(mar=c(5,6,4,2))
plot(loc19$prev_2_10 ~ loc19$year,pch="",ylim=c(0,0.35),
     xlim=c(-1,3),xaxt="n",
     ylab="",yaxt="n",main="Galabat",
     xlab="Time years",cex.lab=1.6,cex.axis=1.6)
axis(1,at=c(-1,0,1,2,3),labels = 2010:2014, cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,3,0.05),cex.lab=1.6,cex.axis=1.6)
mtext(side=2,las=0,"Prevalence (2 - 10-years)",line=4,cex=1.6)
abline(v=6,lty=2)

#polygon(c(trialg2nets[,1],rev(trialg2nets[,1])),c(upper[,1],rev(lower[,1])),border=NA,col=transp("blue",0.2))
#polygon(c(trialg2nets[,2],rev(trialg2nets[,2])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("blue",0.2))
polygon(c(trialnets[,1],rev(trialnets[,1])),c(upper[,3],rev(lower[,3])),border=NA,col=transp("red",0.5))
#polygon(c(trialpbonets[,1],rev(trialpbonets[,1])),c(upper[,5],rev(lower[,5])),border=NA,col=transp("blue",0.4))

#lines(loc19$prev_2_10 ~ loc19$year,lwd=2,col="blue",lty=2)
lines(loc19n$prev_2_10 ~ loc19n$year,lwd=2,col="darkred",lty=1)
#lines(loc19p$prev_2_10 ~ loc19p$year,lwd=2,col="blue",lty=3)


#polygon(c(trialg2nets_irs[,1],rev(trialg2nets_irs[,1])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("red",0.2))
##polygon(c(trialg2nets[,2],rev(trialg2nets[,2])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("blue",0.2))
polygon(c(trialnets_irs[,1],rev(trialnets_irs[,1])),c(upper[,4],rev(lower[,4])),border=NA,col=transp("blue",0.5))
#polygon(c(trialpbonets_irs[,1],rev(trialpbonets_irs[,1])),c(upper[,6],rev(lower[,6])),border=NA,col=transp("red",0.4))


#lines(loc20$prev_2_10 ~ loc20$year,lwd=2,col="darkred",lty=2)
lines(loc20n$prev_2_10 ~ loc20n$year,lwd=2,col="blue",lty=2)
#lines(loc20p$prev_2_10 ~ loc19$year,lwd=2,col="darkred",lty=3)

#legend(1,0.3,legend=c("Standard nets","Nets with IRS"),
#       lty = c(1,2),col=c(transp(c("red","blue"),0.5)),pch=15,cex=1.2)

ngal = c(7,5,5)/100#measured prev nets
nsgal = c(10,4,3)/100#measured prev nets and sprays

ngalu = c(3,2,3)/100 #mins
nsgalu = c(6,2,2)/100

ngall = c(14,10,9)/100 # max
nsgall = c(16,7,5)/100

x_vals = c(1,2,3)

for(i in 1:3){
  segments(x0=x_vals[i],x1=x_vals[i],y0=ngalu[i],y1=ngall[i],col="darkred",lwd=2)
  segments(x0=x_vals[i]+0.05,x1=x_vals[i]+0.05,y0=nsgalu[i],y1=nsgall[i],col="blue",lwd=2)
}
points(ngal ~ x_vals,pch=15,col=transp("darkred",0.8),cex=1.4)
points(nsgal ~ c(x_vals+0.05),pch=15,col=transp("blue",0.8),cex=1.4)

#legend(2,0.3,legend=c("Standard nets","G2 nets","PBO nets","No IRS","With IRS"),
#       lty = c(1,2,3,NA,NA),col=c("black","black","black",transp(c("blue","red"),0.2)),pch=15,cex=1.2)



##
### Repeating for 0 -10 prevalence
trialg2nets = trialg2nets_irs = trialnets = trialnets_irs = trialpbonets = trialpbonets_irs = expand.grid(time = seq(-5,31,length=nrow(loc20)))

trialg2nets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_1\\Net_runs_G2_19_1.txt'),header=TRUE)[,1]
trialg2nets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_1\\Net_runs_G2_20_1.txt'),header=TRUE)[,1]
trialnets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_1\\Net_runs_standard_19_1.txt'),header=TRUE)[,1]
trialnets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_1\\Net_runs_standard_19_1.txt'),header=TRUE)[,1]
trialpbonets[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_1\\Net_runs_PBO_19_1.txt'),header=TRUE)[,1]
trialpbonets_irs[,1] = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_1\\Net_runs_PBO_19_1.txt'),header=TRUE)[,1]

draw = 1:50
for(j in 1:50){
  trialg2nets[,j+1]       =     read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_', draw[j], '\\Net_runs_G2_19_', draw[j], '.txt'),header=TRUE)[,4]
  trialg2nets_irs[,j+1]       = read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_G2\\draw_', draw[j], '\\Net_runs_G2_20_', draw[j], '.txt'),header=TRUE)[,4]
  trialnets[,j+1] =       read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_', draw[j], '\\Net_runs_standard_19_', draw[j], '.txt'),header=TRUE)[,4]
  trialnets_irs[,j+1] =   read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_standard\\draw_', draw[j], '\\Net_runs_standard_20_', draw[j], '.txt'),header=TRUE)[,4]
  trialpbonets[,j+1] =         read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_', draw[j], '\\Net_runs_PBO_19_', draw[j], '.txt'),header=TRUE)[,4]
  trialpbonets_irs[,j+1]      =      read.table(paste0('F:\\Ellies_output_folder\\G2_v2\\Net_runs_PBO\\draw_', draw[j], '\\Net_runs_PBO_20_', draw[j], '.txt'),header=TRUE)[,4]
  
}

upper = lower = medians = array(dim=c(1301,6))
for(i in 1:1301){
  upper[i,1] =  quantile(as.numeric(trialg2nets[i,2:50]),0.1)
  lower[i,1] =  quantile(as.numeric(trialg2nets[i,2:50]),0.9)
  upper[i,2] =  quantile(as.numeric(trialg2nets_irs[i,2:50]),0.1)
  lower[i,2] =  quantile(as.numeric(trialg2nets_irs[i,2:50]),0.9)
  upper[i,3] =  quantile(as.numeric(trialnets[i,2:50]),0.1)
  lower[i,3] =  quantile(as.numeric(trialnets[i,2:50]),0.9)
  upper[i,4] =  quantile(as.numeric(trialnets_irs[i,2:50]),0.1)
  lower[i,4] =  quantile(as.numeric(trialnets_irs[i,2:50]),0.9)
  upper[i,5] =  quantile(as.numeric(trialpbonets[i,2:50]),0.1)
  lower[i,5] =  quantile(as.numeric(trialpbonets[i,2:50]),0.9)
  upper[i,6] =  quantile(as.numeric(trialpbonets_irs[i,2:50]),0.1)
  lower[i,6] =  quantile(as.numeric(trialpbonets_irs[i,2:50]),0.9)
  
  medians[i,1] = median(as.numeric(trialg2nets[i,2:50]))
  medians[i,2] = median(as.numeric(trialg2nets_irs[i,2:50]))
  medians[i,3] = median(as.numeric(trialnets[i,2:50]))
  medians[i,4] = median(as.numeric(trialnets_irs[i,2:50]))
  medians[i,5] = median(as.numeric(trialpbonets[i,2:50]))
  medians[i,6] = median(as.numeric(trialpbonets_irs[i,2:50]))
}
par(mar=c(5,6,4,2))
plot(loc19$prev_0_10 ~ loc19$year,pch="",ylim=c(0,0.3),
     xlim=c(0,6),ylab="",yaxt="n",main="Galabat",
     xlab="Time years",cex.lab=1.6,cex.axis=1.6,xaxt="n")
axis(1,at=c(-1,0,1,2,3,4,5,6),labels = 2010:2017, cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,3,0.05),cex.lab=1.6,cex.axis=1.6)
mtext(side=2,las=0,"Prevalence (0 - 10-years)",line=4,cex=1.6)
abline(v=3,lty=2)

polygon(c(trialg2nets[,1],rev(trialg2nets[,1])),c(upper[,1],rev(lower[,1])),border=NA,col=transp("blue",0.2))
#polygon(c(trialg2nets[,2],rev(trialg2nets[,2])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("blue",0.2))
polygon(c(trialnets[,1],rev(trialnets[,1])),c(upper[,3],rev(lower[,3])),border=NA,col=transp("orange",0.2))
polygon(c(trialpbonets[,1],rev(trialpbonets[,1])),c(upper[,5],rev(lower[,5])),border=NA,col=transp("blue",0.4))

lines(loc19$prev_2_10 ~ loc19$year,lwd=2,col="blue",lty=2)
lines(loc19n$prev_2_10 ~ loc19n$year,lwd=2,col="black",lty=1)
lines(loc19p$prev_2_10 ~ loc19p$year,lwd=2,col="blue",lty=3)


polygon(c(trialg2nets_irs[,1],rev(trialg2nets_irs[,1])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("red",0.2))
#polygon(c(trialg2nets[,2],rev(trialg2nets[,2])),c(upper[,2],rev(lower[,2])),border=NA,col=transp("blue",0.2))
polygon(c(trialnets_irs[,1],rev(trialnets_irs[,1])),c(upper[,4],rev(lower[,4])),border=NA,col=transp("orange",0.2))
polygon(c(trialpbonets_irs[,1],rev(trialpbonets_irs[,1])),c(upper[,6],rev(lower[,6])),border=NA,col=transp("red",0.4))


lines(loc20$prev_2_10 ~ loc20$year,lwd=2,col="darkred",lty=2)
lines(loc20n$prev_2_10 ~ loc20n$year,lwd=2,col="black",lty=1)
lines(loc20p$prev_2_10 ~ loc20p$year,lwd=2,col="darkred",lty=3)

legend(2,0.3,legend=c("Standard nets","G2 nets","PBO nets","No IRS","With IRS"),
       lty = c(1,2,3,NA,NA),col=c("black","black","black",transp(c("blue","red"),0.2)),pch=15,cex=1.2)

#################################################
## Calculating smoothed clinical incidence
## Averaging over the year post intervention
fun_clin = function(dat1n,dat1){
  effect_inc = (dat1n$clin_inc_all -
                  dat1$clin_inc_all) / 
    dat1n$clin_inc_all
  
  cases_av = (dat1n$clin_inc_all -
                  dat1$clin_inc_all)
  
  means2 = mean(effect_inc[262:365]) ## up to 3 years
  sums2 = sum(effect_inc[262:365])
  per95_2 = quantile(effect_inc[262:365],c(0.05,0.95)) ## up to 2 years after smoothed
  
  means3 = mean(effect_inc[262:417]) ## up to 3 years
  sums3 = sum(effect_inc[262:417])
  per95_3 = quantile(effect_inc[262:417],c(0.05,0.95)) ## up to 2 years after smoothed
  
  means2ca = mean(cases_av[262:365]) ## up to 3 years
  sums2ca = sum(cases_av[262:365])
  per95_2ca = quantile(cases_av[262:365],c(0.05,0.95)) ## up to 2 years after smoothed
  
  means3ca = mean(cases_av[262:417]) ## up to 3 years
  sums3ca = sum(cases_av[262:417])
  per95_3ca = quantile(cases_av[262:417],c(0.05,0.95)) ## up to 2 years after smoothed
  
  
  return(list(Years2 = cbind(means2,
                    sums2,
                    per95_2),
              Years3 = cbind(means3,
              sums3,
              per95_3),
              Years2ca = cbind(means2ca,
                             sums2ca,
                             per95_2ca),
              Years3ca = cbind(means3ca,
                             sums3ca,
                             per95_3ca)))
}

fun_clin(loc1n,loc1)
fun_clin(loc2n,loc2)
fun_clin(loc3n,loc3)
fun_clin(loc4n,loc4)
fun_clin(loc5n,loc5)
fun_clin(loc6n,loc6)
fun_clin(loc7n,loc7)
fun_clin(loc8n,loc8)
fun_clin(loc9n,loc9)

fun_clin(loc10n,loc10)
fun_clin(loc11n,loc11)
fun_clin(loc12n,loc12)
fun_clin(loc13n,loc13)
fun_clin(loc14n,loc14)
fun_clin(loc15n,loc15)
fun_clin(loc16n,loc16)
fun_clin(loc17n,loc17)
fun_clin(loc18n,loc18)

fun_clin(loc1n,loc1p)
fun_clin(loc2n,loc2p)
fun_clin(loc3n,loc3p)
fun_clin(loc4n,loc4p)
fun_clin(loc5n,loc5p)
fun_clin(loc6n,loc6p)
fun_clin(loc7n,loc7p)
fun_clin(loc8n,loc8p)
fun_clin(loc9n,loc9p)

fun_clin(loc10n,loc10p)
fun_clin(loc11n,loc11p)
fun_clin(loc12n,loc12p)
fun_clin(loc13n,loc13p)
fun_clin(loc14n,loc14p)
fun_clin(loc15n,loc15p)
fun_clin(loc16n,loc16p)
fun_clin(loc17n,loc17p)
fun_clin(loc18n,loc18p)


fun_clin_GALABAT = function(dat1n,dat1){
  effect_inc = (dat1n$clin_inc_all -
                  dat1$clin_inc_all) / 
    dat1n$clin_inc_all
  
  cases_av = (dat1n$clin_inc_all -
                dat1$clin_inc_all)
  
  means2 = mean(effect_inc[573:677],na.rm = TRUE) ## up to 2 years
  sums2 = sum(effect_inc[573:677])
  per95_2 = quantile(effect_inc[573:677],c(0.05,0.95),na.rm = TRUE) ## up to 2 years after smoothed
  
  means3 = mean(effect_inc[573:729],na.rm = TRUE) ## up to 3 years
  sums3 = sum(effect_inc[573:729])
  per95_3 = quantile(effect_inc[573:729],c(0.05,0.95),na.rm = TRUE) ## up to 2 years after smoothed
  
  means2ca = mean(cases_av[573:677],na.rm = TRUE) ## up to 3 years
  sums2ca = sum(cases_av[573:677])
  per95_2ca = quantile(cases_av[573:677],c(0.05,0.95),na.rm = TRUE) ## up to 2 years after smoothed
  
  means3ca = mean(cases_av[573:729],na.rm = TRUE) ## up to 3 years
  sums3ca = sum(cases_av[573:729])
  per95_3ca = quantile(cases_av[573:729],c(0.05,0.95),na.rm = TRUE) ## up to 2 years after smoothed
  
  
  return(list(Years2 = cbind(means2,
                             sums2,
                             per95_2),
              Years3 = cbind(means3,
                             sums3,
                             per95_3),
              Years2ca = cbind(means2ca,
                               sums2ca,
                               per95_2ca),
              Years3ca = cbind(means3ca,
                               sums3ca,
                               per95_3ca)))
}

fun_clin_GALABAT(loc19n,loc19)
fun_clin_GALABAT(loc20n,loc20)

fun_clin_GALABAT(loc19n,loc19p)
fun_clin_GALABAT(loc20n,loc20p)

##################################################
## Calculating prevalence in 2 to 10-years
## Point prevalence over the year after 
which(loc1$year == 1.92308e-02)
which(loc1$year == 1)
which(loc1$year == 2)
which(loc1$year == 3)
which(loc1$year == 6)
which(loc1$year == 7)
which(loc1$year == 8)

fun_prev = function(dat1n,dat1){
  effect = 100*((dat1n$prev_2_10_smooth -
              dat1$prev_2_10_smooth) / 
    dat1n$prev_2_10_smooth)
  
  reduction_prev = 100*((dat1n$prev_2_10_smooth -
                      dat1$prev_2_10_smooth) )
  
  return(list(c(effect[313], ## 1 year after the start of the intervention
                effect[365], ## 2 year after the start of the intervention
                effect[417]),
              c(reduction_prev[313],
                reduction_prev[365],
                reduction_prev[417])) ) ## 1 year after the start of the intervention
}

fun_prev(loc1n,loc1) #Burkina1_Diebougou
fun_prev(loc2n,loc2) #Burkina2_Danu
fun_prev(loc3n,loc3) #Mali_Baroueli
fun_prev(loc4n,loc4) #Mali_Bla
fun_prev(loc5n,loc5) #Mali_Kati
fun_prev(loc6n,loc6) #Mali_Koulikoro
fun_prev(loc7n,loc7) #Malawi_Phalombe
fun_prev(loc8n,loc8) #Malawi_Lilongwe
fun_prev(loc9n,loc9) #Malawi_Chitipa

fun_prev(loc10n,loc10) #Benin
fun_prev(loc11n,loc11) #Uganda
fun_prev(loc12n,loc12) #Uganda
fun_prev(loc13n,loc13) #Uganda
fun_prev(loc14n,loc14) #Kenya Bondo
fun_prev(loc15n,loc15) #Kenya Ra
fun_prev(loc16n,loc16) #Kenya Nyando
fun_prev(loc17n,loc17) #Kenya Teso
fun_prev(loc18n,loc18) #Sudan Gerataf

fun_prevGal = function(dat1n,dat1){
  effect = 100*((dat1n$prev_2_10 -
                   dat1$prev_2_10) / 
                  dat1n$prev_2_10)
  
  reduction_prev = 100*((dat1n$prev_2_10 -
                           dat1$prev_2_10) )
  
  return(list(c(effect[573], ## 1 year after the start of the intervention
                effect[625], ## 2 year after the start of the intervention
                effect[677]),
              c(reduction_prev[573],
                reduction_prev[625],
                reduction_prev[677])) ) ## 1 year after the start of the intervention
}
fun_prev(loc19n,loc19) #Sudan Gerataf
fun_prev(loc20n,loc20) #Sudan Gerataf

fun_prev(loc19n,loc19p) #Sudan Gerataf
fun_prev(loc20n,loc20p) #Sudan Gerataf
