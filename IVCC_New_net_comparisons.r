
#########################################################
##
## Confirming new products fit the predictions of 'all LLINs' 
##
##
library(adegenet)

library(MalariaLaunchR)
library(adegenet)


##Data list in "Original_DATA_cHURCHER20166.R"
exit_from_hut = rep_u/caught_u
mosq_survival_hut = 1 - dead_u/caught_u
successful = (mosq_survival_hut-exit_from_hut)

data_1 = data.frame(rep_u,caught_u,dead_u,is_treat,mosq_survival_hut)

deterred = (caught_u[data_1$is_treat == 0] - caught_u[data_1$is_treat == 1])/
                              caught_u[data_1$is_treat == 0] 


##Unwashed_hut_trial_data
G2_dat = read.csv("H:/Ellie/IRS and resistance/IVCC/Data_IVCC_newLLINsG2Interceptor/INTERCEPTOR_G2_DATA.R.csv",header=TRUE)
names(G2_dat)

G2_dat$G2_deterred  = (G2_dat$N_mosq_C - G2_dat$N_mosq)/G2_dat$N_mosq_C
G2_dat$G2_72hr_dead = ( (G2_dat$N_dead_72/G2_dat$N_mosq) *(1-(G2_dat$N_dead_72_C/G2_dat$N_mosq_C)) )
G2_dat$G2_mosq_survival = 1 - G2_dat$G2_72hr_dead
G2_dat$G2_exiting   = (G2_dat$N_exited/G2_dat$N_mosq) * (1 - G2_dat$G2_72hr_dead)
G2_dat$G2_successful = 1 - G2_dat$G2_72hr_dead - G2_dat$G2_exiting

plot(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"] ~
       G2_dat$bioassay_mort_PERMETHRIN[G2_dat$Net_type == "interceptor_LN"],ylim=c(0,1),xlim=c(0,1))
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
plot(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2"] ~ 
       G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"],ylim=c(0,1),xlim=c(0,1),
     ylab="",yaxt="n",cex=1.6,
     xlab="",xaxt="n",cex.axis=1.6,cex.lab=1.6)
axis(1,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=c(0,0.2,0.4,0.6,0.8,1),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
mtext(side = 2,line = 4, "Mosquito mortality in Interceptor G2 hut trial (%)",cex=1.6)
mtext(side = 1, line = 5, "Mosquito mortality in Interceptor LN hut trial (%)",cex=1.6)
points(G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2" & G2_dat$N_washes == 20] ~ 
         G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN" & G2_dat$N_washes == 20],pch=15,cex=2)
abline(0,1,lty=2)

##Relationship 1 Normal LN and bioassay
surv_bioassay=seq(0,1,0.05)		#measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}
mort = 1 - surv_bioassay

ln_mort = 1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5))) 

##Relationship 2 G2 benefit on top of Normal LN 
##LINEAR BENEFIT
#load libraries
library(rstan)
library(coda)
set.seed(20151204)
options(mc.cores = parallel::detectCores())
#the explanatory variables
dat<-data.frame(x1 = G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"])

#the model
X<-model.matrix(~x1,dat)

#the regression slopes
betas<-runif(2,-1,1)

#the standard deviation for the simulated data
sigma<-1

#the simulated data
y_norm<-G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_G2"]

#a matrix to get the predicted y values
new_X<-model.matrix(~x1,expand.grid(x1=seq(0,max(dat$x1),length=40)))

#lines(sort(y_norm) ~ as.numeric(new_X[,2]))

#the model
m_norm<-stan(file="Q:/RProjects/IVCC_product_comparisons/biomortality_G2fit_linear.stan",
             data = list(N=11,N2=40,K=2,y=y_norm,X=X,new_X=new_X),
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
lines(new_x$x1.x1[10:40],pred[2,10:40],col="red",lwd=3)
lines(new_x$x1.x1[10:40],pred[1,10:40],col="red",lwd=1,lty=2)
lines(new_x$x1.x1[10:40],pred[3,10:40],col="red",lwd=1,lty=2)


##Relationship 2 G2 benefit on top of Normal LN 
##LOGISTIC BENEFIT
d_t = G2_dat$N_dead_72[G2_dat$Net_type == "interceptor_G2"] # number of mosquitoes dying IRS HUTS
n_t = G2_dat$N_mosq[G2_dat$Net_type == "interceptor_G2"] # Number of mosquitoes entering IRS huts
x1 = G2_dat$G2_72hr_dead[G2_dat$Net_type == "interceptor_LN"]
x = seq(0,1,length=100)
fm2 <- cbind(d_t,n_t-d_t) ~ x1 
glm_2 <- glm(fm2, family = binomial())
role.fitted2 <- predict(glm_2, se.fit = TRUE, type = "response")
summary(glm_2)


g2_option2 = 1 / (1 + exp(-1.19*x - 0.45))
g2_option2low = 1 / (1 + exp(-(1.19-0.43)*x - (0.45-0.09)))
g2_option2upp = 1 / (1 + exp(-(1.19+0.43)*x - (0.45+0.09)))

lines(g2_option2[14:63] ~ x[14:63], col="blue",lwd=3)
lines(g2_option2low[14:63] ~ x[14:63], col="blue",lwd=1, lty=2)
lines(g2_option2upp[14:63] ~ x[14:63], col="blue",lwd=1, lty=2)


#plot(ln_mort ~ surv_bioassay)

##linear
G2_benefit_lin = median(parm$beta) * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) + pred[2,1]
plot(G2_benefit_lin ~ surv_bioassay,ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n",pch="",
     ylab="Mortlaity hut trial (%)",xlab="Survival at bioassay (%)",cex.lab=1.6)
axis(1,at=seq(0,1,0.2),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
axis(2,las=2,at=seq(0,1,0.2),labels=seq(0,100,20),cex.axis=1.6,cex.lab=1.6)
lines(G2_benefit_lin ~ surv_bioassay,col="red")
lines(ln_mort ~ surv_bioassay,lty=2)

##logistic
G2_benefit_log = 1 / (1 + exp(-1.19 * (1 / (1 + exp(0.63 + 4*(surv_bioassay-0.5)))) - 0.45 ))
lines(G2_benefit_log ~ surv_bioassay,col="blue")

G2_dat$bioassay_mort_PERMETHRIN[G2_dat$Net_type == "interceptor_LN"]
#Benefit of PBO in assay		
#g2beta1=	2
#g2beta2=	5
#g2beta3=	0.5

#g2_a  = 2*(1/(1+exp(beta1 * mort + beta2)))

#g2_benefit_a<-g2beta1+g2beta2*((1-surv_bioassay)-0.5)/(1+g2beta3*((1-surv_bioassay)-0.5))   
#g2_benefit<-exp(g2_benefit_a)/(1+exp(g2_benefit_a))

#lines(g2_benefit ~ mort)
lines(pbo_benefit ~ surv_bioassay,lty=2,lwd=2)

polygon(c(surv_bioassay[11],surv_bioassay[21],
          surv_bioassay[21],surv_bioassay[11]),
        c(0,0,1,1),border=NA,
        col=transp("grey",0.4))

Run_seas<-function(site, 
                   ITN, IRS,
                   
                   itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                   itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                   itn_halflife_1,
                   itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                   itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                   itn_halflife_2,
                   
                   itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                   itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                   itn_halflife_3,
                   run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/perennial/Site_Perennial_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/perennial/Site_Perennial_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}


##Read in the csv with the appropriate data
inp_temp <- read.csv("f:/G2_interceptor_runs.csv",header=TRUE)
inp = data.frame(inp_temp[,1:10],inp_temp[,17:23],inp_temp[,30:36])
dim(inp)
## ON COMP
for(i in 1:6){
  Run_seas(site = inp[i,1],
           ITN = inp[i,2], IRS = inp[i,3],
           itn_repel_fun_1 = inp[i,4], itn_repel_gamb_ss_1 = inp[i,5], itn_repel_arab_1 = inp[i,6],
           itn_kill_fun_1 = inp[i,7], itn_kill_gamb_ss_1 = inp[i,8], itn_kill_arab_1 = inp[i,9],
           itn_halflife_1 = inp[i,10],
           itn_repel_fun_2 = inp[i,11], itn_repel_gamb_ss_2 = inp[i,12], itn_repel_arab_2 = inp[i,13],
           itn_kill_fun_2 = inp[i,14], itn_kill_gamb_ss_2 = inp[i,15], itn_kill_arab_2 = inp[i,16],
           itn_halflife_2 = inp[i,17],
           itn_repel_fun_3 = inp[i,18], itn_repel_gamb_ss_3 = inp[i,19], itn_repel_arab_3 = inp[i,20],
           itn_kill_fun_3 = inp[i,21], itn_kill_gamb_ss_3 = inp[i,22], itn_kill_arab_3 = inp[i,23],
           itn_halflife_3 = inp[i,24],
           run_name="G2_testrun")
  
}

res0 =  read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_1_0.txt",header = TRUE)
res20 = read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_2_0.txt",header = TRUE)
res40 = read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_3_0.txt",header = TRUE)
res60 = read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_4_0.txt",header = TRUE)
res80 = read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_5_0.txt",header = TRUE)
res100 =read.table("F:\\Ellies_output_folder\\G2_testrun\\draw_0\\G2_testrun_6_0.txt",header = TRUE)

par(mfrow=c(1,1))
plot(res0$prev_2_10 ~ res0$year,pch="",ylim=c(0,0.4),
     xlim=c(-3,12),ylab="Prevalence in 2 - 10 years",yaxt="n",
     xlab="Time (years)",cex.lab=1.6,cex.axis=1.6,xaxt="n")
axis(1,at=c(-3,0,3,6,9,12),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=c(0,0.1,0.2,0.3,0.4),cex.lab=1.6,cex.axis=1.6)
lines(res0$prev_2_10 ~ res0$year)
abline(v=0,lty=2)
abline(v=3,lty=2)
abline(v=6,lty=2)
lines(res20$prev_2_10 ~ res20$year,lwd=1.5)
lines(res40$prev_2_10 ~ res40$year,lwd=2)
lines(res60$prev_2_10 ~ res60$year,lwd=2.5)
lines(res80$prev_2_10 ~ res80$year,lwd=3)
lines(res100$prev_2_10 ~ res100$year,lwd=3.5)

text(0,0.4,"Introduce standard nets (80% cover)",cex=1.2)
text(3,0.36,"Introduce increasing resistance (0 - 100%)",cex=1.2)
text(6,0.32,"Switch to G2 nets",cex=1.2)

legend(8,0.4,title="Level of insecticide resistance",
       legend=c("100%","80%","60%","40%","20%","0%"),
       lty=c(3.5,3,2.5,2,1.5,1))
####
## Set up for different potential RCT sites

################
##
## BURKINA FASO
##
################

###########
##
## Burkina 1 : Debougou
###########
##################################################
## Try fitting M

## As each place as a different prevalence to fit
fitM_function<-function(targ, site, 
                        ITN, IRS,
                        
                        itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                        itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                        itn_halflife_1,
                        itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                        itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                        itn_halflife_2,
                        
                        itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                        itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                        itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Diebougou_101.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)

    ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Diebougou_101.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 15, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

##Read in the csv with the appropriate data
BUKINA1 <- read.csv("f:/G2_interceptor_RCT_site_options_input.csv",header=TRUE)

head(BUKINA1)
#inp$irs_decay_det2_1 = 0
## ON COMP
i=1
fitM_function(targ = 0.614, site = BUKINA1[i,1],
              ITN = 0.596, IRS = BUKINA1[i,3],
              itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
              itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
              itn_halflife_1 = BUKINA1[i,10],
              itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
              itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
              itn_halflife_2 = BUKINA1[i,17],
              itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
              itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
              itn_halflife_3 = BUKINA1[i,24])



###########
##
## Burkina 2 : Danu
###########
##################################################
## Try fitting M

## As each place as a different prevalence to fit
fitM_function2<-function(targ, site, 
                        ITN, IRS,
                        
                        itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                        itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                        itn_halflife_1,
                        itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                        itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                        itn_halflife_2,
                        
                        itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                        itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                        itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Danu_101.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Danu_101.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.1, interval = c(1e-04, 1000),
        maxiter = 10)
  
  
}

##Read in the csv with the appropriate data
BUKINA1 <- read.csv("f:/G2_interceptor_RCT_site_options_input.csv",header=TRUE)

head(BUKINA1)
#inp$irs_decay_det2_1 = 0
## ON COMP
i=2
fitM_function2(targ = 0.614, site = BUKINA1[i,1],
              ITN = 0.596, IRS = BUKINA1[i,3],
              itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
              itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
              itn_halflife_1 = BUKINA1[i,10],
              itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
              itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
              itn_halflife_2 = BUKINA1[i,17],
              itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
              itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
              itn_halflife_3 = BUKINA1[i,24])



#####################################################################

################
##
## MALI
##
################

###########
##
## Mali 1 : Segou, (eg. Baroueli, Bla or Segou)
###########

##################################################
## Try fitting M

## As each place as a different prevalence to fit
fitM_function3<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Baroueli_404.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Baroueli_404.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.5, interval = c(1e-04, 1000),
        maxiter = 10)
  
  
}

i=3
fitM_function3(targ = 0.367, site = BUKINA1[i,1],
               ITN = 0.581, IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])


## As each place as a different prevalence to fit
fitM_function4<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Bla_404.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Bla_404.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.5, interval = c(1e-04, 1000),
        maxiter = 10)
  
  
}

i=4
fitM_function4(targ = 0.367, site = BUKINA1[i,1],
               ITN = 0.581, IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])

## As each place as a different prevalence to fit
fitM_function5<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Kati_402.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Kati_402.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 15, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

i=5
fitM_function5(targ = 0.348, site = BUKINA1[i,1],
               ITN = 0.66, IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])


## As each place as a different prevalence to fit
fitM_function6<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Koulikoro_402.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Koulikoro_402.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.5, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

i=6
fitM_function6(targ = 0.348, site = BUKINA1[i,1],
               ITN = 0.66, IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])


###################################################################

#########################
##
## Malawi
##
#########################

################
## As each place as a different prevalence to fit
fitM_function7<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Phalombe_397.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Phalombe_397.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 15, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

i=7
fitM_function7(targ = 0.0922, site = BUKINA1[i,1],
               ITN = 0.42, IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])

## As each place as a different prevalence to fit
fitM_function8<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Lilongwe_395.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Lilongwe_395.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 15, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

i=8
fitM_function8(targ = 0.1718, site = BUKINA1[i,1],
               ITN = BUKINA1[i,2], IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])


## As each place as a different prevalence to fit
fitM_function9<-function(targ, site, 
                         ITN, IRS,
                         
                         itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                         itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                         itn_halflife_1,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_halflife_2,
                         
                         itn_repel_fun_3, itn_repel_gamb_ss_3, itn_repel_arab_3, 
                         itn_kill_fun_3, itn_kill_gamb_ss_3, itn_kill_arab_3,
                         itn_halflife_3){
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/site_G2RCT_Chitipa_396.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2,
                    'add change_itn_2 1 change_itn_time_2 6',
                    'itn_repel_fun_2', itn_repel_fun_3, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_3, 'itn_repel_arab_2', itn_repel_arab_3,
                    'itn_kill_fun_2', itn_kill_fun_3, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_3, 'itn_kill_arab_2', itn_kill_arab_3,
                    'itn_half_life_2',itn_halflife_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_G2RCT_Chitipa_396.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 15, interval = c(1e-04, 10000),
        maxiter = 10)
  
  
}

i=9
fitM_function9(targ = 0.1357, site = BUKINA1[i,1],
               ITN = BUKINA1[i,2], IRS = BUKINA1[i,3],
               itn_repel_fun_1 = BUKINA1[i,4], itn_repel_gamb_ss_1 = BUKINA1[i,5], itn_repel_arab_1 = BUKINA1[i,6],
               itn_kill_fun_1 = BUKINA1[i,7], itn_kill_gamb_ss_1 = BUKINA1[i,8], itn_kill_arab_1 = BUKINA1[i,9],
               itn_halflife_1 = BUKINA1[i,10],
               itn_repel_fun_2 = BUKINA1[i,11], itn_repel_gamb_ss_2 = BUKINA1[i,12], itn_repel_arab_2 = BUKINA1[i,13],
               itn_kill_fun_2 = BUKINA1[i,14], itn_kill_gamb_ss_2 = BUKINA1[i,15], itn_kill_arab_2 = BUKINA1[i,16],
               itn_halflife_2 = BUKINA1[i,17],
               itn_repel_fun_3 = BUKINA1[i,18], itn_repel_gamb_ss_3 = BUKINA1[i,19], itn_repel_arab_3 = BUKINA1[i,20],
               itn_kill_fun_3 = BUKINA1[i,21], itn_kill_gamb_ss_3 = BUKINA1[i,22], itn_kill_arab_3 = BUKINA1[i,23],
               itn_halflife_3 = BUKINA1[i,24])


