##################################################
##
## Following Tom's email 4th October
## 

##Instruction:
##A 3D one with endemicity on the x axes and bioassay mortality on the y. 
##The colour will be % efficacy against prevalence (panel A) and 
##then clinical incidence (panel B). 
##At the time of the start of the trial the existing bednet coverage would be 40% 
##increasing to 80% at the start of the trial. 
##Ultimately we could then do this for G2 when we have more data 
##but in the meantime it would be great to show it for PBO.

library(MalariaLaunchR)
library(adegenet)

##(A) Moderate seasonality, say in Benin?
##Start at endemic equilibrium
##No other interventions other than treatment at a moderate level
##No pyrethroid resistance
##An. gambiae s.s. mosquitoes
##Current phi and phiB estimates
##All other parameters default

##Stage one, create a function to assess panel A options
##First extimate the endemicity so fit to the mosquito prevalence estimates for the range of endemicities chosen
## c(0.01,seq(0.05,0.8,0.05))
Fit_Run_IVCC1<-function(targ, site, ITN, run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/exampleIVCC1/Site_BENIN615_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 0')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="P:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/exampleIVCC1/Site_BENIN615_', site, '.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.05, interval = c(1e-04, 1000),
        maxiter = 10)
  
}
sites_vals = 1:17
targ = c(0.01,seq(0.05,0.8,0.05))
for(i in 2:17){
  Fit_Run_IVCC1(targ = targ[i], site = sites_vals[i], ITN = 0.8, run_name = "IVCC1")
}

##Stage one, create a function to fit to the assessed data.(Running this on cluster)
Fit_Run_IVCC2<-function(targ, site,
                        
                        itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                        itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                        itn_halflife_1,
                        
                        itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                        itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                        itn_halflife_2){

  # Load the site_file file
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/exampleIVCC2/Site_BENIN615_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage 0.8 irs 0',
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2)
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="P:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/exampleIVCC2/Site_BENIN615_', site, '.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.05, interval = c(1e-04, 1000),
        maxiter = 10)
}
input_file0 <- read.csv("P:/IVCC2_PanelB.csv",header=TRUE)

sites_vals = 1:357
targ = c(0.01,seq(0.05,0.8,0.05))
for(i in 1:3){
  Fit_Run_IVCC2(targ = targ[i], site = sites_vals[i], 
                itn_repel_fun_1 = input_file0[i,3], 
                itn_repel_gamb_ss_1 = input_file0[i,4], 
                itn_repel_arab_1 = input_file0[i,5], 
                itn_kill_fun_1 = input_file0[i,6], 
                itn_kill_gamb_ss_1 = input_file0[i,7], 
                itn_kill_arab_1 = input_file0[i,8],
                itn_halflife_1 = input_file0[i,9],
                
                itn_repel_fun_2 = input_file0[i,10], 
                itn_repel_gamb_ss_2 = input_file0[i,11], 
                itn_repel_arab_2 = input_file0[i,12], 
                itn_kill_fun_2 = input_file0[i,13], 
                itn_kill_gamb_ss_2 = input_file0[i,14], 
                itn_kill_arab_2 = input_file0[i,15],
                itn_halflife_2 = input_file0[i,16])
}

##Stage two, create a function to assess panel A options now that the endemicity is set
Run_IVCC1<-function(site, ITN, run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/exampleIVCC1/Site_BENIN615_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 0 num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 0')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("P:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "P:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="P:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/exampleIVCC1/Site_BENIN615_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}
ITN = rep(seq(0,1,0.05),each=17)
sites_vals = 1:357
for(i in 102:357){
  Run_IVCC1(site = sites_vals[i], ITN = ITN[i], run_name = "IVCC1")
}


##(B) Same as A but now LLINs have been in place for 10 years with 40% coverage 
##and variably bioassay mortality (both have remained constant for 10 years).
##New PBO LLIN come in with 80% coverage

##(C) Same as B but with previous LLIN coverage being 80% up to the start of the trial

##Stage one, create a function to assess panel B and C options ##just need to change input files from starting 0.4 to starting 0.8
Run_IVCC2<-function(site, 
                    
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
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/exampleIVCC2/Site_BENIN615_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 0 num_people', pop_size, 'itn 1 itn_coverage 0.8 irs 0',
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2)
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("P:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "P:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="P:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/exampleIVCC2/Site_BENIN615_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}
input_file0 <- read.csv("P:/IVCC2_PanelB.csv",header=TRUE)
for(i in 1:357){
  Run_IVCC2(site = input_file0[i,1], 
                itn_repel_fun_1 =     input_file0[i,2], 
                itn_repel_gamb_ss_1 = input_file0[i,3], 
                itn_repel_arab_1 =    input_file0[i,4], 
                itn_kill_fun_1 =      input_file0[i,5], 
                itn_kill_gamb_ss_1 =  input_file0[i,6], 
                itn_kill_arab_1 =     input_file0[i,7],
                itn_halflife_1 =      input_file0[i,8],
                
                itn_repel_fun_2 =     input_file0[i,9], 
                itn_repel_gamb_ss_2 = input_file0[i,10], 
                itn_repel_arab_2 =    input_file0[i,11], 
                itn_kill_fun_2 =      input_file0[i,12], 
                itn_kill_gamb_ss_2 =  input_file0[i,13], 
                itn_kill_arab_2 =     input_file0[i,14],
                itn_halflife_2 =      input_file0[i,15],
            run_name = "IVCC2")
}


Run_IVCC3<-function(site, 
                    
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
  site_file<-read.table(paste0('P:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/exampleIVCC3/Site_BENIN615_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 0 num_people', pop_size, 'itn 1 itn_coverage 0.8 irs 0',
                    'itn_repel_fun', itn_repel_fun_1, 'itn_repel_gamb_ss', itn_repel_gamb_ss_1, 'itn_repel_arab', itn_repel_arab_1,
                    'itn_kill_fun', itn_kill_fun_1, 'itn_kill_gamb_ss', itn_kill_gamb_ss_1, 'itn_kill_arab', itn_kill_arab_1,
                    'itn_half_life',itn_halflife_1,
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_halflife_2)
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("P:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "P:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="P:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/exampleIVCC3/Site_BENIN615_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}
##These have been run on cluster and the outputs are in P:\Ellies_output_folder\  ... IVCC1, IVCC2 and IVCC3
##For figure 1, compare all to the first 17 data where coverage was 0

##PREVALENCE
site = 1:357
site_Start = seq(1,340,17)
site_End = site_Start + 16 
dataC = array(dim=c(1301,18))
data1 = array(dim=c(1301,18,20))

dataC[,1] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[1], '_0.txt'),header=TRUE)$year
data1[,1,] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[2], '_0.txt'),header=TRUE)$year

for(i in 1:17){
  dataC[,i+1] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site[i], '_0.txt'),header=TRUE)$prev_2_10
}
for(j in 1:20){
  for(i in 1:17){
    data1[,i+1,j] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[j+1]+i-1, '_0.txt'),header=TRUE)$prev_2_10
  }
}
Reduction_prev1 = Reduction_prev2 = Reduction_prev3 = array(dim=c(17,20))
for(i in 1:20){
  Reduction_prev1[,i] = dataC[which(dataC[,1] == 1),2:18] - data1[which(dataC[,1] == 1),2:18,i]
  Reduction_prev2[,i] = dataC[which(dataC[,1] == 2),2:18] - data1[which(dataC[,1] == 2),2:18,i]
  Reduction_prev3[,i] = dataC[which(dataC[,1] == 3),2:18] - data1[which(dataC[,1] == 3),2:18,i]
}
#Reduction_prev2 = dataC[which(data1[,1] == 2),2:18] - data1[which(data1[,1] == 2),2:18]
#Reduction_prev3 = dataC[which(data1[,1] == 3),2:18] - data1[which(data1[,1] == 3),2:18]

Endemicity = rep(c(0.01,seq(0.05,0.8,0.05)),21)
ITN_Cover = rep(seq(0.0,1,0.05),each=17)

prev_year1 = c(rep(0,17),Reduction_prev1)
prev_year2 = c(rep(0,17),Reduction_prev2)
prev_year3 = c(rep(0,17),Reduction_prev3)

data_summary = data.frame(Endemicity,ITN_Cover,prev_year1,prev_year2,prev_year3)

##3d plot
library(plotly)
library(stringr)
library(reshape2)
require(grDevices) # for colours

y = seq(0, 0.8, length.out = 17)
x = seq(0, 1, length.out = 21)
z = matrix(nrow=17,ncol=21,data = data_summary$prev_year3)

filled.contour(y = seq(0, 1, length.out = ncol(z)), 
               x = seq(0, 0.8, length.out = nrow(z)), 
               z,
               color = topo.colors,
               plot.title = title(main = "% Reduction in prevalence 2 - 10 years",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               #plot.axes = { axis(1, seq(0, 1, by = 0.05))
               #   axis(2, seq(0, 1, by = 0.05))},
               #nlevels = 6,
               #levels=c(min(data_summary$prev_year1),
               #          max(data_summary$prev_year1),
               #          length=0.01),
               key.title = title(main = ""))  # maybe also asp = 1