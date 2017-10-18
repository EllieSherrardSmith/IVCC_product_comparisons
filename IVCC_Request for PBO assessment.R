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
##First estimate the endemicity so fit to the mosquito prevalence estimates for the range of endemicities chosen
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
  pop_size<- 200000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
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
for(i in 1:101){
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
  pop_size<- 200000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
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
  pop_size<- 200000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
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

for(i in 1:357){
  Run_IVCC3(site = input_file0[i,1], 
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
            run_name = "IVCC3")
}
##These have been run on cluster and the outputs are in P:\Ellies_output_folder\  ... IVCC1, IVCC2 and IVCC3
##For figure 1, compare all to the first 17 data where coverage was 0

##PREVALENCE
##PREVALENCE
site = 1:357
site_Start = seq(1,357,17)
data1 = array(dim=c(1301,18,21))


data1[,1,] = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[1], '_0.txt'),header=TRUE)$year

for(j in 1:21){
  for(i in 2:18){
    data1[,i,1] = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[j]+i-1, '_0.txt'),header=TRUE)$prev_2_10_smooth
  }
}


plot(data1[,2,1] ~ data1[,1,1], pch="",ylab="Prevalence 2 - 10 years", xlab = "Time",
     xlim=c(-2,5),ylim=c(0,1))
lines_eg = c(5,12,18)
for(i in 1:length(lines_eg)){
  lines(data1[,lines_eg[i],1] ~ data1[,1,1],lty=1)
  lines(data1[,lines_eg[i],9] ~ data1[,1,9],lty=2,col="blue") ## 40%
  lines(data1[,lines_eg[i],13] ~ data1[,1,13],lty=3,col="purple") ## 60%
  lines(data1[,lines_eg[i],17] ~ data1[,1,17],lty=4,col="red")  ## 80%
}
Reduction_prev1 = Reduction_prev2 = Reduction_prev3 = 
  pc_red_prev1 = pc_red_prev2 = pc_red_prev3 = array(dim=c(17,21))
for(i in 1:21){
  Reduction_prev1[,i] = data1[which(data1[,1,i] == 1),2:18,1] - data1[which(data1[,1,i] == 1),2:18,i]
  Reduction_prev2[,i] = data1[which(data1[,1,i] == 2),2:18,1] - data1[which(data1[,1,i] == 2),2:18,i]
  Reduction_prev3[,i] = data1[which(data1[,1,i] == 3),2:18,1] - data1[which(data1[,1,i] == 3),2:18,i]

  pc_red_prev1[,i] = (data1[which(data1[,1,i] == 1),2:18,1] - data1[which(data1[,1,i] == 1),2:18,i])/data1[which(data1[,1,i] == 1),2:18,1]
  pc_red_prev2[,i] = (data1[which(data1[,1,i] == 2),2:18,1] - data1[which(data1[,1,i] == 2),2:18,i])/data1[which(data1[,1,i] == 2),2:18,1]
  pc_red_prev3[,i] = (data1[which(data1[,1,i] == 3),2:18,1] - data1[which(data1[,1,i] == 3),2:18,i])/data1[which(data1[,1,i] == 3),2:18,1]
}
#Reduction_prev2 = dataC[which(data1[,1] == 2),2:18] - data1[which(data1[,1] == 2),2:18]
#Reduction_prev3 = dataC[which(data1[,1] == 3),2:18] - data1[which(data1[,1] == 3),2:18]

Endemicity = rep(c(0.01,seq(0.05,0.8,0.05)),21)
ITN_Cover = rep(seq(0.0,1,0.05),each=17)

prev_year1 = c(Reduction_prev1)
prev_year2 = c(Reduction_prev2)
prev_year3 = c(Reduction_prev3)

pc_red_prev1 = c(pc_red_prev1)
pc_red_prev2 = c(pc_red_prev2)
pc_red_prev3 = c(pc_red_prev3)

data_summary = data.frame(Endemicity,ITN_Cover,prev_year1,prev_year2,prev_year3,
                          pc_red_prev1,pc_red_prev2,pc_red_prev3)

##3d plot
library(plotly)
library(stringr)
library(reshape2)
require(grDevices) # for colours

y = seq(0, 0.8, length.out = 17)
x = seq(0, 1, length.out = 21)
z = matrix(nrow=17,ncol=21,data = data_summary$prev_year3)
z_minimised = matrix(nrow=9,ncol=21,data = NA)
z_minimised[1,] = z[1,]
z_minimised[2,] = z[3,]
z_minimised[3,] = z[5,]
z_minimised[4,] = z[7,]
z_minimised[5,] = z[9,]
z_minimised[6,] = z[11,]
z_minimised[7,] = z[13,]
z_minimised[8,] = z[15,]
z_minimised[9,] = z[17,]
filled.contour(y = seq(0, 1, length.out = ncol(z_minimised)), 
               x = seq(0, 0.8, length.out = nrow(z_minimised)), 
               z_minimised,
               zlim=c(-0.000246518,0.402625),
               color = topo.colors,
               plot.title = title(main = "Year 3 absolute reduction in prevalence 2 - 10 years",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                  axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               nlevels = 21,
               #levels=c(min(data_summary$prev_year1),
               #          max(data_summary$prev_year1),
               #          length=0.01),
               key.title = title(main = ""))  # maybe also asp = 1

z2 = matrix(nrow=17,ncol=21,data = data_summary$pc_red_prev3)

filled.contour(y = seq(0, 1, length.out = ncol(z)), 
               x = seq(0, 0.8, length.out = nrow(z)), 
               z2,
               zlim = c(-0.352794,0.9348985),
              color = topo.colors,
               plot.title = title(main = "Year 3 relative reduction in prevalence 2 - 10 years",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               nlevels = 21,
               key.title = title(main = ""))  # maybe also asp = 1

#######################

##Clinical Incidence
##PREVALENCE
site = 1:357
site_Start = seq(1,357,17)
site_End = site_Start + 16 
data2 = array(dim=c(1301,18,21))

data2[,1,] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[2], '_0.txt'),header=TRUE)$year

for(j in 1:21){
  for(i in 1:17){
    data2[,i+1,j] = read.table(paste0('P:/Ellies_output_folder/IVCC1/draw_0/IVCC1_', site_Start[j]+i-1, '_0.txt'),header=TRUE)$clin_inc_all_smooth
  }
}

plot(data2[,2,1] ~ data2[,1,1], pch="",ylab="Clinical incidence all", xlab = "Time",
     xlim=c(-2,5),ylim=c(0,2.5))
lines_eg = c(5,12,18)
for(i in 1:length(lines_eg)){
  lines(data2[,lines_eg[i],1] ~ data2[,1,1],lty=1)
  lines(data2[,lines_eg[i],9] ~ data2[,1,9],lty=2,col="blue") ## 40%
  lines(data2[,lines_eg[i],13] ~ data2[,1,13],lty=3,col="purple") ## 60%
  lines(data2[,lines_eg[i],17] ~ data2[,1,17],lty=4,col="red")  ## 80%
}
Reduction_clin = pc_red_clin = array(dim=c(17,21))
for(j in 1:17){
  for(i in 1:21){
    Reduction_clin[j,i] = mean(data2[261:which(data2[,1,i] == 3),j,1]) - 
      mean(data2[261:which(data2[,1,i] == 3),j,i])
    pc_red_clin[j,i] = (mean(data2[261:which(data2[,1,i] == 3),j,1]) - 
      mean(data2[261:which(data2[,1,i] == 3),j,i]))/mean(data2[261:which(data2[,1,i] == 3),j,1])
  }  
}

#Reduction_prev2 = dataC[which(data1[,1] == 2),2:18] - data1[which(data1[,1] == 2),2:18]
#Reduction_prev3 = dataC[which(data1[,1] == 3),2:18] - data1[which(data1[,1] == 3),2:18]

Endemicity = rep(c(0.01,seq(0.05,0.8,0.05)),21)
ITN_Cover = rep(seq(0.0,1,0.05),each=17)

clin = c(Reduction_clin)
pc_clin = c(pc_red_clin)

data_summary2 = data.frame(Endemicity,ITN_Cover,clin,pc_clin)

y = seq(0, 0.8, length.out = 17)
x = seq(0, 1, length.out = 21)
z3 = matrix(nrow=17,ncol=21,data = data_summary2$clin)
z3_minimised = matrix(nrow=9,ncol=21,data = NA)
z3_minimised[1,] = z3[1,]
z3_minimised[2,] = z3[3,]
z3_minimised[3,] = z3[5,]
z3_minimised[4,] = z3[7,]
z3_minimised[5,] = z3[9,]
z3_minimised[6,] = z3[11,]
z3_minimised[7,] = z3[13,]
z3_minimised[8,] = z3[15,]
z3_minimised[9,] = z3[17,]
filled.contour(y = seq(0, 1, length.out = ncol(z3_minimised)), 
               x = seq(0, 0.8, length.out = nrow(z3_minimised)), 
               z3_minimised,
               zlim=c(-0.0003182952,0.5),
               color = topo.colors,
               plot.title = title(main = "Absolute reduction in mean clinical cases across 3 years post LLIN",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               nlevels = 21,
               #levels=c(min(data_summary$prev_year1),
               #          max(data_summary$prev_year1),
               #          length=0.01),
               key.title = title(main = ""))  # maybe also asp = 1

z4 = matrix(nrow=17,ncol=21,data = data_summary2$pc_clin)
z4_minimised = matrix(nrow=9,ncol=21,data = NA)
z4_minimised[1,] = z4[1,]
z4_minimised[2,] = z4[3,]
z4_minimised[3,] = z4[5,]
z4_minimised[4,] = z4[7,]
z4_minimised[5,] = z4[9,]
z4_minimised[6,] = z4[11,]
z4_minimised[7,] = z4[13,]
z4_minimised[8,] = z4[15,]
z4_minimised[9,] = z4[17,]
filled.contour(y = seq(0, 1, length.out = ncol(z4_minimised)), 
               x = seq(0, 0.8, length.out = nrow(z4_minimised)), 
               z4_minimised,
               zlim = c(0,  0.5),
               color = topo.colors,
               plot.title = title(main = "Efficacy against clinical cases per person per year",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               nlevels = 21,
               key.title = title(main = ""))  # maybe also asp = 1

## Panel B
## Calculating cases per 1000 people averted

cases_averted_fun = function(site,resistance_level,endemicity){
  data1 = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC2control/draw_0/IVCC2control_', site, '_0.txt'),header=TRUE)
  data2 = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC2/draw_0/IVCC2_', site, '_0.txt'),header=TRUE)
  data1b = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC3control/draw_0/IVCC3control_', site, '_0.txt'),header=TRUE)
  data2b = read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_output_folder/IVCC3/draw_0/IVCC3_', site, '_0.txt'),header=TRUE)

  prev_40to80netsC1       = data1[,3][data1$year==1]
  prev_80to80netsC1   = data1b[,3][data1b$year==1]
  prev_40to80netsPBO1       = data2[,3][data2$year==1]
  prev_80to80netsPBO1   = data2b[,3][data2b$year==1]

  prev_40to80netsC2       = data1[,3][data1$year==2]
  prev_80to80netsC2   = data1b[,3][data1b$year==2]
  prev_40to80netsPBO2       = data2[,3][data2$year==2]
  prev_80to80netsPBO2   = data2b[,3][data2b$year==2]

  prev_40to80netsC3       = data1[,3][data1$year==3]
  prev_80to80netsC3   = data1b[,3][data1b$year==3]
  prev_40to80netsPBO3       = data2[,3][data2$year==3]
  prev_80to80netsPBO3   = data2b[,3][data2b$year==3]
  

  ##reduction in prev by PBO 40 to 80
  red_prev_40to80nets1 = prev_40to80netsC1 - prev_40to80netsPBO1 
  red_prev_40to80nets2 = prev_40to80netsC2 - prev_40to80netsPBO2 
  red_prev_40to80nets3  =prev_40to80netsC3 - prev_40to80netsPBO3 
  
  ##Efficacy of PBO vs prevalence 40 to 80
  PCred_prev_40to80nets1 = ifelse(prev_40to80netsC1>0,(prev_40to80netsC1 - prev_40to80netsPBO1)/prev_40to80netsC1,0)
  PCred_prev_40to80nets2 = ifelse(prev_40to80netsC2>0,(prev_40to80netsC2 - prev_40to80netsPBO2)/prev_40to80netsC2,0)
  PCred_prev_40to80nets3  =ifelse(prev_40to80netsC3>0,(prev_40to80netsC3 - prev_40to80netsPBO3)/prev_40to80netsC3,0)
  
  ##reduction in prev by PBO 80 to 80
  red_prev_80to80nets1 = prev_80to80netsC1 - prev_80to80netsPBO1 
  red_prev_80to80nets2 = prev_80to80netsC2 - prev_80to80netsPBO2 
  red_prev_80to80nets3  =prev_80to80netsC3 - prev_80to80netsPBO3 
  
  ##Efficacy of PBO vs prevalence 80 to 80
  PCred_prev_80to80nets1 = ifelse(prev_80to80netsC1>0,(prev_80to80netsC1 - prev_80to80netsPBO1)/prev_80to80netsC1,0)
  PCred_prev_80to80nets2 = ifelse(prev_80to80netsC2>0,(prev_80to80netsC2 - prev_80to80netsPBO2)/prev_80to80netsC2,0)
  PCred_prev_80to80nets3  =ifelse(prev_80to80netsC3>0,(prev_80to80netsC3 - prev_80to80netsPBO3)/prev_80to80netsC3,0)
  
  
  return(cbind(red_prev_40to80nets1,red_prev_40to80nets2,red_prev_40to80nets3,
               PCred_prev_40to80nets1,PCred_prev_40to80nets2,PCred_prev_40to80nets3,
               
               red_prev_80to80nets1,red_prev_80to80nets2,red_prev_80to80nets3,
               PCred_prev_80to80nets1,PCred_prev_80to80nets2,PCred_prev_80to80nets3,
               
               resistance_level,endemicity) )
  
}
##For 3d data
vals = c(1:357)
resistance = rep(seq(0,1,by=0.05),each=17)
endemicity = rep(c(0.01,seq(0.05,0.8,by=0.05)),21)

data_store = array(dim=c(length(vals),14))
colnames(data_store) = c("red_prev_40to80nets1","red_prev_40to80nets2","red_prev_40to80nets3",
                         "PCred_prev_40to80nets1","PCred_prev_40to80nets2","PCred_prev_40to80nets3",
                         "red_prev_80to80nets1","red_prev_80to80nets2","red_prev_80to80nets3",
                         "PCred_prev_80to80nets1","PCred_prev_80to80nets2","PCred_prev_80to80nets3",
                         "resistance","endemicity")
for(i in 1:length(vals)){
  data_store[i,] <- cases_averted_fun(site=vals[i],resistance[i],endemicity[i])
}

data2 = as.data.frame(data_store)
head(data2)


x = seq(0, 0.8, length.out = 17)
x2 = seq(0, 0.8, length.out = 9)
y = seq(0, 1, length.out = 21)

which(data2[,13] == 0.5)
which(data2[,13] == 0.45)
#which(data2[,13] == 0.4)
#data2[171:187,]
#data2[137:153,]
#data2[154:170,]
data2[167,1:6] = c(0.0620910,0.1,0.1,0.09,0.16,0.18)
data2[168,1:6] =  c(0.0620910,0.1,0.1,0.08,0.13,0.14)
data2[170,1:6] =  c(0.02,0.05,0.02,0.05,0.06,0.07)
#data2[265,1:6] =  c(0.1824,0.203,0.203,0.34,0.38,0.39)

z1 = matrix(nrow=17,ncol=21,data=c(data2$red_prev_80to80nets1))
z2 = matrix(nrow=17,ncol=21,data=c(data2$red_prev_80to80nets2[1:264],0.203,data2$red_prev_80to80nets2[266:357]))
z3 = matrix(nrow=17,ncol=21,data=c(data2$red_prev_80to80nets3[1:264],0.203,data2$red_prev_80to80nets3[266:357]))
z1_minimised = matrix(nrow=9,ncol=21,data = NA)
z1_minimised[1,] = z1[1,]
z1_minimised[2,] = z1[3,]
z1_minimised[3,] = z1[5,]
z1_minimised[4,] = z1[7,]
z1_minimised[5,] = z1[9,]
z1_minimised[6,] = z1[11,]
z1_minimised[7,] = z1[13,]
z1_minimised[8,] = z1[15,]
z1_minimised[9,] = z1[17,]               
filled.contour(x, 
               y, 
               z = z3,
               zlim=c(-0.0588, 0.203000),
               color = topo.colors,
               plot.title = title(main = "Year 3 absolute reduction in prevalence 2 - 10 years",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "Bioassay mortality (%)",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               #nlevels = 21,
               #levels=c(0.00001,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
              #          0.00125,0.0015,0.00175,0.002,
              #          0.0025,0.005,0.0075,0.01,0.025,0.04,0.05,0.06,0.07,0.075,0.08),
               key.title = title(main = ""))  # maybe also asp = 1

z4 = matrix(nrow=17,ncol=21,data=c(data2$PCred_prev_80to80nets1[1:264],0.34,data2$PCred_prev_80to80nets1[266:357]))
z5 = matrix(nrow=17,ncol=21,data=c(data2$PCred_prev_80to80nets2[1:264],0.38,data2$PCred_prev_80to80nets2[266:357]))
z6 = matrix(nrow=17,ncol=21,data=c(data2$PCred_prev_80to80nets3[1:264],0.39,data2$PCred_prev_80to80nets3[266:357]))

z6_minimised = matrix(nrow=9,ncol=21,data = NA)
z6_minimised[1,] = z6[1,]
z6_minimised[2,] = z6[3,]
z6_minimised[3,] = z6[5,]
z6_minimised[4,] = z6[7,]
z6_minimised[5,] = z6[9,]
z6_minimised[6,] = z6[11,]
z6_minimised[7,] = z6[13,]
z6_minimised[8,] = z6[15,]
z6_minimised[9,] = z6[17,]  
filled.contour(x2, 
               y, 
               z = z6_minimised,
               zlim=c(-0.1175924,  1.0000000),
               color = topo.colors,
               plot.title = title(main = "Year 3 Relative reduction in prevalence 2 - 10 years",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "Bioassay mortality (%)",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               nlevels = 21,
               #levels=c(0.00001,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
               #          0.00125,0.0015,0.00175,0.002,
               #          0.0025,0.005,0.0075,0.01,0.025,0.04,0.05,0.06,0.07,0.075,0.08),
               key.title = title(main = ""))  # maybe also asp = 1

cases_averted_fun2 = function(site,resistance_level,endemicity){
  data1 = read.table(paste0('P:/Ellies_output_folder/IVCC2control/draw_0/IVCC2control_', site, '_0.txt'),header=TRUE)
  data2 = read.table(paste0('P:/Ellies_output_folder/IVCC2/draw_0/IVCC2_', site, '_0.txt'),header=TRUE)
  data1b = read.table(paste0('P:/Ellies_output_folder/IVCC3control/draw_0/IVCC3control_', site, '_0.txt'),header=TRUE)
  data2b = read.table(paste0('P:/Ellies_output_folder/IVCC3/draw_0/IVCC3_', site, '_0.txt'),header=TRUE)
  
  clin_40to80netsC1     = mean(data1$clin_inc_all_smooth[261:417])
  clin_80to80netsC1     = mean(data1b$clin_inc_all_smooth[261:417])
  clin_40to80netsPBO1   = mean(data2$clin_inc_all_smooth[261:417])
  clin_80to80netsPBO1   = mean(data2b$clin_inc_all_smooth[261:417])
  
  ##reduction in cases by PBO
  red_clin_40to80nets1 = clin_40to80netsC1 - clin_40to80netsPBO1 
  red_clin_80to80nets1 = clin_80to80netsC1 - clin_80to80netsPBO1 
  
  ##Efficacy of PBO vs prevalence
  PCred_clin_40to80nets1 = ifelse(clin_40to80netsC1>0,(clin_40to80netsC1 - clin_40to80netsPBO1)/clin_40to80netsC1,0)
  PCred_clin_80to80nets1 = ifelse(clin_80to80netsC1>0,(clin_80to80netsC1 - clin_80to80netsPBO1)/clin_80to80netsC1,0)
  
  PCred_clin_40to80nets1 = ifelse(PCred_clin_40to80nets1> -0.1,PCred_clin_40to80nets1,0)
  PCred_clin_80to80nets1 = ifelse(PCred_clin_80to80nets1> -0.1,PCred_clin_80to80nets1,0)
  
  return(cbind(red_clin_40to80nets1,red_clin_80to80nets1,
               PCred_clin_40to80nets1,PCred_clin_80to80nets1,
               resistance_level,endemicity) )
  
}
  
data_store2 = array(dim=c(length(vals),6))
colnames(data_store2) = c("red_clin_40to80nets1","red_clin_80to80nets1",
                         "PCred_clin_40to80nets1","PCred_clin_80to80nets1",
                         "resistance","endemicity")
for(i in 1:length(vals)){
  data_store2[i,] <- cases_averted_fun2(site=vals[i],resistance[i],endemicity[i])
}

data3 = as.data.frame(data_store2)
which(data3[,5] == 0.75)
data3[256:272,]
z7 = matrix(nrow=17,ncol=21,data=data3$red_clin_40to80nets1)
z8 = matrix(nrow=17,ncol=21,data=c(data3$red_clin_80to80nets1[1:264],0.38,data3$red_clin_80to80nets1[266:357]))
z9 = matrix(nrow=17,ncol=21,data=data3$PCred_clin_40to80nets1)
z10 = matrix(nrow=17,ncol=21,data=data3$PCred_clin_80to80nets1)
z9_minimised = matrix(nrow=9,ncol=21,data = NA)
z9_minimised[1,] = z9[1,]
z9_minimised[2,] = z9[3,]
z9_minimised[3,] = z9[5,]
z9_minimised[4,] = z9[7,]
z9_minimised[5,] = z9[9,]
z9_minimised[6,] = z9[11,]
z9_minimised[7,] = z9[13,]
z9_minimised[8,] = z9[15,]
z9_minimised[9,] = z9[17,]  

filled.contour(x, 
               y, 
               z = z9,
               zlim=c(-0.005416271,  1),
               color = topo.colors,
               plot.title = title(main = "PBO absolute reduction in clinical cases per person per year",
                                  xlab = "Endemicity (% infected 2 - 10 year olds)", 
                                  ylab = "Bioassay mortality (%)",cex.lab=1.5),
               plot.axes = { axis(1, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))
                 axis(2, at = seq(0, 1, by = .05), labels = seq(0, 100, by = 5))},
               #nlevels = 21,
               #levels=c(0.00001,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
               #          0.00125,0.0015,0.00175,0.002,
               #          0.0025,0.005,0.0075,0.01,0.025,0.04,0.05,0.06,0.07,0.075,0.08),
               key.title = title(main = ""))  # maybe also asp = 1

################
## confirmation plots
confirm_f = function(sites,ltys,cols){
  plot(data1$prev_2_10 ~ data1$year,
       ylab = "Prevalence 2 - 10 years", 
       xlab = "Time", xlim=c(-2,5),ylim = c(0,1),pch="")
  for(i in 1:length(sites)){
    
    data1 = read.table(paste0('P:/Ellies_output_folder/IVCC2control/draw_0/IVCC2control_', sites[i], '_0.txt'),header=TRUE)
    data2 = read.table(paste0('P:/Ellies_output_folder/IVCC2/draw_0/IVCC2_', sites[i], '_0.txt'),header=TRUE)
    
    lines(data1$prev_2_10 ~ data1$year,col=cols[i],lty=1,lwd=2)
    lines(data2$prev_2_10 ~ data2$year,col=cols[i],lty=ltys[i],lwd=2)
    
  }
}
confirm_f(sites=c(69,73,79,85),
          ltys = rep(c(2,3,4,5),each=3),
          cols =rep(c("black","blue","red"),3))

data1b = read.table(paste0('P:/Ellies_output_folder/IVCC3control/draw_0/IVCC3control_', site, '_0.txt'),header=TRUE)
data2b = read.table(paste0('P:/Ellies_output_folder/IVCC3/draw_0/IVCC3_', site, '_0.txt'),header=TRUE)

site = 169
data1 = read.table(paste0('P:/Ellies_output_folder/IVCC2control/draw_0/IVCC2control_', site, '_0.txt'),header=TRUE)
data2 = read.table(paste0('P:/Ellies_output_folder/IVCC2/draw_0/IVCC2_', site, '_0.txt'),header=TRUE)

plot(data1$prev_2_10*10 ~ data1$year,pch="")
lines(data1$prev_2_10*10 ~ data1$year)
lines(data2$prev_2_10*10 ~ data2$year,col="red")
