library(MalariaLaunchR)
library(adegenet)

draws = 1:50
for(j in 1:50){
  pbo_trial_f<-function(site,
                        
                        ITN_1, ITN_2, IRS_1,
                        
                        itn_repel_fun_1,	itn_repel_gamb_ss_1,	itn_repel_arab_1,
                        itn_kill_fun_1,	itn_kill_gamb_ss_1,	itn_kill_arab_1,
                        
                        itn_half_life,
                        
                        run_name){
    Run_name<-run_name
    draw<-draws[j]
    
    # Load the site_file file
    site_file<-read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_Copy/Africa_sites_0/Tanz_Kagera_677_', site, '.txt'))
    pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
    
    Int_set_up<-paste('num_people', pop_size, 'itn_irs_corr',	1, 'add output_type 0 itn 0 itn_start 0 continue_itn 0 itn_flexible 1 itn_leave_dur',	2.81333, 
                      
                      'num_runs 1 itn_cov_0_0', ITN_1, 'itn_cov_1_0 0',
                      'itn_cov_2_0 0 itn_cov_3_0 0 itn_cov_4_0 0 itn_cov_5_0 0 itn_cov_6_0 0 itn_cov_7_0 0 itn_cov_8_0 0 itn_cov_9_0 0 itn_cov_10_0 0',
                      'itn_cov_11_0 0 itn_cov_12_0 0 itn_cov_13_0 0 itn_cov_14_0 0 itn_cov_15_0 0 itn_cov_16_0 0 itn_cov_17_0 0 itn_cov_18_0 0 itn_cov_19_0 0',
                      
                      'add change_itn 1 change_itn_time 0',
                      'itn_repel_fun_1', itn_repel_fun_1, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_1, 'itn_repel_arab_1', itn_repel_arab_1,
                      'itn_kill_fun_1', itn_kill_fun_1, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_1, 'itn_kill_arab_1', itn_kill_arab_1,
                      'itn_half_life_1',itn_half_life,
                      
                      'add irs 1 irs_coverage', IRS_1, 'irs_start 0 irs_max_rounds 1 irs_offset_absolute 1 irs_offset 0.083 add irs_2 1 irs_coverage_2 0 irs_start_2 0 irs_max_rounds_2 20', 
                      
                      'add irs_ellie 1')
    Options<-paste(Int_set_up)
    
    ## Run the simulation
    Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                   OutputRoot = paste0("H:/Ellie/Rprojects/Malaria/Ellies_output_folder/", Run_name, '/draw_', draw),
                   Options=Options,
                   Exe = "H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                   Root="H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files",
                   Site=paste0('Africa_Sites_Ellie_Copy/Africa_sites_0/Tanz_Kagera_677_', site, '.txt'),
                   Parameter_draw = draw,
                   Return_output = FALSE)
  }
  
  ##Read in the csv with the appropriate data
  
  inp_2 <- read.csv("H:/Ellie/Rprojects/Malaria/PBO_Kagera_trial_redone_18062017.csv",header=TRUE)
  
  ## ON COMP
  for(i in c(1:4)){
    pbo_trial_f(site=inp_2[i,1], 
                
                ITN_1=inp_2[i,2], IRS_1=inp_2[i,3],
                
                itn_repel_fun_1=inp_2[i,4], itn_repel_gamb_ss_1=inp_2[i,5], itn_repel_arab_1=inp_2[i,6], 
                itn_kill_fun_1=inp_2[i,7], itn_kill_gamb_ss_1=inp_2[i,8], itn_kill_arab_1=inp_2[i,9],
                
                itn_half_life = inp_2[i,10],
                run_name="PBO_testsCI_FINAL_ESTIMATES")
    
  }
  
  
}


###################
##
## Bradley Bioko Island
library(MalariaLaunchR)
library(adegenet)

draws = 1:50
for(j in 1:50){
  trialbioko_f<-function(site,IRS_1,
                         irs_decay_mort1_1,irs_decay_mort2_1,
                         irs_decay_succ1_1,irs_decay_succ2_1,
                         irs_decay_det1_1,irs_decay_det2_1,
                         run_name){
    Run_name<-run_name
    draw<-draws[j]
    
    # Load the site_file file
    site_file<-read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_Copy/Africa_sites_0/Bioko_244_', site, '.txt'))
    pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
    
    Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', 0.3, 'irs 1 irs_coverage', 0.75, 'itn_irs_corr 1 output_type 0',
                      'add change_irs 1 change_irs_time 0',
                      'irs_decay_mort1_1', irs_decay_mort1_1, 'irs_decay_mort2_1',  irs_decay_mort2_1,
                      'irs_decay_succ1_1', irs_decay_succ1_1, 'irs_decay_succ2_1',  irs_decay_succ2_1,
                      'irs_decay_det1_1', irs_decay_det1_1, ' irs_decay_det2_1',  irs_decay_det2_1,
                      'irs_offset_absolute 1 irs_offset 0.3',
                      'add irs_ellie 1')
    
    Options<-paste(Int_set_up)
    
    ## Run the simulation
    Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                   OutputRoot = paste0("H:/Ellie/Rprojects/Malaria/Ellies_output_folder/", Run_name, '/draw_', draw),
                   Options=Options,
                   Exe = "H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                   Root="H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files",
                   Site=paste0('Africa_Sites_Ellie_Copy/Africa_sites_0/Bioko_244_', site, '.txt'),
                   Parameter_draw = draw,
                   Return_output = FALSE)
  }
  #
  inp_2 <- read.csv("H:/Ellie/Rprojects/Malaria/Bioko_trial_deltamethrin_resistance_bendio arm.csv",header=TRUE)
  trialbioko_f(site=inp_2[1,1],IRS_1=inp_2[1,2],
               irs_decay_mort1_1=inp_2[1,3],irs_decay_mort2_1=inp_2[1,4],	
               irs_decay_succ1_1=inp_2[1,5],irs_decay_succ2_1=inp_2[1,6],	
               irs_decay_det1_1=inp_2[1,7],	irs_decay_det2_1=inp_2[1,8],
               run_name="Bioko_test_NEW_estimates_bendio")
  
}

for(j in 1:50){
  trialbioko_f<-function(site,IRS_1,
                         irs_decay_mort1_1,irs_decay_mort2_1,
                         irs_decay_succ1_1,irs_decay_succ2_1,
                         irs_decay_det1_1,irs_decay_det2_1,
                         run_name){
    Run_name<-run_name
    draw<-draws[j]
    
    # Load the site_file file
    site_file<-read.table(paste0('H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_Copy/Africa_sites_0/Bioko_244_', site, '.txt'))
    pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
    
    Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', 0.3, 'irs 1 irs_coverage', 0.75, 'itn_irs_corr 1 output_type 0',
                      'add change_irs 1 change_irs_time 0',
                      'irs_decay_mort1_1', irs_decay_mort1_1, 'irs_decay_mort2_1',  irs_decay_mort2_1,
                      'irs_decay_succ1_1', irs_decay_succ1_1, 'irs_decay_succ2_1',  irs_decay_succ2_1,
                      'irs_decay_det1_1', irs_decay_det1_1, ' irs_decay_det2_1',  irs_decay_det2_1,
                      'irs_offset_absolute 1 irs_offset 0.3',
                      'add irs_ellie 1')
    
    Options<-paste(Int_set_up)
    
    ## Run the simulation
    Model_launcher(OutputName = paste(Run_name, site, draw, sep='_'),
                   OutputRoot = paste0("H:/Ellie/Rprojects/Malaria/Ellies_output_folder/", Run_name, '/draw_', draw),
                   Options=Options,
                   Exe = "H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                   Root="H:/Ellie/Rprojects/Malaria/Ellies_cool_model_folder2/model_files",
                   Site=paste0('Africa_Sites_Ellie_Copy/Africa_sites_0/Bioko_244_', site, '.txt'),
                   Parameter_draw = draw,
                   Return_output = FALSE)
  }
  #
  inp_3 <- read.csv("H:/Ellie/Rprojects/Malaria/Bioko_trial_deltamethrin_resistance_delta arm.csv",header=TRUE)
  
  trialbioko_f(site=inp_3[1,1],IRS_1=inp_3[1,2],
               irs_decay_mort1_1=inp_3[1,3],irs_decay_mort2_1=inp_3[1,4],	
               irs_decay_succ1_1=inp_3[1,5],irs_decay_succ2_1=inp_3[1,6],	
               irs_decay_det1_1=inp_3[1,7],	irs_decay_det2_1=inp_3[1,8],
               run_name="Bioko_test_NEW_estimates_delta")
  
}
