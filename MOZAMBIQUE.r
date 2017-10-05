#########################################
##
##
## MOZAMBIQUE

##################################################
##
## Function to consider the combined impact of coverage and resistance on cases averted using 80% IRS (pyrethroid or Actellic)
COV_RES_MOZO<-function(site, 
                       
                       itn_repel_fun, itn_repel_gamb_ss, itn_repel_arab, 
                       itn_kill_fun, itn_kill_gamb_ss, itn_kill_arab,
                       itn_half_life,
                       
                       irs_decay_mort1_1, irs_decay_mort2_1, 
                       irs_decay_succ1_1, irs_decay_succ2_1, 
                       irs_decay_det1_1, irs_decay_det2_1,
                       
                       irs_decay_mort1_2, irs_decay_mort2_2, 
                       irs_decay_succ1_2, irs_decay_succ2_2, 
                       irs_decay_det1_2, irs_decay_det2_2,
                       
                       run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 0 num_people', pop_size, 'itn 1 itn_coverage 0.8 irs 1 irs_coverage 0.8', 
                    'add change_irs 1 change_irs_time 0',
                    'add change_itn 1 change_itn_time 0',
                    'itn_repel_fun_1', itn_repel_fun, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss, 'itn_repel_arab_1', itn_repel_arab,
                    'itn_kill_fun_1', itn_kill_fun, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss, 'itn_kill_arab_1', itn_kill_arab,
                    'itn_half_life_1',itn_half_life,
                    'irs_decay_mort1_1', irs_decay_mort1_1, 'irs_decay_mort2_1', irs_decay_mort2_1,
                    'irs_decay_succ1_1', irs_decay_succ1_1, 'irs_decay_succ2_1', irs_decay_succ2_1,
                    'irs_decay_det1_1', irs_decay_det1_1, 'irs_decay_det2_1', irs_decay_det2_1, ##Given some level of resistance
                    'add change_irs_2 1 change_irs_time_2 3',
                    'irs_decay_mort1_2', irs_decay_mort1_2, 'irs_decay_mort2_2', irs_decay_mort2_2,
                    'irs_decay_succ1_2', irs_decay_succ1_2, 'irs_decay_succ2_2', irs_decay_succ2_2,
                    'irs_decay_det1_2', irs_decay_det1_2, 'irs_decay_det2_2', irs_decay_det2_2,  ## Switch to Bendiocarb, or Actellic
                    'add irs_ellie 1')
  
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}

fitM_function<-function(targ,site, 
                       
                       itn_repel_fun, itn_repel_gamb_ss, itn_repel_arab, 
                       itn_kill_fun, itn_kill_gamb_ss, itn_kill_arab,
                       itn_half_life,
                       
                       irs_decay_mort1_1, irs_decay_mort2_1, 
                       irs_decay_succ1_1, irs_decay_succ2_1, 
                       irs_decay_det1_1, irs_decay_det2_1,
                       
                       irs_decay_mort1_2, irs_decay_mort2_2, 
                       irs_decay_succ1_2, irs_decay_succ2_2, 
                       irs_decay_det1_2, irs_decay_det2_2){
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('output_type 1 recalculate 0 num_people', pop_size, 'itn 1 itn_coverage 0.8 irs 1 irs_coverage 0.8', 
                    'add change_itn_1 1 change_itn_time_1 0',
                    'itn_repel_fun_1', itn_repel_fun, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss, 'itn_repel_arab_1', itn_repel_arab,
                    'itn_kill_fun_1', itn_kill_fun, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss, 'itn_kill_arab_1', itn_kill_arab,
                    'itn_half_life_1',itn_half_life,
                    'add change_irs_1 1 change_irs_time_1 0',
                    'irs_decay_mort1_1', irs_decay_mort1_1, 'irs_decay_mort2_1',  irs_decay_mort2_1,
                    'irs_decay_succ1_1', irs_decay_succ1_1, 'irs_decay_succ2_1',  irs_decay_succ2_1,
                    'irs_decay_det1_1', irs_decay_det1_1, 'irs_decay_det2_1',  irs_decay_det2_1, ##Given some level of resistance
                    
                    'add change_irs_2 1 change_irs_time_2 3',
                    'irs_decay_mort1_2', irs_decay_mort1_2, 'irs_decay_mort2_2',  irs_decay_mort2_2,
                    'irs_decay_succ1_2', irs_decay_succ1_2, 'irs_decay_succ2_2',  irs_decay_succ2_2,
                    'irs_decay_det1_2', irs_decay_det1_2, 'irs_decay_det2_2',  irs_decay_det2_2,  ## Switch to Bendiocarb, or Actellic
                    
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Fit M
  ## **For each location make sure you change the years for the adjusted parameterisations as per info above**
  Fit_M(variable = "prev_2_10", 
        target = targ, 
        year=0, 
        Root="F:/Ellies_cool_model_folder2/model_files",
        Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'),
        Options=Options,
        overwrite = TRUE, tolerance = 0.0368, interval = c(30, 300),
        maxiter = 10)
}

inp <- read.csv("F:/mozambique_zambiesia_exploration.csv",header=TRUE)
names(inp)

for (i in 1:3){
  fitM_function(targ = 0.6, site=inp[i,1], 
                itn_repel_fun=inp[i,2], itn_repel_gamb_ss=inp[i,3], itn_repel_arab=inp[i,4], 
                itn_kill_fun=inp[i,5], itn_kill_gamb_ss=inp[i,6], itn_kill_arab=inp[i,7],
                itn_half_life=inp[i,8],
                
                irs_decay_mort1_1=inp[i,9], irs_decay_mort2_1=inp[i,10], 
                irs_decay_succ1_1=inp[i,11], irs_decay_succ2_1=inp[i,12], 
                irs_decay_det1_1=inp[i,13], irs_decay_det2_1=inp[i,14],
                
                irs_decay_mort1_2=inp[i,15], irs_decay_mort2_2=inp[i,16], 
                irs_decay_succ1_2=inp[i,17], irs_decay_succ2_2=inp[i,18], 
                irs_decay_det1_2=inp[i,19], irs_decay_det2_2=inp[i,20])
  
}
  


##previous total_M	272.777749673982
##prevalence from Pete is 0.4091 (but reportedly higher from Molly so using 0.6)

##Read in the csv with the appropriate data
## ON COMP
for (i in 3){
  COV_RES_MOZO(site=1,
               itn_repel_fun=inp[i,2], itn_repel_gamb_ss=inp[i,3], itn_repel_arab=inp[i,4], 
               itn_kill_fun=inp[i,5], itn_kill_gamb_ss=inp[i,6], itn_kill_arab=inp[i,7],
               itn_half_life=inp[i,8],
               irs_decay_mort1_1=inp[i,9], irs_decay_mort2_1=inp[i,10], 
               irs_decay_succ1_1=inp[i,11], irs_decay_succ2_1=inp[i,12], 
               irs_decay_det1_1=inp[i,13], irs_decay_det2_1=inp[i,14],
               irs_decay_mort1_2=inp[i,15], irs_decay_mort2_2=inp[i,16], 
               irs_decay_succ1_2=inp[i,17], irs_decay_succ2_2=inp[i,18], 
               irs_decay_det1_2=inp[i,19], irs_decay_det2_2=inp[i,20],
               run_name="mozo")
}


data1 = read.table(paste0('F:/Ellies_output_folder/mozo/draw_0/mozo_1_0.txt'),header=TRUE)
data50 = read.table(paste0('F:/Ellies_output_folder/mozo/draw_0/mozo_2_0.txt'),header=TRUE)
data100 = read.table(paste0('F:/Ellies_output_folder/mozo/draw_0/mozo_3_0.txt'),header=TRUE)

plot(data1[,3] ~ data1$year,pch="",ylim=c(0,1),
     xlim=c(-5,10),ylab="Prevalence in 2 to 10 year olds",yaxt="n",
     xlab="Time (years)",cex.lab=1.6,cex.axis=1.6,xaxt="n")
axis(1,at=c(-3,0,3,6,9,12),cex.lab=1.6,cex.axis=1.6)
axis(2,las=2,at=seq(0,1,0.2),cex.lab=1.6,cex.axis=1.6)

lines(data1[,3] ~ data1$year,lty=1)
lines(data50[,3] ~ data50$year,lty=2)
lines(data100[,3] ~ data100$year,lty=3,lwd=2)





OutputName, OutputRoot = NULL, Options = NULL, Parameter_draw = 0, 
Return_output = TRUE, Overwrite = TRUE, Exe = system.file("bin", 
                                                          "Indiv_MalariaLaunchR_2.0.1.exe", package = "MalariaLaunchR"), 
Root = system.file("model_files", package = "MalariaLaunchR"), 
Parms = "Default_parms.txt", Site = "Site_Perennial.txt", 
Demog = "Generic_demog.txt", Pop = "Generic_pop.txt"

OutputName = paste(Run_name, site, draw, sep='_') ,
OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
Options=Options,
Exe = ,
Root="F:/Ellies_cool_model_folder2/model_files",
Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'),
Parameter_draw = draw,
Return_output = FALSE

Input <- list(Root = "F:/Ellies_cool_model_folder2/model_files",
              Executable = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
              Parameters = "Default_parms.txt", 
      Site = paste0('Africa_Sites_Ellie_eip/G2RCToptions/site_GF_449_', site, '.txt'),
      Demography = "Generic_demog.txt", Population = "Generic_pop.txt",
      OutputRoot = "debug2", 
      OutputName = "debug", Options = Options_str_to_df(Options))


Options_str_to_df(Options)

Options_str <- Options
split <- unlist(strsplit(Options_str, " "))
if ("add" %in% split) {
  add_index <- which(split == "add")
  s1 <- str_to_df(split[1:(add_index - 1)])
  s2 <- str_to_df(split[(add_index + 1):length(split)])
  s <- rbind(s1, c("add", NA), s2)
}
else {
  s <- str_to_df(split)
}
