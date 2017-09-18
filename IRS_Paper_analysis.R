library(MalariaLaunchR)
library(adegenet)

#############################################
##
## IRS PAPER 
## FUNCTION FOR ALL ADMIN UNITS EACH LEVEL OF RESISTANCE 0 TO 100% 


WHY_CHANGE_IRS<-function(site, 
                         ITN, IRS,
                         irs_decay_mort1_1, irs_decay_mort2_1, 
                         irs_decay_succ1_1, irs_decay_succ2_1, 
                         irs_decay_det1_1, irs_decay_det2_1,
                         irs_decay_mort1_2, irs_decay_mort2_2, 
                         irs_decay_succ1_2, irs_decay_succ2_2, 
                         irs_decay_det1_2, irs_decay_det2_2,
                         itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                         itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                         itn_half_life_2,
                         irs_decay_mort1_3, irs_decay_mort2_3, 
                         irs_decay_succ1_3, irs_decay_succ2_3, 
                         irs_decay_det1_3, irs_decay_det2_3,
                         run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie/Africa_sites_0/Site_GF_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn_irs_corr 1 itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'irs_decay_mort1', irs_decay_mort1_1, ' irs_decay_mort2',  irs_decay_mort2_1,
                    'irs_decay_succ1', irs_decay_succ1_1, ' irs_decay_succ2',  irs_decay_succ2_1,
                    'irs_decay_det1', irs_decay_det1_1, ' irs_decay_det2',  irs_decay_det2_1,
                    'add change_irs_2 1 change_irs_time_2 3',
                    'add change_itn_2 1 change_itn_time_2 3',
                    'irs_decay_mort1_2', irs_decay_mort1_2, ' irs_decay_mort2_2',  irs_decay_mort2_2,
                    'irs_decay_succ1_2', irs_decay_succ1_2, ' irs_decay_succ2_2',  irs_decay_succ2_2,
                    'irs_decay_det1_2', irs_decay_det1_2, ' irs_decay_det2_2',  irs_decay_det2_2,
                    'itn_repel_fun_2', itn_repel_fun_2, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_2, 'itn_repel_arab_2', itn_repel_arab_2,
                    'itn_kill_fun_2', itn_kill_fun_2, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_2, 'itn_kill_arab_2', itn_kill_arab_2,
                    'itn_half_life_2',itn_half_life_2,
                    'add change_irs_3 1 change_irs_time_3 6',
                    'irs_decay_mort1_3', irs_decay_mort1_3, ' irs_decay_mort2_3',  irs_decay_mort2_3,
                    'irs_decay_succ1_3', irs_decay_succ1_3, ' irs_decay_succ2_3',  irs_decay_succ2_3,
                    'irs_decay_det1_3', irs_decay_det1_3, ' irs_decay_det2_3',  irs_decay_det2_3,
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("F:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "F:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="F:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie/Africa_sites_0/Site_GF_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}

##################################################
##
## Function to consider the combined impact of coverage and resistance on cases averted using 80% IRS (pyrethroid or Actellic)
WHY_COV_RES<-function(site, 
                      ITN, IRS,
                      irs_decay_mort1_1, irs_decay_mort2_1, 
                      irs_decay_succ1_1, irs_decay_succ2_1, 
                      irs_decay_det1_1, irs_decay_det2_1,
                      irs_decay_mort1_2, irs_decay_mort2_2, 
                      irs_decay_succ1_2, irs_decay_succ2_2, 
                      irs_decay_det1_2, irs_decay_det2_2,
                      itn_repel_fun_2, itn_repel_gamb_ss_2, itn_repel_arab_2, 
                      itn_kill_fun_2, itn_kill_gamb_ss_2, itn_kill_arab_2,
                      itn_half_life_2,
                      irs_decay_mort1_3, irs_decay_mort2_3, 
                      irs_decay_succ1_3, irs_decay_succ2_3, 
                      irs_decay_det1_3, irs_decay_det2_3,
                      run_name){
  Run_name<-run_name
  draw<-0
  
  # Load the site_file file
  site_file<-read.table(paste0('F:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/perennial/Site_Perennial_', site, '.txt'))
  pop_size<- 80000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
  
  Int_set_up<-paste('num_people', pop_size, 'itn 1 itn_coverage', ITN, 'irs 1 irs_coverage', IRS,
                    'irs_decay_mort1', irs_decay_mort1_1, ' irs_decay_mort2',  irs_decay_mort2_1,
                    'irs_decay_succ1', irs_decay_succ1_1, ' irs_decay_succ2',  irs_decay_succ2_1,
                    'irs_decay_det1', irs_decay_det1_1, ' irs_decay_det2',  irs_decay_det2_1,
                    'add change_irs 1 change_irs_time 3',
                    'irs_decay_mort1_1', irs_decay_mort1_2, ' irs_decay_mort2_1',  irs_decay_mort2_2,
                    'irs_decay_succ1_1', irs_decay_succ1_2, ' irs_decay_succ2_1',  irs_decay_succ2_2,
                    'irs_decay_det1_1', irs_decay_det1_2, ' irs_decay_det2_1',  irs_decay_det2_2,
                    'add change_itn 1 change_itn_time 3',
                    'itn_repel_fun_1', itn_repel_fun_2, 'itn_repel_gamb_ss_1', itn_repel_gamb_ss_2, 'itn_repel_arab_1', itn_repel_arab_2,
                    'itn_kill_fun_1', itn_kill_fun_2, 'itn_kill_gamb_ss_1', itn_kill_gamb_ss_2, 'itn_kill_arab_1', itn_kill_arab_2,
                    'itn_half_life_1',itn_half_life_2,
                    'add change_irs_2 1 change_irs_time_2 6',
                    'irs_decay_mort1_2', irs_decay_mort1_3, ' irs_decay_mort2_2',  irs_decay_mort2_3,
                    'irs_decay_succ1_2', irs_decay_succ1_3, ' irs_decay_succ2_2',  irs_decay_succ2_3,
                    'irs_decay_det1_2', irs_decay_det1_3, ' irs_decay_det2_2',  irs_decay_det2_3,
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
inp <- read.csv("F:/IRS_PAPER_3D_estimates.csv",header=TRUE)
names(inp)
## ON COMP
for(i in 1:2){
  WHY_COV_RES(site=inp[i,1], 
              ITN=inp[i,2], IRS=inp[i,3],
              
              irs_decay_mort1_1=inp[i,4], irs_decay_mort2_1=inp[i,5], 
              irs_decay_succ1_1=inp[i,6], irs_decay_succ2_1=inp[i,7], 
              irs_decay_det1_1=inp[i,8], irs_decay_det2_1=inp[i,9],
              
              irs_decay_mort1_2=inp[i,10], irs_decay_mort2_2=inp[i,11], 
              irs_decay_succ1_2=inp[i,12], irs_decay_succ2_2=inp[i,13], 
              irs_decay_det1_2=inp[i,14], irs_decay_det2_2=inp[i,15],
              
              itn_repel_fun_2=inp[i,16], itn_repel_gamb_ss_2=inp[i,17], itn_repel_arab_2=inp[i,18], 
              itn_kill_fun_2=inp[i,19], itn_kill_gamb_ss_2=inp[i,20], itn_kill_arab_2=inp[i,21],
              itn_half_life_2=inp[i,22],
              
              irs_decay_mort1_3=inp[i,23], irs_decay_mort2_3=inp[i,24], 
              irs_decay_succ1_3=inp[i,25], irs_decay_succ2_3=inp[i,26], 
              irs_decay_det1_3=inp[i,27], irs_decay_det2_3=inp[i,28],
              
              run_name="3d_res_cov_switching_irs")
  
}

##
###
####
##### RUN ON CLUSTER AND THE OUTPUTS ARE STORED HERE: 

## F:\Ellies_output_folder\IRS_paper_switchingRES_0 ## Or equivalent for other resistance levels

## Calculating cases per 1000 people averted

cases_averted_fun = function(site,resistance_level,coverage){
  
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_0pyr/draw_0/IRS_paper_switchingRES_0pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_10pyr/draw_0/IRS_paper_switchingRES_10pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_20pyr/draw_0/IRS_paper_switchingRES_20pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_30pyr/draw_0/IRS_paper_switchingRES_30pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_40pyr/draw_0/IRS_paper_switchingRES_40pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_50pyr/draw_0/IRS_paper_switchingRES_50pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_60pyr/draw_0/IRS_paper_switchingRES_60pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_70pyr/draw_0/IRS_paper_switchingRES_70pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_80pyr/draw_0/IRS_paper_switchingRES_80pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_90pyr/draw_0/IRS_paper_switchingRES_90pyr_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_100pyr/draw_0/IRS_paper_switchingRES_100pyr_', site, '_0.txt'),header=TRUE)
  
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_0act/draw_0/IRS_paper_switchingRES_0act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_10act/draw_0/IRS_paper_switchingRES_10act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_20act/draw_0/IRS_paper_switchingRES_20act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_30act/draw_0/IRS_paper_switchingRES_30act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_40act/draw_0/IRS_paper_switchingRES_40act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_50act/draw_0/IRS_paper_switchingRES_50act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_60act/draw_0/IRS_paper_switchingRES_60act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_70act/draw_0/IRS_paper_switchingRES_70act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_80act/draw_0/IRS_paper_switchingRES_80act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_90act/draw_0/IRS_paper_switchingRES_90act_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_100act/draw_0/IRS_paper_switchingRES_100act_', site, '_0.txt'),header=TRUE)
  
  #data1 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_irs_pyr/draw_0/3d_res_cov_switching_irs_pyr_', site, '_0.txt'),header=TRUE)
  #data2 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_irs_ACT/draw_0/3d_res_cov_switching_irs_ACT_', site, '_0.txt'),header=TRUE)
  data1 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_IRS_pyr_highly_seasonal/draw_0/3d_res_cov_switching_IRS_pyr_highly_seasonal_', site, '_0.txt'),header=TRUE)
  data2 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_IRS_act_highly_seasonal/draw_0/3d_res_cov_switching_IRS_act_highly_seasonal_', site, '_0.txt'),header=TRUE)
  ##Number of Cases
                     #pyr_noRes = data1$clin_inc_all[data1$year==3]
                     #pyr_res   = data1$clin_inc_all[data1$year==6]
                     #actellic  = data1$clin_inc_all[data1$year==9]
                     
                     pyr_noIRS       = data1$clin_inc_all[data1$year==3]
                     pyr_noIRS_res   = data1$clin_inc_all[data1$year==6]
                     pyr_res_IRS     = data1$clin_inc_all[data1$year==9]
                     act_res_IRS     = data2$clin_inc_all[data1$year==9]
                     
                     ##Number of Cases averted
                     #pyr_noRes_avert = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==3]
                     #pyr_res_avert   = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==6]
                     #actellic_avert  = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==9]
                     
                     ##Number of Cases averted BY irs
                     pyrethroid_irs = data1$clin_inc_all[data1$year==6] - data1$clin_inc_all[data1$year==9] 
                     actellicCS_irs = data2$clin_inc_all[data2$year==6] - data2$clin_inc_all[data2$year==9] 
                     actel_v_pyreth  =data1$clin_inc_all[data1$year==9] - data2$clin_inc_all[data2$year==9] 
                     
                     ##return
                     #return(cbind(pyr_noRes,pyr_res,actellic,
                    #              pyr_noRes_avert,pyr_res_avert,actellic_avert,
                    #              resistance_level,coverage))
                     return(cbind(pyr_noIRS,pyr_noIRS_res,pyr_res_IRS,act_res_IRS,
                                  pyrethroid_irs,actellicCS_irs,actel_v_pyreth,
                                  resistance_level,coverage) )
                     
}

sites_temp = read.csv("F:/IRS_PAPER_SWITCHING_10PC_RESISTANCE.csv",header=TRUE)
vals = c(sites_temp$site)
data_store = array(dim=c(length(vals),8))
colnames(data_store) = c("Pyrethroid_80%_no_resistance",
                         "Pyrethroids_with_res",
                         "Actellic",
                         "Pyrethroid_80%_no_resistance_Cases_averted",
                         "Pyrethroids_with_res_Cases_averted",
                         "Actellic_Cases_averted",
                         "resistance_level","llin_coverage_level")
for(i in 1:length(vals)){
  data_store[i,] <- cases_averted_fun(site=vals[i],0,NA)
}
data2 = as.data.frame(data_store)
head(data2)

write.csv(data2,"F:/IRS_paper_summary_cases_averted/Data_summary_temp.csv")


##For 3d data
vals = c(1:441)
resistance = rep(seq(0,1,by=0.05),20)
coverage = rep(seq(0,1,by=0.05),each=20)

data_store = array(dim=c(length(vals),9))
colnames(data_store) = c("Pyrethroid_no_IRS",
                         "Pyrethroids_no_IRS_with_res",
                         "Pyrethroids_IRS_with_res",
                         "Actellic_IRS_with_res",
                         "CASES_AVERT_Pyrethroid_80%",
                         "CASES_AVERT_ACTELLIC_80%",
                         "Act_vs_Pyr_CASE_AVERT",
                         "resistance_level","llin_coverage_level")
for(i in 1:length(vals)){
  data_store[i,] <- cases_averted_fun(site=vals[i],resistance[i],coverage[i])
}

data2 = as.data.frame(data_store)

library(plotly)
library(stringr)
library(reshape2)

p = plot_ly(data2, x = ~resistance_level, 
        y = ~llin_coverage_level, 
        z = ~Pyrethroids_with_res_Cases_averted, 
        type = "contour") 


a = plot_ly(data2, x = ~resistance_level, 
        y = ~llin_coverage_level, 
        z = ~Actellic_Cases_averted, 
        type = "contour") 
BOTH <- subplot(p, a)
BOTH

require(grDevices) # for colours

x = seq(0, 1, length.out = 21)
y = seq(0, 1, length.out = 21)


filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$Pyrethroids_IRS_with_res),
               color = topo.colors,
               plot.title = title(main = "Cases per person per year: Pyrethroid IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05))},
               nlevels = 21,
               levels=c(0.00001,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
                        0.00125,0.0015,0.00175,0.002,
                        0.0025,0.005,0.0075,0.01,0.025,0.04,0.05,0.06,0.07,0.075,0.08),
               key.title = title(main = ""))  # maybe also asp = 1

filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$"Actellic_IRS_with_res"),
               color = topo.colors,
               plot.title = title(main = "Cases per person per year: Actellic IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05))},
               nlevels = 23,
               levels=c(0.00001,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
                        0.00125,0.0015,0.00175,0.002,
                        0.0025,0.005,0.0075,0.01,0.025,0.04,0.05,0.06,0.07,0.075,0.08,0.09,0.1),
               key.title = title(main = ""))  # maybe also asp = 1
#mtext(paste("filled.contour(.) from", R.version.string),
#      side = 1, line = 4, adj = 1, cex = .66)
topo.colors_Rev = function (n, alpha = 1) 
{
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n%/%3
    k <- n%/%3
    i <- n - j - k
    rev(c(if (i > 0) hsv(h = seq.int(from = 43/60, to = 31/60, 
                                     length.out = i), alpha = alpha), if (j > 0) hsv(h = seq.int(from = 23/60, 
                                                                                                 to = 11/60, length.out = j), alpha = alpha), if (k > 
                                                                                                                                                  0) hsv(h = seq.int(from = 10/60, to = 6/60, length.out = k), 
                                                                                                                                                         alpha = alpha, s = seq.int(from = 1, to = 0.3, length.out = k), 
                                                                                                                                                         v = 1)) )
  }
  else character()
}

filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$"CASES_AVERT_Pyrethroid_80%"),
               color = heat.colors,
               plot.title = title(main = "Cases per person per year averted with Pyrethroid IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05)) },
               nlevels = 17,
               levels=seq(0.005,0.45,0.025),
               key.title = title(main = "", cex.main=0.8))  # maybe also asp = 1


filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$"CASES_AVERT_ACTELLIC_80%"),
               color = heat.colors,
               plot.title = title(main = "Cases per person per year averted with Actellic IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05)) },
               nlevels = 17,
               levels=seq(0.005,0.45,0.025),
               key.title = title(main = "", cex.main=0.8))  # maybe also asp = 1


filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$"Act_vs_Pyr_CASE_AVERT"),
               color = topo.colors_Rev,
               plot.title = title(main = "Cases per person per year averted by Actellic IRS in place of Pyrethroid IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05)) },
               nlevels = 17,
               levels=seq(0.00011,0.0735720700,length=18),
               key.title = title(main = "", cex.main=0.8))  # maybe also asp = 1


#############
##
## checks
site = 313
data0 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_0/draw_0/IRS_paper_switchingRES_0_', site, '_0.txt'),header=TRUE)
data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_10/draw_0/IRS_paper_switchingRES_10_', site, '_0.txt'),header=TRUE)
data2 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_20/draw_0/IRS_paper_switchingRES_20_', site, '_0.txt'),header=TRUE)
data3 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_30/draw_0/IRS_paper_switchingRES_30_', site, '_0.txt'),header=TRUE)
data4 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_40/draw_0/IRS_paper_switchingRES_40_', site, '_0.txt'),header=TRUE)
data5 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_50/draw_0/IRS_paper_switchingRES_50_', site, '_0.txt'),header=TRUE)
data6 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_60/draw_0/IRS_paper_switchingRES_60_', site, '_0.txt'),header=TRUE)
data7 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_70/draw_0/IRS_paper_switchingRES_70_', site, '_0.txt'),header=TRUE)
data8 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_80/draw_0/IRS_paper_switchingRES_80_', site, '_0.txt'),header=TRUE)
data9 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_90/draw_0/IRS_paper_switchingRES_90_', site, '_0.txt'),header=TRUE)
data10= read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_100/draw_0/IRS_paper_switchingRES_100_', site, '_0.txt'),header=TRUE)
plot(data0$clin_inc_all~data1$year,pch="",ylim=c(0,0.2))
lines(data0$clin_inc_all~data1$year)
lines(data1$clin_inc_all~data1$year)
lines(data2$clin_inc_all~data1$year)
lines(data3$clin_inc_all~data1$year)
lines(data4$clin_inc_all~data1$year)
lines(data5$clin_inc_all~data1$year)
lines(data6$clin_inc_all~data1$year)
lines(data7$clin_inc_all~data1$year)
lines(data8$clin_inc_all~data1$year)
lines(data9$clin_inc_all~data1$year)
lines(data10$clin_inc_all~data1$year)

			
time=1:365
##no resistance
mort_hut_IRS = 1 / (1 + exp(-(-0.008226786*time + 1.179879445)) )
succ_hut_IRS = 1 / (1 + exp(-(0.006254687*time - 2.701416346)) )
det_hut_IRS = 1 / (1 + exp(-(0*time + -1.335781322)) )
rep_hut_IRS = 1 - mort_hut_IRS - succ_hut_IRS
surv_bioassay=0

##80% resistance
mort_hut_IRS = 1 / (1 + exp(-(-0.004314865*time + -0.767313459)) )
succ_hut_IRS = 1 / (1 + exp(-(6.88E-05*time + -1.404541238)) )
det_hut_IRS = 1 / (1 + exp(-(0*time + -1.074252818)) )
rep_hut_IRS = 1 - mort_hut_IRS - succ_hut_IRS
surv_bioassay=0.8	
#plot(mort_hut_IRS ~ time,ylim=c(0,1))
#lines(mort_hut_IRS ~ time,col="blue",lwd=2)
#lines(succ_hut_IRS ~ time,col="darkred",lwd=2)
#lines(det_hut_IRS ~ time,col="green",lwd=2)

kp_IRS = (1 - det_hut_IRS) * succ_hut_IRS
jp_IRS = (1 - det_hut_IRS) * rep_hut_IRS + det_hut_IRS
lp_IRS = (1 - det_hut_IRS) * mort_hut_IRS

k0 = 0.699
r_IRS = (1 - kp_IRS/k0) * (jp_IRS/(lp_IRS + jp_IRS))
d_IRS = (1 - kp_IRS/k0) * (lp_IRS/(lp_IRS + jp_IRS))
s_IRS = kp_IRS/k0

#plot(r_IRS ~ time, ylim = c(0,1),pch="")
#lines(d_IRS ~ time,col="blue",lwd=2,lty=5)
#lines(s_IRS ~ time,col="darkred",lwd=2,lty=5)
#lines(r_IRS ~ time,col="green",lwd=2,lty=5)

#checker = numeric(365)
#for(i in 1:365) {
#  checker[i] = d_IRS[i] + s_IRS[i] + r_IRS[i]
#}
#checker

## And for nets#
#checker = numeric(21)
#for(i in 1:21) {
#  checker[i] = ERG_d_ITN0[i] + ERG_s_ITN0[i] + ERG_r_ITN0[i]
#}
#checker


##{halflife}
#checker_nets = function(surv_bioassay,val3){
  #surv_bioassay=0.8	#	{measure of resistance 0=no resistance 1=100% survival in discriminating dose bioassay}
  mort_assay=1-surv_bioassay
  
  mort_hut_a = 0.6338 + 3.9970*(mort_assay-0.5)	#			{relationship mortality in bioassay -> hut trial, logit scale}
  mort_hut   = exp(mort_hut_a)/(1+exp(mort_hut_a))
  
  det_hut_a = 0.07117+1.257*(mort_hut-0.5)+-1.517*(mort_hut-0.5)^2#	{relationship hut trial mortality -> deterrence}
  det_hut   = if(det_hut_a < 0) 0 else det_hut_a	#		{censored to stop becoming negative}
  suc_hut   = 0.02491 *exp( 3.317 *(1-mort_hut))			#	{relationship hut trial mortality -> success}
  rep_hut   = 1-suc_hut-mort_hut
  
  n1n0 = 1-det_hut
  kp1  = n1n0*suc_hut
  jp1  = n1n0*rep_hut+(1-n1n0)
  lp1  = n1n0*mort_hut
  
  r_ITN0  = (1-kp1/0.699)*(jp1/(lp1+jp1))		#	{probability of dying with an encounter with ITN (max)}
  d_ITN0  = (1-kp1/0.699)*(lp1/(lp1+jp1))		#	{probability of repeating behaviour (max)}
  
  
  hut_max_a = 0.6338 + 3.9970*(1-0.5)				#{maximum mortality seen in huts, used to adjust}
  hut_max   = exp(hut_max_a)/(1+exp(hut_max_a))
  
  my_max_washes_a = -2.360 +-3.048*(hut_max-0.5)		
  my_max_washes   = log(2)/(exp(my_max_washes_a)/(1+exp(my_max_washes_a)))
  
  wash_decay_rate_a = -2.360+-3.048*(mort_hut-0.5)
  wash_decay_rate   = exp(wash_decay_rate_a)/(1+exp(wash_decay_rate_a))
  
  itn_half_life = numeric(365)
  for(i in 1:365){
    itn_half_life[i]     = (log(2)/wash_decay_rate)/my_max_washes*2.64*time[i]
    
  }
  
  itn_loss = log(2)/itn_half_life
 
  ITN_decay = exp(-itn_loss)
  
  r_ITN_min=0.24 
  d_ITN = d_ITN0*ITN_decay 		#	; insecticide mortality rate 
  r_ITN = r_ITN_min + (r_ITN0 - r_ITN_min)*ITN_decay 
  s_ITN = 1 - d_ITN0 - r_ITN			#; successful protected human biting 
  
  #plot(d_ITN ~ time, ylim=c(0,1),pch="")
  #lines(d_ITN ~ time,col="blue",lwd=2,lty=val3)
  #lines(r_ITN ~ time,col="green",lwd=2,lty=val3)
  #lines(s_ITN ~ time,col="darkred",lwd=2,lty=val3)
  
  
#}
#plot(d_ITN ~ time, ylim=c(0,1),pch="")
#checker_nets(0,1) ## all die bioassay
#checker_nets(0.2,2) ## 
#checker_nets(0.4,3) ## 
#checker_nets(0.6,4) ## 
#checker_nets(0.8,5) ## 
#checker_nets(1,6) ## all survive

######
## Probabilities of combined interventions
# derived ITN/IRS quantities
# prob bites and survives
#
PHI_B = 0.75
PHI_I = 0.85
w=yy=z=array(dim=c(365,3))
w[,1] = 1 - PHI_B + PHI_B*s_ITN		#			{probability of surviving biting given that there is ITN}
w[,2] = 1 - PHI_I + PHI_I*(1-r_IRS)*s_IRS		#		{probability of surviving biting given that there is IRS}
w[,3] = 1 - PHI_I + PHI_B*(1-r_IRS)*s_ITN*s_IRS + (PHI_I - PHI_B)*(1-r_IRS)*s_IRS #{probability of surviving biting given that there is ITN & IRS}

# prob any bite (if there is IRS, a mosquito may bite and then die immediately afterwards)
#
yy[,1] = w[1]
yy[,2] = 1 - PHI_I + PHI_I*(1-r_IRS)
yy[,3] = 1 - PHI_I + PHI_B*(1-r_IRS)*s_ITN + (PHI_I - PHI_B)*(1-r_IRS)

# prob repelled
#
z[,1] = PHI_B*r_ITN
z[,2] = PHI_I*r_IRS
z[,3] = PHI_B*(r_IRS+ (1-r_IRS)*r_ITN) + (PHI_I - PHI_B)*r_IRS

#par(mfrow=c(2,3))
plot(w[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq survives biting") #{probability of mosquito surviving biting}
cols = c("blue","grey","black")
for(i in 1:3){
  lines(w[,i]~time,col=cols[i],lty=2)  
}
plot(yy[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq dies immediately after biting",
       main="no resistance") #{probability of mosquito biting then immediately dying}
cols = c("blue","grey","black")
for(i in 1:3){
  lines(yy[,i]~time,col=cols[i],lty=2)  
}
plot(z[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq is repelled") #{# prob repelled}
cols = c("blue","grey","black")
for(i in 1:3){
  lines(z[,i]~time,col=cols[i],lty=2)  
}


########################################################
##
## ORIGINAL CODE PRIOR TO INTRO OF RESISTANCE
#;*************************************************************************************************
#  ;************************************************************************************************* 
#  {entomolgical parameters} 
Q0=0.92#  		; anthropophagy
chi	=0.86	# 	; endophily 	
PHI_B	=0.89 #	; endophagy in bed
PHI_I	=0.97 #	; endophagy indoors

fv0=0.33333 #
tau1	 = 0.69#				; duration of host seeking, assumed to be constant between species #
tau2 	 = 1/fv0-tau1#		
av0=Q0*fv0#
mu0	= 0.132#				; daily hazard of death from external causes 
p10	= exp(-mu0*tau1) #		; probability of surviving one feeding cycle 
p2	= exp(-mu0*tau2)	#	; probability of surviving one resting cycle 

#;************************************************************************************************* 
#{Intervention parameters, ITNs, IRS------------------------------------------------------------------------} 
irs_half_life=0.5*time
itn_half_life=2.64*time
irs_loss = log(2)/irs_half_life 
itn_loss = log(2)/itn_half_life
IRS_interval=1*time
ITN_interval=3*time

ITN_decay = exp(-itn_loss) 
IRS_decay = exp(-irs_loss)

#{ITN parameters} 
d_ITN0=0.41 #		{probability of dying with an encounter with ITN (max)}
r_ITN0=0.56	#		{probability of repeating behaviour (max)}
r_ITN_min=0.24 
d_ITN = d_ITN0*ITN_decay 		#	; insecticide mortality rate 
r_ITN = r_ITN_min + (r_ITN0- r_ITN_min)*ITN_decay 
s_ITN = 1 - d_ITN - r_ITN		#	; successful protected human biting 

#{IRS parameters}
r_IRS0=0.6
d_IRS0=1
r_IRS = r_IRS0*IRS_decay		#	; cycle repeating rate 
d_IRS = chi*d_IRS0*IRS_decay	#	; insecticide mortality rate 
s_IRS = 1 - d_IRS				#; successful protected human biting 

w=yy=z=array(dim=c(365,3))
w[,1] = 1 - PHI_B + PHI_B*s_ITN		#			{probability of surviving biting given that there is ITN}
w[,2] = 1 - PHI_I + PHI_I*(1-r_IRS)*s_IRS		#		{probability of surviving biting given that there is IRS}
w[,3] = 1 - PHI_I + PHI_B*(1-r_IRS)*s_ITN*s_IRS + (PHI_I - PHI_B)*(1-r_IRS)*s_IRS #{probability of surviving biting given that there is ITN & IRS}

# prob any bite (if there is IRS, a mosquito may bite and then die immediately afterwards)
#
yy[,1] = w[1]
yy[,2] = 1 - PHI_I + PHI_I*(1-r_IRS)
yy[,3] = 1 - PHI_I + PHI_B*(1-r_IRS)*s_ITN + (PHI_I - PHI_B)*(1-r_IRS)

# prob repelled
#
z[,1] = PHI_B*r_ITN
z[,2] = PHI_I*r_IRS
z[,3] = PHI_B*(r_IRS+ (1-r_IRS)*r_ITN) + (PHI_I - PHI_B)*r_IRS

#par(mfrow=c(2,3))
plot(w[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq survives biting") #{probability of mosquito surviving biting}
cols = c("blue","grey","black")
for(i in 1:3){
  lines(w[,i]~time,col=cols[i])  
}
plot(yy[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq dies immediately after biting",
     main="Griffin original: no resistance") #{probability of mosquito biting then immediately dying}
cols = c("blue","grey","black")
for(i in 1:3){
  lines(yy[,i]~time,col=cols[i])  
}
plot(z[,1] ~ time,ylim=c(0,1),pch="",
     ylab="Probability mosq is repelled") #{# prob repelled}
cols = c("blue","grey","black")
lwds = c(1,1,2)
for(i in 1:3){
  lines(z[,i]~time,col=cols[i],lwd=lwds)  
}

 
zhi=0.8*z
zh = sum(zhi[1,])
whi=0.8*w
wh = sum(whi[1,])
zbar=Q0*zh
wbar=1-Q0+Q0*wh

#;**************************************************************************************************************
#  ; death rate mu, anthropophagy Q and biting rates with ITNs/IRS
fv = 1/( tau1/(1-zbar) + tau2 )
p1=wbar*p10/(1-zbar*p10)
mu = -fv*log(p1*p2)
Q=1-(1-Q0)/wbar
av=fv*Q
av_mosq = av*w[i]/wh
av_human = av*yy[i]/wh

