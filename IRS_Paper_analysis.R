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
  
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_0/draw_0/IRS_paper_switchingRES_0_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_10/draw_0/IRS_paper_switchingRES_10_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_20/draw_0/IRS_paper_switchingRES_20_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_30/draw_0/IRS_paper_switchingRES_30_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_40/draw_0/IRS_paper_switchingRES_40_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_50/draw_0/IRS_paper_switchingRES_50_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_60/draw_0/IRS_paper_switchingRES_60_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_70/draw_0/IRS_paper_switchingRES_70_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_80/draw_0/IRS_paper_switchingRES_80_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_90/draw_0/IRS_paper_switchingRES_90_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/IRS_paper_switchingRES_100/draw_0/IRS_paper_switchingRES_100_', site, '_0.txt'),header=TRUE)
  
  #data1 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_IRS_perennial/draw_0/3d_res_cov_switching_irs_', site, '_0.txt'),header=TRUE)
  #data1 = read.table(paste0('F:/Ellies_output_folder/3d_res_cov_switching_IRS_highly_seasonal/draw_0/3d_res_cov_switching_IRS_highly_seasonal_', site, '_0.txt'),header=TRUE)
  ##Number of Cases
                     pyr_noRes = data1$clin_inc_all[data1$year==3]
                     pyr_res   = data1$clin_inc_all[data1$year==6]
                     actellic  = data1$clin_inc_all[data1$year==9]
  
                     ##Number of Cases averted
                     pyr_noRes_avert = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==3]
                     pyr_res_avert   = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==6]
                     actellic_avert  = data1$clin_inc_all[data1$year==0] - data1$clin_inc_all[data1$year==9]
                     
                     ##return
                     return(cbind(pyr_noRes,pyr_res,actellic,
                                  pyr_noRes_avert,pyr_res_avert,actellic_avert,
                                  resistance_level,coverage))
                     
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

data_store = array(dim=c(length(vals),8))
colnames(data_store) = c("Pyrethroid_80%_no_resistance",
                         "Pyrethroids_with_res",
                         "Actellic",
                         "Pyrethroid_80%_no_resistance_Cases_averted",
                         "Pyrethroids_with_res_Cases_averted",
                         "Actellic_Cases_averted",
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
               z = matrix(nrow=21,ncol=21,data=data2$Pyrethroids_with_res),
               color = topo.colors,
               plot.title = title(main = "Cases per 1000 people per year: Pyrethroid IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05))},
               nlevels = 16,
               levels=c(0,0.00001,0.000025,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
                        0.0025,0.005,0.0075,0.01,0.025,0.05,0.075,0.1),
               key.title = title(main = ""))  # maybe also asp = 1

filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$Actellic),
               color = topo.colors,
               plot.title = title(main = "Cases per 1000 people per year: Actellic IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05))},
               nlevels = 16,
               levels=c(0,0.00001,0.000025,0.00005,0.0001,0.00025,0.0005,0.00075,0.001,
                        0.0025,0.005,0.0075,0.01,0.025,0.05,0.075,0.1),
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
               z = matrix(nrow=21,ncol=21,data=data2$Pyrethroids_with_res_Cases_averted),
               color = heat.colors,
               plot.title = title(main = "Cases per 1000 per year averted with Pyrethroid IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05)) },
               nlevels = 10,
               levels=c(0.43,0.45,0.47,0.48,seq(0.487,0.4875,0.0001),0.50),
               key.title = title(main = "", cex.main=0.8))  # maybe also asp = 1


filled.contour(x, 
               y, 
               z = matrix(nrow=21,ncol=21,data=data2$Actellic_Cases_averted),
               color = heat.colors,
               plot.title = title(main = "Cases per 1000 per year averted with Actellic IRS (80% cover)",
                                  xlab = "Level of resistance (0 = no resistant mosquitoes, 1 = all resistant mosquitoes)", 
                                  ylab = "LLIN coverage",cex.lab=1.5),
               plot.axes = { axis(1, seq(0, 1, by = 0.05))
                 axis(2, seq(0, 1, by = 0.05)) },
               nlevels = 10,
               levels=c(0.43,0.45,0.47,0.48,seq(0.487,0.4875,0.0001),0.50),
               key.title = title(main = "", cex.main=0.8))  # maybe also asp = 1



