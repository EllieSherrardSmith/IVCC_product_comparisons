library(adegenet)
library(MalariaLaunchR)
library(adegenet)

#################################################
##
## Once everything is fitted

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
                    
                    'add irs_ellie 1')
  Options<-paste(Int_set_up)
  
  ## Run the simulation
  Model_launcher(OutputName = paste(Run_name, site, draw, sep='_') ,
                 OutputRoot = paste0("p:/Ellies_output_folder/", Run_name, '/draw_', draw, '/'),
                 Options=Options,
                 Exe = "p:/Ellies_cool_model_folder2/bin/Indiv_MalariaLaunchR_2.0.exe",
                 Root="p:/Ellies_cool_model_folder2/model_files",
                 Site=paste0('Africa_Sites_Ellie_eip/G2RCToptions/Site_G2RCT_', site, '.txt'),
                 Parameter_draw = draw,
                 Return_output = FALSE)
}


##Read in the csv with the appropriate data
inp <- read.csv("p:/G2_interceptor_RCT_site_options_input_MODEL1.csv",header=TRUE)
names(inp)

for(i in 9){
  Run_seas(site = inp[i,1],
           ITN = inp[i,2], 
           
           itn_repel_fun_1 = inp[i,4], itn_repel_gamb_ss_1 = inp[i,5], itn_repel_arab_1 = inp[i,6],
           itn_kill_fun_1 = inp[i,7], itn_kill_gamb_ss_1 = inp[i,8], itn_kill_arab_1 = inp[i,9],
           itn_halflife_1 = inp[i,10],
           
           itn_repel_fun_2 = inp[i,11], itn_repel_gamb_ss_2 = inp[i,12], itn_repel_arab_2 = inp[i,13],
           itn_kill_fun_2 = inp[i,14], itn_kill_gamb_ss_2 = inp[i,15], itn_kill_arab_2 = inp[i,16],
           itn_halflife_2 = inp[i,17],
           
           run_name="G2_Locations")
  
}



inp2 <- read.csv("p:/G2_interceptor_RCT_site_options_input_noChangeNetType.csv",header=TRUE)
for(i in 9){
  Run_seas(site = inp2[i,1],
           ITN = inp2[i,2], 
           
           itn_repel_fun_1 = inp2[i,4], itn_repel_gamb_ss_1 = inp2[i,5], itn_repel_arab_1 = inp2[i,6],
           itn_kill_fun_1 = inp2[i,7], itn_kill_gamb_ss_1 = inp2[i,8], itn_kill_arab_1 = inp2[i,9],
           itn_halflife_1 = inp2[i,10],
           
           itn_repel_fun_2 = inp2[i,11], itn_repel_gamb_ss_2 = inp2[i,12], itn_repel_arab_2 = inp2[i,13],
           itn_kill_fun_2 = inp2[i,14], itn_kill_gamb_ss_2 = inp2[i,15], itn_kill_arab_2 = inp2[i,16],
           itn_halflife_2 = inp2[i,17],
           
           run_name="G2_Locations_nochange")
  
}

loc1 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_1_0.txt",header = TRUE)
loc2 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_2_0.txt",header = TRUE)
loc3 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_3_0.txt",header = TRUE)
loc4 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_4_0.txt",header = TRUE)
loc5 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_5_0.txt",header = TRUE)
loc6 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_6_0.txt",header = TRUE)
loc7 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_7_0.txt",header = TRUE)
loc8 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_8_0.txt",header = TRUE)
loc9 =  read.table("F:\\Ellies_output_folder\\G2_Locations\\draw_0\\G2_Locations_9_0.txt",header = TRUE)
loc1n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_1_0.txt",header = TRUE)
loc2n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_2_0.txt",header = TRUE)
loc3n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_3_0.txt",header = TRUE)
loc4n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_4_0.txt",header = TRUE)
loc5n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_5_0.txt",header = TRUE)
loc6n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_6_0.txt",header = TRUE)
loc7n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_7_0.txt",header = TRUE)
loc8n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_8_0.txt",header = TRUE)
loc9n = read.table("F:\\Ellies_output_folder\\G2_Locations_nochange\\draw_0\\G2_Locations_nochange_9_0.txt",header = TRUE)

par(mar=c(5,6,2,2))
par(mfrow=c(2,2))
plots_fun = function(data1, data2,outcome,cols,location,measure){
  plot(data1[,measure] ~ data1$year,pch="",ylim=c(0,ifelse(measure == 2,1,3)),
       xlim=c(-3,6),ylab=outcome,yaxt="n",
       xlab="Time (years)",cex.lab=1.6,cex.axis=1.6,xaxt="n")
  axis(1,at=c(-3,0,3,6,9,12),cex.lab=1.6,cex.axis=1.6)
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
  
  if(measure == 2){
    legend(-3,0.4,
           title = "Net type",bty="n",
           legend=c("Standard net","Interceptor G2"),
           lty=c(2,1),cex=1.6)
  } else {}
  
  text(0,ifelse(measure == 2,1,3),location,cex=1.5)
}
plots_fun(loc1,loc1n,"Prevalence in 2 to 10 year olds",
          "blue","Burkina Faso: (Diebougou)",2)
plots_fun(loc1,loc1n,"Clinical cases (per person per year)",
          "blue","Burkina Faso: (Diebougou)",10)
plots_fun(loc2,loc2n,"Prevalence in 2 to 10 year olds",
          "blue","Burkina Faso: (Danu)",2)
plots_fun(loc2,loc2n,"Clinical cases (per person per year)",
          "blue","Burkina Faso: (Danu)",10)

plots_fun(loc3,loc3n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Segou (Bareouli)",2)
plots_fun(loc3,loc3n,"Clinical cases (per person per year)",
          "blue","Mali: Segou (Bareouli)",10)
plots_fun(loc4,loc4n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Segou (Bla)",2)
plots_fun(loc4,loc4n,"Clinical cases (per person per year)",
          "blue","Mali: Segou (Bla)",10)

plots_fun(loc5,loc5n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Koulikoro (Kati)",2)
plots_fun(loc5,loc5n,"Clinical cases (per person per year)",
          "blue","Mali: Koulikoro (Kati)",10)
plots_fun(loc6,loc6n,"Prevalence in 2 to 10 year olds",
          "blue","Mali: Koulikoro (Koulikoro)",2)
plots_fun(loc6,loc6n,"Clinical cases (per person per year)",
          "blue","Mali: Koulikoro (Koulikoro)",10)

plots_fun(loc8,loc8n,"Prevalence in 2 to 10 year olds",
          "blue","Malawi: Southern region (Phalombe)",2)
plots_fun(loc8,loc8n,"Clinical cases (per person per year)",
          "blue","Malawi: Southern region (Phalombe)",10)
plots_fun(loc7,loc7n,"Prevalence in 2 to 10 year olds",
          "blue","Malawi: Central region (Lilongwe)",2)
plots_fun(loc7,loc7n,"Clinical cases (per person per year)",
          "blue","Malawi: Central region (Lilongwe)",10)

calc_eff = function(data1,data2){
  effect = array(dim=c(6,2))
  rownames(effect) = c("Seasonal",
                       "smoothed",
                       "reduction_Max_time_pt",
                       "reduction_Min_time_pt",
                       "smoothed_reduction_Max_time_pt",
                       "smoothed_reduction_Min_time_pt")
  colnames(effect) = c("Prevalence 2 to 10",
                       "Clinical cases all")
  effect[1,1] = (data1$prev_2_10[data1$year == 1] -
    data2$prev_2_10[data2$year == 1]) / 
    data1$prev_2_10[data1$year == 1]
  effect[2,1] = (data1$prev_2_10_smooth[data1$year == 1] -
                 data2$prev_2_10_smooth[data2$year == 1]) / 
    data1$prev_2_10[data1$year == 1]
  effect[3,1] = (data1$prev_2_10[data1$year == 2.69231e+00] -
                   data2$prev_2_10[data2$year == 2.69231e+00]) / 
    data1$prev_2_10[data1$year == 2.69231e+00]
  effect[4,1] = (data1$prev_2_10[data1$year == 1.19231e+00] -
                   data2$prev_2_10[data2$year == 1.19231e+00]) / 
    data1$prev_2_10[data1$year == 1.19231e+00]
  effect[5,1] = (data1$prev_2_10_smooth[data1$year == 2.69231e+00] -
                   data2$prev_2_10_smooth[data2$year == 2.69231e+00]) / 
    data1$prev_2_10[data1$year == 2.69231e+00]
  effect[6,1] = (data1$prev_2_10_smooth[data1$year == 1.19231e+00] -
                   data2$prev_2_10_smooth[data2$year == 1.19231e+00]) / 
    data1$prev_2_10[data1$year == 1.19231e+00]
  
  effect[1,2] = (data1$clin_inc_all[data1$year == "1"] -
                 data2$clin_inc_all[data2$year == "1"]) / 
    data1$clin_inc_all_smooth[data1$year == "1"]
  effect[2,2] = (data1$clin_inc_all_smooth[data1$year == "1"] -
                 data2$clin_inc_all_smooth[data2$year == "1"]) / 
    data1$clin_inc_all_smooth[data1$year == "1"]
  effect[3,2] = (data1$clin_inc_all[data1$year == 2.69231e+00] -
                   data2$clin_inc_all[data2$year == 2.69231e+00]) / 
    data1$clin_inc_all_smooth[data1$year == 2.69231e+00]
  effect[4,2] = (data1$clin_inc_all[data1$year == 1.19231e+00] -
                   data2$clin_inc_all[data2$year == 1.19231e+00]) / 
    data1$clin_inc_all_smooth[data1$year == 1.19231e+00]
  effect[5,2] = (data1$clin_inc_all_smooth[data1$year == 2.69231e+00] -
                   data2$clin_inc_all_smooth[data2$year == 2.69231e+00]) / 
    data1$clin_inc_all_smooth[data1$year == 2.69231e+00]
  effect[6,2] = (data1$clin_inc_all_smooth[data1$year == 1.19231e+00] -
                   data2$clin_inc_all_smooth[data2$year == 1.19231e+00]) / 
    data1$clin_inc_all_smooth[data1$year == 1.19231e+00]
  
  return(effect)
}
calc_eff(loc1n,loc1)
calc_eff(loc2n,loc2)
calc_eff(loc3n,loc3)
calc_eff(loc4n,loc4)
calc_eff(loc5n,loc5)
calc_eff(loc6n,loc6)
calc_eff(loc7n,loc7)
calc_eff(loc8n,loc8)
calc_eff(loc9n,loc9)
