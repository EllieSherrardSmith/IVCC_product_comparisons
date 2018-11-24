########################################
##
## Confirm all simulations have been run 
##
########################################
input_dat2 = read.csv("E:/run_set2_maximum.csv",header=TRUE)
input_dat1 = read.csv("E:/run_set2_minimum.csv",header=TRUE)

for(i in 1:nrow(input_dat1)){
  input_dat1$name[i] = paste("systematic_runs_minimum_5_5",input_dat1[i,1],input_dat1[i,2],input_dat1[i,3],input_dat1[i,17],input_dat1[i,18],"0",sep='_')
}

for(i in 1:6534){

  ff <- paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_5\\draw_0\\",input_dat1$name[i],".txt")
  if (file.exists(ff))
    input_dat1$PRESENT[i] <- "yes"
  else(input_dat1$PRESENT[i] <- "no")
  
}

which(input_dat1$name == "systematic_runs_minimum_4_4_15_0_0.4_0.8_3_0")

redo = c(which(input_dat1$PRESENT == "no"))
length(redo)
write.csv(redo,"P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_5\\redos.csv")


for(i in 1:nrow(input_dat2)){
  input_dat2$name[i] = paste("systematic_runs_maximum_4_4",input_dat2[i,1],input_dat2[i,2],input_dat2[i,3],input_dat2[i,17],input_dat2[i,18],"0",sep='_')
}

for(i in 1:6534){
  
  ff <- paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_maximum_4\\draw_0\\",input_dat2$name[i],".txt")
  if (file.exists(ff))
    input_dat2$PRESENT[i] <- "yes"
  else(input_dat2$PRESENT[i] <- "no")
  
}

redo = c(which(input_dat2$PRESENT == "no"))
length(redo)
write.csv(redo,"P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_maximum_4\\redos.csv")













total_M1 = c(0.5,3,15)
covITN =  seq(0, 1, by = 0.1)
covIRS =  seq(0, 1, by = 0.1)
resistance = seq(0, 1, by = 0.2)
time = 1:35
#Relative_Prevalence_difference = function(time,total_M1, covITN, covIRS, resistance, type_net){

## OutputName = paste(Run_name, site, total_M1 [m], covITN [j ], covIRS [k], resistance [r], type_net [i], draw, sep='_')

## site is 1 - 5 for seasonal with 1 = no historic intervention, 2 = 25%, 3 = 50%, 4 = 75% and 5 = 100% historic net use
## total_M1 varies from 0.5, 3, 15 for low (5%), medium (30%) or high (60%) transmission setting
## covITN varies from seq(0, 1, by = 0.1)
## covIRS varies from seq(0, 1, by = 0.1)
## resistance varies from seq(0, 1, by = 0.2)
## type_net indicates standard = 1; PBO = 2 and G2 = 3
#output_prev_check = array(dim=c(35,3)) ## This should be all possible outputs for seasonal site = 1 with year in col 1
#output_prev_check[,1] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_45\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_1_45.txt"),header=TRUE)$year
#output_prev_check[,2] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_45\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_1_45.txt"),header=TRUE)$clin_inc_all
#output_prev_check[,3] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_45\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_2_45.txt"),header=TRUE)$clin_inc_all

output_prev_check2 = array(dim=c(35,3)) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_check2[,1] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_1_0.txt"),header=TRUE)$year
output_prev_check2[,2] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_1_0.txt"),header=TRUE)$clin_inc_all
output_prev_check2[,3] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_0.5_0.5_0.1_1_2_0.txt"),header=TRUE)$clin_inc_all

output_prev_checkmn = array(dim=c(35,3)) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_checkmn[,1] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs\\draw_0\\systematic_runs_1_0.5_0.5_0.1_1_1_0.txt"),header=TRUE)$year
output_prev_checkmn[,2] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs\\draw_0\\systematic_runs_1_0.5_0.5_0.1_1_1_0.txt"),header=TRUE)$clin_inc_all
output_prev_checkmn[,3] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs\\draw_0\\systematic_runs_1_0.5_0.5_0.1_1_2_0.txt"),header=TRUE)$clin_inc_all

plot(output_prev_checkmn[,3] ~ output_prev_checkmn[,1],ylim=c(0,0.1))
lines(output_prev_checkmn[,3] ~ output_prev_checkmn[,1])
lines(output_prev_check[,3] ~ output_prev_check[,1],col="red")
lines(output_prev_check2[,3] ~ output_prev_check2[,1],col="red",lty=2)

output_prev_net1 = output_prev_net1_2 = output_prev_net1_3 = output_prev_net1_4 = output_prev_net1_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_net2 = output_prev_net2_2 = output_prev_net2_3 = output_prev_net2_4 = output_prev_net2_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_net3 = output_prev_net3_2 = output_prev_net3_3 = output_prev_net3_4 = output_prev_net3_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1

output_prev_net1_6 = output_prev_net1_7 = output_prev_net1_8 = output_prev_net1_9 = output_prev_net1_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_net2_6 = output_prev_net2_7 = output_prev_net2_8 = output_prev_net2_9 = output_prev_net2_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_prev_net3_6 = output_prev_net3_7 = output_prev_net3_8 = output_prev_net3_9 = output_prev_net3_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1

output_case_net1 = output_case_net1_2 = output_case_net1_3 = output_case_net1_4 = output_case_net1_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_case_net2 = output_case_net2_2 = output_case_net2_3 = output_case_net2_4 = output_case_net2_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_case_net3 = output_case_net3_2 = output_case_net3_3 = output_case_net3_4 = output_case_net3_5 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1

output_case_net1_6 = output_case_net1_7 = output_case_net1_8 = output_case_net1_9 = output_case_net1_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_case_net2_6 = output_case_net2_7 = output_case_net2_8 = output_case_net2_9 = output_case_net2_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1
output_case_net3_6 = output_case_net3_7 = output_case_net3_8 = output_case_net3_9 = output_case_net3_10 = array(dim=c(length(time),length(covITN),length(covIRS),length(total_M1),length(resistance))) ## This should be all possible outputs for seasonal site = 1 with year in col 1

for(j in 1:11){
  for(k in 1:11){
    for(m in 1:3){
      for(r in 1:6){
        
 #       output_prev_net1[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
    #    output_prev_net3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
     #   output_case_net1[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
       # output_case_net3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_1\\draw_0\\systematic_runs_minimum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_2\\draw_0\\systematic_runs_minimum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_3\\draw_0\\systematic_runs_minimum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_4\\draw_0\\systematic_runs_minimum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_5\\draw_0\\systematic_runs_minimum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_6\\draw_0\\systematic_runs_minimum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_7\\draw_0\\systematic_runs_minimum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_8\\draw_0\\systematic_runs_minimum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_minimums\\systematic_runs_minimum_9\\draw_0\\systematic_runs_minimum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
#        output_prev_net1_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
 #       output_prev_net2_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
  #      output_prev_net3_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
   #     
    #    output_case_net1_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
     #   output_case_net2_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
      #  output_case_net3_10[,j,k,m,r] = read.table(paste0("P:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_minimum_10\\draw_0\\systematic_runs_minimum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
      }
    }
  }
}



saveRDS(output_case_net1, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_1min.Rds")
saveRDS(output_case_net2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_1min.Rds")
saveRDS(output_case_net3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_1min.Rds")

saveRDS(output_case_net1_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_2min.Rds")
saveRDS(output_case_net2_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_2min.Rds")
saveRDS(output_case_net3_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_2min.Rds")

saveRDS(output_case_net1_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_3min.Rds")
saveRDS(output_case_net2_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_3min.Rds")
saveRDS(output_case_net3_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_3min.Rds")

saveRDS(output_case_net1_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_4min.Rds")
saveRDS(output_case_net2_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_4min.Rds")
saveRDS(output_case_net3_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_4min.Rds")

saveRDS(output_case_net1_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_5min.Rds")
saveRDS(output_case_net2_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_5min.Rds")
saveRDS(output_case_net3_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_5min.Rds")

saveRDS(output_case_net1_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_6min.Rds")
saveRDS(output_case_net2_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_6min.Rds")
saveRDS(output_case_net3_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_6min.Rds")

saveRDS(output_case_net1_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_7min.Rds")
saveRDS(output_case_net2_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_7min.Rds")
saveRDS(output_case_net3_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_7min.Rds")

saveRDS(output_case_net1_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_8min.Rds")
saveRDS(output_case_net2_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_8min.Rds")
saveRDS(output_case_net3_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_8min.Rds")

saveRDS(output_case_net1_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_9min.Rds")
saveRDS(output_case_net2_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_9min.Rds")
saveRDS(output_case_net3_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_9min.Rds")

saveRDS(output_case_net1_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_10min.Rds")
saveRDS(output_case_net2_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_10min.Rds")
saveRDS(output_case_net3_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_10min.Rds")

#######################Save to USB
saveRDS(output_case_net1, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_1min.Rds")
saveRDS(output_case_net2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_1min.Rds")
saveRDS(output_case_net3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_1min.Rds")

saveRDS(output_case_net1_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_2min.Rds")
saveRDS(output_case_net2_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_2min.Rds")
saveRDS(output_case_net3_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_2min.Rds")

saveRDS(output_case_net1_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_3min.Rds")
saveRDS(output_case_net2_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_3min.Rds")
saveRDS(output_case_net3_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_3min.Rds")

saveRDS(output_case_net1_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_4min.Rds")
saveRDS(output_case_net2_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_4min.Rds")
saveRDS(output_case_net3_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_4min.Rds")

saveRDS(output_case_net1_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_5min.Rds")
saveRDS(output_case_net2_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_5min.Rds")
saveRDS(output_case_net3_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_5min.Rds")

saveRDS(output_case_net1_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_6min.Rds")
saveRDS(output_case_net2_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_6min.Rds")
saveRDS(output_case_net3_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_6min.Rds")

saveRDS(output_case_net1_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_7min.Rds")
saveRDS(output_case_net2_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_7min.Rds")
saveRDS(output_case_net3_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_7min.Rds")

saveRDS(output_case_net1_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_8min.Rds")
saveRDS(output_case_net2_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_8min.Rds")
saveRDS(output_case_net3_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_8min.Rds")

saveRDS(output_case_net1_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_9min.Rds")
saveRDS(output_case_net2_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_9min.Rds")
saveRDS(output_case_net3_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_9min.Rds")

saveRDS(output_case_net1_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_10min.Rds")
saveRDS(output_case_net2_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_10min.Rds")
saveRDS(output_case_net3_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_10min.Rds")
