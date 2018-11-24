
total_M1 = c(0.5,3,15)
covITN =  seq(0, 1, by = 0.1)
covIRS =  seq(0, 1, by = 0.1)
resistance = seq(0, 1, by = 0.2)
time = 1:35


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
        
        output_prev_net1[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_1\\draw_0\\systematic_runs_maximum_1_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_2[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_2\\draw_0\\systematic_runs_maximum_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_3[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_3\\draw_0\\systematic_runs_maximum_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_4[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_4\\draw_0\\systematic_runs_maximum_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_5[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_5\\draw_0\\systematic_runs_maximum_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_6[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_6\\draw_0\\systematic_runs_maximum_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_7[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_7\\draw_0\\systematic_runs_maximum_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_8[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_8\\draw_0\\systematic_runs_maximum_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_9[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_9\\draw_0\\systematic_runs_maximum_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
        
        output_prev_net1_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
        output_prev_net2_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
        output_prev_net3_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
        
        output_case_net1_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
        output_case_net2_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
        output_case_net3_10[,j,k,m,r] = read.table(paste0("E:\\High Q0 High Phi Mean\\systematic_runs_maximums\\systematic_runs_maximum_10\\draw_0\\systematic_runs_maximum_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
      }
    }
  }
}



saveRDS(output_case_net1, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_1max.Rds")
saveRDS(output_case_net2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_1max.Rds")
saveRDS(output_case_net3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_1max.Rds")

saveRDS(output_case_net1_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_2max.Rds")
saveRDS(output_case_net2_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_2max.Rds")
saveRDS(output_case_net3_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_2max.Rds")

saveRDS(output_case_net1_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_3max.Rds")
saveRDS(output_case_net2_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_3max.Rds")
saveRDS(output_case_net3_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_3max.Rds")

saveRDS(output_case_net1_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_4max.Rds")
saveRDS(output_case_net2_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_4max.Rds")
saveRDS(output_case_net3_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_4max.Rds")

saveRDS(output_case_net1_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_5max.Rds")
saveRDS(output_case_net2_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_5max.Rds")
saveRDS(output_case_net3_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_5max.Rds")

saveRDS(output_case_net1_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_6max.Rds")
saveRDS(output_case_net2_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_6max.Rds")
saveRDS(output_case_net3_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_6max.Rds")

saveRDS(output_case_net1_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_7max.Rds")
saveRDS(output_case_net2_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_7max.Rds")
saveRDS(output_case_net3_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_7max.Rds")

saveRDS(output_case_net1_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_8max.Rds")
saveRDS(output_case_net2_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_8max.Rds")
saveRDS(output_case_net3_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_8max.Rds")

saveRDS(output_case_net1_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_9max.Rds")
saveRDS(output_case_net2_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_9max.Rds")
saveRDS(output_case_net3_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_9max.Rds")

saveRDS(output_case_net1_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_10max.Rds")
saveRDS(output_case_net2_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_10max.Rds")
saveRDS(output_case_net3_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_10max.Rds")

#######################Save to USB
saveRDS(output_case_net1, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_1max.Rds")
saveRDS(output_case_net2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_1max.Rds")
saveRDS(output_case_net3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_1max.Rds")

saveRDS(output_case_net1_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_2max.Rds")
saveRDS(output_case_net2_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_2max.Rds")
saveRDS(output_case_net3_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_2max.Rds")

saveRDS(output_case_net1_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_3max.Rds")
saveRDS(output_case_net2_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_3max.Rds")
saveRDS(output_case_net3_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_3max.Rds")

saveRDS(output_case_net1_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_4max.Rds")
saveRDS(output_case_net2_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_4max.Rds")
saveRDS(output_case_net3_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_4max.Rds")

saveRDS(output_case_net1_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_5max.Rds")
saveRDS(output_case_net2_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_5max.Rds")
saveRDS(output_case_net3_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_5max.Rds")

saveRDS(output_case_net1_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_6max.Rds")
saveRDS(output_case_net2_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_6max.Rds")
saveRDS(output_case_net3_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_6max.Rds")

saveRDS(output_case_net1_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_7max.Rds")
saveRDS(output_case_net2_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_7max.Rds")
saveRDS(output_case_net3_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_7max.Rds")

saveRDS(output_case_net1_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_8max.Rds")
saveRDS(output_case_net2_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_8max.Rds")
saveRDS(output_case_net3_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_8max.Rds")

saveRDS(output_case_net1_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_9max.Rds")
saveRDS(output_case_net2_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_9max.Rds")
saveRDS(output_case_net3_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_9max.Rds")

saveRDS(output_case_net1_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_10max.Rds")
saveRDS(output_case_net2_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_10max.Rds")
saveRDS(output_case_net3_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_10max.Rds")