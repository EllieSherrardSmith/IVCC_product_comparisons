########################################################
##
##
##
## AIM 1: re-create the clinical cases estimates from Pete

## Read in the raw data

# Load packages
library(tidyverse)

## Populations for Admin units over time
popns = read.csv("F:\\populations2.csv")
pop = popns$X2017

## Subset the data by just subsaharan countries
data_resource = read.csv("F:\\Copy of Intervention_coverage.csv")
vec_count = c(levels(data_resource$ISO[data_resource$CONTINENT == "Africa"]))
vec_count_val = c(data_resource$DIDE_CODE[data_resource$CONTINENT == "Africa"])

count_names = as.factor(data_resource$ISO[data_resource$CONTINENT == "Africa"])

## Population adjustments
adj_pop_temp = read.csv("H:\\Ellie\\IVCC_Resistance_Runs\\PopData_renormalised_2015.csv",header=TRUE)
adj_pop = subset(adj_pop_temp,adj_pop_temp$Year > 1999 & adj_pop_temp$Year < 2035)
#adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[1]]

N_admins = numeric(length(vec_count))
for(i in 1:length(vec_count)) N_admins[i] = length(data_resource$DIDE_CODE[data_resource$CONTINENT == "Africa" & data_resource$ISO == vec_count[i]])

years = 2000:2033

popul_2000_2035 = array(dim=c(34,601))
DIDE_CODE = popns$DIDE_CODE[popns$DIDE_CODE > 48 & popns$DIDE_CODE < 861]

j=1

##this is the popn in 2015, 
aa = (popns$X2017[popns$DIDE_CODE == DIDE_CODE[j]]/adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[j]][18])
##need to create a vector by multiplying the  renorm by this
aa * adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count[j]]
## and store this in an array

##Create estimates for each country for each variable required 
vec_count2 = c(rep(vec_count[1],N_admins[1]),
               rep(vec_count[2],N_admins[2]),
               rep(vec_count[3],N_admins[3]),
               rep(vec_count[4],N_admins[4]),
               rep(vec_count[5],N_admins[5]),
               rep(vec_count[6],N_admins[6]),
               rep(vec_count[7],N_admins[7]),
               rep(vec_count[8],N_admins[8]),
               rep(vec_count[9],N_admins[9]),
               rep(vec_count[10],N_admins[10]),
               rep(vec_count[11],N_admins[11]),
               rep(vec_count[12],N_admins[12]),
               rep(vec_count[13],N_admins[13]),
               rep(vec_count[14],N_admins[14]),
               rep(vec_count[15],N_admins[15]),
               rep(vec_count[16],N_admins[16]),
               rep(vec_count[17],N_admins[17]),
               rep(vec_count[18],N_admins[18]),
               rep(vec_count[19],N_admins[19]),
               rep(vec_count[20],N_admins[20]),
               rep(vec_count[21],N_admins[21]),
               rep(vec_count[22],N_admins[22]),
               rep(vec_count[23],N_admins[23]),
               rep(vec_count[24],N_admins[24]),
               rep(vec_count[25],N_admins[25]),
               rep(vec_count[26],N_admins[26]),
               rep(vec_count[27],N_admins[27]),
               rep(vec_count[28],N_admins[28]),
               rep(vec_count[29],N_admins[29]),
               rep(vec_count[30],N_admins[30]),
               rep(vec_count[31],N_admins[31]),
               rep(vec_count[32],N_admins[32]),
               rep(vec_count[33],N_admins[33]),
               rep(vec_count[34],N_admins[34]),
               rep(vec_count[35],N_admins[35]),
               rep(vec_count[36],N_admins[36]),
               rep(vec_count[37],N_admins[37]),
               rep(vec_count[38],N_admins[38]),
               rep(vec_count[39],N_admins[39]),
               rep(vec_count[40],N_admins[40]),
               rep(vec_count[41],N_admins[41]),
               rep(vec_count[42],N_admins[42]),
               rep(vec_count[43],N_admins[43]))



##Confirm that ISO are in the same order in the popn file and the norm file
for(j in 1:601){
  for(i in 1:34){
    popul_2000_2035[i,j] = (popns$X2017[popns$DIDE_CODE == DIDE_CODE[j]]/
                              adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count2[j]][18]) * 
      adj_pop$Scale_Factor_renormalised[adj_pop$ISO == vec_count2[j]][i]  ##2017 populations (dont have 2015 so go with these...)
    }
  
}

#CHECK ALL MATCHES UP SO FAR  
checks = data.frame(DIDE_CODE,vec_count2,popul_2000_2035[18,])
write.csv(checks,"H:\\Ellie\\IVCC_Resistance_Runs\\checker1.csv")


## Now we have the population estimates for each admin unit 1 across the time period for the model runs

# we just need the median parameter draw (draw = 0)
no_res_raw <- readRDS("H:\\Ellie\\IVCC_Resistance_Runs\\Run_output\\No_resistance_IVCC_raw.rds") %>% filter(Var2 == 0)
res_raw <- readRDS("H:\\Ellie\\IVCC_Resistance_Runs\\Run_output\\Resistance_IVCC_raw.rds")%>% filter(Var2 == 0)

head(no_res_raw)
head(res_raw)

##Create arrays for each admin unit for the required info
Africa_only_no_res_clin_inc_all_smooth = Africa_only_res_clin_inc_all_smooth = 
  Africa_only_no_res_sev_inc_all_smooth = Africa_only_res_sev_inc_all_smooth = 
  Africa_only_no_res_sev_inc_u5 = Africa_only_res_sev_inc_u5 = 
  Africa_only_no_res_clin_inc_0_5_smooth = Africa_only_res_clin_inc_0_5_smooth = 
  Africa_only_no_res_prop_age_u5_smooth = Africa_only_res_prop_age_u5_smooth = array(dim=c(409,length(vec_count_val)+1))

Time = rep(seq(1997,2031,length=409))
which(Time == 2000)
length(Time[38:408])

##CHANGE IN INCIDENCE IF 50% RESISTANCE EMERGES OVER NIGHT IN 2017
Africa_only_no_res_sev_inc_u5[,1] = Time
Africa_only_res_sev_inc_u5[,1] = Time
Africa_only_no_res_clin_inc_0_5_smooth[,1] = Time
Africa_only_res_clin_inc_0_5_smooth[,1] = Time
Africa_only_no_res_sev_inc_all_smooth[,1] = Time
Africa_only_res_sev_inc_all_smooth[,1] = Time
Africa_only_no_res_clin_inc_all_smooth[,1] = Time
Africa_only_res_clin_inc_all_smooth[,1] = Time
Africa_only_no_res_prop_age_u5_smooth[,1] = Time
Africa_only_res_prop_age_u5_smooth[,1] = Time

for(i in 1:length(vec_count_val)){
  Africa_only_no_res_sev_inc_u5[,i+1] = no_res_raw$sev_inc_u5[no_res_raw$Var1 == vec_count_val[i]]
  Africa_only_res_sev_inc_u5[,i+1] = res_raw$sev_inc_u5[res_raw$Var1 == vec_count_val[i]]
  
  Africa_only_no_res_clin_inc_0_5_smooth[,i+1] = no_res_raw$clin_inc_0_5_smooth[no_res_raw$Var1 == vec_count_val[i]]
  Africa_only_res_clin_inc_0_5_smooth[,i+1] = res_raw$clin_inc_0_5_smooth[res_raw$Var1 == vec_count_val[i]]
  
  Africa_only_no_res_sev_inc_all_smooth[,i+1] = no_res_raw$sev_inc_all_smooth[no_res_raw$Var1 == vec_count_val[i]]
  Africa_only_res_sev_inc_all_smooth[,i+1] = res_raw$sev_inc_all_smooth[res_raw$Var1 == vec_count_val[i]]
  
  Africa_only_no_res_clin_inc_all_smooth[,i+1] = no_res_raw$clin_inc_all_smooth[no_res_raw$Var1 == vec_count_val[i]]
  Africa_only_res_clin_inc_all_smooth[,i+1] = res_raw$clin_inc_all_smooth[res_raw$Var1 == vec_count_val[i]]
  
  Africa_only_no_res_prop_age_u5_smooth[,i+1] = no_res_raw$clin_inc_all_smooth[no_res_raw$Var1 == vec_count_val[i]]
  Africa_only_res_prop_age_u5_smooth[,i+1] = res_raw$clin_inc_all_smooth[res_raw$Var1 == vec_count_val[i]]
  
}

## The number of U5s in the populations need to be grouped by years
times = unique(res_raw$year)
st_times = seq(1,408,by=12)
en_times = c(st_times[2:length(st_times)]-1,408)

Severe_inc_U5_no_res = Clin_inc_0_5_smooth_no_res = 
  Severe_inc_all_no_res = Clin_inc_all_smooth_no_res = 
  prop_age_u5_smooth_no_res = 
  Severe_inc_U5_res = Clin_inc_0_5_smooth_res = 
  Severe_inc_all_res = Clin_inc_all_smooth_res = 
  prop_age_u5_smooth_res = array(dim=c(34,length(vec_count_val)))#
for(j in 1:length(vec_count_val)){
  for(i in 1:34){ 
    Severe_inc_U5_no_res[i,j] = weighted.mean(c(Africa_only_no_res_sev_inc_u5[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Clin_inc_0_5_smooth_no_res[i,j] = weighted.mean(c(Africa_only_no_res_clin_inc_0_5_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Severe_inc_all_no_res[i,j] = weighted.mean(c(Africa_only_no_res_sev_inc_all_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Clin_inc_all_smooth_no_res[i,j] = weighted.mean(c(Africa_only_no_res_clin_inc_all_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    prop_age_u5_smooth_no_res[i,j] = mean(c(Africa_only_no_res_prop_age_u5_smooth[st_times[i]:en_times[i],j+1]))
    
    Severe_inc_U5_res[i,j] = weighted.mean(c(Africa_only_res_sev_inc_u5[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Clin_inc_0_5_smooth_res[i,j] = weighted.mean(c(Africa_only_res_clin_inc_0_5_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Severe_inc_all_res[i,j] = weighted.mean(c(Africa_only_res_sev_inc_all_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    Clin_inc_all_smooth_res[i,j] = weighted.mean(c(Africa_only_res_clin_inc_all_smooth[st_times[i]:en_times[i],j+1]),rep(popul_2000_2035[i,j],each=12))
    prop_age_u5_smooth_res[i,j] = mean(c(Africa_only_res_prop_age_u5_smooth[st_times[i]:en_times[i],j+1]))
    
  }
  
}

##Checks
##Now to estimate the number of Under 5s in the populations
Number_under5s_no_res = prop_age_u5_smooth_no_res * popul_2000_2035#
Number_under5s_res = prop_age_u5_smooth_res * popul_2000_2035

## Severe cases for Under 5s
Severe_cases_under5_no_res = Number_under5s_no_res * Severe_inc_U5_no_res
Severe_cases_under5_res = Number_under5s_res * Severe_inc_U5_res
SEVERE_CASES_AVERTED_UNDER5 = Severe_cases_under5_res - Severe_cases_under5_no_res

##Clinical cases Under 5s
Clinical_cases_under5_no_res = Clin_inc_0_5_smooth_no_res * Number_under5s_no_res
Clinical_cases_under5_res = Clin_inc_0_5_smooth_res * Number_under5s_res
CLINICAL_CASES_AVERTED_UNDER5 = Clinical_cases_under5_res - Clinical_cases_under5_no_res


##Now to estimate the number of Over 5s in the populations
Number_over5s_no_res = (1 - prop_age_u5_smooth_no_res) * popul_2000_2035#
Number_over5s_res = (1 - prop_age_u5_smooth_res) * popul_2000_2035

## Severe cases for All
Severe_cases_all_no_res = popul_2000_2035 * Severe_inc_all_no_res
Severe_cases_all_res = popul_2000_2035 * Severe_inc_all_res
SEVERE_CASES_AVERTED_ALL = Severe_cases_all_res - Severe_cases_all_no_res

##Clinical cases All
Clinical_cases_all_no_res = Clin_inc_0_5_smooth_no_res * popul_2000_2035
Clinical_cases_all_res = Clin_inc_0_5_smooth_res * popul_2000_2035
CLINICAL_CASES_AVERTED_ALL = Clinical_cases_all_res - Clinical_cases_all_no_res

##checks
plot(Clinical_cases_all_no_res[,1])
lines(Clinical_cases_all_res[,1],col="red")



##Finally, we need to take the weighted means for the places to work out each country burden
##Create a vector that is the length of the number of admin units in each country
N_admins = numeric(length(vec_count))
for(i in 1:length(vec_count)) N_admins[i] = length(data_resource$DIDE_CODE[data_resource$CONTINENT == "Africa" & data_resource$ISO == vec_count[i]])

tester = numeric(43)
vecs = 1:43
for(i in 1:length(vec_count)) tester[i] = sum(N_admins[1:vecs[i]])+1
starts = c(1,tester[1:42])
ends = c(tester)-1


##Create estimates for each country for each variable required 

Severe_inc_U5_no_res2 = Clin_inc_0_5_smooth_no_res2 = 
  Severe_inc_all_no_res2 = Clin_inc_all_smooth_no_res2 = 
  prop_age_u5_smooth_no_res2 = 
  Severe_inc_U5_res2 = Clin_inc_0_5_smooth_res2 = 
  Severe_inc_all_res2 = Clin_inc_all_smooth_res2 = 
  prop_age_u5_smooth_res2 = 
  populations_by_country = 
  Clinical_cases_all_no_res2 = Clinical_cases_all_res2 = 
  Clinical_cases_under5_no_res2 = Clinical_cases_under5_res2 = 
  Severe_cases_all_no_res2 = Severe_cases_all_res2 = 
  Severe_cases_under5_no_res2 = Severe_cases_under5_res2 = array(dim=c(34,43))#
for(j in 1:43){
  for(i in 1:34){ 
    Clinical_cases_all_no_res2[i,j] = sum(Clinical_cases_all_no_res[i,starts[j]:ends[j]])
    Clinical_cases_all_res2[i,j] = sum(Clinical_cases_all_res[i,starts[j]:ends[j]])
    Clinical_cases_under5_no_res2[i,j] = sum(Clinical_cases_under5_no_res[i,starts[j]:ends[j]])
    Clinical_cases_under5_res2[i,j] = sum(Clinical_cases_under5_res[i,starts[j]:ends[j]])
    
    Severe_cases_all_no_res2[i,j] = sum(Severe_cases_all_no_res[i,starts[j]:ends[j]])
    Severe_cases_all_res2[i,j] = sum(Severe_cases_all_res[i,starts[j]:ends[j]])
    Severe_cases_under5_no_res2[i,j] = sum(Severe_cases_under5_no_res[i,starts[j]:ends[j]])
    Severe_cases_under5_res2[i,j] = sum(Severe_cases_under5_res[i,starts[j]:ends[j]])
    
    populations_by_country[i,j] = mean(popul_2000_2035[i,starts[j]:ends[j]])
  }
  
}
##And work out the estimtes by country and year
##Now to estimate the number of Under 5s in the populations
Number_under5s_no_res2 = prop_age_u5_smooth_no_res2 * populations_by_country#
Number_under5s_res2 = prop_age_u5_smooth_res2 * populations_by_country

## Severe cases for Under 5s
Severe_cases_under5_no_res2 = Number_under5s_no_res2 * Severe_inc_U5_no_res2
Severe_cases_under5_res2 = Number_under5s_res2 * Severe_inc_U5_res2
SEVERE_CASES_AVERTED_UNDER5 = Severe_cases_under5_res2 - Severe_cases_under5_no_res2

##Clinical cases Under 5s
Clinical_cases_under5_no_res2 = Clin_inc_0_5_smooth_no_res2 * Number_under5s_no_res2
Clinical_cases_under5_res2 = Clin_inc_0_5_smooth_res2 * Number_under5s_res2
CLINICAL_CASES_AVERTED_UNDER5 = Clinical_cases_under5_res2 - Clinical_cases_under5_no_res2


##Now to estimate the number of Over 5s in the populations
Number_over5s_no_res2 = (1 - prop_age_u5_smooth_no_res2) * populations_by_country#
Number_over5s_res2 = (1 - prop_age_u5_smooth_res2) * populations_by_country

## Severe cases for All
Severe_cases_all_no_res2 = populations_by_country * Severe_inc_all_no_res2
Severe_cases_all_res2 = populations_by_country * Severe_inc_all_res2
SEVERE_CASES_AVERTED_ALL = Severe_cases_all_res2 - Severe_cases_all_no_res2

##Clinical cases All
Clinical_cases_all_no_res2 = Clin_inc_0_5_smooth_no_res2 * populations_by_country
Clinical_cases_all_res2 = Clin_inc_0_5_smooth_res2 * populations_by_country
CLINICAL_CASES_AVERTED_ALL = Clinical_cases_all_res2 - Clinical_cases_all_no_res2

##checks
plot(Clinical_cases_all_no_res2[,1])
lines(Clinical_cases_all_res2[,1],col="red")

colnames(Clinical_cases_all_no_res2)  = c(vec_count)
colnames(Clinical_cases_all_res2)  = c(vec_count)
colnames(CLINICAL_CASES_AVERTED_ALL)  = c(vec_count)

CLINICAL_CASES_AVERTED_ALL[20:26,]

Clinical_cases_all_no_res2[20:25,]/144
no_res_per_yr2

correction_ratio =(Clinical_cases_all_no_res2[20:25,]/12) / no_res_per_yr2
