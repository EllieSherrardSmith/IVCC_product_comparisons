#######################################################################
##
##
## Matrix plots of the outputs

#######################################################
filled.contour3 <-
  function (x = seq(0, 1, length.out = nrow(z)),
            y = seq(0, 1, length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
            levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
            col = color.palette(length(levels) - 1), plot.title, plot.axes, 
            key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
            axes = TRUE, frame.plot = axes,mar, ...)   {
    # modification by Ian Taylor of the filled.contour function
    # to remove the key and facilitate overplotting with contour()
    # further modified by Carey McGilliard and Bridget Ferris
    # to allow multiple plots on one page
    
    if (missing(z)) {
      if (!missing(x)) {
        if (is.list(x)) {
          z <- x$z
          y <- x$y
          x <- x$x
        }
        else {
          z <- x
          x <- seq.int(0, 1, length.out = nrow(z))
        }
      }
      else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
      y <- x$y
      x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
      stop("increasing 'x' and 'y' values expected")
    # mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    # on.exit(par(par.orig))
    # w <- (3 + mar.orig[2]) * par("csi") * 2.54
    # par(las = las)
    # mar <- mar.orig
    plot.new()
    # par(mar=mar)
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
      stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
      storage.mode(z) <- "double"
    .filled.contour(as.double(x), as.double(y), z, as.double(levels), 
                    col = col)
    if (missing(plot.axes)) {
      if (axes) {
        title(main = "", xlab = "", ylab = "")
        Axis(x, side = 1,line=1)
        Axis(y, side = 2,line=1)
      }
    }
    else plot.axes
    if (frame.plot) 
      box()
    if (missing(plot.title)) 
      title(...)
    else plot.title
    invisible()
  }


filled.legend <-function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
                                                                       length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
                          ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
                          levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
                          col = color.palette(length(levels) - 1), plot.title, plot.axes, 
                          key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
                          axes = TRUE, frame.plot = axes, ...){
  # modification of filled.contour by Carey McGilliard and Bridget Ferris
  # designed to just plot the legend
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      }
      else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    }
    else stop("no 'z' matrix specified")
  }
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  #  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  #  on.exit(par(par.orig))
  #  w <- (3 + mar.orig[2L]) * par("csi") * 2.54
  #layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
  #  par(las = las)
  #  mar <- mar.orig
  #  mar[4L] <- mar[2L]
  #  mar[2L] <- 1
  #  par(mar = mar)
  # plot.new()
  plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
  if (missing(key.axes)) {
    if (axes) 
      axis(4)
  }
  else key.axes
  box()
}


heat.colors_Rev = function (n, alpha = 1) {
  if ((n <- as.integer(n[1L])) > 0) {
    j <- n%/%4
    i <- n - j
    rev(c(rainbow(i, start = 0, end = 1/6, alpha = alpha), if (j > 
                                                               0) hsv(h = 1/6, s = seq.int(from = 1 - 1/(2 * j), 
                                                                                           to = 1/(2 * j), length.out = j), v = 1, alpha = alpha)))
  }
  else character()
}

#This code uses a modified version of filled.contour called filled.contour3 (created by Carey McGilliard, Ian Taylor, and Bridget Ferris)
#to make an example figure of four contour plots sharing a legend (to the right).
#The example demonstrates how to use various color schemes for the contour plots and legend, but the user will want to
#pick one color scheme for all four plots such that the legend matches the plots.
#Changing the x- and y-axis values will change the placement of text added to the figure using the text() function and adjustments will be necessary


#gplots has the function colorpanel, which is handy for making gray-scale contour plots
library(gplots)
library(colorRamps)



##
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
  output_prev_check = array(dim=c(35,3)) ## This should be all possible outputs for seasonal site = 1 with year in col 1
  output_prev_check[,1] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_0.5_0_0_0_1_0.txt"),header=TRUE)$year
  output_prev_check[,2] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_15_0.5_0_0.8_1_0.txt"),header=TRUE)$clin_inc_all
  output_prev_check[,3] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_15_0.5_0_0.8_2_0.txt"),header=TRUE)$clin_inc_all
  
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
            
            output_prev_net1[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs\\draw_0\\systematic_runs_1_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
 
            output_prev_net1_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_2[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_2\\draw_0\\systematic_runs_2_2_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
          
            output_prev_net1_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_3[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_3\\draw_0\\systematic_runs_3_3_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
            
            output_prev_net1_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_4[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_4\\draw_0\\systematic_runs_4_4_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all

            output_prev_net1_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_5[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_5\\draw_0\\systematic_runs_5_5_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all

            output_prev_net1_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_6[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_6\\draw_0\\systematic_runs_6_6_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
            
            output_prev_net1_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_7[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_7\\draw_0\\systematic_runs_7_7_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
            
            output_prev_net1_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_8[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_8\\draw_0\\systematic_runs_8_8_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
            
            output_prev_net1_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_9[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_9\\draw_0\\systematic_runs_9_9_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
            
            output_prev_net1_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$prev_2_10
            output_prev_net2_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$prev_2_10
            output_prev_net3_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$prev_2_10
            
            output_case_net1_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_1_0.txt"),header=TRUE)$clin_inc_all
            output_case_net2_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_2_0.txt"),header=TRUE)$clin_inc_all
            output_case_net3_10[,j,k,m,r] = read.table(paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\systematic_runs_10_10_",total_M1[m],"_",covITN[j],"_",covIRS[k],"_",resistance[r],"_3_0.txt"),header=TRUE)$clin_inc_all
          }
        }
      }
    }

  
  
  saveRDS(output_case_net1, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_1.Rds")
  saveRDS(output_case_net2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_1.Rds")
  saveRDS(output_case_net3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_1.Rds")
  
  saveRDS(output_case_net1_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_2.Rds")
  saveRDS(output_case_net2_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_2.Rds")
  saveRDS(output_case_net3_2, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_2.Rds")
  
  saveRDS(output_case_net1_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_3.Rds")
  saveRDS(output_case_net2_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_3.Rds")
  saveRDS(output_case_net3_3, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_3.Rds")
  
  saveRDS(output_case_net1_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_4.Rds")
  saveRDS(output_case_net2_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_4.Rds")
  saveRDS(output_case_net3_4, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_4.Rds")
  
  saveRDS(output_case_net1_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_5.Rds")
  saveRDS(output_case_net2_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_5.Rds")
  saveRDS(output_case_net3_5, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_5.Rds")
  
  saveRDS(output_case_net1_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_6.Rds")
  saveRDS(output_case_net2_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_6.Rds")
  saveRDS(output_case_net3_6, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_6.Rds")
  
  saveRDS(output_case_net1_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_7.Rds")
  saveRDS(output_case_net2_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_7.Rds")
  saveRDS(output_case_net3_7, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_7.Rds")
  
  saveRDS(output_case_net1_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_8.Rds")
  saveRDS(output_case_net2_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_8.Rds")
  saveRDS(output_case_net3_8, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_8.Rds")
  
  saveRDS(output_case_net1_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_9.Rds")
  saveRDS(output_case_net2_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_9.Rds")
  saveRDS(output_case_net3_9, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_9.Rds")
  
  saveRDS(output_case_net1_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net1_10.Rds")
  saveRDS(output_case_net2_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net2_10.Rds")
  saveRDS(output_case_net3_10, file="Q:\\RProjects\\IVCC_product_comparisons\\Seas1_output_case_net3_10.Rds")
  
  #######################Save to USB
  saveRDS(output_case_net1, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_1.Rds")
  saveRDS(output_case_net2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_1.Rds")
  saveRDS(output_case_net3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_1.Rds")
  
  saveRDS(output_case_net1_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_2.Rds")
  saveRDS(output_case_net2_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_2.Rds")
  saveRDS(output_case_net3_2, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_2.Rds")
  
  saveRDS(output_case_net1_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_3.Rds")
  saveRDS(output_case_net2_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_3.Rds")
  saveRDS(output_case_net3_3, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_3.Rds")
  
  saveRDS(output_case_net1_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_4.Rds")
  saveRDS(output_case_net2_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_4.Rds")
  saveRDS(output_case_net3_4, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_4.Rds")
  
  saveRDS(output_case_net1_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_5.Rds")
  saveRDS(output_case_net2_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_5.Rds")
  saveRDS(output_case_net3_5, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_5.Rds")
  
  saveRDS(output_case_net1_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_6.Rds")
  saveRDS(output_case_net2_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_6.Rds")
  saveRDS(output_case_net3_6, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_6.Rds")
  
  saveRDS(output_case_net1_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_7.Rds")
  saveRDS(output_case_net2_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_7.Rds")
  saveRDS(output_case_net3_7, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_7.Rds")
  
  saveRDS(output_case_net1_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_8.Rds")
  saveRDS(output_case_net2_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_8.Rds")
  saveRDS(output_case_net3_8, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_8.Rds")
  
  saveRDS(output_case_net1_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_9.Rds")
  saveRDS(output_case_net2_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_9.Rds")
  saveRDS(output_case_net3_9, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_9.Rds")
  
  saveRDS(output_case_net1_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_10.Rds")
  saveRDS(output_case_net2_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net2_10.Rds")
  saveRDS(output_case_net3_10, file="E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net3_10.Rds")
  
  ## OutputName = paste(time, covITN [j ], covIRS [k], transmission, resistance [r])
  output_case_net1 = readRDS(paste0("E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_1.Rds"))
  output_prev_net1 = readRDS(paste0("E:\\High Q0 High Phi Mean\\cleaned_outputs\\Seas1_output_case_net1_1.Rds"))
  dim(output_case_net1)
  
  output_case_net1[,4,1,1,4]
  
  
  prev_net2_rel_eff = ifelse(output_prev_net1 != 0, (output_prev_net1 - output_prev_net2) / output_prev_net1, 0)
  prev_net3_rel_eff = ifelse(output_prev_net1 != 0, (output_prev_net1 - output_prev_net3) / output_prev_net1, 0)
  
  plot(prev_net2_rel_eff[20,11,1,3,]~resistance,ylim=c(-1,1),xlim=c(-1,1))
  points(prev_net3_rel_eff[20,11,1,3,]~resistance,col="blue",pch=20)
  
  ## 1 Where we are now.
  
  ## No historic net use, seasonal setting, standard nets at 50% coverage, no resistance, low transmission setting
  ## For prevalence we are comparing after reaching equalibrium, i = 20
  ## Net cover at this baseline is set to be 50%: j = 0.5
  ## There is no baseline IRS k = 0
  ## This is a low endemicity setting m = 0.5
  ## and we are saying there is no resistance r = 0
  
  ##Baseline is therefore:
  
  baseline = output_prev_net1[20,6,1,3,1]
  
  ##For comparisons we keep the same time, the same endemicity and the same level of resistance
  changing_interventions1 = array(dim=c(11,11,3))#this is net cov (rows), spray cover (cols), and net type (tables)
  changing_interventions1[,,1] = (baseline- output_prev_net1[20,,,1,1]) / baseline
  changing_interventions1[,,2] = (baseline- output_prev_net2[20,,,1,1]) / baseline
  changing_interventions1[,,3] = (baseline- output_prev_net3[20,,,1,1]) / baseline
  
  #############################################
  ##
  ## Set up a function to automatically compare these (Prevalence)
  ##
  #############################################
  ## Describe Where we are now (baseline).
  
  population = 100000
  net_price1 = 15
  spray_price1 = 3
  
  
  
  ## No historic net use, seasonal setting, standard nets at 50% coverage, no resistance, low transmission setting
  ## For prevalence we are comparing after reaching equalibrium, i = 20
  ## Net cover at this baseline is set to be 50%: j = 0.5
  ## There is no baseline IRS k = 0
  ## This is a low endemicity setting m = 0.5
  ## and we are saying there is no resistance r = 0
  auto_fun_prev_change = function(total_M1_a,resistance_a,covITN_a,covIRS_a,
                                  population,net_price1,net_price2,net_price3,spray_price1){
    
    total_M1 = c(0.5,3,15)
    covITN =  seq(0, 1, by = 0.1)
    covIRS =  seq(0, 1, by = 0.1)
    resistance = seq(0, 1, by = 0.2)
    time = 1:35
    
    baseline = output_prev_net1[20,covITN_a,1,total_M1_a,resistance_a]
    
    ##For comparisons we keep the same time, the same endemicity and the same level of resistance
    changing_interventions1 = array(dim=c(11,11,3))#this is net cov (rows), spray cover (cols), and net type (tables)
    changing_interventions1[,,1] = (baseline - output_prev_net1[20,,,total_M1_a,resistance_a]) / baseline
    changing_interventions1[,,2] = (baseline - output_prev_net2[20,,,total_M1_a,resistance_a]) / baseline
    changing_interventions1[,,3] = (baseline - output_prev_net3[20,,,total_M1_a,resistance_a]) / baseline
    
    
    ##For the cost of these interventions
    baseline_costs = population * net_price1 * covITN[covITN_a] + population * spray_price1 * covIRS[covIRS_a]
    
    ##Cost Effectiveness
    interventions_added = array(dim=c(11,11,3))
    for(i in 1:11){
      for(k in 1:11){
        interventions_added[i,k,1] = population * net_price1 * covITN[i] + population * spray_price1 * covIRS[k] 
        interventions_added[i,k,2] = population * net_price2 * covITN[i] + population * spray_price1 * covIRS[k] 
        interventions_added[i,k,3] = population * net_price3 * covITN[i] + population * spray_price1 * covIRS[k] 
      }
    }
    

      changing_costs = (baseline_costs - interventions_added) / baseline_costs

    
    
    return(list(changing_interventions1, 
                changing_costs))
    
  }
    
  par(mfrow=c(2,2))
  par(mar=c(5,5,5,2))
  test = auto_fun_prev_change(total_M1_a = which(total_M1 == 15),
                              resistance_a = which(resistance >0.56 & resistance < 0.66 ),
                              covITN_a = which(covITN == 0.8),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,1),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "Low transmission, 20% pyrethroid resistance",
       ylab="Cost effectiveness",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,1,1))
  
  chooseWhatToSeeNETS = c("standard LLIN","PBO nets","G2 nets")
  chooseWhatToSeeSPRAY = c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
  chooseWhatToSeeNETScover= c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
  
  values_nets = ifelse(chooseWhatToSeeNETS == chooseWhatToSeeNETS[1],1,ifelse(chooseWhatToSeeNETS == chooseWhatToSeeNETS[2],2,3))
  values_nets_cover = ifelse(chooseWhatToSeeNETScover == "0%",1,
                             ifelse(chooseWhatToSeeNETScover == "10%",2,
                                    ifelse(chooseWhatToSeeNETScover == "20%",3,
                                           ifelse(chooseWhatToSeeNETScover == "30%",4,
                                                  ifelse(chooseWhatToSeeNETScover == "40%",5,
                                                         ifelse(chooseWhatToSeeNETScover == "50%",6,
                                                                ifelse(chooseWhatToSeeNETScover == "60%",7,
                                                                       ifelse(chooseWhatToSeeNETScover == "70%",8,
                                                                              ifelse(chooseWhatToSeeNETScover == "80%",9,
                                                                                     ifelse(chooseWhatToSeeNETScover == "90%",10,11))))))))))
  
  
  values_sprays = ifelse(chooseWhatToSeeSPRAY == "0%",1,
                         ifelse(chooseWhatToSeeSPRAY == "10%",2,
                                ifelse(chooseWhatToSeeSPRAY == "20%",3,
                                       ifelse(chooseWhatToSeeSPRAY == "30%",4,
                                              ifelse(chooseWhatToSeeSPRAY == "40%",5,
                                                     ifelse(chooseWhatToSeeSPRAY == "50%",6,
                                                            ifelse(chooseWhatToSeeSPRAY == "60%",7,
                                                                   ifelse(chooseWhatToSeeSPRAY == "70%",8,
                                                                          ifelse(chooseWhatToSeeSPRAY == "80%",9,
                                                                                 ifelse(chooseWhatToSeeSPRAY == "90%",10,11))))))))))
  
  values_pch = 15
  values_net_col = c("blue","red","orange")
  values_col = c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)
  values_nets_cover_size = values_col+0.9
  
  
  values_nets = 1:2
  values_sprays = c(1,5:8)
  values_nets_cover = c(1,6:8)
  
  tableData = expand.grid(rep(chooseWhatToSeeNETS[unique(values_nets)],
                              each=(length(unique(values_sprays))*length(unique(values_nets_cover)))))
  
  
  tableData[,2] = rep(chooseWhatToSeeNETScover[unique(values_nets_cover)],
      (length(unique(values_sprays))*length(unique(values_nets))))
  
  tableData[,3] = rep(rep(chooseWhatToSeeSPRAY[unique(values_sprays)],
                          each=length(unique(values_nets_cover))),length(unique(unique(values_nets))))
  tableData[,4] = c(test[[1]][unique(values_nets_cover),unique(values_sprays),unique(values_nets)])
  tableData[,5] = c(test[[2]][unique(values_nets_cover),unique(values_sprays),unique(values_nets)])
  
  colnames(tableData) = c("Type of net","Net cover","IRS cover","Relative efficacy against cases","Relative change in cost")
  
  dim(tableData)
  
  library(data.table)
  tableData1 = as.data.table(tableData, keep.rownames=FALSE)
  
  colnames(tableData) = c("Type of net","Net cover","IRS cover","Relative efficacy against cases","Relative change in cost")
  
  tableData
  
  for(j in values_sprays[1:2]){ 
      for(k in values_nets_cover[1:4]){
          for(i in values_nets[1:3]){
          points(test[[2]][k,j,i] ~ test[[1]][k,j,i],pch=15,cex=values_nets_cover_size[k], col = transp(values_net_col[i],values_col[j]))
           }
    }
  }  
  

  
  
  
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  text(-0.8,1,"Less effective")
  text(-0.8,0.8,"Less expensive")

  text(-0.8,-1.8,"Less effective")
  text(-0.8,-2,"More expensive")
  
  text(0.8,1,"More effective")
  text(0.8,0.8,"Less expensive")
 
  text(0.8,-1.8,"More effective")
  text(0.8,-2,"More expensive")
  
  points(0,0,pch = 20,cex=3)
  
  test = auto_fun_prev_change(total_M1_a = which(total_M1 == 15),
                              resistance_a = which(resistance == 0.2),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,1),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "High transmission, 20% pyrethroid resistance",
       ylab="Cost effectiveness",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,1,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  
  test = auto_fun_prev_change(total_M1_a = which(total_M1 == 0.5),
                              resistance_a = which(resistance == 0.8),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,1),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "Low transmission, 80% pyrethroid resistance",
       ylab="Cost effectiveness",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,1,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  test = auto_fun_prev_change(total_M1_a = which(total_M1 == 15),
                              resistance_a = which(resistance == 0.8),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,1),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "High transmission, 80% pyrethroid resistance",
       ylab="Cost effectiveness",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,1,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  legend("topleft",legend=c("Baseline:","50% standard nets, no IRS"),
          col="black",pch=c(20,NA),bty="n",pt.cex=3)
  
  legend("bottomleft",legend=c("Standard nets","PBO nets","G2 nets"," + IRS","Lower coverage","Higher coverage"),
         col=c("blue","red","orange","black",transp("black",0.3),"black"),
         pch=c(15,15,15,17,15,15),bty="n")
  
  #############################################
  ##
  ## Set up a function to automatically compare these (Cases averted)
  ##
  #############################################
  ## Describe Where we are now (baseline).
  
  population = 100000
  net_price1 = 15
  spray_price1 = 3
  
  
  
  ## No historic net use, seasonal setting, standard nets at 50% coverage, no resistance, low transmission setting
  ## For prevalence we are comparing after reaching equalibrium, i = 20
  ## Net cover at this baseline is set to be 50%: j = 0.5
  ## There is no baseline IRS k = 0
  ## This is a low endemicity setting m = 0.5
  ## and we are saying there is no resistance r = 0
  auto_fun_case_change = function(total_M1_a,resistance_a,covITN_a,covIRS_a,
                                  population,net_price1,net_price2,net_price3,spray_price1){
    
    total_M1 = c(0.5,3,15)
    covITN =  seq(0, 1, by = 0.1)
    covIRS =  seq(0, 1, by = 0.1)
    resistance = seq(0, 1, by = 0.2)
    time = 1:35
    
    baseline = mean(output_case_net1[20:25,covITN_a,1,total_M1_a,resistance_a])
    
    ##For comparisons we keep the same time, the same endemicity and the same level of resistance
    changing_interventions1 = array(dim=c(11,11,3))#this is net cov (rows), spray cover (cols), and net type (tables)
    for(i in 1:11){
      for(j in 1:11){
        changing_interventions1[i,j,1] = (baseline - mean(output_case_net1[20:25,i,j,total_M1_a,resistance_a])) / baseline
        changing_interventions1[i,j,2] = (baseline - mean(output_case_net2[20:25,i,j,total_M1_a,resistance_a])) / baseline
        changing_interventions1[i,j,3] = (baseline - mean(output_case_net3[20:25,i,j,total_M1_a,resistance_a])) / baseline
        
      }
    }
    
    
    ##For the cost of these interventions
    baseline_costs = population * net_price1 * covITN[covITN_a] + population * spray_price1 * covIRS[covIRS_a]
    
    ##Cost Effectiveness
    interventions_added = array(dim=c(11,11,3))
    for(i in 1:11){
      for(j in 1:11){
        interventions_added[i,j,1] = population * net_price1 * covITN[i] + population * spray_price1 * covIRS[j] 
        interventions_added[i,j,2] = population * net_price2 * covITN[i] + population * spray_price1 * covIRS[j] 
        interventions_added[i,j,3] = population * net_price3 * covITN[i] + population * spray_price1 * covIRS[j] 
      }
    }
    
    
    changing_costs = -1 *((baseline_costs - interventions_added) / baseline_costs)
    
    
    
    return(list(changing_interventions1, 
                changing_costs))
    
  }
  
  test = auto_fun_case_change(total_M1_a = which(total_M1 == 0.5),
                              resistance_a = which(resistance == 0.2),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,2),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "Low transmission, 20% pyrethroid resistance",
       ylab="Relative difference in cost",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,2,1))
  
  
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  text(-0.8,2,"Less effective")
  text(-0.8,1.8,"More expensive")
  
  text(-0.8,-1.8,"Less effective")
  text(-0.8,-2,"Less expensive")
  
  text(0.8,2,"More effective")
  text(0.8,1.8,"More expensive")
  
  text(0.8,-1.8,"More effective")
  text(0.8,-2,"Less expensive")
  
  points(0,0,pch = 20,cex=3)
  
  test = auto_fun_case_change(total_M1_a = which(total_M1 == 15),
                              resistance_a = which(resistance == 0.2),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,2),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "High transmission, 20% pyrethroid resistance",
       ylab="Relative difference in cost",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,2,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  
  test = auto_fun_case_change(total_M1_a = which(total_M1 == 0.5),
                              resistance_a = which(resistance == 0.8),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,2),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "Low transmission, 80% pyrethroid resistance",
       ylab="Relative difference in cost",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,2,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  test = auto_fun_case_change(total_M1_a = which(total_M1 == 15),
                              resistance_a = which(resistance == 0.8),
                              covITN_a = which(covITN == 0.5),
                              covIRS_a = which(covIRS == 0),
                              population = 100000,
                              net_price1 = 5,net_price2 = 5,net_price3 = 5,
                              spray_price1 = 2)
  
  plot(test[[2]] ~ test[[1]],ylim=c(-2,2),xlim=c(-1,1),pch="",
       xaxt="n",yaxt="n",bty="n",main = "High transmission, 80% pyrethroid resistance",
       ylab="Relative difference in cost",xlab="Intervention efficacy")
  axis(1, pos=0,at=seq(-1,1,1))
  axis(2, las=2,pos=0,at=seq(-2,2,1))
  
  points(0,0,pch = 20,cex=3)
  points(test[[2]][,1,1] ~ test[[1]][,1,1],pch=15,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,2] ~ test[[1]][,1,2],pch=15,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,1,3] ~ test[[1]][,1,3],pch=15,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  points(test[[2]][,,1] ~ test[[1]][,,1],pch=17,col=transp("blue",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,2] ~ test[[1]][,,2],pch=17,col=transp("red",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  points(test[[2]][,,3] ~ test[[1]][,,3],pch=17,col=transp("orange",c(0.1,0.2,0.3,0.4,0.5,0.55,0.6,0.7,0.8,0.9,1)))
  
  legend("topleft",legend=c("Baseline:","50% standard nets, no IRS"),
         col="black",pch=c(20,NA),bty="n",pt.cex=3)
  
  legend("bottomleft",legend=c("Standard nets","PBO nets","G2 nets"," + IRS","Lower coverage","Higher coverage"),
         col=c("blue","red","orange","black",transp("black",0.3),"black"),
         pch=c(15,15,15,17,15,15),bty="n")
  
  #############################################
  ##
  ## PRIORITERISATION OF VECTOR INTERVENTIONS
  ##
  
  priority_interventions_V2_f<-function(total_M1,
                                        covITN,covIRS,
                                        
                                        itn_repel_fun_1, itn_repel_gamb_ss_1, itn_repel_arab_1, 
                                        itn_kill_fun_1, itn_kill_gamb_ss_1, itn_kill_arab_1,
                                        itn_halflife_1,
                                        
                                        irs_decay_mort1_1,irs_decay_mort2_1,
                                        irs_decay_succ1_1,irs_decay_succ2_1,
                                        irs_decay_det1_1,irs_decay_det2_1,
                                        
                                        resistance,
                                        type_net,
                                        
                                        site,
                                        run_name){
    Run_name<-run_name
    draw<-0
    
    # Load the site_file file
    site_file<-read.table(paste0('K:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/prioritorisation/Season_', site, '.txt'))
    pop_size<- 50000 #Sim_pop_size(site_file[site_file[,1]=='prev',2])
    
    Int_set_up<-paste('total_M', total_M1, 'num_people', pop_size, 'itn_irs_corr', 1, 
                      'output_type 1 itn 1 itn_coverage', covITN,
                      'add change_itn 1 change_itn_time 0',
                      'itn_repel_fun 0.31 itn_repel_gamb_ss 0.31 itn_repel_arab 0.31',
                      'itn_kill_fun 0.51 itn_kill_gamb_ss 0.51 itn_kill_arab 0.51',
                      'itn_half_life 2.64',
                      
                      'irs 1 irs_coverage', covIRS,'irs_start 0 irs_max_rounds 20 irs_offset_absolute 1 irs_offset 0', 
                      ## sprays are off historically and then switched on or not as per csv files
                      'change_irs 1 change_irs_time 0',
                      'irs_decay_mort1_1', irs_decay_mort1_1, 'irs_decay_mort2_1', irs_decay_mort2_1,
                      'irs_decay_succ1_1', irs_decay_succ1_1, 'irs_decay_succ2_1', irs_decay_succ2_1,
                      'irs_decay_det1_1', irs_decay_det1_1, 'irs_decay_det2_1', irs_decay_det2_1,
                      ##the csv file defined level of resistance or spray type is used
                      
                      'change_itn_2 1 change_itn_time_2 0',
                      'itn_repel_fun_2', itn_repel_fun_1, 'itn_repel_gamb_ss_2', itn_repel_gamb_ss_1, 'itn_repel_arab_2', itn_repel_arab_1,
                      'itn_kill_fun_2', itn_kill_fun_1, 'itn_kill_gamb_ss_2', itn_kill_gamb_ss_1, 'itn_kill_arab_2', itn_kill_arab_1,
                      'itn_half_life_2', itn_halflife_1,
                      ## Nets are switched on at time 0 (no historic use) and used at the defined coverage for this run see csv files
                      
                      'irs_ellie 1')
    Options<-paste(Int_set_up)
    
    ## Run the simulation
    Model_launcher(OutputName = paste(Run_name, site, total_M1, covITN, covIRS, resistance, type_net, draw, sep='_'),
                   OutputRoot = paste0("K:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/prioritorisation/outputs_v2/", Run_name, '/draw_', draw),
                   Options=Options,
                   Exe = "K:/Ellies_cool_model_folder2/bin/IRS_test3.exe",
                   Root="K:/Ellies_cool_model_folder2/model_files",
                   Site=paste0('/Africa_Sites_Ellie_eip/prioritorisation/Season_', site, '.txt'),
                   Parameter_draw = draw,
                   Return_output = FALSE)
  }
  
  ########################################
  ##
  ## Confirm all simulations have been run 
  ##
  ########################################
  input_dat1 = read.csv("K:/Ellies_cool_model_folder2/model_files/sites/Africa_Sites_Ellie_eip/prioritorisation/run_set2.csv",header=TRUE)
  
  for(i in 1:nrow(input_dat1)){
    input_dat1$name[i] = paste("systematic_runs_10_10",input_dat1[i,1],input_dat1[i,2],input_dat1[i,3],input_dat1[i,17],input_dat1[i,18],"0",sep='_')
    }
  
  for(i in 1:6534){
  
  ff <- paste0("K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\draw_0\\",input_dat1$name[i],".txt")
  if (file.exists(ff))
    input_dat1$PRESENT[i] <- "yes"
  else(input_dat1$PRESENT[i] <- "no")
  
  }
  
  redo = c(which(input_dat1$PRESENT == "no"))
  length(redo)
  write.csv(redo,"K:\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs_v2\\systematic_runs_10\\redos.csv")
  #redo = c(568, 1765, 1913, 2282, 2296, 2431, 2435, 2478, 2897, 3338, 3800, 3815, 4156, 4218, 4727, 5600, 5662, 5845, 5871)
  for(i in 1:length(redo)){
  priority_interventions_V2_f(total_M1 = input_dat1[redo[i],1],
                                        covITN = input_dat1[redo[i],2],covIRS = input_dat1[redo[i],3],
                                        
                                        itn_repel_fun_1 = input_dat1[redo[i],4], itn_repel_gamb_ss_1 = input_dat1[redo[i],5], itn_repel_arab_1 = input_dat1[redo[i],6], 
                                        itn_kill_fun_1 = input_dat1[redo[i],7], itn_kill_gamb_ss_1 = input_dat1[redo[i],8], itn_kill_arab_1 = input_dat1[redo[i],9],
                                        itn_halflife_1 = input_dat1[redo[i],10],
                                        
                                        irs_decay_mort1_1 = input_dat1[redo[i],11],irs_decay_mort2_1 = input_dat1[redo[i],12],
                                        irs_decay_succ1_1 = input_dat1[redo[i],13],irs_decay_succ2_1 = input_dat1[redo[i],14],
                                        irs_decay_det1_1 = input_dat1[redo[i],15],irs_decay_det2_1 = input_dat1[redo[i],16],
                                        
                                        resistance = input_dat1[redo[i],17],
                                        type_net = input_dat1[redo[i],18],
                                        
                                        site=1,
                                        run_name="systematic_runs")
  }
  ########################################
  ########################################
  
  
  
  Prevalence_summary = array(dim=c(,))
  Prevalence_summary[,3] = output_prev_ne1[,2:485][output_prev_a1[,1] == 1]
  Prevalence_summary[,4] = output_prev_a1[,2:485][output_prev_a1[,1] == 2]
  Prevalence_summary[,5] = output_prev_a1[,2:485][output_prev_a1[,1] == 3]
  Prevalence_summary[,2] = rep(rep(seq(0,1,length=11),each=11),4)
  Prevalence_summary[,1] = rep(seq(0,1,length=11),44)
  
  vec_base = rep(1:11,44)
  vec = 1:484
  Prevalence_summary[i,6] = (Prevalence_summary[vec_base[i],3] - Prevalence_summary[vec[i],3])/Prevalence_summary[vec_base[i],3]
  Prevalence_summary[i,7] = (Prevalence_summary[vec_base[i],4] - Prevalence_summary[vec[i],4])/Prevalence_summary[vec_base[i],4]
  Prevalence_summary[i,8] = (Prevalence_summary[vec_base[i],5] - Prevalence_summary[vec[i],5])/Prevalence_summary[vec_base[i],5]
  
  colnames(Prevalence_summary) = c("Resistance","Net_coverage",
                                   "Prevalence_2_10_at_year_1","Prevalence_2_10_at_year_2","Prevalence_2_10_at_year_3",
                                   "Relative_red_to_no_nets_Prev_yr_1","Relative_red_to_no_nets_Prev_yr_2","Relative_red_to_no_nets_Prev_yr_3")
  
  return(Prevalence_summary)


Relative_Clin_inc_difference = function(name_of_data){
  
  site = read.csv("B:\\Ellie\\Rprojects\\Malaria\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\run_set1_standard_nets_no_irs_prev05.csv",header=TRUE)$site
  output_clin_a1 = output_clin_a2 = array(dim=c(1301,length(site)+1))
  output_clin_a1[,1] = read.table(paste0("B:\\Ellie\\Rprojects\\Malaria\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs\\",name_of_data,'\\draw_0\\',name_of_data,'_1_0.txt'),header=TRUE)$year
  aa = length(site)
  vecs = seq(1,484,11)
  
  for(k in 1:length(vecs)){
    for(i in 1:11){
      output_clin_a1[,vecs[k]+i] = read.table(paste0("B:\\Ellie\\Rprojects\\Malaria\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs\\",name_of_data,'\\draw_0\\',name_of_data,'_',site[i],'_0.txt'),header=TRUE)$clin_inc_all
    }  
  }
  
  for(i in 1:aa){
    output_clin_a2[,i+1] = read.table(paste0("B:\\Ellie\\Rprojects\\Malaria\\Ellies_cool_model_folder2\\model_files\\sites\\Africa_Sites_Ellie_eip\\prioritorisation\\outputs\\",name_of_data,'\\draw_0\\',name_of_data,'_',site[i],'_0.txt'),header=TRUE)$clin_inc_all
  }
  
  dat_clin_summary = array(dim=c(484,5))
  for(i in 1:484){
    dat_clin_summary[i,3] = 100 * mean((output_clin_a1[262:313,i+1] - output_clin_a2[262:313,i+1])/output_clin_a1[262:313,i+1])
    dat_clin_summary[i,4] = 100 * mean((output_clin_a1[262:365,i+1] - output_clin_a2[262:365,i+1])/output_clin_a1[262:365,i+1])
    dat_clin_summary[i,5] = 100 * mean((output_clin_a1[262:417,i+1] - output_clin_a2[262:417,i+1])/output_clin_a1[262:417,i+1])
  }
  
  dat_clin_summary[,2] = rep(rep(seq(0,1,length=11),each=11),4)
  dat_clin_summary[,1] = rep(seq(0,1,length=11),44)
  
  colnames(dat_clin_summary) = c("Resistance","Net_coverage",
                                 "Relative_red_clinc_inc_yrs_1","Relative_red_clinc_inc_yrs_2","Relative_red_clinc_inc_yrs_3")
  
  return(dat_clin_summary)
}

vec_base = rep(1:11,44)
vec = 1:484

prev_05_st_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_05_standard_nets_no_irs")
for(i in 1:484){
  prev_05_st_nets_no_irs[i,6] = 100*(prev_05_st_nets_no_irs[vec_base[i],3]-prev_05_st_nets_no_irs[vec[i],3])/prev_05_st_nets_no_irs[vec_base[i],3]
  prev_05_st_nets_no_irs[i,7] = 100*(prev_05_st_nets_no_irs[vec_base[i],4]-prev_05_st_nets_no_irs[vec[i],4])/prev_05_st_nets_no_irs[vec_base[i],4]
  prev_05_st_nets_no_irs[i,8] = 100*(prev_05_st_nets_no_irs[vec_base[i],5]-prev_05_st_nets_no_irs[vec[i],5])/prev_05_st_nets_no_irs[vec_base[i],5]
}
prev_30_st_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_30_standard_nets_no_irs")
for(i in 1:484){
  prev_30_st_nets_no_irs[i,6] = 100*(prev_30_st_nets_no_irs[vec_base[i],3]-prev_30_st_nets_no_irs[vec[i],3])/prev_30_st_nets_no_irs[vec_base[i],3]
  prev_30_st_nets_no_irs[i,7] = 100*(prev_30_st_nets_no_irs[vec_base[i],4]-prev_30_st_nets_no_irs[vec[i],4])/prev_30_st_nets_no_irs[vec_base[i],4]
  prev_30_st_nets_no_irs[i,8] = 100*(prev_30_st_nets_no_irs[vec_base[i],5]-prev_30_st_nets_no_irs[vec[i],5])/prev_30_st_nets_no_irs[vec_base[i],5]
}
prev_60_st_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_60_standard_nets_no_irs")
for(i in 1:484){
  prev_60_st_nets_no_irs[i,6] = 100*(prev_60_st_nets_no_irs[vec_base[i],3]-prev_60_st_nets_no_irs[vec[i],3])/prev_60_st_nets_no_irs[vec_base[i],3]
  prev_60_st_nets_no_irs[i,7] = 100*(prev_60_st_nets_no_irs[vec_base[i],4]-prev_60_st_nets_no_irs[vec[i],4])/prev_60_st_nets_no_irs[vec_base[i],4]
  prev_60_st_nets_no_irs[i,8] = 100*(prev_60_st_nets_no_irs[vec_base[i],5]-prev_60_st_nets_no_irs[vec[i],5])/prev_60_st_nets_no_irs[vec_base[i],5]
}

prev_05_pbo_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_05_PBO_nets_no_irs")
for(i in 1:484){
  prev_05_pbo_nets_no_irs[i,6] = 100*(prev_05_pbo_nets_no_irs[vec_base[i],3]-prev_05_pbo_nets_no_irs[vec[i],3])/prev_05_pbo_nets_no_irs[vec_base[i],3]
  prev_05_pbo_nets_no_irs[i,7] = 100*(prev_05_pbo_nets_no_irs[vec_base[i],4]-prev_05_pbo_nets_no_irs[vec[i],4])/prev_05_pbo_nets_no_irs[vec_base[i],4]
  prev_05_pbo_nets_no_irs[i,8] = 100*(prev_05_pbo_nets_no_irs[vec_base[i],5]-prev_05_pbo_nets_no_irs[vec[i],5])/prev_05_pbo_nets_no_irs[vec_base[i],5]
}
prev_30_pbo_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_30_PBO_nets_no_irs")
for(i in 1:484){
  prev_30_pbo_nets_no_irs[i,6] = 100*(prev_30_pbo_nets_no_irs[vec_base[i],3]-prev_30_pbo_nets_no_irs[vec[i],3])/prev_30_pbo_nets_no_irs[vec_base[i],3]
  prev_30_pbo_nets_no_irs[i,7] = 100*(prev_30_pbo_nets_no_irs[vec_base[i],4]-prev_30_pbo_nets_no_irs[vec[i],4])/prev_30_pbo_nets_no_irs[vec_base[i],4]
  prev_30_pbo_nets_no_irs[i,8] = 100*(prev_30_pbo_nets_no_irs[vec_base[i],5]-prev_30_pbo_nets_no_irs[vec[i],5])/prev_30_pbo_nets_no_irs[vec_base[i],5]
}
prev_60_pbo_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_60_PBO_nets_no_irs")
for(i in 1:484){
  prev_60_pbo_nets_no_irs[i,6] = 100*(prev_60_pbo_nets_no_irs[vec_base[i],3]-prev_60_pbo_nets_no_irs[vec[i],3])/prev_60_pbo_nets_no_irs[vec_base[i],3]
  prev_60_pbo_nets_no_irs[i,7] = 100*(prev_60_pbo_nets_no_irs[vec_base[i],4]-prev_60_pbo_nets_no_irs[vec[i],4])/prev_60_pbo_nets_no_irs[vec_base[i],4]
  prev_60_pbo_nets_no_irs[i,8] = 100*(prev_60_pbo_nets_no_irs[vec_base[i],5]-prev_60_pbo_nets_no_irs[vec[i],5])/prev_60_pbo_nets_no_irs[vec_base[i],5]
}

prev_05_G2_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_05_G2_nets_no_irs")
for(i in 1:484){
  prev_05_G2_nets_no_irs[i,6] = 100*(prev_05_G2_nets_no_irs[vec_base[i],3]-prev_05_G2_nets_no_irs[vec[i],3])/prev_05_G2_nets_no_irs[vec_base[i],3]
  prev_05_G2_nets_no_irs[i,7] = 100*(prev_05_G2_nets_no_irs[vec_base[i],4]-prev_05_G2_nets_no_irs[vec[i],4])/prev_05_G2_nets_no_irs[vec_base[i],4]
  prev_05_G2_nets_no_irs[i,8] = 100*(prev_05_G2_nets_no_irs[vec_base[i],5]-prev_05_G2_nets_no_irs[vec[i],5])/prev_05_G2_nets_no_irs[vec_base[i],5]
}
prev_30_G2_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_30_G2_nets_no_irs")
for(i in 1:484){
  prev_30_G2_nets_no_irs[i,6] = 100*(prev_30_G2_nets_no_irs[vec_base[i],3]-prev_30_G2_nets_no_irs[vec[i],3])/prev_30_G2_nets_no_irs[vec_base[i],3]
  prev_30_G2_nets_no_irs[i,7] = 100*(prev_30_G2_nets_no_irs[vec_base[i],4]-prev_30_G2_nets_no_irs[vec[i],4])/prev_30_G2_nets_no_irs[vec_base[i],4]
  prev_30_G2_nets_no_irs[i,8] = 100*(prev_30_G2_nets_no_irs[vec_base[i],5]-prev_30_G2_nets_no_irs[vec[i],5])/prev_30_G2_nets_no_irs[vec_base[i],5]
}
prev_60_G2_nets_no_irs = Relative_Prevalence_difference("transmission_setting_prev_60_G2_nets_no_irs")
for(i in 1:484){
  prev_60_G2_nets_no_irs[i,6] = 100*(prev_60_G2_nets_no_irs[vec_base[i],3]-prev_60_G2_nets_no_irs[vec[i],3])/prev_60_G2_nets_no_irs[vec_base[i],3]
  prev_60_G2_nets_no_irs[i,7] = 100*(prev_60_G2_nets_no_irs[vec_base[i],4]-prev_60_G2_nets_no_irs[vec[i],4])/prev_60_G2_nets_no_irs[vec_base[i],4]
  prev_60_G2_nets_no_irs[i,8] = 100*(prev_60_G2_nets_no_irs[vec_base[i],5]-prev_60_G2_nets_no_irs[vec[i],5])/prev_60_G2_nets_no_irs[vec_base[i],5]
}

clin_05_st_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_05_standard_nets_no_irs")
clin_30_st_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_30_standard_nets_no_irs")
clin_60_st_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_60_standard_nets_no_irs")
clin_05_PBO_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_05_PBO_nets_no_irs")
clin_30_PBO_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_30_PBO_nets_no_irs")
clin_60_PBO_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_60_PBO_nets_no_irs")
clin_05_G2_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_05_G2_nets_no_irs")
clin_30_G2_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_30_G2_nets_no_irs")
clin_60_G2_nets_no_irs = Relative_Clin_inc_difference("transmission_setting_prev_60_G2_nets_no_irs")

Preps_f = function(dats,ro){
  z1 = dats[1:121,ro]##perennial
  z2 = dats[122:242,ro]##seasonal
  z3 = dats[243:363,ro]##Highly_seasonal
  z4 = dats[364:484,ro]##Bimodal
  matrix1 = matrix(z1,nrow=11,ncol=11,byrow=F)
  matrix2 = matrix(z2,nrow=11,ncol=11,byrow=F)
  matrix3 = matrix(z3,nrow=11,ncol=11,byrow=F)
  matrix4 = matrix(z4,nrow=11,ncol=11,byrow=F)
  
  return(list(matrix1,matrix2,matrix3,matrix4))
}

xcoords = unique(rep(seq(0,1,length=11),44))
ycoords = unique(rep(rep(seq(0,1,length=11),each=11),4))


##Organisation of the outputs
prev_05_st_nets_no_irsM = Preps_f(prev_05_st_nets_no_irs,8)
prev_30_st_nets_no_irsM = Preps_f(prev_30_st_nets_no_irs,8)
prev_60_st_nets_no_irsM = Preps_f(prev_60_st_nets_no_irs,8) 
prev_05_pbo_nets_no_irsM = Preps_f(prev_05_pbo_nets_no_irs,8)
prev_30_pbo_nets_no_irsM = Preps_f(prev_30_pbo_nets_no_irs,8)
prev_60_pbo_nets_no_irsM = Preps_f(prev_60_pbo_nets_no_irs,8)
prev_05_G2_nets_no_irsM = Preps_f(prev_05_G2_nets_no_irs,8)
prev_30_G2_nets_no_irsM = Preps_f(prev_30_G2_nets_no_irs,8)
prev_60_G2_nets_no_irsM = Preps_f(prev_60_G2_nets_no_irs,8)


clin_05_st_nets_no_irsM  = Preps_f(clin_05_st_nets_no_irs,3)
clin_30_st_nets_no_irsM  = Preps_f(clin_30_st_nets_no_irs,3)
clin_60_st_nets_no_irsM  = Preps_f(clin_60_st_nets_no_irs,3)
clin_05_PBO_nets_no_irsM = Preps_f(clin_05_PBO_nets_no_irs,3)
clin_30_PBO_nets_no_irsM = Preps_f(clin_30_PBO_nets_no_irs,3)
clin_60_PBO_nets_no_irsM = Preps_f(clin_60_PBO_nets_no_irs,3)
clin_05_G2_nets_no_irsM  = Preps_f(clin_05_G2_nets_no_irs,3)
clin_30_G2_nets_no_irsM  = Preps_f(clin_30_G2_nets_no_irs,3)
clin_60_G2_nets_no_irsM  = Preps_f(clin_60_G2_nets_no_irs,3)


plotting_matrices_f = function(datathing,dataz,name_data,col_choice){
  ##Highly seasonal  Long lasting
  filled.contour3(xcoords,
                  ycoords,
                  datathing,
                  color=col_choice,
                  plot.axes = { axis(1, at = seq(0, 1, by = 0.2), seq(0, 100, by = 20))
                    axis(2, at = seq(0, 1, by = 0.2), seq(0, 100, by = 20)) },
                  nlevels = 10, levels = seq(min(dataz), max(dataz),length=11),
                  main = name_data,
                  xlim = c(min(xcoords),max(xcoords)),
                  ylim = c(min(ycoords),max(ycoords)),
                  zlim = c(min(dataz),max(dataz)))
  
  # the contour part - draw iso-lines
  contour(xcoords,
          ycoords,
          datathing,
          color=col_choice,
          xlab = "",
          ylab = "",
          nlevels = 11, levels = seq(min(dataz), max(dataz),length=11),
          labels = round(seq(min(dataz), max(dataz),length=11),3),
          xlim = c(min(xcoords),max(xcoords)),
          ylim = c(min(ycoords),max(ycoords)),
          zlim = c(min(dataz),max(dataz)),
          add=TRUE,                 # add the contour plot to filled-contour,
          #thus making an overlay
          col = gray(0.9)           # color of overlay-lines
  )
  
}

plot.new()

##Bottom left plot:
par(new = "TRUE",  
    plt = c(0.09,0.33,0.09,0.33),   # using plt instead of mfcol (compare
    las = 1,                      # orientation of axis labels
    cex.axis = 0.8,                 # size of axis annotation
    tck = -0.02 )                 # major tick size and direction, < 0 means outside
plotting_matrices_f(prev_60_st_nets_no_irsM[[1]],  prev_60_st_nets_no_irsM[[1]], "",col_choice = blue2green)#"Perennial, high transmission, standard nets, no IRS")
text(0.05,0.95,"g",col="grey15",cex=0.8)

##mid left plot:
par(new = "TRUE",  
    plt = c(0.09,0.33,0.38,0.62),   # using plt instead of mfcol (compare
    las = 1,                      # orientation of axis labels
    cex.axis = 0.8,                 # size of axis annotation
    tck = -0.02 )                 # major tick size and direction, < 0 means outside
plotting_matrices_f(prev_30_st_nets_no_irsM[[1]], prev_30_st_nets_no_irsM[[1]],"",col_choice = magenta2green)#"Perennial, medium transmission, standard nets, no IRS")
text(0.05,0.95,"d",cex=0.8,col="grey15")

