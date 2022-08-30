

###############################################################################
#                                                                             #
#    Cognitive processes underlying                                           #
#    prosocial decision making in callous-unemotional traits                  #
#                                                                             #
#     Drew E Winters, PhD.                                                    #
#                                                                             #
###############################################################################



# Cognitive processes underlying prosocial decision making in callous-unemotional traits 

  # Hypotheses 
  
    #1) that CU traits would associate with disruptions to cognitive processes supporting prosocial decision making that are independent of conduct problems
        # Knowledge accumulation (threshold separation)
          # Decision style  during decision that will benefit them will be more conservative at higher CU traits
        # non-Decision time 
          # higher CU traits would associate with higher non-decision time
    
    #2) that CU traits would have a proclivity toward decisions that benefit them and that this is independent of conduct problems
        # Drift Rate
          # higher CU traits will have a proclivity to accept trials where participant gain more
          # higher CU traits will have a proclivity to reject trials where they give more 
    
    #3) sex would moderate these effects 


  # Note
    # we will run within model stats in one model and before model stats in another model
      # drift and threshold separation in one model
      # non-decision time and bias in another model 




                            #### PREPARING FOR ANALYSIS ####

# packages
library(DstarM)   ## drift diffusion modeling 
library(ggplot2)  ## plotting 
library(MKinfer)  ## Bootstrapped T tests
library(lavaan)   ## For path analysis  
library(ggpubr)   ## to arrange plots 


# Uploading Files ####

  # T1 files
t1_files <- list.files("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\DPRG main project\\Analyses and Data\\full_data_87\\T1_full_87_n87")


for (i in 1:length(t1_files)){
  assign(paste0("t1",i),
         read.csv(paste0("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\DPRG main project\\Analyses and Data\\full_data_87\\T1_full_87_n87\\",
                         t1_files[i]),check.names = FALSE, header=FALSE))
}

dda1 <-NULL
dda1 <- mget(paste0("t1", 1:length(t1_files)))
length(dda1)



                          #### Drift Diffusion Model ####
# note
  # the DstarM package automatically sorts the first response option as the "lower option" 
    # documentation on this 
      # see: <https://cran.r-project.org/web/packages/DstarM/DstarM.pdf>
        # Response options will be alphabetically sorted and the first response option will be treated as the
        # 'lower' option. This means that if the observed proportion of the first response options is higher,
        # the drift speed will most likely be negative.
    # Thus the re-coding we do here is to ensure that the responses are coded correctly. 


# Initializing lists 
ddm_list = list()
nd_list = list()

# for loop for DDM model
    ## Note takes ~12 hours tp run
for (i in c(1:length(dda1))){
  # priting participant # bing calculated
  print(i)
  # calculating individual reaction time - in milliseconds
  rt <- as.numeric(dda1[[i]][18:53,43])/1000
  # recoding response times to meet conditions we are coding
  response <- as.factor(c(as.factor(car::recode(dda1[[i]][c(17+1),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+2),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+3),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+4),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+5,17+6),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+7,17+8),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+9,17+10,17+11,17+12,17+13, 17+14),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+15),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+16,17+17),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+18),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+19,17+20),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+21),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+22,17+23),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+24),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+25,17+26,17+27,17+28,17+29, 17+30,17+31,17+32),42], "'2'='lower'; '1'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+33,17+34),42], "'1'='lower'; '2'='upper'")),
                          as.factor(car::recode(dda1[[i]][c(17+35,17+36),42], "'2'='lower'; '1'='upper'"))))
                            # creating a condition variable for 
                              #> 1 greater self keeping 
                                  #> consisting of trials where 
                                  #> They Gain more than give
                                  #> Reverse coded where they lose more than they give
                                  #> consistent with prior investigations demonstrating a high correlation between these trial types
                                    #> we reverse code this with the logic that if they reject losing then they are for gaining more for themselves 
                              #> 2 greater Red Cross donation 
                                  #> consisting of trials where They give more than they gain
                                  #> Reverse coded where the Red Cross looses more than they gain
                                  #> consistent with prior investigations demonstrating a high correlation between these trial types
                                    #> we reverse code this with the logic that if they reject the Red Cross losing then they are for the Red Cross getting more 
                              #> 3 Calculation Trials
                              #> 4 Attention Trials 
                              #> 5 Equal trials - they both get and give the same amount
  condition <- rep(0,length(dda1[[i]][18:53,10]))
  condition[c(1,14,16,28,29,2,4,8,24,33)] <- 1 # for self
  condition[c(13,19,26,27,31,7,15,18,21,34)] <- 2 # for RC
  condition[grep("Calcul",dda1[[i]][18:53,10])] <- 3 # calculation 
  condition[grep("attention",dda1[[i]][18:53,10])] <- 4 # attention 
  condition[c(11,35)] <- 5 # equal
  
  # creating the data frame
  choice <- data.frame(rt,response,condition)
  
  # Time frame DDM model measured against
  tt = seq(0 , (max(rt) + .2), .2)
  
  # DDM model
  res = estDstarM(data = choice, tt = tt, splits = c(1:5)) # splits denotes trial types for non-decision time
  # Non Decision
  nd_list[[i]] <- estND(res)$descriptives[c(2,4,6)]
  # DDM values
  ddm_list[[i]] <- coef(res)
  
}


# Making dataframes
    # DDM
(ddm_vals = do.call(rbind, ddm_list))
# Non-decision
(nd_vals = do.call(rbind, nd_list))

    # data frames
ddm <- data.frame(ddm_vals)
nd <- data.frame(nd_vals)
colnames(nd) <- c("nd_median", "nd_mean", "nd_mode")




























































































