

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


# Writing the created dataframes so we can upload easier later
  # to reduce time estimating DDM
# write.csv(nd,"C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\ppr5_DDM\\pub_prosocial_DDM\\nd_data.csv")
# write.csv(ddm,"C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\ppr5_DDM\\pub_prosocial_DDM\\ddm_data.csv")



# loading calculated dataframes
  # to reduce time estimating DDM
nd <- read.csv("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\ppr5_DDM\\pub_prosocial_DDM\\nd_data.csv")
nd <- nd[,-1]
nd <- nd[,6:10]
ddm <- read.csv("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\1 Publications\\ppr5_DDM\\pub_prosocial_DDM\\ddm_data.csv")
ddm <- ddm[,-1]

# loading behavioral data
dat <- read.csv("C:\\Users\\wintersd\\OneDrive - The University of Colorado Denver\\DPRG main project\\Analyses and Data\\full_data_87\\full_dat_87.csv")




# data quality 
  # extracting attention and calculation trials
check = list()
for (i in c(1:length(dda1))){
  check[[i]] <- as.numeric(dda1[[i]][c(17+3, 17+5, 17+6, 17+9, 17+10, 17+12, 17+17, 17+20, 17+22, 17+23, 17+25, 17+30, 17+32, 17+36),42])
}

    # Creating data frame
(checks = do.call(rbind, check))
checks <- data.frame(checks)


  # recoding all vars to identify those to remove 
checked = checks
checked$X1 <- ifelse(checks$X1 == 2, 0,1)
checked$X2 <- ifelse(checks$X2 == 2, 0,1)
checked$X3 <- ifelse(checks$X3 == 2, 0,1)
checked$X4 <- ifelse(checks$X4 == 1, 0,1)
checked$X5 <- ifelse(checks$X5 == 1, 0,1)
checked$X6 <- ifelse(checks$X6 == 1, 0,1)
checked$X7 <- ifelse(checks$X7 == 1, 0,1)
checked$X8 <- ifelse(checks$X8 == 1, 0,1)
checked$X9 <- ifelse(checks$X9 == 1, 0,1)
checked$X10 <- ifelse(checks$X10 == 1, 0,1)
checked$X11 <- ifelse(checks$X11 == 2, 0,1)
checked$X12 <- ifelse(checks$X12 == 2, 0,1)
checked$X13 <- ifelse(checks$X13 == 2, 0,1)
checked$X14 <- ifelse(checks$X14 == 2, 0,1)

  # calculating percept correct
checked$perc <- apply(checked, 1, mean)

  # identifying those 3sd below the mean
    # this means they had a low % of correct responses
length(bd <- which(checked$perc < (mean(checked$perc) - 3*sd(checked$perc)))) # 3 in total 

  # identifying those with careless responses
bd1 <- c(which(dat$bad_resp1==1))

  # joining lists without duplicates to remove
remove <- c(bd,bd1)[!duplicated(c(bd,bd1))]
length(remove) # 15 participants to be removed 87-15 = 72


  # Removing participants
    ## checking participants are those we expect
remove == c(15, 16, 17, 19, 42, 45, 57, 62, 63, 69, 79, 80, 87, 8, 31) # true
    
ddms <- ddm[-remove, ]
dats <- dat[-remove, ]
nds <- nd[-remove, ]


    ## check to make sure same number of participants in each dataframe
NROW(ddms) # 72 participants - correct
NROW(ddms) == NROW(dats) # same
NROW(ddms) == NROW(nds) # same




  # combining dataframe 

dd <- cbind(dats,ddms)
dd <- cbind(dd, nds)



# Parameter names for drift diffusion 
  # a     Threshold Separation - amount for information that is considered for a decision 
              # (large values indicate conservative decision style)
              # typical range 0.5 - 2
  # v     Drift rate - average slope of the information accumulation process
              #> The drift gives information about the speed and direction of the accumulation of information. 
              #> Large (absolute) values of drift indicate a good performance. 
              #> If received information supports the response linked to the upper threshold the sign will be positive and vice versa. 
              #> Typical range: -5 < v < 5
  # z     Starting point - indicator of an a priori bias in decision making 
                ## this article demonstrates assessing bias using the starting point
                  #<https://www.jneurosci.org/content/32/7/2335> 
  # sz    Inter-trial-variability of starting point - Range of a uniform distribution with mean 'z' describing distribution of starting points
              # values different from 0 can predict **fast errors**
              # typical range between 0 and 0.5
  # sv    SD of normal distribution with mean 'v' describing the distribution of actual drift rates
              ## values different from 0 can predict **slow errors**
  # sv    SD of normal distribution with mean 'v' describing the distribution of actual drift rates
              ## values different from 0 can predict **slow errors**






# investigation of missing data

  # to examine missing data 
dat$miss <- rep(0, NROW(dat))
dat$miss[remove] <- 1


t.test(ddm$v1 ~ dat$miss)
t.test(ddm$v2 ~ dat$miss)


set.seed(1982)
lapply(dat[,c("age", 
                 "sex_male",
                 "icu_total" ,
                 "sex_male", 
                 "race_White", 
                 "sdq_total_cd")], 
       function(x) MKinfer::boot.t.test(x ~ dat$miss))

    #  nothing is statistically significant
       # Removing participants did not appear to introduce bias



## data descriptives
  # ddm  
as.data.frame(psych::describe(ddms))[-1,c(3:4,8:9)]
  # non decision time
as.data.frame(psych::describe(nds))[,c(3:4,8:9)]
  # demographic descriptives
    # age, icu, cd
as.data.frame(psych::describe(dats[,c("age", 
                                      "icu_total" , 
                                      "sdq_total_cd")]))[,c(3:4,8:9)]
    # sex
rbind(table(dats$sex_male), 
      round(table(dats$sex_male)/NROW(dats),2))
    # race white and non-white
rbind(table(dats$race_White), 
      round(table(dats$race_White)/NROW(dats),2))




# testing assumptions 

test_dat <- as.data.frame(cbind(
  dats[,c("icu_total" ,
          "age",
          "sex_male", 
          "race_White",
          "sdq_total_cd")],
  ddms[,-c(4:5,9:length(ddms))],
  nds[,1:2]))


### *Additivity - Multicollinerity*
corrt <- cor(test_dat)
symnum(corrt) 
  #  There are no '+', '*', or 'B' suggesting a high correlation betwen variables so these are fine. 


### *Normality of residuals*

random <- rchisq(nrow(test_dat), 10)
# creating a fake regression 
# remove the ID variable since you are regression the entire data set
fake <- lm(random~.,data=test_dat)
# creating studentized resiguals
studentized <- rstudent(fake)


psych::describe(studentized)
mean(fake$residuals)
    # mean is close to 0 no skewed or kurtotic so we are good. 


### *Distribution of residuals*
hist(studentized,breaks = 5,
     main="Histogram of Residuals", 
     xlab="Studentized Residuals", 
     border="black", 
     col="blue")

  # distribution looks like it has a slight tail but good



# residual plot
  # creating a z-scored fitted variable vector for interpretation 
fitted <- scale(fake$fitted.values)
  # residual plot - 
plot(fitted, studentized,
     main="Residual Plot of Standardized Fitted Values", 
     xlab="Fitted", 
     ylab = "Studentized",
     col="blue")
abline(0,0, col="red")
abline(v=0, col="red") 
    ## no noticeable pattern 

# test of autocorrelation 
car::durbinWatsonTest(fake)
  # Test is not statistically significant (p>.05) and the statistic is close to 2


### *Multicollinearity of predictors and controls*
library(olsrr)
ols_vif_tol(assumpt<-lm(random ~ icu_total + age + sex_male + race_White + sdq_total_cd + a1 + v1 + a2 + v2 + nd_mean_1 + nd_mean_2 + z1 + z2, data = test_dat))

  ## After including all variables that will be used as controls and predictors a 
    # only a small portion of each variable is filtered away meaning we do not have multicollinearity = assumption met.
      # indicated by all VIF < 2.5 all tolerance is >=.5




























































































