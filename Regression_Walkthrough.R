
install.packages("readxl")
install.packages("dplyr")
require("readxl")
require("dplyr")
library(tidyverse)
getwd()

setwd('S:/Sams things/homework/UCSB 2018-/PSTAT 126 -  Regression Analysis/Group project/states2/states/data')

filenames <- list.files()
filenames # should see the name of each year of data
             #### function to get variables ### 
get_variable <- function(var,area){
  x <- data.frame()
  count = 0
  for(year in 2003:2011)
  {
    count = count + 1
    data <- read_excel(filenames[count])
    index <- which(data$Item == var)
    if(length(index)!=1)
      index = index[1]
    val <- data[index,toupper(area)]
    new <- data.frame(year = year, `var` = as.numeric(val))
    x <- bind_rows(x,new)
  }
  for(year in 2012:2018)
  {
    count = count + 1
    data <- read_excel(filenames[count])
    index <- which(data$`(Thousands of Dollars)` == var)
    if(length(index)!=1)
      index = index[1]
    val <- data[index,area]
    new <- data.frame(year = year, var = as.numeric(val))
    x <- bind_rows(x,new)
  }
  names(x)[2] <- var
  
  return(x)
}

data <- read_excel(filenames[1])
data['Item']
get_state <- function(state){
  print(filenames)
}
######################################################################
######## Define Statistics to be used ################################
######################################################################
## Definitions: https://www.census.gov/govs/definitions/index.html ###
######### Potential Predictors US: Forms of income/growth ####

Total_revenue.US <-get_variable('Total revenue','United States') # Taxes + funds from Gov
Total_revenue.US
Taxes.US <- get_variable('Taxes','United States') # all other taxes are a subset of taxes
Ind_inc_tax.US <- get_variable('Individual income tax', 'United States') 
Corp_inc_tax.US <- get_variable('Corporate income tax', 'United States')
Sales_tax_gen.US <- get_variable('General sales', 'United States')
Sales_tax_sel.US <- get_variable('Selective sales', 'United States')
Sales_tax_net.US <- cbind(Sales_tax_gen.US[1], (Sales_tax_gen.US[2] + Sales_tax_sel.US[2])) # Gross sales tax
intergov_Rev.US <- get_variable('Intergovernmental revenue', 'United States')

######### Potential Predictors CA: Forms of income/growth ###########

Total_revenue.CA <-get_variable('Total revenue','California')
Taxes.CA <- get_variable('Taxes','California')
Ind_inc_tax.CA <- get_variable('Individual income tax', 'California') 
Corp_inc_tax.CA <- get_variable('Corporate income tax', 'California')
Sales_tax_gen.CA <- get_variable('General sales', 'California')
Sales_tax_sel.CA <- get_variable('Selective sales', 'California')
Sales_tax_net.CA <- cbind(Sales_tax_gen.CA[1], (Sales_tax_gen.CA[2] + Sales_tax_sel.CA[2])) # Net sales tax
intergov_Rev.CA <- get_variable('Intergovernmental revenue', 'California')

######### Potential Predictors NY: Forms of income/growth ###########

Total_revenue.NY <-get_variable('Total revenue','New York')
Taxes.NY <- get_variable('Taxes','New York')
Ind_inc_tax.NY <- get_variable('Individual income tax', 'New York') 
Corp_inc_tax.NY <- get_variable('Corporate income tax', 'New York')
Sales_tax_gen.NY <- get_variable('General sales', 'New York')
Sales_tax_sel.NY <- get_variable('Selective sales', 'New York')
Sales_tax_net.NY <- cbind(Sales_tax_gen.NY[1], (Sales_tax_gen.NY[2] + Sales_tax_sel.NY[2])) # Net sales tax
intergov_Rev.NY <- get_variable('Intergovernmental revenue', 'New York')

######### Potential Predictors FL: Forms of income/growth ###########

Total_revenue.FL <-get_variable('Total revenue','Florida')
Taxes.FL <- get_variable('Taxes','Florida')
Ind_inc_tax.FL <- get_variable('Individual income tax', 'Florida') 
Corp_inc_tax.FL <- get_variable('Corporate income tax', 'Florida')
Sales_tax_gen.FL <- get_variable('General sales', 'Florida')
Sales_tax_sel.FL <- get_variable('Selective sales', 'Florida')
Sales_tax_net.FL <- cbind(Sales_tax_gen.FL[1], (Sales_tax_gen.FL[2] + Sales_tax_sel.FL[2])) # Net sales tax
intergov_Rev.FL <- get_variable('Intergovernmental revenue', 'Florida')

######### Potential Predictors TX: Forms of income/growth ####

Total_revenue.TX <-get_variable('Total revenue','Texas')
Taxes.TX <- get_variable('Taxes','Texas')
Ind_inc_tax.TX <- get_variable('Individual income tax', 'Texas') 
Corp_inc_tax.TX <- get_variable('Corporate income tax', 'Texas')
Sales_tax_gen.TX <- get_variable('General sales', 'Texas')
Sales_tax_sel.TX <- get_variable('Selective sales', 'Texas')
Sales_tax_net.TX <- cbind(Sales_tax_gen.TX[1], (Sales_tax_gen.TX[2] + Sales_tax_sel.TX[2])) # Net sales tax
intergov_Rev.TX <- get_variable('Intergovernmental revenue', 'Texas')

#####################################################################
#####################################################################
#####################################################################


## Potenial Response Variables US: Forms of spending/inv.  ##########

Total_expenditure.US <- get_variable('Total expenditure','United States')
intergov_exp.US <- get_variable('Intergovernmental expenditure', 'United States')
Dir_exp.US <- get_variable('Direct expenditure', 'United States') # All expenditure - intergov
Education.US <- get_variable('Education', 'United States')
Welfare.US <- get_variable('Public welfare', 'United States')
Hospitals.US <- get_variable('Hospitals', 'United States')
Health.US <- get_variable('Health', 'United States')
Highways.US <- get_variable('Highways', 'United States')
Police.US <- get_variable('Police protection', 'United States')
Prisons.US <- get_variable('Correction', 'United States')
Nat_resources.US <- get_variable('Natural resources', 'United States')
Parks.US <- get_variable('Parks and recreation', 'United States')
Debt.US <- get_variable('Debt at end of fiscal year', 'United States')
Cash.US <- get_variable('Cash and security holdings', 'United States')
Net_cash.US <- cbind(Cash.US[1], (Cash.US[2] - Debt.US[2])) # Net Cash
Ann_Def.US <- cbind(Total_expenditure.US[1], (Total_revenue.US[2]  - Total_expenditure.US[2])) 

## Potenial Response Variables CA: Forms of spending/inv.  ##########

Total_expenditure.CA <- get_variable('Total expenditure','California')
intergov_exp.CA <- get_variable('Intergovernmental expenditure', 'California')
Dir_exp.CA <- get_variable('Direct expenditure', 'California') # All expenditure - intergov
Education.CA <- get_variable('Education', 'California')
Welfare.CA <- get_variable('Public welfare', 'California')
Hospitals.CA <- get_variable('Hospitals', 'California')
Health.CA <- get_variable('Health', 'California')
Highways.CA <- get_variable('Highways', 'California')
Police.CA <- get_variable('Police protection', 'California')
Prisons.CA <- get_variable('Correction', 'California')
Nat_resources.CA <- get_variable('Natural resources', 'California')
Parks.CA <- get_variable('Parks and recreation', 'California')
Debt.CA <- get_variable('Debt at end of fiscal year', 'California')
Cash.CA <- get_variable('Cash and security holdings', 'California')
Net_cash.CA <- cbind(Cash.CA[1], (Cash.CA[2] - Debt.CA[2])) # Net Cash
Ann_Def.CA <- cbind(Total_expenditure.CA[1], (Total_revenue.CA[2]  - Total_expenditure.CA[2])) 

## Potenial Response Variables NY: Forms of spending/inv.  ##########

Total_expenditure.NY <- get_variable('Total expenditure','New York')
intergov_exp.NY <- get_variable('Intergovernmental expenditure', 'New York')
Dir_exp.NY <- get_variable('Direct expenditure', 'New York') # All expenditure - intergov
Education.NY <- get_variable('Education', 'New York')
Welfare.NY <- get_variable('Public welfare', 'New York')
Hospitals.NY <- get_variable('Hospitals', 'New York')
Health.NY <- get_variable('Health', 'New York')
Highways.NY <- get_variable('Highways', 'New York')
Police.NY <- get_variable('Police protection', 'New York')
Prisons.NY <- get_variable('Correction', 'New York')
Nat_resources.NY <- get_variable('Natural resources', 'New York')
Parks.NY <- get_variable('Parks and recreation', 'New York')
Debt.NY <- get_variable('Debt at end of fiscal year', 'New York')
Cash.NY <- get_variable('Cash and security holdings', 'New York')
Net_cash.NY <- cbind(Cash.NY[1], (Cash.NY[2] - Debt.NY[2])) # Net Cash
Ann_Def.NY <- cbind(Total_expenditure.NY[1], (Total_revenue.NY[2]  - Total_expenditure.NY[2])) 

## Potenial Response Variables FL: Forms of spending/inv.  ##########

Total_expenditure.FL <- get_variable('Total expenditure','Florida')
intergov_exp.FL <- get_variable('Intergovernmental expenditure', 'Florida')
Dir_exp.FL <- get_variable('Direct expenditure', 'Florida') # All expenditure - intergov
Education.FL <- get_variable('Education', 'Florida')
Welfare.FL <- get_variable('Public welfare', 'Florida')
Hospitals.FL <- get_variable('Hospitals', 'Florida')
Health.FL <- get_variable('Health', 'Florida')
Highways.FL <- get_variable('Highways', 'Florida')
Police.FL <- get_variable('Police protection', 'Florida')
Prisons.FL <- get_variable('Correction', 'Florida')
Nat_resources.FL <- get_variable('Natural resources', 'Florida')
Parks.FL <- get_variable('Parks and recreation', 'Florida')
Debt.FL <- get_variable('Debt at end of fiscal year', 'Florida')
Cash.FL <- get_variable('Cash and security holdings', 'Florida')
Net_cash.FL <- cbind(Cash.FL[1], (Cash.FL[2] - Debt.FL[2])) # Net Cash
Ann_Def.FL <- cbind(Total_expenditure.FL[1], (Total_revenue.FL[2]  - Total_expenditure.FL[2])) 

## Potenial Response Variables TX: Forms of spending/inv.  ##########

Total_expenditure.TX <- get_variable('Total expenditure','Texas')
intergov_exp.TX <- get_variable('Intergovernmental expenditure', 'Texas')
Dir_exp.TX <- get_variable('Direct expenditure', 'Texas') # All expenditure - intergov
Education.TX <- get_variable('Education', 'Texas')
Welfare.TX <- get_variable('Public welfare', 'Texas')
Hospitals.TX <- get_variable('Hospitals', 'Texas')
Health.TX <- get_variable('Health', 'Texas')
Highways.TX <- get_variable('Highways', 'Texas')
Police.TX <- get_variable('Police protection', 'Texas')
Prisons.TX <- get_variable('Correction', 'Texas')
Nat_resources.TX <- get_variable('Natural resources', 'Texas')
Parks.TX <- get_variable('Parks and recreation', 'Texas')
Debt.TX <- get_variable('Debt at end of fiscal year', 'Texas')
Cash.TX <- get_variable('Cash and security holdings', 'Texas')
Net_cash.TX <- cbind(Cash.TX[1], (Cash.TX[2] - Debt.TX[2])) # Net Cash
Ann_Def.TX <- cbind(Total_expenditure.TX[1], (Total_revenue.TX[2]  - Total_expenditure.TX[2])) 

#####################################################################
### Statistics that have been created     ###########################
# annual deficit ==> Total Rev - Total exp
# Net cash position ==> Cash and security holdings - Debt at end of fiscal year
# Income tax net ==> Individual income tax + corporate income tax

#####################################################################
#####################################################################
#####################################################################


#### Research questions - Proposed Response #########################
#####################################################################
# Growth of states income - Total rev or Taxes (doesn't include money flow between states and Fed)

# Growth of Cash position - Net_cash



######################################################################
######################################################################
######################################################################
## scatter plot matrix
pairs(~CA_df$`Taxes.CA$Taxes` + CA_df$`Cash and security holdings` + CA_df$`Total expenditure`
      + CA_df$`Direct expenditure`)

pairs(~CA_df$`Taxes.CA$Taxes` + CA_df$`Public welfare` + CA_df$`Police protection`
      + CA_df$Hospitals )
pairs(~CA_df$`Taxes.CA$Taxes` + CA_df$Education + CA_df$Correction + CA_df$Highways)

######################################################################
######################################################################
######################################################################
### Model for Growth using taxes: CA #################################

CA_df <- cbind(Total_expenditure.CA, Dir_exp.CA[2], Net_cash.CA[2], Ann_Def.CA[2], Education.CA[2],
               Prisons.CA[2], Welfare.CA[2], Police.CA[2], Prisons.CA[2], Hospitals.CA[2], Hospitals.CA[2],
               Highways.CA[2], Taxes.CA$Taxes )
CA_df

################### Step-wise regression #############################
Model_growth.CA <- lm(CA_df$`Taxes.CA$Taxes` ~ 1) # base model is fully reduced
### use add1() to iterate through predictors
add1(Model_growth.CA, ~. + CA_df$`Cash and security holdings` + CA_df$`Total expenditure`
     + CA_df$`Direct expenditure` + CA_df$Education + CA_df$Correction
     + CA_df$`Public welfare` + CA_df$`Police protection`
     + CA_df$Hospitals + CA_df$Highways, test = 'F' ) # need to pull from CA_df to insert into model
#### shows which variable to add next: notes 12 #######################
#### update model ####
Model_growth.CA1 <- update(Model_growth.CA, ~.+ CA_df$`Total expenditure`) # add best predictor from initial pass

################################# test which predictor to add next ##
add1(Model_growth.CA1, ~. + CA_df$`Cash and security holdings` + 
       CA_df$`Direct expenditure` + CA_df$Education + CA_df$Correction
     + CA_df$`Public welfare` + CA_df$`Police protection`
     + CA_df$Hospitals + CA_df$Highways, test = 'F' ) # flags education as the only sig stat.

################################ update model ########################
Model_growth.CA2 <- update(Model_growth.CA1, ~. + CA_df$Education)

############################# Check all predictors still sig #########
summary(Model_growth.CA2) # total expenditure no longer sig. #########

############################# Remove non sig predictors ##############

Model_growth.CA3 <- update(Model_growth.CA, ~. + CA_df$Education)

summary(Model_growth.CA3) # all sig

############################ Check for next variable #################
add1(Model_growth.CA3, ~. + CA_df$`Cash and security holdings` + 
       CA_df$`Direct expenditure` + CA_df$Correction
     + CA_df$`Public welfare` + CA_df$`Police protection`
     + CA_df$Hospitals + CA_df$Highways, test = 'F' ) # police protection

############################# Update model ###########################
Model_growth.CA4 <- update(Model_growth.CA3, ~. + CA_df$`Police protection`)

########################## Check significance of predictors in model #
summary(Model_growth.CA4)   #### all sig continue iterations

add1(Model_growth.CA4, ~. + CA_df$`Cash and security holdings` + 
       CA_df$`Direct expenditure` + CA_df$Correction
     + CA_df$`Public welfare` + CA_df$Hospitals + CA_df$Highways, test = 'F' ) # Highways

############################# Update model ###########################
Model_growth.CA5 <- update(Model_growth.CA4, ~. + CA_df$Highways)

########################## Check significance of predictors in model #
summary(Model_growth.CA5) # all sig

############################ Check for next variable #################
add1(Model_growth.CA5, ~. + CA_df$`Cash and security holdings` + 
       CA_df$`Direct expenditure` + CA_df$Correction
     + CA_df$`Public welfare` + CA_df$Hospitals, test = 'F') # no sig predictors to be added to model 

########################### Plots ####################################
plot(Model_growth.CA5) # residuals seem alright, Normal Q-Q is questionable




######################################################################
######################################################################
######################################################################




### Model for Growth using taxes: US ################################

US_df <- cbind(Total_expenditure.US, Dir_exp.US[2], Net_cash.US[2], Ann_Def.US[2], Education.US[2],
               Prisons.US[2], Welfare.US[2], Police.US[2], Prisons.US[2], Hospitals.US[2], Hospitals.US[2],
               Highways.US[2], Taxes.US$Taxes )
US_df

################### Step-wise regression #############################
US_mod0 <- lm(US_df$`Taxes.US$Taxes` ~ 1) # base model is fully reduced


############################ Check for next variable #################
add1(US_mod0, ~. + US_df$`Cash and security holdings` + US_df$`Total expenditure`
     + US_df$`Direct expenditure` + US_df$Education + US_df$Correction
     + US_df$`Public welfare` + US_df$`Police protection`
     + US_df$Hospitals + US_df$Highways, test = 'F' ) # Highways has most sig.

############################# Update model ###########################
US_mod1 <- update(US_mod0, ~.+ US_df$Highways) 


########################## Check significance of predictors in model #
summary(US_mod1)

############################ Check for next variable #################
add1(US_mod1, ~. + US_df$`Cash and security holdings` + US_df$`Total expenditure`
     + US_df$`Direct expenditure` + US_df$Education + US_df$Correction
     + US_df$`Public welfare` + US_df$`Police protection`
     + US_df$Hospitals, test = 'F' ) # nothing significant 

######################################################################
######################################################################
######################################################################




### Model for Growth using taxes: FL  ################################
FL_df <- cbind(Total_expenditure.FL, Dir_exp.FL[2], Net_cash.FL[2], Ann_Def.FL[2], Education.FL[2],
               Prisons.FL[2], Welfare.FL[2], Police.FL[2], Prisons.FL[2], Hospitals.FL[2], Hospitals.FL[2],
               Highways.FL[2], Taxes.FL$Taxes )
FL_df

################### Step-wise regression #############################
FL_mod0 <- lm(FL_df$`Taxes.FL$Taxes` ~ 1) # base model is fully reduced


############################ Check for next variable #################
add1(FL_mod0, ~. + FL_df$`Cash and security holdings` + FL_df$`Total expenditure`
     + FL_df$`Direct expenditure` + FL_df$Education + FL_df$Correction
     + FL_df$`Public welfare` + FL_df$`Police protection`
     + FL_df$Hospitals + FL_df$Highways, test = 'F' ) # highways

############################# Update model ###########################
FL_mod1 <- update(FL_mod0,~.+ FL_df$Highways)
########################## Check significance of predictors in model #
summary(FL_mod1)
############################ Check for next variable #################
add1(FL_mod1, ~. + FL_df$`Cash and security holdings` + FL_df$`Total expenditure`
     + FL_df$`Direct expenditure` + FL_df$Education + FL_df$Correction
     + FL_df$`Public welfare` + FL_df$`Police protection`
     + FL_df$Hospitals, test = 'F' ) # nothing sig - need new method




######################################################################
######################################################################
######################################################################




### Model for Growth using taxes: NY  ################################
NY_df <- cbind(Total_expenditure.NY, Dir_exp.NY[2], Net_cash.NY[2], Ann_Def.NY[2], Education.NY[2],
               Prisons.NY[2], Welfare.NY[2], Police.NY[2], Prisons.NY[2], Hospitals.NY[2], Hospitals.NY[2],
               Highways.NY[2], Taxes.NY$Taxes )
NY_df

################### Step-wise regression #############################
NY_mod0 <- lm(NY_df$`Taxes.NY$Taxes` ~ 1) # base model is fully reduced


############################ Check for next variable #################
add1(NY_mod0, ~. + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
     + NY_df$`Direct expenditure` + NY_df$Education + NY_df$Correction
     + NY_df$`Public welfare` + NY_df$`Police protection`
     + NY_df$Hospitals + NY_df$Highways, test = 'F' ) # total expend
############################# Update model ###########################
NY_mod1 <- update(NY_mod0, ~. + NY_df$`Total expenditure`)
########################## Check significance of predictors in model #
summary(NY_mod1)

############################ Check for next variable #################
add1(NY_mod0, ~. + NY_df$`Cash and security holdings`
     + NY_df$`Direct expenditure` + NY_df$Education + NY_df$Correction
     + NY_df$`Public welfare` + NY_df$`Police protection`
     + NY_df$Hospitals + NY_df$Highways, test = 'F' ) # direct expend

############################# Update model ###########################
NY_mod2 <- update(NY_mod1, ~. + NY_df$`Direct expenditure`)
########################## Check significance of predictors in model #
summary(NY_mod2) # direct expenditure not sig (just flagged as most sig)
# revert back to mod1, update to include education which was runner up)

############################# Update model ###########################
NY_mod3 <- update(NY_mod1, ~. + NY_df$Education)

########################## Check significance of predictors in model #
summary(NY_mod3) # education not sig, Try again not using Totel exp
# as the first predictor use runner up there and see what happens


######################### Reverting back to mod0 for 3rd attempt #####

NY_mod4 <- update(NY_mod0, ~. + NY_df$`Direct expenditure`)

########################## Check significance of predictors in model #
summary(NY_mod4) # only 1 predictor so shouldn't ever be a problem

############################ Check for next variable #################
add1(NY_mod4, ~. + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
     + NY_df$Education + NY_df$Correction
     + NY_df$`Public welfare` + NY_df$`Police protection`
     + NY_df$Hospitals + NY_df$Highways, test = 'F' ) # total expend and education only sig Same as before

############################# Update model ###########################
NY_mod5 <- update(NY_mod4, ~. + NY_df$Education)
########################## Check significance of predictors in model #
summary(NY_mod5) # both predictors sig


############################ Check for next variable #################
add1(NY_mod5, ~. + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
     + NY_df$Correction
     + NY_df$`Public welfare` + NY_df$`Police protection`
     + NY_df$Hospitals + NY_df$Highways, test = 'F' ) # hospitals sig at .06424 | welfare 0.06758


############################# Update model ###########################
NY_mod6 <- update(NY_mod5, ~. + NY_df$Hospitals)
########################## Check significance of predictors in model #
summary(NY_mod6) # all sig
############################ Check for next variable #################
add1(NY_mod6, ~. + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
     + NY_df$Correction
     + NY_df$`Public welfare` + NY_df$`Police protection`
     + NY_df$Highways, test = 'F' ) # nothing to add model complete ###################################

######################## Alternative NY model if using welfare #######
NY_mod7 <- update(NY_mod5, ~. + NY_df$`Public welfare`)

########################## Check significance of predictors in model #
summary(NY_mod7) # direct expenditure needs to be removed

########################## Update model ##############################
NY_mod8 <- lm(NY_df$`Taxes.NY$Taxes` ~ NY_df$Education + NY_df$`Public welfare`)

########################## Check significance of predictors in model #
summary(NY_mod8) # all sig

############################ Check for next variable #################
add1(NY_mod8, ~. + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
     + NY_df$Correction
     + NY_df$`Police protection`
     + NY_df$Highways, test = 'F' ) # nothing to add model complete ###################################



######################################################################
######################################################################
######################################################################




### Model for Growth using taxes: TX  ################################
TX_df <- cbind(Total_expenditure.TX, Dir_exp.TX[2], Net_cash.TX[2], Ann_Def.TX[2], Education.TX[2],
               Prisons.TX[2], Welfare.TX[2], Police.TX[2], Prisons.TX[2], Hospitals.TX[2], Hospitals.TX[2],
               Highways.TX[2], Taxes.TX$Taxes )
TX_df

################### Step-wise regression #############################
TX_mod0 <- lm(TX_df$`Taxes.TX$Taxes` ~ 1) # base model is fully reduced


############################ Check for next variable #################
add1(TX_mod0, ~. + TX_df$`Cash and security holdings` + TX_df$`Total expenditure`
     + TX_df$`Direct expenditure` + TX_df$Education + TX_df$Correction
     + TX_df$`Public welfare` + TX_df$`Police protection`
     + TX_df$Hospitals + TX_df$Highways, test = 'F' ) # Hospitals
############################# Update model ###########################
TX_mod1 <- update(TX_mod0, ~. + TX_df$Hospitals)
########################## Check significance of predictors in model #
summary(TX_mod1)

############################ Check for next variable #################
add1(TX_mod1, ~. + TX_df$`Cash and security holdings`
     + TX_df$`Direct expenditure` + TX_df$Education + TX_df$Correction
     + TX_df$`Public welfare` + TX_df$`Police protection`
     + TX_df$Highways, test = 'F' ) # EDUCATION


############################# Update model ###########################
TX_mod2 <- update(TX_mod1, ~. + TX_df$Education)

########################## Check significance of predictors in model #
summary(TX_mod2)

############################ Check for next variable #################
add1(TX_mod2, ~. + TX_df$`Cash and security holdings`
     + TX_df$`Direct expenditure` + TX_df$Correction
     + TX_df$`Public welfare` + TX_df$`Police protection`
     + TX_df$Highways, test = 'F' ) # Public welfare

############################# Update model ###########################
TX_mod3 <- update(TX_mod2, ~. + TX_df$`Public welfare`)

########################## Check significance of predictors in model #
summary(TX_mod3) # all sig

############################ Check for next variable #################
add1(TX_mod3, ~. + TX_df$`Cash and security holdings` + TX_df$`Total expenditure`
     + TX_df$`Direct expenditure` + TX_df$Correction
     + TX_df$`Police protection`
     + TX_df$Highways, test = 'F' ) # model complete 




######################################################################
######################################################################
######################################################################

######################## Akaike's Information Criterion ##############
######################################################################

###################### US ############################################
US0 <- lm(US_df$`Taxes.US$Taxes`  ~ 1) # base 
US.upper <- lm( US_df$`Taxes.US$Taxes` ~  + US_df$`Cash and security holdings` + US_df$`Total expenditure`
                + US_df$`Direct expenditure` + US_df$Education + US_df$Correction
                + US_df$`Public welfare` + US_df$`Police protection`
                + US_df$Hospitals + US_df$Highways ) # upper limit with all potential predictors| Note: not all potential response variables but those used
# while building the models using stepwise regression

########################### Identify subsets ##########################
step(US0, scope = list(lower = US0, upper = US.upper))

############################ Final Model ##############################
US1 <- lm(US_df$`Taxes.US$Taxes` ~ US_df$Highways) # same model as previous stepwise method 


######################## Akaike's Information Criterion ##############
######################################################################

###################### CA ############################################
CA0 <- lm(CA_df$`Taxes.CA$Taxes`  ~ 1) # base 
CA.upper <- lm( CA_df$`Taxes.CA$Taxes` ~  + CA_df$`Cash and security holdings` + CA_df$`Total expenditure`
                + CA_df$`Direct expenditure` + CA_df$Education + CA_df$Correction
                + CA_df$`Public welfare` + CA_df$`Police protection`
                + CA_df$Hospitals + CA_df$Highways )
rstudent(CA.upper)

########################### Identify subsets ##########################
step(CA0, scope = list(lower = CA0, upper = CA.upper))

############################ Final Model ##############################
CA1 <- lm(CA_df$`Taxes.CA$Taxes` ~ CA_df$Education + CA_df$Highways +
            CA_df$`Police protection` + CA_df$Correction) # same model as previous but with addition of corrections


######################################################################


######################## Akaike's Information Criterion ##############
######################################################################

###################### NY ############################################
NY0 <- lm(NY_df$`Taxes.NY$Taxes`  ~ 1) # base 
NY.upper <- lm( NY_df$`Taxes.NY$Taxes` ~  + NY_df$`Cash and security holdings` + NY_df$`Total expenditure`
                + NY_df$`Direct expenditure` + NY_df$Education + NY_df$Correction
                + NY_df$`Public welfare` + NY_df$`Police protection`
                + NY_df$Hospitals + NY_df$Highways )

########################### Identify subsets ##########################
step(NY0, scope = list(lower = NY0, upper = NY.upper))

############################ Final Model ##############################
NY1 <- lm(formula = NY_df$`Taxes.NY$Taxes` ~ NY_df$`Total expenditure` + 
            NY_df$`Direct expenditure` + NY_df$`Public welfare`) # Novel model


######################################################################

######################## Akaike's Information Criterion ##############
######################################################################

###################### FL ############################################
FL0 <- lm(FL_df$`Taxes.FL$Taxes`  ~ 1) # base 
FL.upper <- lm( FL_df$`Taxes.FL$Taxes` ~  + FL_df$`Cash and security holdings` + FL_df$`Total expenditure`
                + FL_df$`Direct expenditure` + FL_df$Education + FL_df$Correction
                + FL_df$`Public welfare` + FL_df$`Police protection`
                + FL_df$Hospitals + FL_df$Highways )

########################### Identify subsets ##########################
step(FL0, scope = list(lower = FL0, upper = FL.upper))

############################ Final Model ##############################
FL1 <- lm(formula = FL_df$`Taxes.FL$Taxes` ~ FL_df$Highways + FL_df$`Police protection` + 
            FL_df$`Cash and security holdings`) # Novel model


######################################################################

######################## Akaike's Information Criterion ##############
######################################################################

###################### TX ############################################
TX0 <- lm(TX_df$`Taxes.TX$Taxes`  ~ 1) # base 
TX.upper <- lm( TX_df$`Taxes.TX$Taxes` ~  + TX_df$`Cash and security holdings` + TX_df$`Total expenditure`
                + TX_df$`Direct expenditure` + TX_df$Education + TX_df$Correction
                + TX_df$`Public welfare` + TX_df$`Police protection`
                + TX_df$Hospitals + TX_df$Highways )

########################### Identify subsets ##########################
step(TX0, scope = list(lower = TX0, upper = TX.upper))

############################ Final Model ##############################
TX1 <- lm(formula = TX_df$`Taxes.TX$Taxes` ~ TX_df$Hospitals + TX_df$Education + 
            TX_df$`Public welfare` + TX_df$Highways + TX_df$Correction) # new model 


######################################################################

######################################################################
######################################################################
######################################################################

######################### Step wise regression models ################
######################### F - test ###################################
######################################################################
######################################################################

############ US ###
US_mod1 # Only highways was significant: try another method

############ CA ###

Model_growth.CA5 # Education, police protection, Highways

############ FL ###
FL_mod1 # highways only: method fail?


############ NY ###
NY_mod6 # Dir exp, Edu, Hosp

NY_mod8 # Edu, Welfare

############ TX ###
TX_mod3 # hospitals, education, public welfare

############################# AIC - test ############################

############ US ###
US1 # Only highways was significant: try another method

############ CA ###

CA1 # Education, police protection, Highways

############ FL ###
FL1 # Highways, Police protection, Cash and security holdings 


############ NY ###
NY1 # Total expenditure, Direct expenditure, public welfare

############ TX ###
TX1 # hospitals, education, public welfare, Highways, Correction



############################ Best Subset regression #################
install.packages('leaps')
library(leaps)
################################## US ###############################
# can use df from before for x variables in regsubsets(), need to remove taxes and year
US_df
US_predictors <- subset(US_df, select = -c(`Taxes.US$Taxes`, year, `Total revenue`))
US_predictors
US_bsr <- regsubsets(US_predictors, US_df$`Taxes.US$Taxes` )
summary.US_bsr <- summary(US_bsr)
summary.US_bsr$which # defines each best fit model with T/F matrix
summary.US_bsr$rsq # shows r^2 of each model, no significant jumps in R^2 at any point

########################## Model #####################################
US_bsr_fit <- lm(US_df$`Taxes.US$Taxes` ~ US_df$`Public welfare` + US_df$Highways)

######################################################################
######################################################################
################################## CA ################################
CA_predictors <- subset(CA_df, select = -c(`Taxes.CA$Taxes`, year, `Total revenue`))
CA_predictors

###################  regsubsets() ####################################
CA_bsr <- regsubsets(CA_predictors, CA_df$`Taxes.CA$Taxes`)

################################ Summary and results #################
summary.CA_bsr <- summary(CA_bsr)
summary.CA_bsr$which
summary.CA_bsr$rsq # second jump is the last significant jump, indicating a 3 predictor model as best
summary.CA_bsr$adjr2 # same as above ( in findings)

########################## Model #####################################
CA_bsr_fit <- lm(CA_df$`Taxes.CA$Taxes` ~ CA_df$Education + CA_df$Highways + CA_df$`Police protection`)

######################################################################
######################################################################
################################## NY ################################
NY_predictors <- subset(NY_df, select = -c(`Taxes.NY$Taxes`, year, `Total revenue`))
NY_predictors

###################  regsubsets() ####################################
NY_bsr <- regsubsets(NY_predictors, NY_df$`Taxes.NY$Taxes`)

################################ Summary and results #################
summary.NY_bsr <- summary(NY_bsr)
summary.NY_bsr$which
summary.NY_bsr$rsq # first jump is the largest, indicating a two predictor model as best

########################## Model #####################################
NY_bsr_fit <- lm(NY_df$`Taxes.NY$Taxes` ~ NY_df$`Public welfare` + NY_df$Education)

######################################################################
######################################################################
################################## FL ################################
FL_predictors <- subset(FL_df, select = -c(`Taxes.FL$Taxes`, year, `Total revenue`))
FL_predictors

###################  regsubsets() ####################################
FL_bsr <- regsubsets(FL_predictors, FL_df$`Taxes.FL$Taxes`)

################################ Summary and results #################
summary.FL_bsr <- summary(FL_bsr)
summary.FL_bsr$which
summary.FL_bsr$rsq # 2nd jump is the last influential: 3 predictors

########################## Model #####################################
FL_bsr_fit <- lm(FL_df$`Taxes.FL$Taxes` ~ FL_df$`Direct expenditure` + FL_df$`Total expenditure` + FL_df$`Police protection`)

######################################################################
######################################################################
################################## TX ################################
TX_predictors <- subset(TX_df, select = -c(`Taxes.TX$Taxes`, year, `Total revenue`))
TX_predictors

###################  regsubsets() ####################################
TX_bsr <- regsubsets(TX_predictors, TX_df$`Taxes.TX$Taxes`)

################################ Summary and results #################
summary.TX_bsr <- summary(TX_bsr)
summary.TX_bsr$which
summary.TX_bsr$rsq # second jump: 3 predictors

########################## Model #####################################
TX_bsr_fit <- lm(TX_df$`Taxes.TX$Taxes` ~ TX_df$Education + TX_df$`Public welfare`
                 + TX_df$Hospitals)

######################################################################
######################################################################
######################################################################

########################### Best subset models #######################
#################################### rsq #############################
############ US ###
US_bsr_fit # welfare, highways

############ CA ###

CA_bsr_fit # education, highways, police protection

############ FL ###
FL_bsr_fit # Highways, Police protection, Cash and security holdings 


############ NY ###
NY_bsr_fit # Education, public welfare

############ TX ###
TX_bsr_fit # hospitals, education, public welfare
