#' Print Summary
#' 
#' Wrapper for summary
#' 
#' @export
#' @param df_full some object or dataset
printsummary <- function(df_full, dvname, preddv){
  #override default 
  options(max.print=99999999);
  options(width=120);
	
clean_df<-function(inp_df)
{
#read the data
data<-inp_df

#consider target variable name given as input in HybridFS function as DV'
names(data)[names(data)==dvname] <- "DV"
#names(data)[names(data)==target.var.name] <- "DV" 

#get the list of categorical variables
cat_var=data.frame()

df_temp<-inp_df
df_temp<-df_temp[, names(df_temp) != dvname] 


char <- df_temp[,sapply(df_temp,is.character)]
cat_var<-char

#get the list of factor variables 
fac <- df_temp[,sapply(df_temp,is.factor)]
cat_var<-cbind(cat_var,fac)

#get the list of logical variables 
logcl <- df_temp[,sapply(df_temp, is.logical)]
cat_var<-cbind(cat_var,logcl)

#removing the categorical variables in cat_var 
#names(df_temp)
for(i in names(cat_var))
{
  df_temp<-df_temp[, names(df_temp) != i] 
}

names(df_temp)
#determining other categorical variables with less than 52 levels
unique_lvl_cnt<-df_temp[lengths(lapply(df_temp, unique))<=52]
typeof(unique_lvl_cnt)
cat_var<-cbind(cat_var,unique_lvl_cnt)

cat_var_names<-list()
cat_var_names<-names(cat_var)

#display the list of categorical variables
cat_var_names


######################################################################################################################################################
#Categorical variables treatment
######################################################################################################################################################
#Steps
#Replace missing values as "Missing" in categorical variables 
#Check for categorical variables with more than 52 levels and reduce them to the top 10 levels that occur frequently 

#convert categorical variables to factors
for(j in cat_var_names)
{
  data[,j]<-as.factor(data[,j])
}

#summary(data)
#sapply(data,class)


#Identify replace the missing values as Missing in categorical variables
df_cat <- data[,sapply(data,is.factor)]
#names(df_cat)
#View(df_cat)
#sapply(df_cat,class)

for (i in 1:ncol(df_cat))
{
  levels <- levels(df_cat[,i])
  #class(levels)  
  
  if('Missing' %in% levels)
  {}else{
    levels[length(levels) + 1] <- 'Missing'
    df_cat[,i] <- factor(df_cat[,i], levels = levels)          
  }
  
  # refactor to include "Missing" as a factor level
  # and replace NA, null and blanks with "Missing"
  df_cat[,i] <- factor(df_cat[,i], levels = levels)
  df_cat[,i][is.na(df_cat[,i])] <- "Missing"
  df_cat[,i][is.null(df_cat[,i])] <- "Missing"
  df_cat[,i] <- sub("^$", "Missing", df_cat[,i])
  df_cat[,i]<-as.factor(df_cat[,i])
}

#summary(df_cat)

#Check for categorical variables with more than 52 levels and reduce them to the top 10 levels that
#occur frequently 
for(i in names(df_cat))
{
  column<-df_cat[,i]
  uniq_lvls_cnt <- length(unique(column))
  temp<-as.data.frame(column)
  if (uniq_lvls_cnt>52)
  { temp<-data.frame()
  cat_freq_cnt <- data.frame(table(column))
  cat_freq_cnt <- cat_freq_cnt[ which(cat_freq_cnt$column!='Missing' ),]
  cat_sort <- cat_freq_cnt[order(-cat_freq_cnt$Freq),]
  top_1<-head(cat_sort,10)
  top<-as.character(top_1[,1])
  
  data_cnt<-length(column)
  
  
  levels = unique(column)
  levels=as.character(levels)
  temp <- factor(temp, levels = levels)          
  
  if('Missing' %in% levels)
  {}else{
    levels[length(levels) + 1] <- 'Missing'
    temp <- factor(temp, levels = levels)          
  }
  
  for(k in 1:data_cnt)
  {
    value<-column[k]
    if(value %in% top)
    {
      temp<-rbind(temp,as.character(value))
    }else
    { 
      temp<-rbind(temp,'Missing')
    }
  }}
  df_cat[,i]<-temp
}

data[,names(df_cat)]<-df_cat[,names(df_cat)]

######################################################################################################################################################
#continuous variables treatment
######################################################################################################################################################

#Steps
#first get the correlation matrix
#get the variable pairs that highly correlated
#bin the variables using woe binning
#get the significance of these binned variables using chi square test. 
#Remove variables from highly correlated variable list that are not significant
#check if muliticollinearity still exists and keep removing variables until vif drops below 5 for all variables
#get the binned version of the continuous variables 

df<-data

#Removing categorical variables from data
for(i in names(cat_var))
{
  df<-df[, names(df) != i] 
}


df1<-df

#creating correlation matrix for cotinuous variables
df1<-df1[complete.cases(df1), ]
corr<-round(cor(df1),2)
corr_val<-corr

corr_val[lower.tri(corr_val,diag=TRUE)]=NA # make all the diagonal elements as NA
corr_val<-as.data.frame(as.table(corr_val)) # as a dataframe
corr_val<-na.omit(corr_val) # remove NA
corr_val<-corr_val[with(corr_val, order(-Freq)), ] # order by correlation
corr_test_var<-subset(corr_val,Freq>=0.85)


#adding ".binned" to each variable 
corr_test_var$Var1<-paste(corr_test_var$Var1,"binned",sep = ".")
corr_test_var$Var2<-paste(corr_test_var$Var2,"binned",sep = ".")


#woe binning
var_del<-as.character(names(df))

binning <- woeBinning::woe.binning(df, 'DV', df)
tabulate.binning <- woeBinning::woe.binning.table(binning)

data_cont_binned <- woeBinning::woe.binning.deploy(data, binning)
names(data_cont_binned)

#removing original values of variables that have been binned
for(i in var_del)
{
  data_cont_binned<-data_cont_binned[, names(data_cont_binned) != i] 
}

data_cont_binned[is.na(data_cont_binned)] <- "Missing"

data_cont_binned$DV<-data$DV

#getting the correlated variables as a unique list
corr_var<-list()
corr_var<-corr_test_var$Var1
corr_var<-c(corr_var,corr_test_var$Var2)
corr_var_unique<-unique(corr_var)


#getting the chi sq for each highly correlated variable
corr_var_chsq <- data.frame()

for(each in corr_var_unique)
{
  
  p_val=chisq.test((data_cont_binned[[each]]),data_cont_binned$DV)$p.value
  c <- data.frame('Var_name' = each,'p_value' = p_val)
  corr_var_chsq <- rbind(corr_var_chsq,c)
}

#remove the highly correlated variables that are not significant
corr_var_insig<-as.character(corr_var_chsq[which(corr_var_chsq$p_value>0.05),1])

#stripping off the "'binned" from variable names
corr_var_insig_strip<-substr(corr_var_insig, 1, nchar(corr_var_insig)-7) 

#removing the insignificant variables 
for (f in corr_var_insig) {
  df1[[f]] <- NULL
}


#checking if we still have multi collinearity and removing variables with very high vif until no such variable exists
# Loading required packages
library(car)
library(plyr)

# Fit a model to the data
fit=glm(DV ~ ., data=df1)

# Calculating VIF for each independent variable
vif(fit)

# Set a VIF threshold. All the variables having higher VIF than threshold
#are dropped from the model
threshold=5

# Sequentially drop the variable with the largest VIF until
# all variables have VIF less than threshold
drop=TRUE

aftervif=data.frame()
while(drop==TRUE) {
  vfit=vif(fit)
  aftervif=rbind.fill(aftervif,as.data.frame(t(vfit)))
  if(max(vfit)>threshold) { fit=
    update(fit,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
  else { drop=FALSE }}

# How variables were removed sequentially
t_aftervif= as.data.frame(t(aftervif))

# Final (uncorrelated) variables and their VIFs
vfit_d= as.data.frame(vfit)

rem_var<-as.character(rownames(vfit_d))

#retaining only the uncorrelated variables in the final data
df<-df[ , rem_var]

#getting the concatenated version of variables that needs to be retained
rem_var<-paste(rem_var,"binned",sep=".")

#getting the binned continuous variables 
data_cont_binned_fin<-data_cont_binned[,rem_var]

######################################################################################################################################################
#getting the final data frame with the required variables
######################################################################################################################################################

final_data_after_processing=data.frame()

final_data_after_processing<-data[,names(df_cat)]

final_data_after_processing<-cbind(final_data_after_processing,data_cont_binned_fin)

return(final_data_after_processing)
}

  
    ##Calling the Data Cleaning function HERE!!
   #df_full<-clean_df(df_full);
    write.csv(df_full,"C:/opencpuapp_ip/cleaned_data.csv");  
  #print
  #print(summary(read.csv(paste("c:/opencpuapp_ip/",substr("c:/fakepath/train_comp.csv",13,nchar("c:/fakepath/train_comp.csv")),sep=""))))
  print(summary(df_full))
  invisible()
}
