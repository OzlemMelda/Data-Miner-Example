
library(data.table)
train_data<-fread("valami_train.txt",header = FALSE)
test_data<-fread("valami_test.txt",header = FALSE)

#Cleaning####

  #check for missing values
    sapply(train_data,function(x) sum(is.na(x))) #no missing value
    sapply(test_data,function(x) sum(is.na(x))) #no missing value
  
  #check for unique values
    d<-sapply(train_data, function(x) length(unique(x)))
    e<-sapply(train_data, function(x) class(x))
  #columns with one unique value have no variance(each has just one level). We cannot do chi square test on them.
    columns_with_no_variance<-noquote(names(d[d==1]))
  
  #We eliminate the columns with just one unique variable in train data and same factors are eliminated in test data as well.
    train_data[,(columns_with_no_variance):=NULL]
    test_data[,(columns_with_no_variance):=NULL]
  
  str(train_data)
  str(test_data)
  
  #is there any duplicated row?
    sum(duplicated(train_data)*1)
    deduplicated_train_data <- unique( train_data[ , ] )#obs. num. decreased to 95000
    deduplicated_test_data <- unique( test_data[ , ] )#nothing changed
  
  str(deduplicated_train_data)
  str(deduplicated_test_data)
  
#var types####

  library(matrixStats)
  library(dplyr)
  
  #variables separated to two sets which are categorical variables and numeric variables.
    max(deduplicated_train_data)
    binary_variables<-names(d[d==2]) #categorical variables
    
  #check if they are integer
    sum((binary_variables%in%names(e[e=="integer"]))*1)==length(binary_variables)#they are integer
  
    integer_names<-binary_variables #categorical var
    numeric_names<-setdiff(names(deduplicated_train_data),integer_names)#numeric var
  
    deduplicated_train_data[,(integer_names):=lapply(.SD,function(x){as.factor(x)}),.SDcols=integer_names]
    deduplicated_test_data[,(integer_names):=lapply(.SD,function(x){as.factor(x)}),.SDcols=integer_names]

#variances####
  #look at variances in each column for all type of variables and remove ones with low variance 

  #numeric variables
    #how much variance is significant for columns with numeric values
    sd_dedup_train_data<-apply(deduplicated_train_data,2,sd)
    mean_dedup_train_data<-apply(deduplicated_train_data,2,mean)
    sd_mean_dedup_train_data<-sd_dedup_train_data/mean_dedup_train_data
    #columns with variance/mean lower than 0.01
    low_var_variables<-names(sd_dedup_train_data[sd_dedup_train_data<0.01])
    low_var_variables<-intersect(numeric_names,low_var_variables)
    deduplicated_train_data[,(low_var_variables):=NULL]
    deduplicated_test_data[,(low_var_variables):=NULL]
    #remove columns with low variance
    numeric_names<-setdiff(numeric_names,low_var_variables)

  #categorical variables
    #remove columns having very low occurences 
    #occurence is a percentage of how many times fewer used level is used. 
    a <- sapply(deduplicated_train_data[,integer_names,with=F],function(x){
      sum(as.integer(as.character(x)))})
    
    occurence<-data.table(variables=names(a),a)
    occurence[,occurence_1:=round((a/nrow(deduplicated_train_data))*100,digits=2)]
    occurence<-occurence[-1,]
    #if occurence_1 percentage is higher than 90 or lower than 10, remove it since there is no significant variance in this case
    low_variance_categoricals<-occurence[occurence_1>99 | occurence_1<1, variables]
    deduplicated_train_data[,(low_variance_categoricals):=NULL]
    deduplicated_test_data[,(low_variance_categoricals):=NULL]
    integer_names<-setdiff(integer_names,low_variance_categoricals)
    
    deduplicated_train_data[,(integer_names):=lapply(.SD,function(x){as.factor(x)}),.SDcols=integer_names]

#multicollinearty####
  #check multicollinearty of numeric predictor variables
    cor_numerics<-cor(deduplicated_train_data[,numeric_names,with=F],deduplicated_train_data[,numeric_names,with=F])
    
    TempData <- melt(cor_numerics,id.vars="Vars")
    TempData<-as.data.table(TempData)
    TempData<-TempData[(TempData$value>0.7 | TempData$value<(-0.7)),]
    TempData<-TempData[!(TempData$Var1==TempData$Var2),]
    setorder(TempData,value)
    nrow(TempData)==2*length(unique(TempData$value)) #then we can remove rows with duplicated values
    TempData<-TempData[-seq(from=1,to=nrow(TempData)-1,by=2),]
    TempData[,Var1:=as.character(Var1)]
    TempData[,Var2:=as.character(Var2)]
  
    var<-TempData$Var1
    var2<-TempData$Var2
    var<-c(var,var2)
    var<-unique(var)
  
  #changing the class of V1(response var) to numeric is not a problem since it is binary and has two classes
    deduplicated_train_data[,V1:=as.character(V1)]
    deduplicated_train_data[,V1:=as.numeric(V1)]
  
    z<-list()
  
    for (i in seq(from=1,to=length(var),by=1)) {
      z[[i]]<-aov(V1~get(var[i]),data=deduplicated_train_data)
      z[[i]]<-summary(z[[i]])[[1]][["Pr(>F)"]][1]
    }
    z<-unlist(z)
  
  #the values of z have order same in var vector
  
    p_values_all<-data.table(Var1=var,p_values_Var=z)
    
    TempData<-merge(TempData,p_values_all,by="Var1")
    setnames(TempData,"p_values_Var","p_values_Var1")
    setnames(p_values_all,"Var1","Var2")
    TempData<-merge(TempData,p_values_all,by="Var2")
    setnames(TempData,"p_values_Var","p_values_Var2")

  #choose the variable with higher p value and remove other from the data
  #take difference of p values and determine which variable to choose looking at the difference if it is positive of negative
  
    TempData[,difference:=p_values_Var1-p_values_Var2]
    TempData[difference>0,var_to_choose:=Var2]
    TempData[difference<0,var_to_choose:=Var1]
    
    TempData[difference>0,var_to_remove:=Var1]
    TempData[difference<0,var_to_remove:=Var2]
    
    var_to_remove<-TempData$var_to_remove
    var_to_remove<-unique(var_to_remove) 
    
    deduplicated_train_data[,(var_to_remove):=NULL]
    deduplicated_test_data[,(var_to_remove):=NULL]
    numeric_names<-setdiff(numeric_names,var_to_remove)

#effects on response var####
  #look at effects of categorical var on response var and the effects of numeric var on response var
    
    #chi square test for categorical var
      deduplicated_train_data[,V1:=as.character(V1)]
      deduplicated_train_data[,V1:=as.factor(V1)]
      
      chi_dedup_train_data<-deduplicated_train_data[,integer_names,with=F]
      str(chi_dedup_train_data)
      
      matrix_chi<-as.matrix(chi_dedup_train_data)
      str(matrix_chi)
  
      x<-NULL
      for (i in seq(from=2,to=ncol(matrix_chi),by=1)) {
        z<-chisq.test(matrix_chi[,1],matrix_chi[,i])$p.value
        ifelse(z>0.1,
               x<-append(x, i, after = length(x)),
               NA)
      }
      
      #delete the columns with high p value in chi square test
        columns_with_high_p_value<-colnames(matrix_chi[,x])
        deduplicated_train_data[,(columns_with_high_p_value):=NULL]
        deduplicated_test_data[,(columns_with_high_p_value):=NULL]
  
      integer_names<-setdiff(integer_names,columns_with_high_p_value)

    #one way anova test for numeric var
    #one way anova test to see the effect of numeric predictor variables on response(dependent) variable

      #changing the class of V1(response var) to numeric is not a problem since it is binary and has two classes
      deduplicated_train_data[,V1:=as.character(V1)]
      deduplicated_train_data[,V1:=as.numeric(V1)]

      u<-list()
      
      for (i in seq(from=1,to=length(numeric_names),by=1)) {
        u[[i]]<-aov(V1~get(numeric_names[i]),data=deduplicated_train_data)
        u[[i]]<-summary(u[[i]])[[1]][["Pr(>F)"]][1]
      }
      u<-unlist(u)
      #the values of u have order same in numeric_names vector
      p_values_numerics<-data.table(numerics=numeric_names,p_values=u)
      #remove numeric variables having p value higher than 0.1
      high_p_numerics<-p_values_numerics[p_values>0.1,numerics]
      deduplicated_train_data[,(high_p_numerics):=NULL]
      deduplicated_test_data[,(high_p_numerics):=NULL]
      numeric_names<-setdiff(numeric_names,high_p_numerics)

deduplicated_train_data[,V1:=as.character(V1)]
deduplicated_train_data[,V1:=as.factor(V1)]

#building model####

  library(pROC)
  library(stats)
  library(boot)

#specify what proportion of data we want to train the model
  training_size <- 0.80

#use the sample function to select random rows from our data to meet the proportion specified above
  training_rows <- sample(seq_len(nrow(deduplicated_train_data)), size = floor(training_size * nrow(deduplicated_train_data)))

#training set
  train_train_80 <- deduplicated_train_data[training_rows, ]
#validation set
  train_test_20 <- deduplicated_train_data[-training_rows, ]

#check
  which(sapply(train_train_80, function(x) length(unique(x))<2))
  which(sapply(train_test_20, function(x) length(unique(x))<2))
  which(sapply(deduplicated_test_data, function(x) length(unique(x))<2))# V1 is okay since it is "?" and we try to predict it

fit<-glm(V1~.,data=train_train_80,family = binomial(link="logit"))
pred1<-predict(fit,newdata=train_train_80,type="response")
pred2<-predict(fit,newdata=train_test_20,type="response")

roc(train_train_80$V1,predictor = pred1)
  #0.6353
roc(train_test_20$V1,predictor = pred2)
  #0.603

summary(fit)

fit_final<-glm(V1~.,data=deduplicated_train_data,family = binomial(link="logit"))
pred_final<-predict(fit_final,newdata=deduplicated_test_data,type="response")
summary(pred_final)

write.table(pred_final, "mydata.txt",row.names = FALSE,col.names = FALSE)
