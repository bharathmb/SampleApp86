modelling_module<-function(DV,model_selection,predictorClass)
{
  library(pROC)
  library(caret)
  
  train<-read.csv("C:/opencpuapp_ip/train_comp.csv")	
  
  test<-read.csv("C:/opencpuapp_ip/test_comp.csv")
  
  drops <- c("X")
  train<-train[ , !(names(train) %in% drops)]
  test<-test[ , !(names(test) %in% drops)]	
  
  model_evaluations<-setNames(data.frame(matrix(ncol = 9, nrow = 9)), 
                              c("tpr","fpr","tnr","fnr","recall",
                                "precision","f1score","accuracy","roc")
                              )
  rownames(model_evaluations)<-c("lr","rf_rose","rf_over","rf_under",
                                 "rf_both","gbm","svm","nn","nb")
  
  k_stat_value<- function(fullmodel,train,test,pos){
    
    train_KStat <- train
    if(model != 'svm')
    {
      train_KStat$pred <- predict(fullmodel, 
                                  newdata = train,
                                  type = 'response') 
    }
    else
    {
      train_KStat$pred <- predict(fullmodel, 
                                  newdata = train,
                                  type = 'prob')[,pos]
      
      levels(train_KStat$DV) <- c(1,0)
    }
    
    library(SDMTools)
    optimum_threshold = optim.thresh(train_KStat$DV, train_KStat$pred)
    thresh = optimum_threshold$`max.sensitivity+specificity`
    print(thresh)
    
    return(thresh)
  }
  
  evaluatemeasures <- function(DV,predicted_val,pred_roc){
    
    pred_f <- pred_roc
    library(EvaluationMeasures)
    library(pROC)
    
    if(!is.numeric(DV))
    {
      predicted_val <- as.character(predicted_val)
      DV <- as.character(DV)
      
      flagPred <- predicted_val =="Yes"
      dvPred <- DV =="Yes"
      
      predicted_val <- as.numeric(flagPred)
      DV <- as.numeric(dvPred)
    }
    
    tpr<-EvaluationMeasures.TPR(Real = DV,Predicted = predicted_val, Positive = 1)
    fpr<-EvaluationMeasures.FPR(Real = DV,Predicted = predicted_val, Positive = 1)
    tnr<-EvaluationMeasures.TNR(Real = DV,Predicted = predicted_val, Positive = 1)
    fnr<-EvaluationMeasures.FNR(Real = DV,Predicted = predicted_val, Positive = 1)
    recall<-EvaluationMeasures.Recall(Real = DV,Predicted = predicted_val, Positive = 1)
    precision<-EvaluationMeasures.Precision(Real = DV,Predicted = predicted_val, Positive = 1)
    f1score<-EvaluationMeasures.F1Score(Real = DV,Predicted = predicted_val, Positive = 1)
    Accuracy<-EvaluationMeasures.Accuracy(Real = DV,Predicted = predicted_val, Positive = 1)
    res = roc(as.numeric(DV), pred_f)
    plot_res <- plot(res)
    print(table(DV,predicted_val))
    return(c(tpr,fpr,tnr,fnr,recall,precision,f1score,Accuracy,plot_res))
  }
  
  variable_importance <- function(var_imp_mod,flag_svm){
    library(party)
    library(caret)
    
    if(flag_svm == "not_app"){
      return()
    }
    else {
      
      var_imp_res <-data.frame(var_names = character(),
                               Overall = double())
      mod_imp <- varImp(var_imp_mod,numTrees = 3000)
      
      if(flag_svm != "y")
      {
        names <- rownames(mod_imp)
        OverallScore <-mod_imp$Overall  
      }
      else
      {
        names <- rownames(mod_imp$importance)
        OverallScore <- mod_imp$importance[,'Yes']
      }
      
      combinedList <- list(var_names=names,Overall=OverallScore)
      var_imp_res <- rbind(var_imp_res,combinedList)
      mod_imp <- var_imp_res[order(var_imp_res$Overall,decreasing = TRUE),]
      return(mod_imp)
    }
  }
  
  gbm_func <- function(train,test){
    
    train_gbm<-train
    test_gbm<-test
    
    print("running GBM")
    
    library(gbm)
    gbm_model = gbm(DV~.+0, 
                    data=train_gbm, 
                    shrinkage=0.01, 
                    distribution = 'bernoulli', 
                    cv.folds=5, 
                    n.trees=3000, 
                    verbose=F)
    
    predResult <- predFunction(gbm_model,train_gbm,test_gbm,positive_class)
    
    test_gbm <- predResult[[1]]
    pred <- predResult[[2]]
    
    best.iter = gbm.perf(gbm_model, method="cv")
    
    model_evaluations["gbm",] <<- evaluatemeasures(test_gbm$DV,
                                                   test_gbm$predicted,
                                                   pred)
    
    print(model_evaluations["gbm",])
    
    important_variables<- variable_importance(gbm_model,"n")
    
    return (list(important_variables$var_names,na.omit(model_evaluations)))
  }
  
  lr_func <- function(train,test){
    
    print("running LR")
    
    train_lr<-train
    test_lr<-test
    
    lr_model <- glm (DV ~ ., 
                     data =train_lr, 
                     family = binomial)
    
    print(summary(lr_model))
    
    predResult <- predFunction(lr_model,train_lr,test_lr,positive_class)
    
    test_lr <- predResult[[1]]
    pred <- predResult[[2]]
    
    model_evaluations["lr",] <- evaluatemeasures(test_lr$DV,
                                                 test_lr$predicted,
                                                 pred)
    
    print(model_evaluations["lr",])
    important_variables <- variable_importance(lr_model,"n")
    return (list(as.character(important_variables$var_names),
                 na.omit(model_evaluations)))
  }
  
  rf_func <- function(train,test){
    print("running RF")
    train_rf <-train
    test_rf <- test
    
    library(randomForest)
    library(ROSE)
    
    treeimp <- randomForest(DV ~ ., 
                            data = train_rf, 
                            ntrees=100,
                            importance=T)
    #Identifying threshold
    print(summary(treeimp))
    
    predResult <- predFunction(treeimp,train_rf,test_rf,positive_class)
    
    test_rf <- predResult[[1]]
    pred <- predResult[[2]]
    
    roc.curve(test_rf$DV, pred, plotit = F)
    print(table(test_rf$DV,test_rf$predicted))
    important_variables <- variable_importance(treeimp,"n")
    print(important_variables)
    
    model_evaluations["rf",]<- evaluatemeasures(test_rf$DV,
                                                test_rf$predicted,
                                                pred)
    
    return (list(important_variables$var_names,na.omit(model_evaluations)))
  }
  
  nb_func<- function(train,test){
    
    print("running NB")
    train_nb<-train
    test_nb<-test
    
    library(e1071)
    Naive_Bayes_Model <- naiveBayes(as.factor(train_nb$DV) ~., 
                                    data=train_nb)
    
    summary(Naive_Bayes_Model)
    
    predResult <- predFunction(lr_model,train_lr,test_lr,positive_class)
    
    test_lr <- predResult[[1]]
    pred <- predResult[[2]]
    
    model_evaluations["nb",] <- evaluatemeasures( test_nb$DV,
                                                  test_nb$predicted_nb,
                                                  pred)
    print(model_evaluations["nb",])
    important_variables  <- variable_importance(Naive_Bayes_Model,"not_app")
    
    return (list(important_variables$var_names,na.omit(model_evaluations)))
  }
  
  svm_func <- function(test,train){
    print("running SVM")
    train_svm<- train
    test_svm<- test
    
    library(caret)
    
    trctrl <- trainControl(method = "cv", 
                           number =5,
                           classProbs = TRUE,
                           savePredictions = 'final')
    
    set.seed(323)
    
    svm_radial <- train(DV ~., 
                        data = train_svm, 
                        method = "svmRadial",
                        trControl=trctrl)
    
    predResult <- predFunction(svm_radial,train_svm,test_svm,positive_class)
    
    test_svm <- predResult[[1]]
    pred <- predResult[[2]]
    
    model_evaluations["svm",] <- evaluatemeasures(test_svm$DV,
                                                  test_svm$predicted,
                                                  pred)
    
    important_variables  <- variable_importance(svm_radial,"y")
    
    return (list(as.character(important_variables$var_names),
                 na.omit(model_evaluations)))
  }
  
  oem_func<-function(train,test){
    train_oem <- train
    test_oem <- test
    oem_results <- data.frame()
    
    lr_results <- lr_func(train_oem,test_oem)
    nb_results <- nb_func(train_oem,test_oem)
    rf_results <- rf_func(train_oem,test_oem)
    
    oem_results <- rbind(lr_results[2][[1]],
                         rf_results[2][[1]],
                         nb_results[2][[1]])
    
    selectedModel <- rownames(oem_results[which.max(oem_results$accuracy),])
    
    modelVariable <- paste(selectedModel,'results',sep = '_')
    
    return (get(modelVariable))
  }
  
  predFunction <- function(modelInput,trainD,testD,posit_class){
    
    type <-""
    negClass <- ""
    if (model == "svm")
    {
      typeResp <- 'prob'
    }
    else
    {
      typeResp <- 'response'
    }
    
    if(is.null(posit_class))
    {
      if(is.numeric(testD$DV))
      {
        posit_class <- 1 
      }
      else if(is.factor(testD$DV))
      {
        dvList <- tolower(unique(testD$DV))
        if("yes" %in% dvList)
        {
          posit_class <- "yes"
        }
        else
        {
          posit_class <- names(which.max(table(testD$DV)))
        }
      }
      positive_class <<- posit_class
    }
    if(posit_class==1)
    {
      negClass <- 0
    }
    else
    {
      uniqLvls <- as.character(unique(testD$DV))
      negClass <- uniqLvls[uniqLvls != posit_class]
    }
    
    threshold<-k_stat_value(modelInput,trainD,testD,posit_class)
    
    if(model != 'svm')
    {
      pred <- predict(modelInput, 
                      newdata=testD,
                      type = typeResp)  
    }
    else
    {
      pred <- predict(modelInput, 
                      newdata=testD,
                      type = typeResp)[,posit_class]
    }
    
    testD$predicted[pred>max(threshold)] <- posit_class
    testD$predicted[pred<=max(threshold)] <- negClass
    
    return(list(testD,pred))
  }
  
  
  names(train)[names(train)==DV] <- "DV"
  names(test)[names(test)==DV] <- "DV"
  
  ##The class that needs to be predicted when the prob > threshold
  positive_class <- predictorClass
  
  if(model=="svm")
  {
    if(is.numeric(train$DV))
    {
      train$DV <- as.factor(train$DV)
      test$DV <- as.factor(test$DV)
      
      levels(train$DV) <- c('Yes','No')
      levels(test$DV) <- c('Yes','No')
      positive_class <- "Yes"
    }
  }
  
  fn <- match.fun(paste(model,'func',sep='_'))
  vars_imp <- fn(train,test)
  
  return (vars_imp)
}