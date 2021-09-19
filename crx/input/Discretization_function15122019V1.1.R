setwd("D:/Cours Datamining/LAB. R/Chapter 8/trees")
#data preprocessing

# Split dataset
DATA_SET=function(XDataset,YDataset,SplitRatio,seed){
  
  Dataset=cbind(XDataset,YDataset)
  library(caTools)
  set.seed(seed=seed)
  Variables=data.frame(matrix(nrow=1,ncol=length(Dataset)))
  rownames(Variables)=c('attribut')
  colnames(Variables)=names(Dataset)
  for (i in 1:(length(Dataset)-1))
  {
    # Variables[1,i]=paste0("V",i)
    Variables[1,i]=paste0("X[,",i,"]")
  }
  Variables[1,length(Dataset)]="Y"
  for (i in 1:(length(Dataset)))
  {
    names(Dataset)[i]=Variables[1,i]
  }
  split = sample.split(Dataset[,length(Dataset)], SplitRatio = SplitRatio) # Split  preserving relative ratios of different classes
  training_set = subset(Dataset, split == TRUE)
  test_set = subset(Dataset, split == FALSE)
  X=Dataset[,-length(Dataset)]
  Y=Dataset[,length(Dataset)]
  XTrain=training_set[,-length(Dataset)]
  YTrain=training_set[,length(Dataset)]
  XTest=test_set[,-length(Dataset)]
  YTest=test_set[,length(Dataset)]
  
  return(list(X=X,Y=Y,training_set=training_set,test_set=test_set,XTrain=XTrain,YTrain=YTrain,XTest=XTest,YTest=YTest,variables=Variables))
}


# ********************************************
#Selection of trees from RF (Here all trees are extracted. We can add some code to choose 
# some trees by random or depending on some metrics)
# RF2Selectedtrees=function(X,Y,Ntree,nodesize_rate,maxnodes)
RF2Selectedtrees=function(X,Y,Ntree,max_TreeRules = 'default',min_RuleSupport = 'default')
{
  library(randomForest)
  if (max_TreeRules != 'default' & min_RuleSupport !='default') 
  {
    rf = randomForest(x=X,y=Y, ntree=Ntree,nodesize=round(min_RuleSupport*nrow(X),0), keep.forest=TRUE,norm.votes=FALSE,maxnodes=max_TreeRules)
  } else if (max_TreeRules != 'default' & min_RuleSupport =='default') 
  {
    rf = randomForest(x=X,y=Y, ntree=Ntree, keep.forest=TRUE,norm.votes=FALSE,maxnodes=max_TreeRules)
  } else if (max_TreeRules == 'default' & min_RuleSupport !='default')
  {
    rf = randomForest(x=X,y=Y, ntree=Ntree,nodesize=round(min_RuleSupport*nrow(X),0), keep.forest=TRUE,norm.votes=FALSE)
  } else {
    rf = randomForest(x=X,y=Y, ntree=Ntree, keep.forest=TRUE,norm.votes=FALSE)
  }
  rf$forest$xlevels
  
  ID.trees.total=1:Ntree
  Selectedtrees = NULL
  Selectedtrees$ntree = length(ID.trees.total) 
  Selectedtrees$list = vector("list",Selectedtrees$ntree)
  for(i in 1:Selectedtrees$ntree)
  {
    Selectedtrees$list[[i]] = getTree(rf,k=as.integer(ID.trees.total[i]),labelVar=FALSE)
  }
  Selectedtrees$rf=rf
  Selectedtrees$xlevels=rf$forest$xlevels
  Selectedtrees$classes=rf$classes
  rf.err=as.data.frame(rf$err.rate)
  Selectedtrees$IDSelected= ID.trees.total 
  Selectedtrees$err=rf.err[ID.trees.total,] 
  # 19/03/2019: adding continious variables split points
  # -------------------
  continuious_var=vector()
  categorical_var=vector() #add 22/04/2019
  for (i in 1:length(Selectedtrees$xlevels))
  {
    if (is.numeric(Selectedtrees$xlevels[[i]]))
    {
      continuious_var=c(continuious_var,i)
    }else
    {
      categorical_var=c(categorical_var,i)
    }
  }
  
  allt=as.data.frame(Selectedtrees$list[1])
  if (Selectedtrees$ntree > 1)
  {
    for (i in 2:Selectedtrees$ntree) 
    {
      allt=rbind(allt,as.data.frame(Selectedtrees$list[i]))
    }
  }
  for (var in continuious_var) 
  {
    Selectedtrees$xlevels[[var]]=sort(c(-Inf,unique(round(allt[which(allt[,3]== var),4],2)),Inf))
  }
  for (var in categorical_var) 
  {
    Selectedtrees$xlevels[[var]]= gsub("\\s","",Selectedtrees$xlevels[[var]])
  }
  
  # -----------------
  return(Selectedtrees)
}
library(nloptr)
library(moments)

Extract_cont_splits= function(SelectedTREES)
{
  allt=as.data.frame(SelectedTREES$list[1])
  if (SelectedTREES$ntree > 1)
  {
    for (i in 2:SelectedTREES$ntree) 
    {
      allt=rbind(allt,as.data.frame(SelectedTREES$list[i]))
    }
  }
  
  continuous_var=vector()
  categorical_var=vector() 
  for (i in 1:length(SelectedTREES$xlevels))
  {
    if (is.numeric(SelectedTREES$xlevels[[i]]))
    {
      continuous_var=c(continuous_var,i)
    }else
    {
      categorical_var=c(categorical_var,i)
    }
  }
  if (length(continuous_var)>0) 
  {
    # All_splits= vector("list",length(continuous_var))
    # names(All_splits)= continuous_var
    allt_continuous=allt[which(allt$split.var %in% continuous_var),c("split.var", "split.point")]
  }
  return(list(continuous_var=continuous_var,allt_continuous=allt_continuous))
}

Select_cont_splits= function(cont_splits,max_splits,opt_meth)
{
  continuous_var = cont_splits$continuous_var
  allt_continuous = cont_splits$allt_continuous
  if (length(continuous_var)>0) 
  {
    All_splits= vector("list",length(continuous_var))
    names(All_splits)= continuous_var
    i=1
   
    for (var in c(continuous_var)) 
    {
      All_splits[i]= var
      All_splits[[i]]=allt_continuous[which(allt_continuous$split.var == var),"split.point"]
      i=i+1
    }
    # remove continious variable not used in RF splitting
    removeattrib=which(lengths(All_splits)<1)
    if (length(removeattrib)>0)
    {
    All_splits=All_splits[-removeattrib]
    continuous_var=continuous_var[-removeattrib]
    }
    
    Selected_splits= vector("list",length(All_splits)) # splits to be found by optimisation pb
    # Prob_splits= vector("list",length(All_splits))    # probabilties related to selected splits
    # Opt_values = vector()     # value of the objective function
    names(Selected_splits)= names(All_splits)
    # names(Prob_splits)= names(All_splits)
    # summary_find_splits=data.frame(matrix(ncol=20 + 5,nrow=length(continuous_var)))
    summary_find_splits=data.frame(matrix(ncol=20 + 5))
    names(summary_find_splits)=c("var","nsplits","optim_value","S1","S2","S3","S4","S5","S6","S7","S8","S9","S10","P1","P2","P3","P4","P5","P6","P7","P8","P9","P10","score","uniq_split")
   
    i=1
    all_Split_dist=data.frame()
   
    for (j in 1: length(All_splits)) 
    {
      
      RF_Splits=All_splits[[j]]
      w=max(max(RF_Splits),-min(RF_Splits),1)
      decimal_count=vector()
      for (k in 1: length(RF_Splits)) 
      {
        decimal_count[k]=nchar(strsplit(format(RF_Splits[k], scientific = FALSE),".",fixed=TRUE)[[1]][2])
      }
      decimal_count[which(is.na(decimal_count))]=0
      max.decimal=max(decimal_count,na.rm=TRUE)
      S_moments=all.moments(RF_Splits, order.max = 4, central = FALSE, absolute = FALSE, na.rm = FALSE)
      Em=S_moments[2:5]
      S_min=min(RF_Splits)
      S_max=max(RF_Splits)
      S_mean= mean(RF_Splits)
      S_med= median(RF_Splits)
      W=c(1/w^2,1/w^4,1/w^6,1/w^8)
      # W=c(1,1,1,1)
      for (nsplit in 2:max_splits)
      # for (nsplit in 1:max_splits)
      {
        n=nsplit
        x0.splits=c(rep(S_min,n),rep(0,n))
        # x0.splits[1]=S_med
        x0.splits[2]=S_mean
        x0.splits[(n+1):(n+2)]=1/2
        x0.splits[n+1]=1
        fn.splits <- function(x) 
        {
          # W[1]*(sum(x[1:n]*x[(n+1):(2*n)])-Em[1])^2 + W[2]*(sum(x[1:n]^2*x[(n+1):(2*n)])-Em[2])^2 +W[3]*(sum(x[1:n]^3*x[(n+1):(2*n)])-Em[3])^2 +W[4]*(sum(x[1:n]^4*x[(n+1):(2*n)])-Em[4])^2
          # modif 12/12/19
          W[1]*(sum(x[1:n]*x[(n+1):(2*n)])-Em[1])^2 + W[2]*(sum(x[1:n]^2*x[(n+1):(2*n)])-Em[2])^2 +W[3]*(sum(x[1:n]^3*x[(n+1):(2*n)])-Em[3])^2 +W[4]*(sum(x[1:n]^4*x[(n+1):(2*n)])-Em[4])^2 + (1-sum(x[(n+1):(2*n)]))^2
        }
        Lower=c(rep(S_min,n),rep(0,n))
        Upper=c(rep(S_max,n),rep(1,n))
        heq.splits <- function(x) 
        {
          h = (1-sum(x[(n+1):(2*n)]))
          return(h)
        }
        # modif 12/12/19
        # find_best_Splits = slsqp(x0.splits, fn = fn.splits,heq=heq.splits, lower=Lower, upper = Upper,control = list(xtol_rel = 1e-25, check_derivatives = TRUE, maxeval=4000))
        if (opt_meth=="SLSQP")
        {
          find_best_Splits = slsqp(x0.splits, fn = fn.splits,heq=heq.splits,lower=Lower, upper = Upper,
                                   control = list(xtol_rel = 1e-25, check_derivatives = TRUE, maxeval=4000))
        }else if (opt_meth=="NelderMead")
        {
          find_best_Splits = neldermead(x0.splits, fn = fn.splits,lower=Lower, upper = Upper,
                                        control = list(xtol_rel = 1e-25, maxeval=4000))
        }else if(opt_meth=="directL") #
        {
          find_best_Splits=directL(x0.splits, fn = fn.splits,
                                   lower=Lower, upper = Upper,nl.info = TRUE,
                                   control = list(xtol_rel = 1e-25, maxeval=4000))
        }
        
        summary_find_splits[i,1]=continuous_var[j]
        summary_find_splits[i,2]=n
        SP=data.frame(find_best_Splits$par[1:n],find_best_Splits$par[(n+1):(2*n)])
        SP=SP[order(SP[,2],decreasing = TRUE),]
        SP[,1]=round(SP[,1],max.decimal)
        SP[,2]=round(SP[,2],4)
        summary_find_splits[i,3]=find_best_Splits$value 
        summary_find_splits[i,4:(3+n)] = SP[,1]
        # summary_find_splits[i,4:(3+n)] = substr(summary_find_splits[i,4:(3+n)],1,nchar(summary_find_splits[i,4:(3+n)])-2)
        summary_find_splits[i,14:(13+n)] = SP[,2]
       
        U_P=which(summary_find_splits[i,14:(13+n)]>0) # probability>0
        U_S=as.data.frame(summary_find_splits[i,U_P+3]) # splits correcponding to proba>0
        U_S=U_S[1,which(U_S[1,]>0 | U_S[1,]<=0)] #removing NA
        summary_find_splits[i,"uniq_split"]=length(unique(t(U_S)))
        summary_find_splits[i,"score"]= summary_find_splits[i,"optim_value"]*10^(summary_find_splits[i,"uniq_split"]-2)
        # S=round(find_best_Splits$par[1:n],2)
        # P=round(find_best_Splits$par[(n+1):(2*n)],5)
        # summary_find_splits[i,"ROV"]= W[1]*(sum(S*P)-Em[1])^2 + W[2]*(sum(S^2*P)-Em[2])^2 +W[3]*(sum(S^3*P)-Em[3])^2 +W[4]*(sum(S^4*P)-Em[4])^2
        i=i+1
        
      }
      
      # Selected_splits (minimize score + select unique splits which probability>0)
      A=summary_find_splits[which(summary_find_splits[,1]==continuous_var[j]),]
      # A=A[which(A[,"score"]==min(A[,"score"])),] # min score for variable j
      A=A[which(A[,"optim_value"]==min(A[,"optim_value"])),] # min optimum value for variable j
      A=A[1,] # to select one line if there is more than one row whith the same opt value 
      B=which(A[1,14:(13+n)]>0) # probability>0
      Selected_splits[[j]]=unique(A[1,B+3]) # unique splits
      S=A[1,B+3] # splits
      P=A[1,B+13] # probabilities
      SP=data.frame(t(S),t(P))
      Split_dist=aggregate(list(freq=SP[,2]), by=list(Split=SP[,1]), FUN=sum) # unique splits
      Split_dist[,"var"]=continuous_var[j]
      Split_dist[,"opt_value"]=A$optim_value
      # Split_dist[,"Split"]=substr(Split_dist[,"Split"],1,nchar(Split_dist[,"Split"])-2)
      Split_dist=Split_dist[,c(3,1,2,4)]
      Split_dist=Split_dist[order(Split_dist[,2]),]
      all_Split_dist=rbind(all_Split_dist,Split_dist)
      # Selected_splits[[i]]= find_best_Splits$par[1:n]
      # Prob_splits[[i]]= find_best_Splits$par[(n+1):(2*n)]
      # Opt_values[i] = find_best_Splits$value 
      
      # summary_find_splits[i,(14+max_splits)]= W[1]*(sum(SP[,1]*SP[,2])-Em[1])^2 + W[2]*(sum(SP[,1]^2*SP[,2])-Em[2])^2 +W[3]*(sum(SP[,1]^3*SP[,2])-Em[3])^2 +W[4]*(sum(SP[,1]^4*SP[,2])-Em[4])^2
      
    }
  }
  
  # return(list(continuous_var=continuous_var,Selected_splits=Selected_splits,Prob_splits=Prob_splits,Opt_values=Opt_values))
  return(list(All_splits=summary_find_splits,Selected_splits=all_Split_dist))
}


#Function computing perf indicators
Perf_indicators=function(Y_pred,Y_predicted){
  library(ModelMetrics) #kappa
  
  cm1=as.matrix(table(Y_pred,Y_predicted))
  cm=matrix(0,nrow=nrow(cm1),ncol=nrow(cm1)) # necessary step to avoid errors when cm1 isn't a n*n matrix
  rownames(cm)=rownames(cm1)
  colnames(cm)=rownames(cm1)
  
  for (j in 1:length(colnames(cm1)))
  {
    cm[,colnames(cm1)[j]]=cm1[,colnames(cm1)[j]]
  }
  
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy = sum(diag) / n #accuracy
  error=1-accuracy
  # Precision: fraction of correct predictions for a certain class
  # recall:fraction of instances of a class that were correctly predicted. 
  # F-1 score: harmonic mean (or a weighted average) of precision and recall.
  precision = diag / colsums
  precision[which(precision==c("NaN"))]=0
  recall = diag / rowsums 
  recall[which(recall==c("NaN"))]=0
  
  
  f1 = 2 * precision * recall / (precision + recall) 
  f1[which(f1==c("NaN"))]=0
  Class_perf=data.frame(precision, recall, f1) 
  
  # The per-class metrics can be averaged over all the classes resulting 
  # in macro-averaged precision, recall and F-1.
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  # kappa
  Kappa_metrics=vcd::Kappa(cm)
  kappa = Kappa_metrics$Unweighted[1]
  kappa_w = Kappa_metrics$Weighted[1]
  Global_perf=data.frame(accuracy,macroPrecision, macroRecall, macroF1,kappa,kappa_w)
  
  return(list(error=error,CM=cm,Global_perf=Global_perf,Class_perf=Class_perf))
}

get_class_attribute =function(Data,id_target) 
{
  continuous_var=vector()
  categorical_var=vector() 
  id_target=id_target
  for (i in 1:ncol(Data))
  {
    if (is.numeric(Data[,i]))
    {
      continuous_var=c(continuous_var,i)
    }else
    {
      categorical_var=c(categorical_var,i)
    }
    continuous_var=setdiff(continuous_var,id_target)
    categorical_var=setdiff(categorical_var,id_target)
    # 
  }
  return(list(continuous_att=continuous_var,categorical_att=categorical_var,id_target=id_target))
}

RF_disc=function(Data,id_target,Ntree,max_splits,opt_meth)
{
  X=Data[,-id_target]
  Y=Data[,id_target]
  Ntree=Ntree
  max_splits=max_splits
  opt_meth=opt_meth
  SelectedTREES=RF2Selectedtrees(X=X,Y=Y,Ntree=Ntree)
  
  cont_splits=Extract_cont_splits(SelectedTREES) 
  cont_splits$continuous_var
  SS=Select_cont_splits(cont_splits=cont_splits,max_splits=max_splits,opt_meth)
  allsplits=SS$All_splits
  splitsselection=SS$Selected_splits
  # cont_attr=unique(splitsselection[,1])
  # cat_attr=setdiff(1:ncoldata,cont_attr)
  # cat_attr=setdiff(cat_attr,id_target)
  
  Data_disc=Data
  for (i in unique(splitsselection[,1]))
  {
    splitpoints=as.numeric(splitsselection[which(splitsselection[,1]==i),2])
    Data_disc[,i]=cut(Data_disc[,i],c(-Inf,as.numeric(splitpoints),Inf))
    
  }
  
  
  C=splitsselection
  cutp=vector("list", length(unique(splitsselection$var)))
  
  V=unique(C$var)
  
  for (i in 1:length(V))
  {
    if (length(V)==1)
    {
      varm=V
    }else
    {
      varm=V[i]
    }
    
    match=which(C[,"var"] == varm)
    if (length(match)<1)
    {
      cutp[[i]]=unique(C[match,"Split"])
    }else
    {
      for (j in 1:(length(match)))
      {
        cutp[[i]]=c(cutp[[i]],C[match[j],"Split"])
      }
      cutp[[i]]=unique(cutp[[i]])
    }
    
  }
  # C=splitsselection
  # cutp=vector("list", length(cont_splits$continuous_var))
  # V=unique(C$var) 
  
  # for (i in 1:length(V)) 
  # {
  #   varm=V[i]
  #   match=which(C[,"var"] == varm)
  #   for (j in 1:length(match))
  #   {
  #     cutp[[varm]]=c(cutp[[varm]],C[match[j],"Split"])
  #   } 
  # }
  return(list(Data_disc=Data_disc,cut_points=splitsselection,opt_results=allsplits,listcutp=cutp))
}
  






perf_data_iters= function(seed_vec, DataSet,id_target,disc_meth,disc_NB_interval) 
{
  DataSet=DataSet
  ncoldata=ncol(DataSet)
  disc_NB_interval=disc_NB_interval
  class_attr=get_class_attribute(Data=DataSet,id_target=id_target)
  
  cont_attr=class_attr$continuous_att
  # target_name=colnames(DataSet[id_target])
  perf_all_iter=data.frame()
  for (i in (1:length(vec_seed))) 
  {
    Select_seed=vec_seed[i]
    Data=DATA_SET(XDataset=DataSet[,-id_target],YDataset=DataSet[,id_target],SplitRatio=0.7,seed=Select_seed)
    
    perf_all=data.frame()
    
    # Random Forest
    rf_fit = randomForest(x=Data$XTrain,y=Data$YTrain, ntree=200, keep.forest=TRUE,norm.votes=FALSE)
    rfpredtrain=predict(rf_fit,Data$XTrain,type='class', predict.all = FALSE)
    rfpredtest=predict(rf_fit,Data$XTest,type='class', predict.all = FALSE)
    # CART
    CART_fit=rpart(Y ~ ., data = Data$training_set)
    CARTpredtrain=predict(object = CART_fit, newdata = Data$training_set, type = "class")
    CARTpredtest=predict(object = CART_fit, newdata = Data$test_set, type = "class")
    # xgBoost
    xgb_train = xgb.DMatrix(data=data.matrix(Data$XTrain), label=Data$YTrain)
    xgb_test = xgb.DMatrix(data=data.matrix(Data$XTest), label=Data$YTest)
    
    xgbc = xgboost(data=xgb_train, max.depth=3, nrounds=200)
    Boostpredtrain = predict(xgbc, xgb_train)
    Boostpredtrain[(Boostpredtrain>=length(levels(Data$YTrain)))]=length(levels(Data$YTrain))
    Boostpredtrain[(Boostpredtrain<=0.5)]=1
    Boostpredtrain = as.factor(levels(Data$YTrain)[round(Boostpredtrain)])
    Boostpredtest= predict(xgbc, xgb_test)
    Boostpredtest[(Boostpredtest>=length(levels(Data$YTest)))]=length(levels(Data$YTest))
    Boostpredtest[(Boostpredtest<=0.5)]=1
    Boostpredtest = as.factor(levels(Data$YTest)[round(Boostpredtest)])
    # SVM
    SVM_fit=e1071::svm(Y ~ ., data = Data$training_set)
    SVMpredtrain=predict(object = SVM_fit, newdata = Data$training_set, type = "class")
    SVMpredtest=predict(object = SVM_fit, newdata = Data$test_set, type = "class")
    
    # Naive Bayes
    NB_fit=e1071::naiveBayes(x = subset(Data$training_set, select=-Y), y = Data$training_set$Y)
    NBpredtrain=predict(object = NB_fit, newdata = Data$training_set, type = "class")
    
    NBpredtest=predict(object = NB_fit, newdata = Data$test_set, type = "class")
    
    # KNN Classification
    KknnTr_fit=kknn::kknn(Y ~ ., train = Data$training_set, test=Data$training_set)
    KknnTs_fit=kknn::kknn(Y ~ ., train = Data$test_set, test=Data$test_set)
    
    Kknnpredtrain=fitted(KknnTr_fit)
    Kknnpredtest=fitted(KknnTs_fit)
   
    pred_train_fit=list(rfpredtrain,CARTpredtrain,Boostpredtrain,SVMpredtrain,NBpredtrain,Kknnpredtrain)
    pred_test_fit=list(rfpredtest,CARTpredtest,Boostpredtest,SVMpredtest,NBpredtest,Kknnpredtest)
    # pred_train_fit=data.frame(rfpredtrain,CARTpredtrain,Boostpredtrain,SVMpredtrain,NBpredtrain,Kknnpredtrain)
    # pred_test_fit=data.frame(rfpredtest,CARTpredtest,Boostpredtest,SVMpredtest,NBpredtest,Kknnpredtest)
    class_methods=c("RF","CART","Boosting","SVM","NaiveBayes","KNN_Class")
    
    for (j in 1:length(pred_train_fit))
    {
      predtrain=as.vector(pred_train_fit[[j]])
      predtest=as.vector(pred_test_fit[[j]])
      
    # for (j in 1: ncol(pred_train_fit))
    # {
    #   predtrain=as.vector(pred_train_fit[,j])
    #   predtest=as.vector(pred_test_fit[,j])
      PRF_fit_train=Perf_indicators(Y_pred=Data$YTrain,Y_predicted=predtrain)
      PRF_fit_test=Perf_indicators(Y_pred=Data$YTest,Y_predicted=predtest)
      perf_fit=rbind (PRF_fit_train$Global_perf,PRF_fit_test$Global_perf)
      perf_fit[,"data"]=c("train","test")
      perf_fit[,"class_method"]=c(class_methods[j],class_methods[j])
      perf_all=rbind(perf_all,perf_fit)
    }
    
    inco_rate_train=incon(Data$training_set)
    inco_rate_test=incon(Data$test_set)
    perf_all[,"inconsistency"]=inco_rate_train
    perf_all[which(perf_all[,"data"]=="test"),"inconsistency"]=inco_rate_test
    # perf_all[which(perf_all[,"data"]=="test"),"inconsistency"]=inco_rate_test
    perf_all[,"iter"]=i
    perf_all_iter=rbind(perf_all_iter,perf_all) 
  }
  
  perf_all_iter[,"disc_method"]=meth
  perf_all_iter[,"NB_interval"]=disc_NB_interval
  perf_all_iter=perf_all_iter[,c("data","disc_method","NB_interval","inconsistency","class_method","iter","accuracy","macroPrecision","macroRecall","macroF1","kappa","kappa_w")]
  output_mean = sqldf('select data, class_method,disc_method,
                      NB_interval,
                      avg(inconsistency) as mean_incons,               
                      avg(accuracy) as mean_acc,
                      avg(macroPrecision) as mean_Precision,
                      avg(macroRecall) as mean_Recal,
                      avg(macroF1) as mean_F1,
                      avg(kappa) as mean_Kappa,
                      avg(kappa_w) as mean_Kappaw
                      from perf_all_iter 
                      group by data, class_method')                 
  output_std= sqldf('select data, class_method,disc_method, 
                    NB_interval as mean_interval,
                    stdev(inconsistency) as std_incons,               
                    stdev(accuracy) as std_acc,
                    stdev(macroPrecision) as std_Precision,
                    stdev(macroRecall) as std_Recal,
                    stdev(macroF1) as std_F1,
                    stdev(kappa) as std_Kappa,
                    stdev(kappa_w) as std_Kappaw
                    from perf_all_iter 
                    group by data, class_method')  
  return(list(perf_all_iter=perf_all_iter,mean_perf_all_iter= output_mean ,std_perf_all_iter=output_std))
}


perf_data_itera= function(Data_train,Data_tst,Id_target,disc_meth,disc_NB_interval,iteration) 
{
  # DataSet=DataSet
  id_target=Id_target
  Data=vector("list")
  Data$training_set=Data_train
  Data$test_set=Data_tst
  Data$XTrain=Data_train[,-id_target]
  Data$XTest=Data_tst[,-id_target]
  Data$YTrain=Data_train[,id_target]
  Data$YTest=Data_tst[,id_target]
  

  
  # disc_NB_interval=disc_NB_interval
  # class_attr=get_class_attribute(Data=DataSet,id_target=id_target)
  # cont_attr=class_attr$continuous_att
  # target_name=colnames(DataSet[id_target])
  
  perf_all=data.frame()
  
  if (ncol(Data_train)<3) 
  {
    rfData=Data_train
    colnames(rfData)=c("X","Y")
    rf_fit = randomForest(Y~X,data=rfData,xtest=as.data.frame(Data$XTest),ytest=Data$YTest, ntree=200, keep.forest=TRUE,norm.votes=FALSE)
    rfpredtrain =rf_fit$predicted
    rfpredtest = rf_fit$test$predicted
  }else 
  {
    rf_fit = randomForest(x=Data$XTrain,y=Data$YTrain, ntree=200, keep.forest=TRUE,norm.votes=FALSE)
    rfpredtrain= predict(rf_fit,Data$XTrain,type='class')
    rfpredtest = predict(rf_fit,Data$XTest,type='class')
  }
  
  
  
  # CART
  CART_fit=rpart(Y ~ ., data = Data$training_set)
  CARTpredtrain=predict(object = CART_fit, newdata = Data$training_set, type = "class")
  CARTpredtest=predict(object = CART_fit, newdata = Data$test_set, type = "class")
  # xgBoost
  if (ncol(Data_train)<3) 
  {
    xTr=data.matrix(as.data.frame(Data$XTrain))
    colnames(xTr)="X"
    xTs=data.matrix(as.data.frame(Data$XTest))
    colnames(xTs)="X"
    xgb_train = xgb.DMatrix(data=xTr, label=as.numeric(Data$YTrain))
    xgb_test = xgb.DMatrix(data=xTs, label=Data$YTest)
  }else 
  {
    xgb_train = xgb.DMatrix(data=data.matrix(Data$XTrain), label=Data$YTrain)
    xgb_test = xgb.DMatrix(data=data.matrix(Data$XTest), label=Data$YTest)
  }
  
  
  xgbc = xgboost(data=xgb_train, max.depth=3, nrounds=200)
 
  Boostpredtrain = predict(xgbc, xgb_train)
  Boostpredtrain[(Boostpredtrain>=length(levels(Data$YTrain)))]=length(levels(Data$YTrain))
  Boostpredtrain[(Boostpredtrain<=0.5)]=1
  Boostpredtrain = as.factor(levels(Data$YTrain)[round(Boostpredtrain)])
  Boostpredtest= predict(xgbc, xgb_test)
  Boostpredtest[(Boostpredtest>=length(levels(Data$YTest)))]=length(levels(Data$YTest))
  Boostpredtest[(Boostpredtest<=0.5)]=1
  Boostpredtest = as.factor(levels(Data$YTest)[round(Boostpredtest)])
  # SVM (we remove var which has only one level to avoid error Fit (no predictive value)
  SVM_fit=e1071::svm(Y ~ ., data = Data$training_set)
  SVMpredtrain=predict(object = SVM_fit, newdata = Data$training_set, type = "class")
  SVMpredtest=predict(object = SVM_fit, newdata = Data$test_set, type = "class")  
  
  
  
  # Naive Bayes
  NB_fit=e1071::naiveBayes(x = subset(Data$training_set, select=-Y), y = Data$training_set$Y)
  NBpredtrain=predict(object = NB_fit, newdata = Data$training_set, type = "class")
  
  NBpredtest=predict(object = NB_fit, newdata = Data$test_set, type = "class")
  
  # KNN Classification (we remove var which has only one level to avoid error Fit (no predictive value))

  KknnTr_fit=kknn::kknn(Y ~ ., train = Data$training_set, test=Data$training_set)
  KknnTs_fit=kknn::kknn(Y ~ ., train = Data$test_set, test=Data$test_set)
  Kknnpredtrain=fitted(KknnTr_fit)
  Kknnpredtest=fitted(KknnTs_fit)
  
  pred_train_fit=list(rfpredtrain,CARTpredtrain,Boostpredtrain,SVMpredtrain,NBpredtrain,Kknnpredtrain)
  pred_test_fit=list(rfpredtest,CARTpredtest,Boostpredtest,SVMpredtest,NBpredtest,Kknnpredtest)
  # pred_train_fit=data.frame(rfpredtrain,CARTpredtrain,Boostpredtrain,SVMpredtrain,NBpredtrain,Kknnpredtrain)
  # pred_test_fit=data.frame(rfpredtest,CARTpredtest,Boostpredtest,SVMpredtest,NBpredtest,Kknnpredtest)
  class_methods=c("RF","CART","Boosting","SVM","NaiveBayes","KNN_Class")
  
  for (j in 1:length(pred_train_fit))
  {
    predtrain=as.vector(pred_train_fit[[j]])
    predtest=as.vector(pred_test_fit[[j]])
    
    # for (j in 1: ncol(pred_train_fit))
    # {
    #   predtrain=as.vector(pred_train_fit[,j])
    #   predtest=as.vector(pred_test_fit[,j])
    PRF_fit_train=Perf_indicators(Y_pred=Data$YTrain,Y_predicted=predtrain)
    PRF_fit_test=Perf_indicators(Y_pred=Data$YTest,Y_predicted=predtest)
    perf_fit=rbind (PRF_fit_train$Global_perf,PRF_fit_test$Global_perf)
    perf_fit[,"data"]=c("train","test")
    perf_fit[,"class_method"]=c(class_methods[j],class_methods[j])
    perf_all=rbind(perf_all,perf_fit)
  }
  
  if (ncol(Data_train)<3) 
  {
    Dtrain=cbind(1,Data$training_set)
    Dtest=cbind(1,Data$test_set)
    inco_rate_train=incon(Dtrain)
    inco_rate_test=incon(Dtest)
  }else 
  {
    inco_rate_train=incon(Data$training_set)
    inco_rate_test=incon(Data$test_set)
  }
  
  
  perf_all[,"inconsistency"]=inco_rate_train
  perf_all[which(perf_all[,"data"]=="test"),"inconsistency"]=inco_rate_test
  perf_all[,"disc_method"]=disc_method
  perf_all[,"NB_interval"]=disc_NB_interval
  perf_all[,"exe_time"]=time.taken
  perf_all[,"iter"]=I
  return(perf_itera_meth=perf_all)
}

Find_cutpoint= function(cont_attr, discret_Data, cont_data)
{
  DD = discret_Data
  split_Mi=vector("list",length(cont_attr))
  split_Ma=vector("list",length(cont_attr))
  Split_find=vector("list",length(cont_attr))
  
  for (d in 1:length(cont_attr))
  {
    U=unique(DD[,cont_attr[d]])
    U=U[order(U)]
    for (v in U) 
    {
      C=which(DD[,cont_attr[d]]==v)
      Ma=max(cont_data[C,cont_attr[d]])
      Mi=min(cont_data[C,cont_attr[d]])
      split_Ma[[d]]=c(split_Ma[[d]],Ma)
      split_Mi[[d]]=c(split_Mi[[d]],Mi)
    }
    ord=order(split_Ma[[d]])
    split_Ma[[d]]=split_Ma[[d]][ord]
    split_Mi[[d]]=split_Mi[[d]][ord]
    for (j in 1:(length(split_Ma[[d]])-1))
    {
      SF= split_Ma[[d]][j]+(split_Mi[[d]][(j+1)]-split_Ma[[d]][j])/2
      Split_find[[d]] = c(Split_find[[d]],SF)
    }
  }
  return(cont_cutp = Split_find)
}
# split_Ma[[3]]
# split_Mi[[3]][order(split_Ma[[3]])]
# # cont_data=Data_cont
# DD[C,cont_attr[d]]
# discret_Data[C,cont_attr[d]]
# cont_data[C,cont_attr[d]]
library(arules)
library(funModeling)
library(dplyr)
library(RoughSets)
library(sets)
library(discretization)
library(OneR)
library(tree)#CART
library(kknn) #KNN
library(arulesCBA) #CBA
library(e1071) #SVM /Naive bayes
library(vcd) #kappa
library(rpart)#CART
library(caret)# xgboost
library(xgboost) #xgboost
# library(adabag)
library(dplyr)

library(sqldf)
library(randomForest)
library(glmdisc)        