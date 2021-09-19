#random forest
newthyroid=read.table("new_thyroid.data.txt",sep=',',header= F, na.strings="?")
newthyroid=newthyroid[,c(2:6,1)]

for (i in c(6) )
{
  newthyroid[,i]=as.factor(newthyroid[,i])
}

summary(newthyroid)
newthyroid=na.roughfix(newthyroid)
Data_cont=newthyroid


#1- BI-RADS assessment: 1 to 5 (ordinal)  
# 2. Age: patient's age in years (integer)
#    3. Shape: mass shape: round=1 oval=2 lobular=3 irregular=4 (nominal)
#    4. Margin: mass margin: circumscribed=1 microlobulated=2 obscured=3 ill-defined=4 spiculated=5 (nominal)
#    5. Density: mass density high=1 iso=2 low=3 fat-containing=4 (ordinal)

summary(Data_cont)


id_target=6

name="newthyroid"

Data_cont=na.roughfix(Data_cont)
class_attr=get_class_attribute(Data=Data_cont,id_target=id_target)

cont_attr=class_attr$continuous_att
cat_attr=class_attr$categorical_att

Data_orig=Data_cont

DataSet=Data_orig

dataset_summary=data.frame(matrix(ncol=8,nrow=1))
colnames(dataset_summary)=c("name","nrow","ncol","ncont_att","ncat_att","cont","cat","nclass")
dataset_summary[1,]=c(name,nrow(DataSet),
                      ncol(DataSet),length(cont_attr),
                      length(cat_attr),paste0(as.character(cont_attr),collapse=",") ,
                      paste0(as.character(cat_attr),collapse=","),length(unique(DataSet[,id_target])))
write.csv(dataset_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_dataset_summary.csv", row.names = FALSE)
test0=read.table("performance\\newthyroid_dataset_summary.csv",sep=',',header= T, na.strings="?")
# vec0_seed=c(010,120,230,340,450,560,670,780,890,900)
vec_seed=c(012,123,234,345,456,567,678,789,890,901)

test01=read.table("performance\\newthyroid\\newthyroid_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
test02=read.table("performance\\newthyroid\\newthyroid_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
test03=read.table("performance\\newthyroid\\newthyroid_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")
summary(test01)
test01$NB_interval=as.numeric(as.character(test01$NB_interval))
test01$modif_nb_inter=as.numeric(as.character(test01$modif_nb_inter))
unique(test01$disc_method)
perf_all_iter=data.frame()
all_iter_splits=data.frame()
all_splitsselection=data.frame()

for (I in (1:length(vec_seed)))

{
  iteration=I
  Select_seed=vec_seed[I]
  Data=DATA_SET(XDataset=DataSet[,-id_target],
                YDataset=DataSet[,id_target],
                SplitRatio=0.7,seed=Select_seed)
  Data_cont=Data$training_set
  
  class_attr=get_class_attribute(Data=DataSet,id_target=id_target)
  cont_attr=class_attr$continuous_att
  unique(perf_all_iter$disc_method)
  cont_Data=Data_cont
  
  for (meth in c("Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
                 "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200"))

    
  # for (meth in c("Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
  #                  "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200",
  #                  "GLMDISC")) 
  # for (meth in c("GLMDISC"))
    
    
  {
    
    if (meth == "Forest_disc_NLD50") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=50,max_splits=10,opt_meth="NelderMead")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_NLD50"
    }else if (meth == "Forest_disc_NLD100") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=100,max_splits=10,opt_meth="NelderMead")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_NLD100"
    }else if (meth == "Forest_disc_NLD200") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=200,max_splits=10,opt_meth="NelderMead")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_NLD200"
    }else if (meth == "Forest_disc_DIR50") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=50,max_splits=10,opt_meth="directL")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_DIR50"
    }else if (meth == "Forest_disc_DIR100") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=100,max_splits=10,opt_meth="directL")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_DIR100"
    }else if (meth == "Forest_disc_DIR200") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=200,max_splits=10,opt_meth="directL")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_DIR200"
    }else if (meth == "GLMDISC")
    {
      # function computing locally semi-optimal cuts using the local discernibility heuristic.
      start.time = Sys.time()
      Data_disc=Data_cont
      oneLevelAtt=vector()
      for (i in 1:length(cont_attr)) 
      {
        if (length(unique(Data_cont[,cont_attr[i]])) == 1)
        {
          oneLevelAtt=c(oneLevelAtt,cont_attr[i])
        }
      }
      
      if (length(oneLevelAtt)<1) 
      {
        glmdisc_class=glmdisc(as.matrix(as.data.frame(lapply(Data_cont[,cont_attr], as.numeric))),as.factor(Data_cont[,id_target]),iter=50,m_start=10,interact=FALSE,test=FALSE,validation=FALSE,criterion="aic")
        discret_Data=glmdisc_class@disc.data
        AS=glmdisc_class@disc.data
      }else 
      {
        cont_attr0=setdiff(cont_attr,oneLevelAtt)
        glmdisc_class=glmdisc(as.matrix(Data_cont[,cont_attr0]),as.factor(Data_cont[,id_target]),iter=50,m_start=10,interact=FALSE,test=FALSE,validation=FALSE,criterion="aic")
        discret_Data0=glmdisc_class@disc.data
        Data_disc0=data.frame(Data_cont[,oneLevelAtt],discret_Data0) 
        colnames(Data_disc0)=c(colnames(Data_cont[oneLevelAtt]),colnames(discret_Data0)[1:length(cont_attr0)],colnames(Data_cont[id_target]))
        Data_disc0=Data_disc0[,c(colnames(Data_cont[c(cont_attr,id_target)]))]
        discret_Data=Data_disc0
        
      }
      discret_Data=data.frame(Data_cont[,cat_attr],discret_Data) 
      colnames(discret_Data)=c(colnames(Data_cont[cat_attr]),colnames(Data_cont[c(cont_attr,id_target)]))
      discret_Data=discret_Data[,c(colnames(Data_cont))]  
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data) 
      disc_method= "GLMDisc"
    }
    
    
    end.time = Sys.time()
    time.taken = difftime(end.time,start.time,units="secs")
    
    
    if ( meth %in% c("Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
                     "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200"))
    {
      
      iter_splits=RFdisc$opt_results
      iter_splits[,"iter"]=I
      iter_splits[,"forest"]=disc_method
      all_iter_splits=rbind(all_iter_splits,iter_splits)
      
      splitsselection=RFdisc$cut_points
      splitsselection[,"iter"]=I
      splitsselection[,"forest"]=disc_method
      all_splitsselection=rbind(all_splitsselection,splitsselection)
    }
    
    Id_target=id_target
    
    nulcut_var=vector()
    for (i in 1:length(cont_cutp))
    {
      if (!is.numeric(cont_cutp[[i]]) || is.na(cont_cutp[[i]]) || length(cont_cutp[[i]]) == 0)
      {
        nulcut_var=c(nulcut_var,cont_attr[i])
      }
    }
    disc_NB_interval=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)) - length(nulcut_var))/length(cont_cutp)
    # for the variables which are discretized into more than 25 bins, 
    # the bins are clustered into 20 bins or less to gain in interpretability and avoid some classifiers errors(RF for example does not support variables whith more than 53 levels)
    long_disc=which(lengths(cont_cutp)>25)
    if (length(long_disc)>0) 
    {
      for (a in long_disc) 
      {
        d=bin(as.numeric(discret_Data[,cont_attr[a]]), nbins =20,method = "clusters")
        discret_Data[,cont_attr[a]]=d
      }
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
    }
    modif_nb_interv=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)) - length(nulcut_var))/length(cont_cutp)
    Data_disc_train=Data$training_set
    for (i in 1:length(cont_cutp))
    {
      splitpoints=unique(as.numeric(cont_cutp[[i]]))
      Data_disc_train[,cont_attr[i]]=cut(Data_disc_train[,cont_attr[i]],c(-Inf,as.numeric(splitpoints),Inf))
    }
    # summary(Data_disc_tst)
    Data_disc_tst=Data$test_set
    for (i in 1:length(cont_cutp))
    {
      splitpoints=unique(as.numeric(cont_cutp[[i]]))
      Data_disc_tst[,cont_attr[i]]=cut(Data_disc_tst[,cont_attr[i]],c(-Inf,as.numeric(splitpoints),Inf))
    }
    
    # remove attribute thtat had been discretized into only one level (to avoid error when using SVM and naivebayes classifier)
    if (!is.logical(nulcut_var))
    {
      Data_disc_train=Data_disc_train[,-nulcut_var]
      Data_disc_tst=Data_disc_tst[,-nulcut_var]
      Id_target=id_target-length(nulcut_var)
    }
    
    
    Data_train=Data_disc_train
    Data_tst=Data_disc_tst 
    perf_one_iter=perf_data_itera(Data_train,Data_tst,Id_target,disc_meth,disc_NB_interval)
    perf_one_iter[,"modif_nb_inter"]=modif_nb_interv
    perf_all_iter=rbind(perf_all_iter,perf_one_iter) 
  }
}


# test=read.table("performance\\newthyroid_01_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_01_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_01_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_01_Forestdisc_splitsselection.csv", row.names = FALSE)

perf_all_iter[,"name"]=name
perf_all_iter[,"exetimepervar"]=perf_all_iter[,"exe_time"]/length(cont_attr)
all_iter_splits[,"name"]=name
all_splitsselection["name"]=name
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_reliq_Forestdisc_splitsselection.csv", row.names = FALSE)



test1=read.table("performance\\newthyroid_reliq_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
test2=read.table("performance\\newthyroid_reliq_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
test3=read.table("performance\\newthyroid_reliq_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")
# perf_all_iter[,"name"]=name
# perf_all_iter[,"exetimepervar"]=perf_all_iter[,"exe_time"]/length(cont_attr)
# all_iter_splits[,"name"]=name
# all_splitsselection["name"]=name

F_perf_all_iter=rbind(perf_all_iter,test01)
F_all_iter_splits=rbind(all_iter_splits,test02)
F_all_splitsselection=rbind(all_splitsselection,test03)

write.csv(F_perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_F_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_F_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\newthyroid_F_Forestdisc_splitsselection.csv", row.names = FALSE)
test1F=read.table("performance\\newthyroid_F_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
test2F=read.table("performance\\newthyroid_F_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
test3F=read.table("performance\\newthyroid_F_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")



F_perf_all_iter=read.table("performance\\newthyroid\\newthyroid_F_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
F_all_iter_splits=read.table("performance\\newthyroid\\newthyroid_F_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
F_all_splitsselection=read.table("performance\\newthyroid\\newthyroid_F_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")
unique(F_perf_all_iter$name)
F_perf_all_iter$name=name
F_all_iter_splits$name=name
F_all_splitsselection$name=name
