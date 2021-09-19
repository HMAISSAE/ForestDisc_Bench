#random forest
AUTOS=read.table("autos.data.txt",sep=',',header= F, na.strings="?")
summary(AUTOS)


for (i in c(1,3:9,15,16,18) )
{
  AUTOS[,i]=as.factor(AUTOS[,i])
}

summary(AUTOS)
AUTOS=na.roughfix(AUTOS)
AUTOS=AUTOS[,c(2:26,1)]
Data_cont=AUTOS



# # Attributes 
# # Attribute:                Attribute Range:
# ------------------        -----------------------------------------------
#   1. symboling:                -3, -2, -1, 0, 1, 2, 3.
# 2. normalized-losses:        continuous from 65 to 256.
# 3. make:                     alfa-romero, audi, bmw, chevrolet, dodge, honda,
# isuzu, jaguar, mazda, mercedes-benz, mercury,
# mitsubishi, nissan, peugot, plymouth, porsche,
# renault, saab, subaru, toyota, volkswagen, volvo
# 4. fuel-type:                diesel, gas.
# 5. aspiration:               std, turbo.
# 6. num-of-doors:             four, two.
# 7. body-style:               hardtop, wagon, sedan, hatchback, convertible.
# 8. drive-wheels:             4wd, fwd, rwd.
# 9. engine-location:          front, rear.
# 10. wheel-base:               continuous from 86.6 120.9.
# 11. length:                   continuous from 141.1 to 208.1.
# 12. width:                    continuous from 60.3 to 72.3.
# 13. height:                   continuous from 47.8 to 59.8.
# 14. curb-weight:              continuous from 1488 to 4066.
# 15. engine-type:              dohc, dohcv, l, ohc, ohcf, ohcv, rotor.
# 16. num-of-cylinders:         eight, five, four, six, three, twelve, two.
# 17. engine-size:              continuous from 61 to 326.
# 18. fuel-system:              1bbl, 2bbl, 4bbl, idi, mfi, mpfi, spdi, spfi.
# 19. bore:                     continuous from 2.54 to 3.94.
# 20. stroke:                   continuous from 2.07 to 4.17.
# 21. compression-ratio:        continuous from 7 to 23.
# 22. horsepower:               continuous from 48 to 288.
# 23. peak-rpm:                 continuous from 4150 to 6600.
# 24. city-mpg:                 continuous from 13 to 49.
# 25. highway-mpg:              continuous from 16 to 54.
# 26. price:                    continuous from 5118 to 45400.

summary(Data_cont)


id_target=26

name="AUTOS"

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
write.csv(dataset_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_dataset_summary.csv", row.names = FALSE)
test0=read.table("performance\\AUTOS_dataset_summary.csv",sep=',',header= T, na.strings="?")
# vec0_seed=c(010,120,230,340,450,560,670,780,890,900)
vec_seed=c(012,123,234,345,456,567,678,789,890,901)
perf_all_iter=data.frame()
all_iter_splits=data.frame()
all_splitsselection=data.frame()


# perf whithout discretization
meth="cont"
NB_interval=NA
# ncoldata=ncol(Data_cont)
# target_name=colnames(Data_cont[id_target])
perf_cont=perf_data_iters(seed_vec=vec_seed, DataSet=Data_cont,id_target=id_target,disc_meth=meth,disc_NB_interval=NB_interval)
perf_cont_all=perf_cont$perf_all_iter
perf_cont_all[,"exe_time"]=0
perf_cont_all[,"modif_nb_inter"]=NB_interval
perf_cont_all=perf_cont_all[,c("accuracy","macroPrecision","macroRecall","macroF1","kappa","kappa_w","data","class_method","inconsistency","disc_method","NB_interval","exe_time","iter","modif_nb_inter")]

perf_all_iter=rbind(perf_all_iter,perf_cont_all)

for (I in (1:length(vec_seed)))
for (I in (8:length(vec_seed)))
for (I in (7:7))  
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
  
  # for (meth in c("Forest_disc_25","Forest_disc_50","Forest_disc_75","Forest_disc_100",
  #                 "Forest_disc_125","Forest_disc_150","Forest_disc_175","Forest_disc_200","Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
  #                "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200"))
  
  for (meth in c("RDGR","RSTQ","RSTI","RSTGDH","RSTLDH",
                 "chi2","chiM","CAIM", "CACC","AMEVA","extendChi2","MDLP","modChi2",
                 "GLMDISC"))
  # for (meth in c("OneR","equalwidth","equalfrequencies","clusterdisc","infogain",
  #                "logreg","naive","RDGR","RSTQ","RSTI","RSTGDH","RSTLDH",
  #                "chi2","chiM","CAIM", "CACC","AMEVA","extendChi2","MDLP","modChi2",
  #                "GLMDISC"))

  # for (meth in c("chi2","chiM","extendChi2","modChi2"))
  {
    if (meth == "Forest_disc_25") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=25,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_25"
    }else if (meth == "Forest_disc_50") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=50,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_50"
    }else if (meth == "Forest_disc_75") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=75,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_75"
    }else if (meth == "Forest_disc_100") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=100,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_100"
    }else if (meth == "Forest_disc_125") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=125,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_125"
    }else if (meth == "Forest_disc_150") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=150,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_150"
    }else if (meth == "Forest_disc_175") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=175,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_175"
    }else if (meth == "Forest_disc_200") 
    {
      # Forest_Disc
      start.time = Sys.time()
      RFdisc=RF_disc(Data=Data_cont,id_target=id_target,Ntree=200,max_splits=10,opt_meth="SLSQP")
      Data_disc=RFdisc$Data_disc
      discret_Data=Data_disc
      cont_cutp= RFdisc$listcutp
      # cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Forest_disc_200"
    }else if (meth == "Forest_disc_NLD50") 
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
    }else if (meth == "chi2") 
    {
      # chi2
      start.time = Sys.time()
      discret=chi2(Data_cont[,c(cont_attr,id_target)], alp = 0.5, del = 0.05)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Chi2"
      b=Sys.time()
    }else if (meth == "chiM") 
    {
      # chiMerge
      start.time = Sys.time()
      discret=chiM(Data_cont[,c(cont_attr,id_target)], alpha = 0.05)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "ChiMerge"
    }else if (meth == "CAIM")
    {
      # top-down discretization algorithms(CAIM, CACC, Ameva).
      # 1: CAIM algorithm, 2: CACC algorithm, 3: Ameva algorithm.
      start.time = Sys.time()
      discret=disc.Topdown(Data_cont[,c(cont_attr,id_target)], method = 1)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "CAIM"
    }else if (meth == "CACC")
    {
      # top-down discretization algorithms(CAIM, CACC, Ameva).
      # 1: CAIM algorithm, 2: CACC algorithm, 3: Ameva algorithm.
      start.time = Sys.time()
      discret=disc.Topdown(Data_cont[,c(cont_attr,id_target)], method = 2)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "CACC"
    }else if (meth == "AMEVA")
    {
      # top-down discretization algorithms(CAIM, CACC, Ameva).
      # 1: CAIM algorithm, 2: CACC algorithm, 3: Ameva algorithm.
      start.time = Sys.time()
      discret=disc.Topdown(Data_cont[,c(cont_attr,id_target)], method = 3)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "AMEVA"
    }else if (meth == "extendChi2")
    {
      # Extended Chi2
      start.time = Sys.time()
      discret=extendChi2(Data_cont[,c(cont_attr,id_target)], alp = 0.5)
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      summary(discret_Data)
      summary(cont_Data)
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Extended Chi2"
      
      
    }else if (meth == "MDLP")
    {
      # Minimum Description Length Principle
      start.time = Sys.time()
      discret=mdlp_modif(data=Data_cont[,c(cont_attr,id_target)])
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "MDLP"
    }else if (meth == "modChi2")
    {
      # Modified Chi2
      start.time = Sys.time()
      # discret=modChi2(Data_cont[,c(cont_attr,id_target)], alp = 0.5)
      
      discret=modChi2(Data_cont[,c(cont_attr,id_target)], alp = 0.5)
      
      Data_disc0=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
      colnames(Data_disc0)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
      Data_disc0=Data_disc0[,c(colnames(Data_cont))]
      discret_Data=Data_disc0
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "Modified Chi2"
      
    }else if (meth == "OneR") 
    {
      # OneR
      cont_Data=Data_cont
      start.time = Sys.time()
      discret_Data=disconeR(data=Data_cont,convar=cont_attr,binsize=15,out="symb")
      discret_Data=discret_Data[rownames(discret_Data),]
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "OneR"
      discret_Data=discret_Data[rownames(discret_Data),]
    }else if (meth == "equalwidth")
    {
      # equalwidth
      start.time = Sys.time()
      discret_Data=discew(data=Data_cont,varcon=cont_attr,out="symb")
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "equalwidth"
      
      
    }else if (meth == "equalfrequencies")
    {
      # equal frequencies
      start.time = Sys.time()
      discret_Data=discef(data=Data_cont,k=3,varcon=cont_attr,out="symb")
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "equalfrequencies"
      
    }else if (meth == "clusterdisc")
    {
      # Cluster *arules
      start.time = Sys.time()
      Data_disc=Data_cont
      
      for (j in cont_attr)
      {
        if (length(unique(Data_disc[,j]))<3) 
        {
          Data_disc[,j]=Data_disc[,j]
          # Data_disc[,j]=arules::discretize(x=Data_cont[,j], method = "cluster",labels = FALSE,breaks = length(unique(Data_disc[,j]))) 
        }else {
          Data_disc[,j]=arules::discretize(x=Data_cont[,j], method = "cluster",labels = FALSE)
        }  
      }
      
      discret_Data=Data_disc
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "ClusterDisc"
    }else if (meth == "RDGR")
    {
      # Recursive discretization using gain ratio  *funModeling
      start.time = Sys.time()
      Data_disc=Data_cont
      for (j in cont_attr)
        
      {
        if (length(unique(Data_disc[,j]))<3) 
        {
          Data_disc[,j]=Data_disc[,j]
        }else 
        {
          Data_disc[,j]=discretize_rgr_modif(input=Data_cont[,j], target=Data_cont[,id_target],min_perc_bins=0.25,max_n_bins=5)
          Data_disc[which(is.na(Data_disc[,j])),j]=levels(Data_disc[,j])[1]
        }
      }
      discret_Data=Data_disc
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "RDGR"
    }else if (meth == "infogain")
    {
      # Information gain
      start.time = Sys.time()
      discret_Data=optbin(Data_cont, method = "infogain")
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "infogain"
    }
    else if (meth == "logreg")
    {
      # logreg
      start.time = Sys.time()
      discret_Data=optbin(Data_cont, method = "logreg")
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "logreg"
    }else if (meth == "naive")
    {
      #Naive
      start.time = Sys.time()
      discret_Data=optbin(Data_cont, method = "naive")
      cont_cutp=Find_cutpoint(cont_attr,discret_Data, cont_Data)
      disc_method= "naive"
    }else if (meth == "RSTQ")
    {
      # RST Quantile s unsupervised discretization into intervals containing similar number of
      # instances ("quantile-based").
      start.time = Sys.time()
      decisiontable=SF.asDecisionTable(dataset=Data_cont, decision.attr = id_target, indx.nominal = NULL)
      cut.values = D.discretize.quantiles.RST(decisiontable, nOfIntervals = 5)
      cont_cutp=cut.values$cut.values[cont_attr]
      discret_Data = SF.applyDecTable(decisiontable, cut.values)
      # lapply(decisiontable, unique)
      disc_method= "RSTQ"
    }else if (meth == "RSTI")
    {
      # RST Interval length.  discretization into intervals of equal length
      start.time = Sys.time()
      decisiontable=SF.asDecisionTable(dataset=Data_cont, decision.attr = id_target, indx.nominal = NULL)
      cut.values = D.discretize.equal.intervals.RST(decisiontable, nOfIntervals = 5)
      cont_cutp=cut.values$cut.values[cont_attr]
      discret_Data = SF.applyDecTable(decisiontable, cut.values)
      # lapply(decisiontable, unique)
      disc_method= "RSTI"
    }else if (meth == "RSTGDH")
    {
      # function computing globally semi-optimal cuts using the maximum discernibility heuristic.
      start.time = Sys.time()
      decisiontable=SF.asDecisionTable(dataset=Data_cont[,c(cont_attr,id_target)], decision.attr = (length(cont_attr)+1))
      cut.values = D.global.discernibility.heuristic.RST(decisiontable,maxNOfCuts=10)
      cont_cutp=cut.values$cut.value
      Data_disc1 = SF.applyDecTable(decisiontable, cut.values)
      discret_Data=data.frame(Data_cont[,cat_attr],Data_disc1) 
      colnames(discret_Data)=c(colnames(Data_cont[cat_attr]),colnames(Data_cont[c(cont_attr,id_target)]))
      discret_Data=discret_Data[,c(colnames(Data_cont))]
      disc_method= "RSTGDH"
    }else if (meth == "RSTLDH")
    {
      # function computing locally semi-optimal cuts using the local discernibility heuristic.
      start.time = Sys.time()
      decisiontable=SF.asDecisionTable(dataset=Data_cont, decision.attr = id_target, indx.nominal = NULL)
      cut.values =  D.local.discernibility.heuristic.RST(decisiontable)
      cont_cutp=cut.values$cut.values[cont_attr]
      discret_Data = SF.applyDecTable(decisiontable, cut.values)
      # summary(Data_disc)
      # lapply(decisiontable, unique)
      disc_method= "RSTLDH"
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
        glmdisc_class=glmdisc(as.matrix(Data_cont[,cont_attr]),as.factor(Data_cont[,id_target]),iter=50,m_start=10,interact=FALSE,test=FALSE,validation=FALSE,criterion="aic")
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
    
    # time.taken = end.time - start.time
    # C=difftime(end.time,start.time,units="secs")
    # time_taken = time.taken
    # tr=perf_one_iter
    # tr[,"tr"]= C
    if ( meth %in% c("Forest_disc_25","Forest_disc_50","Forest_disc_75","Forest_disc_100",
                     "Forest_disc_125","Forest_disc_150","Forest_disc_175","Forest_disc_200",
                     "Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
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
    # mean_nb_interval=(length(cont_cutp)+sum(lengths(cont_cutp,use.names = TRUE)))/length(cont_cutp)
    # NB_interval=mean_nb_interval
    # disc_NB_interval=NB_interval
    # Data_disc=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
    # Data_disc_train=data.frame(Data_cont[,cat_attr],lapply( discret$Disc.data, factor),check.names = TRUE) 
    # colnames(Data_disc_train)=c(colnames(Data_cont[cat_attr]),colnames(discret$Disc.data))
    # Data_disc_train=Data_disc_train[,c(colnames(Data_cont))]
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
    # for the variables which are discretized into more than 53 bins, 
    # the bins are clustered into 52 bins to avoid some classifiers errors(RF for example does not support variables whith more than 53 levels)
    long_disc=which(lengths(cont_cutp)>25)
    if (length(long_disc)>0) 
    {
      for (a in long_disc) 
      {
        d=bin(as.numeric(discret_Data[,cont_attr[a]]), nbins =20,method = "clusters")
        # iter8-9-10:equalwith: 
        # d=bin(as.numeric(discret_Data[,cont_attr[a]]), nbins =8,method = "clusters")
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

4320/120

# test=read.table("performance\\AUTOS_01_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_02_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_02_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_02_Forestdisc_splitsselection.csv", row.names = FALSE)

perf_all_iter[,"name"]=name
perf_all_iter[,"exetimepervar"]=perf_all_iter[,"exe_time"]/length(cont_attr)
# all_iter_splits=read.table("performance\\AUTOS_01_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
# all_splitsselection=read.table("performance\\AUTOS_01_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")
  
all_iter_splits[,"name"]=name
all_splitsselection["name"]=name
write.csv(perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_All_perf_Iterations.csv", row.names = FALSE)
write.csv(all_iter_splits,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_Forestdisc_perfopt_Iterations.csv", row.names = FALSE)
write.csv(all_splitsselection,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_Forestdisc_splitsselection.csv", row.names = FALSE)
test1=read.table("performance\\AUTOS_All_perf_Iterations.csv",sep=',',header= T, na.strings="?")
test2=read.table("performance\\AUTOS_Forestdisc_perfopt_Iterations.csv",sep=',',header= T, na.strings="?")
test3=read.table("performance\\AUTOS_Forestdisc_splitsselection.csv",sep=',',header= T, na.strings="?")



# perf_all_iter=perf_all_iter_forestdisc

perf_forestdisc= perf_all_iter[which(perf_all_iter[,"disc_method"] %in% 
                                       c("Forest_disc_25","Forest_disc_50","Forest_disc_75",
                                         "Forest_disc_100","Forest_disc_125","Forest_disc_150",
                                         "Forest_disc_175","Forest_disc_200","Forest_disc_NLD50","Forest_disc_NLD100","Forest_disc_NLD200",
                                         "Forest_disc_DIR50","Forest_disc_DIR100","Forest_disc_DIR200")),]
A=all_splitsselection[,c(1,4,5,6)]
B=perf_forestdisc

A1=sqldf('select forest, var, iter, opt_value from A group by forest,var,iter' )
A11 = sqldf('select forest, iter, avg(opt_value) as opt_value from A1 group by forest,iter' )
perf_optsplit_summary = sqldf('select forest, avg(opt_value) as mean_opt_value, 
                              stdev(opt_value) as std_opt_value 
                              from A11 group by forest' )
# names(OSS_summary)
perf_disc_forest_nb = sqldf('select name, data, disc_method, 
                            avg(NB_interval) as mean_NB_interval, 
                            stdev(NB_interval) as std_NB_interval,
                            avg(exe_time) as mean_exe_time, 
                            stdev(exe_time) as std_exe_time,
                            avg(inconsistency) as mean_incons, 
                            stdev(inconsistency) as std_incons, 
                            avg(accuracy) as mean_acc, 
                            stdev(accuracy) as std_acc, 
                            avg(macroPrecision) as mean_Precision, 
                            stdev(macroPrecision) as std_Precision, 
                            avg(macroRecall) as mean_Recall, 
                            stdev(macroRecall) as std_Recall, 
                            avg(macroF1) as mean_F1, 
                            stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                            stdev(kappa) as std_Kappa, 
                            avg(kappa_w) as mean_Kappaw, 
                            stdev(kappa_w) as std_Kappaw 
                            from B WHERE data="test" group by data,disc_method ') 
perf_disc_forest_summary= sqldf('select perf_disc_forest_nb.* , perf_optsplit_summary.* 
                                from perf_disc_forest_nb left join perf_optsplit_summary
                                on perf_disc_forest_nb.disc_method=perf_optsplit_summary.forest ' )


write.csv(perf_disc_forest_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_perf_disc_forest_summary.csv", row.names = FALSE)
test4=read.table("performance\\AUTOS_perf_disc_forest_summary.csv",sep=',',header= T, na.strings="?")

perf_disc_summary = sqldf('select  name, data, disc_method, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exe_time) as mean_exe_time, 
                          stdev(exe_time) as std_exe_time,
                          avg(inconsistency) as mean_incons, 
                          stdev(inconsistency) as std_incons, 
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw, 
                          avg(modif_nb_inter) as mean_NB_interval, 
                          stdev(modif_nb_inter) as std_NB_interval
                          from perf_all_iter group by data,disc_method')

perf_disc_perclassifier = sqldf('select  name, data, class_method, disc_method, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exe_time) as mean_exe_time, 
                          stdev(exe_time) as std_exe_time,
                          avg(inconsistency) as mean_incons, 
                          stdev(inconsistency) as std_incons, 
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw, 
                          avg(modif_nb_inter) as mean_NB_interval, 
                          stdev(modif_nb_inter) as std_NB_interval
                          from perf_all_iter group by data,class_method,disc_method')
write.csv(perf_disc_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_perf_disc_summary.csv", row.names = FALSE)
write.csv(perf_disc_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\performance\\AUTOS_perf_disc_perclassifier.csv", row.names = FALSE)
test5=read.table("performance\\AUTOS_perf_disc_summary.csv",sep=',',header= T, na.strings="?")
test6=read.table("performance\\AUTOS_perf_disc_perclassifier.csv",sep=',',header= T, na.strings="?")

