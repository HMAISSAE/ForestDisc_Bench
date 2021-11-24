
library(sqldf)
filePaths = list.files("D:/Cours Datamining/LAB. R/Chapter 8/trees/ForestDiscPerf/All_files/B_datasets", "\\.csv$", full.names = TRUE)
B_data_summary = do.call(rbind, lapply(filePaths, read.csv))
B_data_summary$name=toupper(B_data_summary$name)
# testwireless=read.csv("ForestDiscPerf/All_files/B_datasets\\wireless_dataset_summary.csv",sep=',',header= T, na.strings="?")
write.csv(B_data_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_data_summary.csv", row.names = FALSE)
testF0=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_data_summary.csv",sep=',',header= T, na.strings="?")
filePaths = list.files("D:/Cours Datamining/LAB. R/Chapter 8/trees/ForestDiscPerf/All_files/B_perf_all", "\\.csv$", full.names = TRUE)
B_perf_all_iter <- do.call(rbind, lapply(filePaths, read.csv))
B_perf_all_iter$name=toupper(B_perf_all_iter$name)
B_perf_all_iter$data=toupper(B_perf_all_iter$data)
B_perf_all_iter$class_method=gsub("KNN_Class","KNNC",B_perf_all_iter$class_method)
B_perf_all_iter$disc_method=gsub("Forest_disc_NLD50","ForestDisc",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("Extended Chi2","ExtendedChi2",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("equalwidth","ScottEqualWidth",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("RSTI","EqualLength",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("infogain","Infogain",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("logreg","Logreg",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("Modified Chi2","ModifiedChi2",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("naive","Naive",B_perf_all_iter$disc_method)
B_perf_all_iter$disc_method=gsub("RSTQ","QuantilDisc",B_perf_all_iter$disc_method)

B_perf_all_iter=B_perf_all_iter[which(B_perf_all_iter$disc_method %in% c("cont","ForestDisc",
                                                                         "OneR","ScottEqualWidth","ClusterDisc","Infogain","Logreg","Naive","RDGR","QuantilDisc",
                                                                         "EqualLength","RSTGDH","RSTLDH","Chi2","ChiMerge","CAIM","CACC","AMEVA","ExtendedChi2",
                                                                         "MDLP","ModifiedChi2","GLMDisc")),]
unique(B_perf_all_iter$disc_method)
A=unique(B_perf_all_iter$disc_method)[order(unique(B_perf_all_iter$disc_method))]

write.csv(B_perf_all_iter,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_all_iter.csv", row.names = FALSE)
testF1=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_all_iter.csv",sep=',',header= T, na.strings="?")
unique(B_perf_all_iter$class_method)
table(B_perf_all_iter$name,B_perf_all_iter$data)
# B_perf_all_iter$class_method=gsub("Boosting","BOOST",B_perf_all_iter$class_method)
# B_perf_all_iter$class_method=gsub("NaiveBayes","NBC",B_perf_all_iter$class_method)
# B_perf_all_iter$disc_method=gsub("cont","No_Discret",B_perf_all_iter$disc_method)

# 10*50*6*2*22
names(B_perf_all_iter)
B_perf_disc_summary_01 = sqldf('select  name, data, disc_method, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          stdev(NB_interval)/sqrt(count(NB_interval)) as SE_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          stdev(exetimepervar)/sqrt(count(exetimepervar)) as SE_exetimepervar,
                          avg(inconsistency) as mean_incons, 
                          stdev(inconsistency) as std_incons, 
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroPrecision) as mean_Precision, 
                          stdev(macroPrecision) as std_Precision, 
                          stdev(macroPrecision)/sqrt(count(macroPrecision)) as SE_macroPrecision,
                          avg(macroRecall) as mean_Recall, 
                          stdev(macroRecall) as std_Recall, 
                          stdev(macroRecall)/sqrt(count(macroRecall)) as SE_macroRecall,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa,
                          avg(kappa_w) as mean_Kappaw, 
                          stdev(kappa_w) as std_Kappaw,
                          stdev(kappa_w)/sqrt(count(kappa_w)) as SE_kappa_w
                          from B_perf_all_iter group by name, data,disc_method')
B_perf_disc_summary_periter_perdata = sqldf('select disc_method,iter, name, data,
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          stdev(NB_interval)/sqrt(count(NB_interval)) as SE_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          stdev(exetimepervar)/sqrt(count(exetimepervar)) as SE_exetimepervar,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc,
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa
                          from B_perf_all_iter where data="TEST" group by disc_method,iter, name')
B_perf_disc_summary_iter = sqldf('select iter, disc_method,data, 
                          avg(mean_NB_interval) as mean_NB_interval, 
                          stdev(mean_NB_interval) as std_NB_interval,
                          stdev(mean_NB_interval)/sqrt(count(mean_NB_interval)) as SE_NB_interval,
                          avg(mean_exetimepervar) as mean_exetimepervar, 
                          stdev(mean_exetimepervar) as std_exetimepervar,
                          stdev(mean_exetimepervar)/sqrt(count(mean_exetimepervar)) as SE_exetimepervar,
                          avg(mean_acc) as mean_acc, 
                          stdev(mean_acc) as std_acc, 
                          stdev(mean_acc)/sqrt(count(mean_acc)) as SE_acc,
                          avg(mean_F1) as mean_F1, 
                          stdev(mean_F1) as std_F1,
                          stdev(mean_F1)/sqrt(count(mean_F1)) as SE_F1,
                          avg(mean_Kappa) as mean_Kappa, 
                          stdev(mean_Kappa) as std_Kappa,
                          stdev(mean_Kappa)/sqrt(count(mean_Kappa)) as SE_Kappa
                          from B_perf_disc_summary_periter_perdata group by iter, disc_method')


B_perf_disc_summary_perdata = sqldf('select disc_method,  name, data,                           avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          stdev(NB_interval)/sqrt(count(NB_interval)) as SE_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          stdev(exetimepervar)/sqrt(count(exetimepervar)) as SE_exetimepervar,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_macroF1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa
                          from B_perf_all_iter where data="TEST" group by disc_method, name')
names(B_data_summary)
B_perf_disc_summary_perdata2 = sqldf('select d1.*, d2.nrow, d2.ncont_att from B_perf_disc_summary_perdata d1 
LEFT JOIN B_data_summary d2 ON (  d1.name = d2.name)')

write.csv(B_perf_disc_summary_perdata,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary_perdata.csv", row.names = FALSE)
write.csv(B_perf_disc_summary_perdata2,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary_perdata2.csv", row.names = FALSE)

B_perf_disc_summary_perclassifier_periter = sqldf('select disc_method,class_method, iter,data, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          stdev(NB_interval)/sqrt(count(NB_interval)) as SE_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          stdev(exetimepervar)/sqrt(count(exetimepervar)) as SE_exetimepervar,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          stdev(accuracy)/sqrt(count(accuracy)) as SE_acc,
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          stdev(macroF1)/sqrt(count(macroF1)) as SE_macroF1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa, 
                          stdev(kappa)/sqrt(count(kappa)) as SE_kappa
                          from B_perf_all_iter group by disc_method,class_method,iter, data')


B_perf_disc_summary_perclassifier = sqldf('select disc_method,class_method, data,
                          avg(mean_NB_interval) as mean_NB_interval, 
                          stdev(mean_NB_interval) as std_NB_interval,
                          stdev(mean_NB_interval)/sqrt(count(mean_NB_interval)) as SE_NB_interval,
                          avg(mean_exetimepervar) as mean_exetimepervar, 
                          stdev(mean_exetimepervar) as std_exetimepervar,
                          stdev(mean_exetimepervar)/sqrt(count(mean_exetimepervar)) as SE_exetimepervar,
                          avg(mean_acc) as mean_acc, 
                          stdev(mean_acc) as std_acc, 
                          stdev(mean_acc)/sqrt(count(mean_acc)) as SE_acc,
                          avg(mean_F1) as mean_F1, 
                          stdev(mean_F1) as std_F1,
                          stdev(mean_F1)/sqrt(count(mean_F1)) as SE_F1,
                          avg(mean_Kappa) as mean_Kappa, 
                          stdev(mean_Kappa) as std_Kappa,
                          stdev(mean_Kappa)/sqrt(count(mean_Kappa)) as SE_Kappa
                          from B_perf_disc_summary_perclassifier_periter where data="TEST" group by disc_method,class_method, data')


B_perf_disc_summary_ = sqldf('select name, disc_method,data, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa 
                          from B_perf_all_iter group by name, disc_method,data')

# write.csv(B_perf_disc_summary_perdiscmeth,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary_perdiscmeth.csv", row.names = FALSE)
# testF2=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary_perdiscmeth",sep=',',header= T, na.strings="?")

B_perf_disc_summary_compare = sqldf('select name, disc_method,data, 
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exetimepervar) as mean_exetimepervar, 
                          stdev(exetimepervar) as std_exetimepervar,
                          avg(accuracy) as mean_acc, 
                          stdev(accuracy) as std_acc, 
                          avg(macroF1) as mean_F1, 
                          stdev(macroF1) as std_F1,
                          avg(kappa) as mean_Kappa, 
                          stdev(kappa) as std_Kappa 
                          from B_perf_all_iter group by name, disc_method,data')

# summary perf regarding testing data
All_DIFFPERF=data.frame()
All_DIFFPERF_summary=data.frame()
PERF2= sqldf('select name, disc_method,data,mean_acc, mean_F1, mean_Kappa
             from B_perf_disc_summary_compare 
             where disc_method ="cont" and data = "TEST" 
             group by name, disc_method,data')

for (discMeth in unique(B_perf_disc_summary_compare$disc_method))
{
  meth=discMeth
  datatest="TEST"
  PERF1=fn$sqldf("select name, disc_method, data,mean_acc, mean_F1, mean_Kappa from B_perf_disc_summary_compare where disc_method ='$meth' and data = '$datatest' group by name, disc_method, data")

  DIFFPERF= sqldf('select PERF1.*, PERF1.mean_acc - PERF2.mean_acc as diff_acc,
                  (PERF1.mean_acc - PERF2.mean_acc)/PERF2.mean_acc as relative_diff_acc,
                  PERF1.mean_F1 - PERF2.mean_F1 as diff_F1,
                  (PERF1.mean_F1 - PERF2.mean_F1)/PERF2.mean_F1 as relative_diff_F1,
                  PERF1.mean_Kappa - PERF2.mean_Kappa as diff_Kappa,
                  (PERF1.mean_Kappa - PERF2.mean_Kappa)/PERF2.mean_F1 as relative_diff_Kappa
                   from PERF1 left join PERF2 on PERF1.name=PERF2.name')
  All_DIFFPERF=rbind(All_DIFFPERF,DIFFPERF)
  DIFFPERF_summary= sqldf('select disc_method, 
                  avg(diff_acc) as mean_diff_acc,
                  avg(relative_diff_acc) as mean_relative_diff_acc , 
                  avg(diff_F1) as mean_diff_F1,
                  avg(relative_diff_F1) as mean_relative_diff_F1,
                  avg(diff_Kappa) as mean_diff_Kappa,
                  avg(relative_diff_Kappa) as mean_relative_diff_Kappa
                   from DIFFPERF')

  DIFFPERF_summary[,"winsACC_rate"]= length(DIFFPERF[which(DIFFPERF$diff_acc>0),"diff_acc"])/length(DIFFPERF$diff_acc)
  DIFFPERF_summary[,"winsF1_rate"]= length(DIFFPERF[which(DIFFPERF$diff_F1>0),"diff_acc"])/length(DIFFPERF$diff_F1)
  DIFFPERF_summary[,"winsKappa_rate"]= length(DIFFPERF[which(DIFFPERF$diff_Kappa>0),"diff_acc"])/length(DIFFPERF$diff_Kappa)
  All_DIFFPERF_summary=rbind(All_DIFFPERF_summary,DIFFPERF_summary)

}
write.csv(All_DIFFPERF,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\All_DIFFPERF.csv", row.names = FALSE)
testF3=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\All_DIFFPERF.csv",sep=',',header= T, na.strings="?")

write.csv(All_DIFFPERF_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\All_DIFFPERF_summary.csv", row.names = FALSE)
testF4=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\All_DIFFPERF_summary.csv",sep=',',header= T, na.strings="?")
str(testF4)
write.csv(B_perf_disc_summary_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary_perclassifier.csv", row.names = FALSE)

B_perf_disc_summary = sqldf('select  data, disc_method, 
                          avg(mean_NB_interval) as mean_NB_interval, 
                          avg(std_NB_interval) as std_NB_interval,
                          avg(SE_NB_interval) as SE_NB_interval,
                          avg(mean_exetimepervar) as mean_exetimepervar, 
                          avg(std_exetimepervar) as std_exetimepervar,
                          avg(SE_exetimepervar) as SE_exetimepervar,
                          avg(mean_acc) as mean_acc, 
                          avg(std_acc) as std_acc,
                          avg(SE_acc) as SE_acc,
                          avg(mean_F1) as mean_F1, 
                          avg(std_F1) as std_F1,
                          avg(SE_F1) as SE_F1,
                          avg(mean_Kappa) as mean_Kappa, 
                          avg(std_Kappa) as std_Kappa,
                          avg(SE_Kappa) as SE_Kappa
                          from B_perf_disc_summary_01 group by data,disc_method')

write.csv(B_perf_disc_summary,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary.csv", row.names = FALSE)
testF5=read.table("D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\B_perf_disc_summary.csv",sep=',',header= T, na.strings="?")

# Dataperf=B_perf_disc_summary_perdata
# Dataperf=B_perf_disc_summary_iter
names(Dataperf)
unique(Dataperf$disc_method)
Dataperf=B_perf_all_iter[which(B_perf_all_iter[,"data"]=="TEST"),]
unique(Dataperf$data)
Dataperf=Dataperf[which(Dataperf[,"disc_method"]!="cont"),]
L=unique(Dataperf$disc_method)
LL=1:length(L)
# wilcox test score in Accuracy
Att_perf="accuracy"
wilcox_accuracy= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_accuracy)=L
colnames(wilcox_accuracy)=L
wilcox_accuracy_pval=wilcox_accuracy
wilcox_accuracy_shift=wilcox_accuracy
wilcox_accuracy_contint_min=wilcox_accuracy
wilcox_accuracy_contint_max=wilcox_accuracy
wilcox_accuracy_score=wilcox_accuracy
for (id_disc_A in (1:length(L)))
{
  A_name=L[id_disc_A]
  A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
  for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
    B_name=L[id_disc_B]
    B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_accuracy_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
    wilcox_accuracy_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
    wilcox_accuracy_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
    wilcox_accuracy_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
    
    }
  
}

for (id_disc_A in (1:length(L)))
{
  
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    if (wilcox_accuracy_pval[id_disc_A,id_disc_B] >= 0.05) 
    {
      wilcox_accuracy_score[id_disc_A,id_disc_B]=0
    } else if (wilcox_accuracy_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_accuracy_contint_max[id_disc_A,id_disc_B]> 0 ) 
    {
      wilcox_accuracy_score[id_disc_A,id_disc_B]=1
    } else if (wilcox_accuracy_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_accuracy_contint_max[id_disc_A,id_disc_B]< 0 )
    {
      wilcox_accuracy_score[id_disc_A,id_disc_B]=-1  
    }else 
    {
      wilcox_accuracy_score[id_disc_A,id_disc_B]=0     
    }
  }
}
count_wilcox_accuracy=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_accuracy)=c("Discretizer","Wins_count","Ties_count","Loss_count")
count_wilcox_accuracy[,"Discretizer"]=L
for (i in (1:length(L)))
{
  count_wilcox_accuracy[i,"Wins_count"]=length(which(wilcox_accuracy_score[i,]==1)) 
  count_wilcox_accuracy[i,"Ties_count"]=length(which(wilcox_accuracy_score[i,]==0))
  count_wilcox_accuracy[i,"Loss_count"]=length(which(wilcox_accuracy_score[i,]==-1))
}

write.csv(wilcox_accuracy_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_pval.csv", row.names = FALSE)
write.csv(wilcox_accuracy_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_shift.csv", row.names = FALSE)
write.csv(wilcox_accuracy_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_min.csv", row.names = FALSE)
write.csv(wilcox_accuracy_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_max.csv", row.names = FALSE)
write.csv(wilcox_accuracy_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_score.csv", row.names = FALSE)
write.csv(count_wilcox_accuracy,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_accuracy.csv", row.names = FALSE)

# wilcox test score in kappa
Att_perf="kappa"
wilcox_kappa= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_kappa)=L
colnames(wilcox_kappa)=L
wilcox_kappa_pval=wilcox_kappa
wilcox_kappa_shift=wilcox_kappa
wilcox_kappa_contint_min=wilcox_kappa
wilcox_kappa_contint_max=wilcox_kappa
wilcox_kappa_score=wilcox_kappa
for (id_disc_A in (1:length(L)))
{
  A_name=L[id_disc_A]
  A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    B_name=L[id_disc_B]
    B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_kappa_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
    wilcox_kappa_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
    wilcox_kappa_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
    wilcox_kappa_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_disc_A in (1:length(L)))
{
  
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    if (wilcox_kappa_pval[id_disc_A,id_disc_B] >= 0.05) 
    {
      wilcox_kappa_score[id_disc_A,id_disc_B]=0
    } else if (wilcox_kappa_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_kappa_contint_max[id_disc_A,id_disc_B]> 0 ) 
    {
      wilcox_kappa_score[id_disc_A,id_disc_B]=1
    } else if (wilcox_kappa_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_kappa_contint_max[id_disc_A,id_disc_B]< 0 )
    {
      wilcox_kappa_score[id_disc_A,id_disc_B]=-1  
    }else 
    {
      wilcox_kappa_score[id_disc_A,id_disc_B]=0     
    }
  }
}
count_wilcox_kappa=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_kappa)=c("Discretizer","Wins_count","Ties_count","Loss_count")
count_wilcox_kappa[,"Discretizer"]=L
for (i in (1:length(L)))
{
  count_wilcox_kappa[i,"Wins_count"]=length(which(wilcox_kappa_score[i,]==1)) 
  count_wilcox_kappa[i,"Ties_count"]=length(which(wilcox_kappa_score[i,]==0))
  count_wilcox_kappa[i,"Loss_count"]=length(which(wilcox_kappa_score[i,]==-1))
}

write.csv(wilcox_kappa_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_pval.csv", row.names = FALSE)
write.csv(wilcox_kappa_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_shift.csv", row.names = FALSE)
write.csv(wilcox_kappa_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_contint_min.csv", row.names = FALSE)
write.csv(wilcox_kappa_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_contint_max.csv", row.names = FALSE)
write.csv(wilcox_kappa_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_score.csv", row.names = FALSE)
write.csv(count_wilcox_kappa,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_kappa.csv", row.names = FALSE)

# wilcox test score in macroF1
Att_perf="macroF1"
wilcox_macroF1= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_macroF1)=L
colnames(wilcox_macroF1)=L
wilcox_macroF1_pval=wilcox_macroF1
wilcox_macroF1_shift=wilcox_macroF1
wilcox_macroF1_contint_min=wilcox_macroF1
wilcox_macroF1_contint_max=wilcox_macroF1
wilcox_macroF1_score=wilcox_macroF1
for (id_disc_A in (1:length(L)))
{
  A_name=L[id_disc_A]
  A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    B_name=L[id_disc_B]
    B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
    wilcox_macroF1_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
    wilcox_macroF1_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
    wilcox_macroF1_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
    wilcox_macroF1_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_disc_A in (1:length(L)))
{
  
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    if (wilcox_macroF1_pval[id_disc_A,id_disc_B] >= 0.05) 
    {
      wilcox_macroF1_score[id_disc_A,id_disc_B]=0
    } else if (wilcox_macroF1_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_macroF1_contint_max[id_disc_A,id_disc_B]> 0 ) 
    {
      wilcox_macroF1_score[id_disc_A,id_disc_B]=1
    } else if (wilcox_macroF1_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_macroF1_contint_max[id_disc_A,id_disc_B]< 0 )
    {
      wilcox_macroF1_score[id_disc_A,id_disc_B]=-1  
    }else 
    {
      wilcox_macroF1_score[id_disc_A,id_disc_B]=0     
    }
  }
}
count_wilcox_macroF1=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_macroF1)=c("Discretizer","Wins_count","Ties_count","Loss_count")
count_wilcox_macroF1[,"Discretizer"]=L
for (i in (1:length(L)))
{
  count_wilcox_macroF1[i,"Wins_count"]=length(which(wilcox_macroF1_score[i,]==1)) 
  count_wilcox_macroF1[i,"Ties_count"]=length(which(wilcox_macroF1_score[i,]==0))
  count_wilcox_macroF1[i,"Loss_count"]=length(which(wilcox_macroF1_score[i,]==-1))
}

write.csv(wilcox_macroF1_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_pval.csv", row.names = FALSE)
write.csv(wilcox_macroF1_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_shift.csv", row.names = FALSE)
write.csv(wilcox_macroF1_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_contint_min.csv", row.names = FALSE)
write.csv(wilcox_macroF1_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_contint_max.csv", row.names = FALSE)
write.csv(wilcox_macroF1_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_score.csv", row.names = FALSE)
write.csv(count_wilcox_macroF1,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_macroF1.csv", row.names = FALSE)


# bins, exetime


# wilcox test score in exetime
Att_perf="exetimepervar"
wilcox_exetimepervar= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_exetimepervar)=L
colnames(wilcox_exetimepervar)=L
wilcox_exetimepervar_pval=wilcox_exetimepervar
wilcox_exetimepervar_shift=wilcox_exetimepervar
wilcox_exetimepervar_contint_min=wilcox_exetimepervar
wilcox_exetimepervar_contint_max=wilcox_exetimepervar
wilcox_exetimepervar_score=wilcox_exetimepervar
# id_disc_A=7
# id_disc_B=8
for (id_disc_A in (1:length(L)))
{
  A_name=L[id_disc_A]
  A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    B_name=L[id_disc_B]
    B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(B,A , conf.int = TRUE,paired=TRUE)
    wilcox_exetimepervar_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
    wilcox_exetimepervar_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
    wilcox_exetimepervar_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
    wilcox_exetimepervar_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_disc_A in (1:length(L)))
{
  
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    if (wilcox_exetimepervar_pval[id_disc_A,id_disc_B] >= 0.05) 
    {
      wilcox_exetimepervar_score[id_disc_A,id_disc_B]=0
    } else if (wilcox_exetimepervar_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_exetimepervar_contint_max[id_disc_A,id_disc_B]> 0 ) 
    {
      wilcox_exetimepervar_score[id_disc_A,id_disc_B]=1
    } else if (wilcox_exetimepervar_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_exetimepervar_contint_max[id_disc_A,id_disc_B]< 0 )
    {
      wilcox_exetimepervar_score[id_disc_A,id_disc_B]=-1  
    }else 
    {
      wilcox_exetimepervar_score[id_disc_A,id_disc_B]=0     
    }
  }
  
}
count_wilcox_exetimepervar=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_exetimepervar)=c("Discretizer","Wins_count","Ties_count","Loss_count")
count_wilcox_exetimepervar[,"Discretizer"]=L
for (i in (1:length(L)))
{
  count_wilcox_exetimepervar[i,"Wins_count"]=length(which(wilcox_exetimepervar_score[i,]==1)) 
  count_wilcox_exetimepervar[i,"Ties_count"]=length(which(wilcox_exetimepervar_score[i,]==0))
  count_wilcox_exetimepervar[i,"Loss_count"]=length(which(wilcox_exetimepervar_score[i,]==-1))
}

write.csv(wilcox_exetimepervar_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_exetimepervar_pval.csv", row.names = FALSE)
write.csv(wilcox_exetimepervar_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_exetimepervar_shift.csv", row.names = FALSE)
write.csv(wilcox_exetimepervar_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_exetimepervar_contint_min.csv", row.names = FALSE)
write.csv(wilcox_exetimepervar_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_exetimepervar_contint_max.csv", row.names = FALSE)
write.csv(wilcox_exetimepervar_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_exetimepervar_score.csv", row.names = FALSE)
write.csv(count_wilcox_exetimepervar,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_exetimepervar.csv", row.names = FALSE)

Att_perf="NB_interval"
wilcox_NB_interval= data.frame(matrix(ncol=length(L),nrow=length(L)))
rownames(wilcox_NB_interval)=L
colnames(wilcox_NB_interval)=L
wilcox_NB_interval_pval=wilcox_NB_interval
wilcox_NB_interval_shift=wilcox_NB_interval
wilcox_NB_interval_contint_min=wilcox_NB_interval
wilcox_NB_interval_contint_max=wilcox_NB_interval
wilcox_NB_interval_score=wilcox_NB_interval
# id_disc_A=7
# id_disc_B=8
for (id_disc_A in (1:length(L)))
{
  A_name=L[id_disc_A]
  A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    B_name=L[id_disc_B]
    B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
    wilcox_AB=wilcox.test(B,A , conf.int = TRUE,paired=TRUE)
    wilcox_NB_interval_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
    wilcox_NB_interval_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
    wilcox_NB_interval_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
    wilcox_NB_interval_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
    
  }
  
}

for (id_disc_A in (1:length(L)))
{
  
  for (id_disc_B in setdiff(LL,id_disc_A)) 
  {
    if (wilcox_NB_interval_pval[id_disc_A,id_disc_B] >= 0.05) 
    {
      wilcox_NB_interval_score[id_disc_A,id_disc_B]=0
    } else if (wilcox_NB_interval_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_NB_interval_contint_max[id_disc_A,id_disc_B]> 0 ) 
    {
      wilcox_NB_interval_score[id_disc_A,id_disc_B]=1
    } else if (wilcox_NB_interval_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_NB_interval_contint_max[id_disc_A,id_disc_B]< 0 )
    {
      wilcox_NB_interval_score[id_disc_A,id_disc_B]=-1  
    }else 
    {
      wilcox_NB_interval_score[id_disc_A,id_disc_B]=0     
    }
  }
  
}
count_wilcox_NB_interval=data.frame(matrix(ncol=4,nrow=length(L)))
colnames(count_wilcox_NB_interval)=c("Discretizer","Wins_count","Ties_count","Loss_count")
count_wilcox_NB_interval[,"Discretizer"]=L
for (i in (1:length(L)))
{
  count_wilcox_NB_interval[i,"Wins_count"]=length(which(wilcox_NB_interval_score[i,]==1)) 
  count_wilcox_NB_interval[i,"Ties_count"]=length(which(wilcox_NB_interval_score[i,]==0))
  count_wilcox_NB_interval[i,"Loss_count"]=length(which(wilcox_NB_interval_score[i,]==-1))
}

write.csv(wilcox_NB_interval_pval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_NB_interval_pval.csv", row.names = FALSE)
write.csv(wilcox_NB_interval_shift,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_NB_interval_shift.csv", row.names = FALSE)
write.csv(wilcox_NB_interval_contint_min,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_NB_interval_contint_min.csv", row.names = FALSE)
write.csv(wilcox_NB_interval_contint_max,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_NB_interval_contint_max.csv", row.names = FALSE)
write.csv(wilcox_NB_interval_score,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_NB_interval_score.csv", row.names = FALSE)
write.csv(count_wilcox_NB_interval,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_NB_interval.csv", row.names = FALSE)


boxplot(data.frame(A,B))


# perf_disc_perclassifier = sqldf('select  name, data, class_method, disc_method, 
#                           avg(NB_interval) as mean_NB_interval, 
#                           stdev(NB_interval) as std_NB_interval,
#                           avg(exetimepervar) as mean_exetimepervar, 
#                           stdev(exetimepervar) as std_exetimepervar,
#                           avg(opt_value) as mean_opt_value, 
#                           stdev(opt_value) as std_opt_value,
#                           avg(inconsistency) as mean_incons, 
#                           stdev(inconsistency) as std_incons, 
#                           avg(accuracy) as mean_acc, 
#                           stdev(accuracy) as std_acc, 
#                           avg(macroPrecision) as mean_Precision, 
#                           stdev(macroPrecision) as std_Precision, 
#                           avg(macroRecall) as mean_Recall, 
#                           stdev(macroRecall) as std_Recall, 
#                           avg(macroF1) as mean_F1, 
#                           stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
#                           stdev(kappa) as std_Kappa, 
#                           avg(kappa_w) as mean_Kappaw, 
#                           stdev(kappa_w) as std_Kappaw, 
#                           avg(nulcut) as mean_nulcut, 
#                           stdev(nulcut) as std_nulcut
#                           from perf_all_iter group by data,class_method,disc_method')
# 
# 
# perf_opt_disc_iter = sqldf('select  name, data, disc_method, iter, 
#                           NB_interval,exe_time, exetimepervar,opt_value, 
#                           inconsistency,
#                           avg(accuracy) as mean_acc, 
#                           stdev(accuracy) as std_acc, 
#                           avg(macroPrecision) as mean_Precision, 
#                           stdev(macroPrecision) as std_Precision, 
#                           avg(macroRecall) as mean_Recall, 
#                           stdev(macroRecall) as std_Recall, 
#                           avg(macroF1) as mean_F1, 
#                           stdev(macroF1) as std_F1,avg(kappa) as mean_Kappa, 
#                           stdev(kappa) as std_Kappa, 
#                           avg(kappa_w) as mean_Kappaw, 
#                           stdev(kappa_w) as std_Kappaw,
#                           nulcut
#                           from perf_all_iter 
#                           where data in ("TEST")
#                           group by data, disc_method, iter')

# wilcoxon per classifier

Dataperf=B_perf_all_iter[which(B_perf_all_iter[,"data"]=="TEST"),]
unique(Dataperf$data)
Dataperf=Dataperf[which(Dataperf[,"disc_method"]!="cont"),]
# Dataperf$disc_method<-factor(Dataperf$disc_method,levels=levDisc[order(levDisc)])
Dataperf$class_method<-factor(Dataperf$class_method,levels=unique(Dataperf$class_method)[c(5,3,4,1,2,6)])
CL=unique(Dataperf$class_method)
L=unique(Dataperf$disc_method)
LL=1:length(L)
Dataperf01=Dataperf

# wilcox test score in Accuracy
Att_perf="accuracy"
"RF" %in% CL
wilcox_accuracy_pval_perclassifier=data.frame()
wilcox_accuracy_shift_perclassifier=data.frame()
wilcox_accuracy_contint_min_perclassifier=data.frame()
wilcox_accuracy_contint_max_perclassifier=data.frame()
wilcox_accuracy_score_perclassifier=data.frame()
count_wilcox_accuracy_perclassifier=data.frame()

for (classifier in CL) 
{
  Dataperf=Dataperf01[which(Dataperf01[,"class_method"]==classifier),]
  wilcox_accuracy= data.frame(matrix(ncol=length(L),nrow=length(L)))
  rownames(wilcox_accuracy)=L
  colnames(wilcox_accuracy)=L
  wilcox_accuracy_pval=wilcox_accuracy
  wilcox_accuracy_shift=wilcox_accuracy
  wilcox_accuracy_contint_min=wilcox_accuracy
  wilcox_accuracy_contint_max=wilcox_accuracy
  wilcox_accuracy_score=wilcox_accuracy
  for (id_disc_A in (1:length(L)))
  {
    A_name=L[id_disc_A]
    A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      B_name=L[id_disc_B]
      B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
      wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
      wilcox_accuracy_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
      wilcox_accuracy_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
      wilcox_accuracy_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
      wilcox_accuracy_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
      
    }
    
  }
  
  for (id_disc_A in (1:length(L)))
  {
    
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      if (wilcox_accuracy_pval[id_disc_A,id_disc_B] >= 0.05) 
      {
        wilcox_accuracy_score[id_disc_A,id_disc_B]=0
      } else if (wilcox_accuracy_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_accuracy_contint_max[id_disc_A,id_disc_B]> 0 ) 
      {
        wilcox_accuracy_score[id_disc_A,id_disc_B]=1
      } else if (wilcox_accuracy_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_accuracy_contint_max[id_disc_A,id_disc_B]< 0 )
      {
        wilcox_accuracy_score[id_disc_A,id_disc_B]=-1  
      }else 
      {
        wilcox_accuracy_score[id_disc_A,id_disc_B]=0     
      }
    }
  }
  count_wilcox_accuracy=data.frame(matrix(ncol=4,nrow=length(L)))
  colnames(count_wilcox_accuracy)=c("Discretizer","Wins_count","Ties_count","Loss_count")
  count_wilcox_accuracy[,"Discretizer"]=L
  for (i in (1:length(L)))
  {
    count_wilcox_accuracy[i,"Wins_count"]=length(which(wilcox_accuracy_score[i,]==1)) 
    count_wilcox_accuracy[i,"Ties_count"]=length(which(wilcox_accuracy_score[i,]==0))
    count_wilcox_accuracy[i,"Loss_count"]=length(which(wilcox_accuracy_score[i,]==-1))
  }
  wilcox_accuracy_pval_CL=wilcox_accuracy_pval
  wilcox_accuracy_pval_CL[,"class_method"]=classifier
  wilcox_accuracy_shift_CL=wilcox_accuracy_shift
  wilcox_accuracy_shift_CL[,"class_method"]=classifier
  wilcox_accuracy_contint_min_CL=wilcox_accuracy_contint_min
  wilcox_accuracy_contint_min_CL[,"class_method"]=classifier
  wilcox_accuracy_contint_max_CL=wilcox_accuracy_contint_max
  wilcox_accuracy_contint_max_CL[,"class_method"]=classifier
  wilcox_accuracy_score_CL=wilcox_accuracy_score
  wilcox_accuracy_score_CL[,"class_method"]=classifier
  count_wilcox_accuracy_CL=count_wilcox_accuracy
  count_wilcox_accuracy_CL[,"class_method"]=classifier
  wilcox_accuracy_pval_perclassifier=rbind(wilcox_accuracy_pval_perclassifier,wilcox_accuracy_pval_CL)
  wilcox_accuracy_shift_perclassifier=rbind(wilcox_accuracy_shift_perclassifier,wilcox_accuracy_shift_CL)
  wilcox_accuracy_contint_min_perclassifier=rbind(wilcox_accuracy_contint_min_perclassifier,wilcox_accuracy_contint_min_CL)
  wilcox_accuracy_contint_max_perclassifier=rbind(wilcox_accuracy_contint_max_perclassifier,wilcox_accuracy_contint_max_CL)
  wilcox_accuracy_score_perclassifier=rbind(wilcox_accuracy_score_perclassifier,wilcox_accuracy_score_CL)
  count_wilcox_accuracy_perclassifier=rbind(count_wilcox_accuracy_perclassifier,count_wilcox_accuracy_CL)
}

write.csv(wilcox_accuracy_pval_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_pval_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_accuracy_shift_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_shift_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_accuracy_contint_min_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_min_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_accuracy_contint_max_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_contint_max_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_accuracy_score_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_accuracy_score_perclassifier.csv", row.names = FALSE)
write.csv(count_wilcox_accuracy_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_accuracy_perclassifier.csv", row.names = FALSE)

# wilcox test score in kappa

Att_perf="kappa"

wilcox_kappa_pval_perclassifier=data.frame()
wilcox_kappa_shift_perclassifier=data.frame()
wilcox_kappa_contint_min_perclassifier=data.frame()
wilcox_kappa_contint_max_perclassifier=data.frame()
wilcox_kappa_score_perclassifier=data.frame()
count_wilcox_kappa_perclassifier=data.frame()

for (classifier in CL) 
{
  Dataperf=Dataperf01[which(Dataperf01[,"class_method"]==classifier),]
  wilcox_kappa= data.frame(matrix(ncol=length(L),nrow=length(L)))
  rownames(wilcox_kappa)=L
  colnames(wilcox_kappa)=L
  wilcox_kappa_pval=wilcox_kappa
  wilcox_kappa_shift=wilcox_kappa
  wilcox_kappa_contint_min=wilcox_kappa
  wilcox_kappa_contint_max=wilcox_kappa
  wilcox_kappa_score=wilcox_kappa
  for (id_disc_A in (1:length(L)))
  {
    A_name=L[id_disc_A]
    A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      B_name=L[id_disc_B]
      B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
      wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
      wilcox_kappa_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
      wilcox_kappa_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
      wilcox_kappa_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
      wilcox_kappa_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
      
    }
    
  }
  
  for (id_disc_A in (1:length(L)))
  {
    
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      if (wilcox_kappa_pval[id_disc_A,id_disc_B] >= 0.05) 
      {
        wilcox_kappa_score[id_disc_A,id_disc_B]=0
      } else if (wilcox_kappa_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_kappa_contint_max[id_disc_A,id_disc_B]> 0 ) 
      {
        wilcox_kappa_score[id_disc_A,id_disc_B]=1
      } else if (wilcox_kappa_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_kappa_contint_max[id_disc_A,id_disc_B]< 0 )
      {
        wilcox_kappa_score[id_disc_A,id_disc_B]=-1  
      }else 
      {
        wilcox_kappa_score[id_disc_A,id_disc_B]=0     
      }
    }
  }
  count_wilcox_kappa=data.frame(matrix(ncol=4,nrow=length(L)))
  colnames(count_wilcox_kappa)=c("Discretizer","Wins_count","Ties_count","Loss_count")
  count_wilcox_kappa[,"Discretizer"]=L
  for (i in (1:length(L)))
  {
    count_wilcox_kappa[i,"Wins_count"]=length(which(wilcox_kappa_score[i,]==1)) 
    count_wilcox_kappa[i,"Ties_count"]=length(which(wilcox_kappa_score[i,]==0))
    count_wilcox_kappa[i,"Loss_count"]=length(which(wilcox_kappa_score[i,]==-1))
  }
  wilcox_kappa_pval_CL=wilcox_kappa_pval
  wilcox_kappa_pval_CL[,"class_method"]=classifier
  wilcox_kappa_shift_CL=wilcox_kappa_shift
  wilcox_kappa_shift_CL[,"class_method"]=classifier
  wilcox_kappa_contint_min_CL=wilcox_kappa_contint_min
  wilcox_kappa_contint_min_CL[,"class_method"]=classifier
  wilcox_kappa_contint_max_CL=wilcox_kappa_contint_max
  wilcox_kappa_contint_max_CL[,"class_method"]=classifier
  wilcox_kappa_score_CL=wilcox_kappa_score
  wilcox_kappa_score_CL[,"class_method"]=classifier
  count_wilcox_kappa_CL=count_wilcox_kappa
  count_wilcox_kappa_CL[,"class_method"]=classifier
  wilcox_kappa_pval_perclassifier=rbind(wilcox_kappa_pval_perclassifier,wilcox_kappa_pval_CL)
  wilcox_kappa_shift_perclassifier=rbind(wilcox_kappa_shift_perclassifier,wilcox_kappa_shift_CL)
  wilcox_kappa_contint_min_perclassifier=rbind(wilcox_kappa_contint_min_perclassifier,wilcox_kappa_contint_min_CL)
  wilcox_kappa_contint_max_perclassifier=rbind(wilcox_kappa_contint_max_perclassifier,wilcox_kappa_contint_max_CL)
  wilcox_kappa_score_perclassifier=rbind(wilcox_kappa_score_perclassifier,wilcox_kappa_score_CL)
  count_wilcox_kappa_perclassifier=rbind(count_wilcox_kappa_perclassifier,count_wilcox_kappa_CL)
}

write.csv(wilcox_kappa_pval_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_pval_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_kappa_shift_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_shift_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_kappa_contint_min_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_contint_min_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_kappa_contint_max_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_contint_max_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_kappa_score_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_kappa_score_perclassifier.csv", row.names = FALSE)
write.csv(count_wilcox_kappa_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_kappa_perclassifier.csv", row.names = FALSE)

# wilcox test score in macroF1

Att_perf="macroF1"
"RF" %in% CL
wilcox_macroF1_pval_perclassifier=data.frame()
wilcox_macroF1_shift_perclassifier=data.frame()
wilcox_macroF1_contint_min_perclassifier=data.frame()
wilcox_macroF1_contint_max_perclassifier=data.frame()
wilcox_macroF1_score_perclassifier=data.frame()
count_wilcox_macroF1_perclassifier=data.frame()

for (classifier in CL) 
{
  Dataperf=Dataperf01[which(Dataperf01[,"class_method"]==classifier),]
  wilcox_macroF1= data.frame(matrix(ncol=length(L),nrow=length(L)))
  rownames(wilcox_macroF1)=L
  colnames(wilcox_macroF1)=L
  wilcox_macroF1_pval=wilcox_macroF1
  wilcox_macroF1_shift=wilcox_macroF1
  wilcox_macroF1_contint_min=wilcox_macroF1
  wilcox_macroF1_contint_max=wilcox_macroF1
  wilcox_macroF1_score=wilcox_macroF1
  for (id_disc_A in (1:length(L)))
  {
    A_name=L[id_disc_A]
    A=Dataperf[which(Dataperf[,"disc_method"]==A_name),Att_perf]
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      B_name=L[id_disc_B]
      B=Dataperf[which(Dataperf[,"disc_method"]==B_name),Att_perf]
      wilcox_AB=wilcox.test(A, B , conf.int = TRUE,paired=TRUE)
      wilcox_macroF1_pval[id_disc_A,id_disc_B]=wilcox_AB$p.value
      wilcox_macroF1_shift[id_disc_A,id_disc_B]=wilcox_AB$estimate
      wilcox_macroF1_contint_min[id_disc_A,id_disc_B]=wilcox_AB$conf.int[1]
      wilcox_macroF1_contint_max[id_disc_A,id_disc_B]=wilcox_AB$conf.int[2]
      
    }
    
  }
  
  for (id_disc_A in (1:length(L)))
  {
    
    for (id_disc_B in setdiff(LL,id_disc_A)) 
    {
      if (wilcox_macroF1_pval[id_disc_A,id_disc_B] >= 0.05) 
      {
        wilcox_macroF1_score[id_disc_A,id_disc_B]=0
      } else if (wilcox_macroF1_contint_min[id_disc_A,id_disc_B] > 0 & wilcox_macroF1_contint_max[id_disc_A,id_disc_B]> 0 ) 
      {
        wilcox_macroF1_score[id_disc_A,id_disc_B]=1
      } else if (wilcox_macroF1_contint_min[id_disc_A,id_disc_B] < 0 & wilcox_macroF1_contint_max[id_disc_A,id_disc_B]< 0 )
      {
        wilcox_macroF1_score[id_disc_A,id_disc_B]=-1  
      }else 
      {
        wilcox_macroF1_score[id_disc_A,id_disc_B]=0     
      }
    }
  }
  count_wilcox_macroF1=data.frame(matrix(ncol=4,nrow=length(L)))
  colnames(count_wilcox_macroF1)=c("Discretizer","Wins_count","Ties_count","Loss_count")
  count_wilcox_macroF1[,"Discretizer"]=L
  for (i in (1:length(L)))
  {
    count_wilcox_macroF1[i,"Wins_count"]=length(which(wilcox_macroF1_score[i,]==1)) 
    count_wilcox_macroF1[i,"Ties_count"]=length(which(wilcox_macroF1_score[i,]==0))
    count_wilcox_macroF1[i,"Loss_count"]=length(which(wilcox_macroF1_score[i,]==-1))
  }
  wilcox_macroF1_pval_CL=wilcox_macroF1_pval
  wilcox_macroF1_pval_CL[,"class_method"]=classifier
  wilcox_macroF1_shift_CL=wilcox_macroF1_shift
  wilcox_macroF1_shift_CL[,"class_method"]=classifier
  wilcox_macroF1_contint_min_CL=wilcox_macroF1_contint_min
  wilcox_macroF1_contint_min_CL[,"class_method"]=classifier
  wilcox_macroF1_contint_max_CL=wilcox_macroF1_contint_max
  wilcox_macroF1_contint_max_CL[,"class_method"]=classifier
  wilcox_macroF1_score_CL=wilcox_macroF1_score
  wilcox_macroF1_score_CL[,"class_method"]=classifier
  count_wilcox_macroF1_CL=count_wilcox_macroF1
  count_wilcox_macroF1_CL[,"class_method"]=classifier
  wilcox_macroF1_pval_perclassifier=rbind(wilcox_macroF1_pval_perclassifier,wilcox_macroF1_pval_CL)
  wilcox_macroF1_shift_perclassifier=rbind(wilcox_macroF1_shift_perclassifier,wilcox_macroF1_shift_CL)
  wilcox_macroF1_contint_min_perclassifier=rbind(wilcox_macroF1_contint_min_perclassifier,wilcox_macroF1_contint_min_CL)
  wilcox_macroF1_contint_max_perclassifier=rbind(wilcox_macroF1_contint_max_perclassifier,wilcox_macroF1_contint_max_CL)
  wilcox_macroF1_score_perclassifier=rbind(wilcox_macroF1_score_perclassifier,wilcox_macroF1_score_CL)
  count_wilcox_macroF1_perclassifier=rbind(count_wilcox_macroF1_perclassifier,count_wilcox_macroF1_CL)
}

write.csv(wilcox_macroF1_pval_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_pval_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_macroF1_shift_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_shift_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_macroF1_contint_min_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_contint_min_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_macroF1_contint_max_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_contint_max_perclassifier.csv", row.names = FALSE)
write.csv(wilcox_macroF1_score_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\wilcox_macroF1_score_perclassifier.csv", row.names = FALSE)
write.csv(count_wilcox_macroF1_perclassifier,"D:\\Cours Datamining\\LAB. R\\Chapter 8\\trees\\ForestDiscPerf\\All_files\\count_wilcox_macroF1_perclassifier.csv", row.names = FALSE)
