
levDisc=unique(dataplotSW[order(dataplotSW$id_disc,decreasing = FALSE),"opt_meth"])

levDisc=unique(B_perf_all_iter$disc_method)[-1]

library(grid)
library(ggplot2)
# perf benchmark
# global performance: accuracy, f1, kappa
B_perf_bench=B_perf_disc_summary
unique(B_perf_bench$disc_method)
names(B_perf_bench)


B_perf_bench=B_perf_bench[-which(B_perf_bench$disc_method=="cont" | B_perf_bench$data=="TRAIN"),]
B_perf_bench$disc_method = factor(B_perf_bench$disc_method,levels=levDisc)
B_perf_overall=B_perf_disc_summary[-which(B_perf_disc_summary$disc_method %in% c("cont")),]
B_perf_overall$disc_method = factor(B_perf_overall$disc_method,levels=levDisc[order(levDisc)])

gg001=ggplot(B_perf_overall, aes(x=disc_method, y=mean_acc,group = data,color=data,lty=data,pch=data))+ 
  labs(title="", x="", y="Accuracy") +
  geom_line() +
  geom_point() +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "top",legend.title = element_blank(),legend.text =element_text(size=7),  legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) 
# +
#   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                               "green","seagreen1","yellowgreen","seagreen",
#                               "pink","plum2","plum4","purple4",
#                               "steelblue2","steelblue","slateblue","slateblue4",
#                               "tan","tan1","tomato","tomato3"))
print(gg001)
gg002=ggplot(B_perf_overall, aes(x=disc_method, y=mean_F1,group = data,color=data,lty=data,pch=data))+ 
  labs(title="", x="", y="F1 score") +
  geom_line() +
  geom_point() +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "none"
  ) 
print(gg002)
gg003=ggplot(B_perf_overall, aes(x=disc_method, y=mean_Kappa,group = data,color=data,lty=data,pch=data))+ 
  labs(title="", x="", y="Kappa") +
  geom_line() +
  geom_point() +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=9,face="bold"), 
         axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
         legend.position = "none"
  )

print(gg003)
grid.newpage()
grid.draw(rbind( ggplotGrob(gg001), ggplotGrob(gg002),ggplotGrob(gg003), size = "last"))
# bins and exetime
names(B_perf_disc_summary_perdata2)
head(B_perf_disc_summary_perdata2)
unique(B_perf_disc_summary_perdata2$disc_method)
B_perf_disc_summary_perdata2=B_perf_disc_summary_perdata2[-which(B_perf_disc_summary_perdata2$disc_method %in% c("cont")),]

B_perf_disc_summary_perdata2$disc_method = factor(B_perf_disc_summary_perdata2$disc_method,levels=levDisc[order(levDisc)])

names(B_data_summary)
# rank (unique(as.numeric(B_perf_disc_summary_perdata2$nrow)))
B_data_summary1=B_data_summary[order(B_data_summary$nrow, decreasing = FALSE),]
B_data_summary1$name2=paste0(B_data_summary1$name,"(",B_data_summary1$nrow,")") 
unique(B_data_summary1$name2)
B_perf_disc_summary_perdata2$name=factor(B_perf_disc_summary_perdata2$name,levels=unique(B_data_summary1$name))
B_perf_disc_summary_perdata2$name2=paste0(B_perf_disc_summary_perdata2$name,"(",B_perf_disc_summary_perdata2$nrow,")") 
B_perf_disc_summary_perdata2$name2=factor(B_perf_disc_summary_perdata2$name2,levels=unique(B_data_summary1$name2))
# library(scales)
# library(wesanderson)


# boxplots exetime and intervals number  
names(B_perf_disc_summary_perdata2)
summary(B_perf_disc_summary_perdata2$mean_NB_interval)
A=tapply(B_perf_disc_summary_perdata2$mean_NB_interval, B_perf_disc_summary_perdata2$disc_method,mean)
B=tapply(B_perf_disc_summary_perdata2$mean_exetimepervar, B_perf_disc_summary_perdata2$disc_method,mean)
B=B[levDisc[order(levDisc)]]
A=A[levDisc[order(levDisc)]]
C=levDisc[order(levDisc)]
calculatedmean<-data.frame(cbind(C,A,B))
colnames(calculatedmean)=c("disc_method","overallmeanInterval","overallmeanExetime")
B_perf_disc_summary_perdata3 = sqldf('select d1.*, d2.* from B_perf_disc_summary_perdata2 d1 
LEFT JOIN calculatedmean d2 ON (  d1.disc_method = d2.disc_method)')


names(B_perf_disc_summary_perdata3)
B_perf_disc_summary_perdata3$overallmeanInterval=as.numeric(as.character(B_perf_disc_summary_perdata3$overallmeanInterval))
B_perf_disc_summary_perdata3$overallmeanExetime =as.numeric(as.character(B_perf_disc_summary_perdata3$overallmeanExetime))
summary(B_perf_disc_summary_perdata3$overallmeanInterval)
B_perf_disc_summary_perdata3=B_perf_disc_summary_perdata3[,-c(22,23)]#remove redundunt columns
gg01=ggplot(B_perf_disc_summary_perdata3, aes(x=disc_method))+ 
  geom_boxplot(aes(y=mean_NB_interval),outlier.shape = NA)+
  geom_jitter(aes(y=mean_NB_interval),width=0.1,alpha=0.1) +
  # geom_point(aes( y=overallmeanInterval,group=disc_method,color="tomato3")) + 
  # geom_line(aes( y=overallmeanInterval,group=disc_method,color="tomato3")) + 
  stat_summary(aes(y=overallmeanInterval,group = 1),fun.y=mean,color="tomato3", geom=c("line"),lty=3) +
  stat_summary(aes(y=overallmeanInterval,group = 1),fun.y=mean,color="tomato3", geom=c("point"),shape=16,size=2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^2)) +
  labs(title="", x="", y="Number of bins") +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=10,face="bold"), 
         axis.title.y=element_text(face="bold",size=10),
         legend.position = "none")

 
 
print(gg02)
grid.newpage()
grid.draw(rbind( ggplotGrob(gg01), ggplotGrob(gg02),size = "last"))
names(B_perf_disc_summary)

# 
# gg2=ggplot(B_perf_disc_summary_perdata2, aes(x=disc_method, y=mean_exetimepervar,group = name2, color=name2))+ 
#   labs(title="", x="", y="Execution time (s)") +
#   geom_line() +
#   geom_point() +
#   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.position = "none"
  ) 
# +
#   scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
#                               "green","seagreen1","yellowgreen","seagreen",
#                               "pink","plum2","plum4","purple4",
#                               "steelblue2","steelblue","slateblue","slateblue4",
#                               "tan","tan1","tomato","tomato3"))
  



gg3=ggplot(B_perf_disc_summary_perdata2, aes(x=disc_method, y=mean_NB_interval,group = name2, color=name2))+ 
  labs(title="", x="", y="Intervals number") +
  geom_line() +
  geom_point() + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  # theme_hc()+ scale_colour_hc()+
  # theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x=element_text(size=14,face="bold"),
        legend.position = "none"
        
  )
# +
  # scale_color_manual(values=c("moccasin","navajowhite3","navajowhite4","mistyrose4",
  #                             "green","seagreen1","yellowgreen","seagreen",
  #                             "pink","plum2","plum4","purple4",
  #                             "steelblue2","steelblue","slateblue","slateblue4",
  #                             "tan","tan1","tomato","tomato3"))


print(gg3)
grid.newpage()
grid.draw(rbind( ggplotGrob(gg2),ggplotGrob(gg3), size = "last"))




# diff perf compared to the case where data is not discretized
diffperfcont<-All_DIFFPERF_summary
diffperfcont<-diffperfcont[-which(diffperfcont$disc_method=="cont"),]
diffperfcont$disc_method = factor(diffperfcont$disc_method,levels=levDisc[order(levDisc)])
names(diffperfcont)
gg4=ggplot(diffperfcont, aes(x=disc_method)) + 
  geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
  geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
  geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
  geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
  geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
  geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
  # geom_line(aes(y = mean_relative_diff_acc, color= "tomato3", lty="2")) +
  # geom_point(aes( y = mean_relative_diff_acc,shape="17",color= "tomato3"))+
  # geom_line(aes(y = mean_relative_diff_F1, color= "slateblue4", lty="2"))+
  # geom_point(aes(y = mean_relative_diff_F1, shape="16",color= "slateblue4"))+
  # geom_line(aes(y = mean_relative_diff_Kappa,color= "seagreen", lty="2"))+
  # geom_point(aes( y = mean_relative_diff_Kappa,shape="15",color= "seagreen"))+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal() +
  scale_colour_manual(name = "Metrics:",
                      labels = c("Kappa", "F1 score", "Accuracy"),
                      values = c("tomato3","slateblue4","seagreen"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(name = "Metrics:",
                     labels = c("Kappa", "F1 score", "Accuracy"),
                     values = c(17,16,15),
                     guide = guide_legend(reverse = TRUE))+
  labs(title="", x="", y="Absolute loss") +
  theme(legend.position = "top",axis.title.x = element_blank(), axis.text.x = element_blank())
print(gg4)
gg5=ggplot(diffperfcont, aes(x=disc_method)) + 
  # geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
  # geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
  # geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
  # geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
  # geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
  # geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
  geom_line(aes(y = mean_relative_diff_acc,group=1, color= "tomato3")) +
  geom_point(aes( y = mean_relative_diff_acc,group=1,shape="17",color= "tomato3"))+
  geom_line(aes(y = mean_relative_diff_F1,group=1,color= "slateblue4"))+
  geom_point(aes(y = mean_relative_diff_F1,group=1,shape="16",color= "slateblue4"))+
  geom_line(aes(y = mean_relative_diff_Kappa,group=1,color= "seagreen"))+
  geom_point(aes( y = mean_relative_diff_Kappa,group=1,shape="15",color= "seagreen"))+
  scale_y_continuous(labels = scales::percent)+
  theme_minimal() +
  labs(title="", x="", y="Relative loss") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x=element_text(size=14,face="bold"),legend.position="none")+
  scale_colour_manual(name = "Metrics:",
                      labels = c("Kappa", "F1 score", "Accuracy"),
                      values = c("tomato3","slateblue4","seagreen"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(name = "Metrics:",
                     labels = c("Kappa", "F1 score", "Accuracy"),
                     values = c(17,16,15),
                     guide = guide_legend(reverse = TRUE)
                     )

  

print(gg5)
grid.newpage()
grid.draw(rbind( ggplotGrob(gg4),ggplotGrob(gg5), size = "last"))

# perf per classifiers
dfclassifiers<-B_perf_disc_summary_perclassifier
dfclassifiers<-dfclassifiers[-which(dfclassifiers$disc_method=="cont"),]
dfclassifiers$disc_method<-factor(dfclassifiers$disc_method,levels=levDisc[order(levDisc)])
dfclassifiers$class_method<-factor(dfclassifiers$class_method,levels=unique(dfclassifiers$class_method)[c(5,3,4,1,2,6)])

unique(dfclassifiers$class_method)[c(5,3,4,1,2,6)]
names(dfclassifiers)
gg7=ggplot(dfclassifiers, aes(x=disc_method)) + 
  # geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
  # geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
  # geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
  # geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
  # geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
  # geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
  geom_line(aes(y = mean_acc,group=1, color= "tomato3")) +
  geom_point(aes( y = mean_acc,group=1,shape="17",color= "tomato3"))+
  geom_line(aes(y = mean_F1,group=1,color= "slateblue4"))+
  geom_point(aes(y = mean_F1,group=1,shape="16",color= "slateblue4"))+
  geom_line(aes(y = mean_Kappa,group=1,color= "seagreen"))+
  geom_point(aes( y = mean_Kappa,group=1,shape="15",color= "seagreen"))+
  theme_minimal() +
  labs(title="", x="", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        axis.title.x=element_text(size=14,face="bold"),legend.position="top")+
  scale_colour_manual(name = "Metrics:",
                      labels = c("Kappa", "F1 score", "Accuracy"),
                      values = c("tomato3","slateblue4","seagreen"),
                      guide = guide_legend(reverse = TRUE)) +
  scale_shape_manual(name = "Metrics:",
                     labels = c("Kappa", "F1 score", "Accuracy"),
                     values = c(17,16,15),
                     guide = guide_legend(reverse = TRUE)
  )

print(gg7)
gg7 + facet_wrap( ~ class_method, ncol=3,scales = "free_y")


# wilcox per classifier
count_wilcox_accuracy_perclassifier
wilcox_accuracy_classifiers<-count_wilcox_accuracy_perclassifier
wilcox_accuracy_classifiers$class_method=factor(wilcox_accuracy_classifiers$class_method,levels=unique(wilcox_accuracy_classifiers$class_method)[c(1,6,5,2,3,4)])
# T= sqldf('select * from wilcox_accuracy_classifiers 
#         order by class_method, Wins_count DESC,Ties_count DESC,Loss_count ASC')

wilcox_accuracy_classifiers=wilcox_accuracy_classifiers[with(wilcox_accuracy_classifiers, order(wilcox_accuracy_classifiers$class_method,-wilcox_accuracy_classifiers$Wins_count,-wilcox_accuracy_classifiers$Ties_count,wilcox_accuracy_classifiers$Loss_count)),]
W_a_C_w=cbind(wilcox_accuracy_classifiers[,c(1,2,5)],rep("Wins"))
colnames(W_a_C_w)=c("Discretizer","Score","class_method","Score_type")
W_a_C_t=cbind(wilcox_accuracy_classifiers[,c(1,3,5)],rep("Ties"))
colnames(W_a_C_t)=c("Discretizer","Score","class_method","Score_type")
W_a_C_l=cbind(wilcox_accuracy_classifiers[,c(1,4,5)],rep("Losses"))
colnames(W_a_C_l)=c("Discretizer","Score","class_method","Score_type")
W_a_C=rbind(W_a_C_w,W_a_C_t,W_a_C_l)
W_a_C$Score_type=factor(W_a_C$Score_type, level= unique(W_a_C$Score_type)[c(3,2,1)])
W_a_C[,"Metric"]="Accuracy"
# F1
wilcox_macroF1_classifiers<-count_wilcox_macroF1_perclassifier
wilcox_macroF1_classifiers$class_method=factor(wilcox_macroF1_classifiers$class_method,
                                               levels=unique(wilcox_macroF1_classifiers$class_method)[c(1,6,5,2,3,4)])
wilcox_macroF1_classifiers=wilcox_macroF1_classifiers[with(wilcox_macroF1_classifiers, order(wilcox_macroF1_classifiers$class_method,-wilcox_macroF1_classifiers$Wins_count,-wilcox_macroF1_classifiers$Ties_count,wilcox_macroF1_classifiers$Loss_count)),]
W_a_C1_w=cbind(wilcox_macroF1_classifiers[,c(1,2,5)],rep("Wins"))
colnames(W_a_C1_w)=c("Discretizer","Score","class_method","Score_type")
W_a_C1_t=cbind(wilcox_macroF1_classifiers[,c(1,3,5)],rep("Ties"))
colnames(W_a_C1_t)=c("Discretizer","Score","class_method","Score_type")
W_a_C1_l=cbind(wilcox_macroF1_classifiers[,c(1,4,5)],rep("Losses"))
colnames(W_a_C1_l)=c("Discretizer","Score","class_method","Score_type")
W_a_C1=rbind(W_a_C1_w,W_a_C1_t,W_a_C1_l)
W_a_C1$Score_type=factor(W_a_C1$Score_type, level= unique(W_a_C1$Score_type)[c(3,2,1)])
W_a_C1[,"Metric"]="F1 score"
# kappa
wilcox_kappa_classifiers<-count_wilcox_kappa_perclassifier
wilcox_kappa_classifiers$class_method=factor(wilcox_kappa_classifiers$class_method,
                                               levels=unique(wilcox_kappa_classifiers$class_method)[c(1,6,5,2,3,4)])
wilcox_kappa_classifiers=wilcox_kappa_classifiers[with(wilcox_kappa_classifiers, order(wilcox_kappa_classifiers$class_method,-wilcox_kappa_classifiers$Wins_count,-wilcox_kappa_classifiers$Ties_count,wilcox_kappa_classifiers$Loss_count)),]
W_a_C2_w=cbind(wilcox_kappa_classifiers[,c(1,2,5)],rep("Wins"))
colnames(W_a_C2_w)=c("Discretizer","Score","class_method","Score_type")
W_a_C2_t=cbind(wilcox_kappa_classifiers[,c(1,3,5)],rep("Ties"))
colnames(W_a_C2_t)=c("Discretizer","Score","class_method","Score_type")
W_a_C2_l=cbind(wilcox_kappa_classifiers[,c(1,4,5)],rep("Losses"))
colnames(W_a_C2_l)=c("Discretizer","Score","class_method","Score_type")
W_a_C2=rbind(W_a_C2_w,W_a_C2_t,W_a_C2_l)
W_a_C2$Score_type=factor(W_a_C2$Score_type, level= unique(W_a_C2$Score_type)[c(3,2,1)])
W_a_C2[,"Metric"]="Kappa"
df=rbind(W_a_C,W_a_C1,W_a_C2)

df_RF=df[which(df$class_method=="RF"),]
df_KNNC=df[which(df$class_method=="KNNC"),]
df_NaiveBayes=df[which(df$class_method=="NaiveBayes"),]
df_CART=df[which(df$class_method=="CART"),]
df_Boosting=df[which(df$class_method=="Boosting"),]
df_SVM=df[which(df$class_method=="SVM"),]
# RF KNNC NaiveBayes CART Boosting SVM
levels_RF=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="RF")])
levels_KNNC=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="KNNC")])
levels_NaiveBayes=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="NaiveBayes")])
levels_CART=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="CART")])
levels_Boosting=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="Boosting")])
levels_SVM=unique(wilcox_accuracy_classifiers$Discretizer[which(wilcox_accuracy_classifiers$class_method=="SVM")])


df_RF$Discretizer=factor(df_RF$Discretizer,levels=rev(levels_RF))
df_KNNC$Discretizer=factor(df_KNNC$Discretizer,levels=rev(levels_KNNC))
df_NaiveBayes$Discretizer=factor(df_NaiveBayes$Discretizer,levels=rev(levels_NaiveBayes))
df_CART$Discretizer=factor(df_CART$Discretizer,levels=rev(levels_CART))
df_Boosting$Discretizer=factor(df_Boosting$Discretizer,levels=rev(levels_Boosting))
df_SVM$Discretizer=factor(df_SVM$Discretizer,levels=rev(levels_SVM))

# W_a_C_RF=W_a_C[which(W_a_C$class_method=="RF"),]
# W_a_C_RF$Discretizer=factor(W_a_C_RF$Discretizer,levels= unique(W_a_C_RF$Discretizer))

# W_a_C$Discretizer=factor(W_a_C$Discretizer)
# + coord_flip()
library(ggplot2)
gg80=ggplot(df_RF, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="RF", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=8),
        axis.title.x=element_text(face="bold"),
        legend.position="top",
        axis.title.y = element_text(face="bold"))
 
print(gg80)
gg080=gg80 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold",size=9),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg080)
gg81=ggplot(df_KNNC, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="KNNC", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_text(face="bold"),legend.position="none",
        axis.text.y = element_text(size=8),
        axis.title.y = element_text(face="bold"))
        # axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
print(gg81)
gg081=gg81 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold"),
        strip.text.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg081)

gg82=ggplot(df_NaiveBayes, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="NaiveBayes", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_text(face="bold"),legend.position="none",
        axis.text.y = element_text(size=8),
        axis.title.y = element_text( face="bold"))
# axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
print(gg82)
gg082=gg82 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg082)

gg83=ggplot(df_CART, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="CART", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_text(face="bold"),legend.position="none",
        axis.text.y = element_text(size=8),
        axis.title.y = element_text( face="bold"))
# axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
print(gg83)
gg083=gg83 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold"),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg083)

gg84=ggplot(df_Boosting, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="Boosting", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_text(face="bold"),legend.position="none",
        axis.text.y = element_text(size=8),
        axis.title.y = element_text( face="bold"))
# axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
print(gg84)
gg084=gg84 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold"),
        strip.text.x = element_blank(),
        axis.text.x=element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg084)

gg85=ggplot(df_SVM, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="SVM", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,face="bold"),
        axis.title.x=element_text(face="bold"),
        axis.text.y = element_text(size=8),
        legend.position="none",
        axis.title.y = element_text( face="bold"))
# axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
print(gg85)
gg085=gg85 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+
  theme(strip.text = element_text(face="bold"),
        strip.text.x = element_blank(),
        plot.margin=unit(c(-0.5,1,-0.5,1), "cm"))
print(gg085)

# library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(gg080),
                ggplotGrob(gg081),
                ggplotGrob(gg082)))

grid.newpage()
grid.draw(rbind(ggplotGrob(gg082),
                ggplotGrob(gg083)
))
grid.newpage()
grid.draw(rbind(ggplotGrob(gg084),
                ggplotGrob(gg085)
))