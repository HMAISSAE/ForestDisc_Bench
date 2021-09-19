
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
levelsdiscr=unique(wilcox_accuracy_classifiers$Discretizer)
wilcox_accuracy_classifiers$Discretizer=factor(wilcox_accuracy_classifiers$Discretizer,levels=levelsdiscr)
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
wilcox_macroF1_classifiers$Discretizer=factor(wilcox_macroF1_classifiers$Discretizer,levels=levelsdiscr)

W_a_C1_w=cbind(wilcox_macroF1_classifiers[,c(1,2,5)],rep("Wins"))
colnames(W_a_C1_w)=c("Discretizer","Score","class_method","Score_type")
W_a_C1_w$Discretizer=factor(W_a_C1_w$Discretizer, levels= unique(W_a_C1_w$Discretizer))
W_a_C1_t=cbind(wilcox_macroF1_classifiers[,c(1,3,5)],rep("Ties"))
colnames(W_a_C1_t)=c("Discretizer","Score","class_method","Score_type")
W_a_C1_t$Discretizer=factor(W_a_C1_t$Discretizer)
W_a_C1_l=cbind(wilcox_macroF1_classifiers[,c(1,4,5)],rep("Losses"))
colnames(W_a_C1_l)=c("Discretizer","Score","class_method","Score_type")
W_a_C1=rbind(W_a_C1_w,W_a_C1_t,W_a_C1_l)
W_a_C1$Score_type=factor(W_a_C1$Score_type, level= unique(W_a_C1$Score_type)[c(3,2,1)])
W_a_C1[,"Metric"]="F1 score"

df=rbind(W_a_C,W_a_C1)
df_RF$Discretizer=factor(df_RF$Discretizer, levels=rev(levels(df_RF$Discretizer)))
df_RF=df[which(df$class_method=="RF"),]
# W_a_C_RF=W_a_C[which(W_a_C$class_method=="RF"),]
# W_a_C_RF$Discretizer=factor(W_a_C_RF$Discretizer,levels= unique(W_a_C_RF$Discretizer))

# W_a_C$Discretizer=factor(W_a_C$Discretizer)
# + coord_flip()
names(W_a_C)
gg8=ggplot(df_RF, aes(x=Discretizer,y=Score,fill=Score_type)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(name = "",values = c("tomato3","gray","seagreen"),guide = guide_legend(reverse = TRUE))+
  # scale_y_discrete(position = "right") +
  coord_flip()+
  labs(title="", x="RF", y="",size=20) +
  # geom_line(aes(y = mean_diff_acc, group=1, color= "tomato3")) +
  # geom_point(aes( y = mean_diff_acc,group=1,shape="17",color= "tomato3"))+
  # geom_line(aes(y = mean_diff_F1, group=1,color= "slateblue4"))+
  # geom_point(aes(y = mean_diff_F1, group=1,shape="16",color= "slateblue4"))+
  # geom_line(aes(y = mean_diff_Kappa,group=1,color= "seagreen"))+
  # geom_point(aes( y = mean_diff_Kappa,group=1,shape="15",color= "seagreen"))+
  # geom_line(aes(y = mean_acc,group=1, color= "tomato3")) +
  # geom_point(aes( y = mean_acc,group=1,shape="17",color= "tomato3"))+
  # geom_line(aes(y = mean_F1,group=1,color= "slateblue4"))+
  # geom_point(aes(y = mean_F1,group=1,shape="16",color= "slateblue4"))+
  # geom_line(aes(y = mean_Kappa,group=1,color= "seagreen"))+
  # geom_point(aes( y = mean_Kappa,group=1,shape="15",color= "seagreen"))+
  theme_minimal() +
  # scale_y_continuous(position = "right")+ 
  # labs(title="", x="", y="") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x=element_text(size=14,face="bold"),legend.position="top",
        axis.title.y = element_text(size=20,angle = 360,vjust = 0.5, hjust=1, face="bold"))
  # scale_colour_manual(name = "Metrics:",
  #                     labels = c("Kappa", "F1 score", "Accuracy"),
  #                     values = c("tomato3","slateblue4","seagreen"),
  #                     guide = guide_legend(reverse = TRUE)) +
  # scale_shape_manual(name = "Metrics:",
  #                    labels = c("Kappa", "F1 score", "Accuracy"),
  #                    values = c(17,16,15),
  #                    guide = guide_legend(reverse = TRUE)
  # )

print(gg8)

gg08=gg8 + facet_wrap( ~ Metric, ncol=3,scales = "free_x")+theme(strip.text = element_text(size=10))
print(gg08)



# gg6=ggplot(B_perf_bench1, aes(x=mean_opt_value, y=mean_acc))+ 
#   labs(title="", x="Optimum value", y="Accuracy") +
#   # geom_point( aes(size=mean_NB_interval)) + 
#   geom_text(
#     aes(label = disc_method,color=disc_method),
#     position=position_jitter(width=0.001,height=0.001)
#   )+
#   # theme_hc()+ scale_colour_hc()+
#   scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                 labels = trans_format("log10", math_format(10^.x))) +
#   theme_bw() +
#   # scale_color_gradient(low="blue", high="red")+
#   # annotation_logticks() +
#   theme(axis.title.y=element_text(face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         legend.position="none"
#   ) 
# print(gg6)

gg6=ggplot(B_perf_bench1, aes(x=mean_opt_value, y=mean_acc))+ 
  labs(title="", x="Optimum value", y="Accuracy") +
  # geom_point( aes(size=mean_NB_interval)) + 
  geom_text(
    aes(label = disc_method,color=disc_method), size=6
    # position=position_jitter(width=0.002,height=0.002),size=3.5
  )+
  # geom_point() +
  
  # theme_hc()+ scale_colour_hc()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  # scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(plot.margin = unit(c(0,0.5,0.5,0), "lines"),
        legend.position="none",
  axis.title.x=element_text(size=18,face="bold"),
  axis.title.y=element_text(size=18,face="bold"),
  axis.text.x=element_text(size=18),
  axis.text.y=element_text(size=18)
  ) 
print(gg6)
gg7=ggplot(B_perf_bench, aes(x=mean_NB_interval, y=mean_acc,color=mean_exetimepervar))+ 
  labs(title="", x="Intervals Number", y="Accuracy") +
  # geom_point( aes(size=mean_NB_interval)) + 
  geom_text(
    aes(label = disc_method,color=mean_exetimepervar), size=6
    # position=position_jitter(width=0.002,height=0.002),size=3.5
  )+
   # geom_point() +
  # theme_hc()+ scale_colour_hc()+
  
  # theme_bw() +
  # scale_fill_gradient(low="blue", high="red")+
  scale_color_gradient(low="blue", high="red")+
  # annotation_logticks() +
  theme(plot.margin = unit(c(0,0.5,0.5,0), "lines"),
        legend.position="left",
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold"),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18)
  ) 
max(B_perf_bench$mean_exetimepervar)
print(gg7)
names(B_perf_bench)
gg8=ggplot(B_perf_bench, aes(x=mean_exetimepervar, y=mean_acc))+ 
  labs(title="", x="Execution time (s)", y="Accuracy") +
  # geom_point( aes(size=mean_NB_interval)) + 
  geom_text(
    aes(label = disc_method,color=mean_NB_interval), size=4
    # position=position_jitter(width=0.002,height=0.002),size=3.5
  )+
  # geom_point() +
  # theme_hc()+ scale_colour_hc()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() +
  scale_color_gradientn(colors=c("green","yellowgreen","seagreen","tan1","tomato","tomato3"))+
  # annotation_logticks() +
  theme(plot.margin = unit(c(0,0.5,0.5,0), "lines"),
        legend.position="left",
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold"),
        axis.text.x=element_text(size=18),
        axis.text.y=element_text(size=18)
  ) 
summary(B_perf_bench$mean_NB_interval)
print(gg8)
grid.arrange(gg6,gg7,gg8,nrow=2,ncol=2)
# scale_color_manual(breaks = c("8", "6", "4"),
                   values=c("red", "blue", "green")

# points(discmeth,OptValue,pch=20,col="black")

# ses <- summarydataplotSW[,c("min_opt_value","max_opt_value")]
# with(summarydataplotSW, 
#      plot(
#        mean_opt_value, id_disc, type="l", ylim=range(ses),log = "y",
#        panel.first=polygon(mean_opt_value,c(ses[,1],rev(ses[,2])),border=NA,log = "y", col="#ebebeb")
#      )
# )
# names(dataplotSW)

# 
# 
# discmeth<-factor(dataplotSW$opt_meth,levels=levDisc)
# OptValue<-dataplotSW$opt_value
# IntervalNbr<-dataplotSW$NB_interval
# ExeTime<-dataplotSW$Exe_t
# 
# 
# #Define Margins. The trick is to use give as much space possible on the left margin (second value)
# par(mar=c(5, 12, 4, 4) + 0.1)
# boxplot(OptValue~discmeth,data=dataplotSW, log = "y",las=2,boxwex=0.5)
# means <- tapply(dataplotSW$opt_value,dataplotSW$opt_meth,mean)
# stds <- tapply(dataplotSW$opt_value,dataplotSW$opt_meth,sd)
# means=means[levDisc]
# stds=stds[levDisc]
# AS=points(means,col="red",pch=18, type="b")
# arrows(x0 =as.vector(levDisc), y0 = as.vector(means)-as.vector(stds), x1 = as.vector(levDisc), y1 = as.vector(means)+as.vector(stds), code=3, angle=90, length=0.1)
# line(y=means,x=levDisc,col="red",pch=18)
# #Plot the first discmeth series. Notice that you don't have to draw the axis nor the labels
# 
# 
# boxplot(discmeth, OptValue, axes=F, ylim=c(0,max(OptValue)), xlab="", ylab="",type="l",col="black", main="")
# points(discmeth,OptValue,pch=20,col="black")
# axis(2, ylim=c(0,max(OptValue)),col="black",lwd=2)
# mtext(2,text="OptValueulation",line=2)
# 
# # Plot the second discmeth series. The command par(new=T) is handy here. If you just need to plot two discmethseries, 
# # you could also use the right vertical axis as well. In that case you have to substitute "2" with "4" 
# # in the functions axis() and mtext(). Notice that in both functions lines is increased so that the new axis 
# # and its label is placed to the left of the first one. You don't need to increase the value if you use 
# # the right vertical axis.
# 
# par(new=T)
# plot(discmeth, ExeTime, axes=F, ylim=c(0,max(ExeTime)), xlab="", ylab="", 
#      type="l",lty=2, main="",xlim=c(7000,3400),lwd=2, col="blue")
# axis(2, ylim=c(0,max(ExeTime)),lwd=2,line=3.5)
# points(discmeth, ExeTime,pch=20)
# mtext(2,text="ExeTimeian Group Size",line=5.5)
# 
# 
# #Plot the third discmeth series. Again the line parameter are both further increased.
# 
# 
# par(new=T)
# plot(discmeth, IntervalNbr, axes=F, ylim=c(0,max(IntervalNbr)), xlab="", ylab="", 
#      type="l",lty=3, main="",xlim=c(7000,3400),lwd=2, col="yellow")
# axis(2, ylim=c(0,max(IntervalNbr)),lwd=2,line=7, col= "yellow")
# 
# points(discmeth, IntervalNbr,pch=20)
# mtext(2,text="Number of Groups",line=9)
# 
# 
# #We can now draw the X-axis, which is of course shared by all the three discmeth-series.
# 
# axis(1,pretty(range(discmeth),10))
# mtext("cal BP",side=1,col="black",line=2)
# 
# 
# #And then plot the legend.
# legend(x=7000,y=12,legend=c("OptValueulation","ExeTimeian Group Size","Number of Groups"),lty=c(1,2,3))
# 
# 
# 
# names(summarydataplotSW)
# ses <- summarydataplotSW[,c("min_opt_value","max_opt_value")]
# with(summarydataplotSW, 
#      plot(
#        mean_opt_value, id_disc, type="l", ylim=range(ses),log = "y",
#        panel.first=polygon(mean_opt_value,c(ses[,1],rev(ses[,2])),border=NA,log = "y", col="#ebebeb")
#      )
# )
# names(dataplotSW)
# 
# 
# values <- sample(1:10)
# names(values) <- sapply(letters[1:10], 
#                         function(x) paste(rep(x, 10), sep="",collapse="") 
# ) 
# 
# # barplot labels are too long for the available space, hence some are not plotted
# barplot(values)
# 
# # to add angled labels, tell barplot not to label the x axis, and store the bar location
# at <- barplot(values, xaxt="n")
# # then use angleAxs
# library(gplots)
# angleAxis(1, at=at, labels = names(values))
# 
# 
# The data have a common independent variable (x)
# x <- 1:10
# 
# # Generate 4 different sets of outputs
# y1 <- runif(10, 0, 1)
# y2 <- runif(10, 100, 150)
# y3 <- runif(10, 1000, 2000)
# y4 <- runif(10, 40000, 50000)
# y <- list(y1, y2, y3, y4)
# 
# # Colors for y[[2]], y[[3]], y[[4]] points and axes
# colors = c("red", "blue", "green")
# 
# # Set the margins of the plot wider
# par(oma = c(0, 2, 2, 3))
# 
# plot(x, y[[1]], yaxt = "n", xlab = "Common x-axis", main = "A bunch of plots on the same graph", 
#      ylab = "")
# lines(x, y[[1]])
# 
# # We use the "pretty" function go generate nice axes
# axis(at = pretty(y[[1]]), side = 2)
# 
# # The side for the axes.  The next one will go on 
# # the left, the following two on the right side
# sides <- list(2, 4, 4) 
# 
# # The number of "lines" into the margin the axes will be
# lines <- list(2, NA, 2)
# 
# for(i in 2:4) {
#   par(new = TRUE)
#   plot(x, y[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "")
#   axis(at = pretty(y[[i]]), side = sides[[i-1]], line = lines[[i-1]], 
#        col = colors[i - 1])
#   lines(x, y[[i]], col = colors[i - 1])
# }
# 
# # Profit.
                   library(ggplot2)
                   library(ggthemes) 
                   df=testF5
                   df1=testF5[testF5$data=="TEST",]
                   names(df)
                   nique(df$data)
                   gg=ggplot(df, aes(x=disc_method, y=mean_acc, color=data, group = data)) + 
                     labs(title="", x="", y="Accuracy") +
                     geom_errorbar(aes(ymin=mean_acc-SE_acc, ymax=mean_acc+SE_acc),  colour="black", width=.1) +
                     geom_line() +
                     geom_point() + 
                     # theme_hc()+ scale_colour_hc()+
                     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                           axis.title.y=element_text(size=18,face="bold"),
                           axis.title.x=element_text(face="bold"),
                           legend.position = "none")
                   
                   print(gg)
                   ggplot(melt(df, id.vars=c("disc_method", "mean_NB_interval")), aes(disc_method, value)) + 
                     geom_bar(stat="identity") + facet_wrap(~variable)
                   pd <- position_dodge(0.1) # move them .05 to the left and right
                   ggplot(df, aes(x=disc_method, y=mean_acc, colour=data, group=data)) + 
                     geom_errorbar(aes(ymin=mean_acc-SE_acc, ymax=mean_acc+SE_acc), colour="black", width=.1, position=pd) +
                     geom_line(position=pd) +
                     geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
                     xlab("Discretizer") +
                     ylab("Accuracy") +
                     scale_colour_hue(name="Supplement type",    # Legend label, use darker colors
                                      breaks=c("OJ", "VC"),
                                      labels=c("Orange juice", "Ascorbic acid"),
                                      l=40) +                    # Use darker colors, lightness=40
                     ggtitle("") +
                     expand_limits(y=0.6) +                        # Expand y range
                     scale_y_continuous(breaks=0.6:1*4) +         # Set tick every 4
                     theme_bw() +
                     theme(legend.justification=c(1,0),
                           legend.position=c(1,0))               # Position legend in bottom right
                   
                   # install.packages("ggthemes") 
                   
                   # dataplot=B_perf_disc_summary
                   # dataplot=read.csv("opt_performance\\Iris\\Iris_perf_opt_disc_iter.csv",sep=',',header= T, na.strings="?")
                   # dataplot1=read.csv("opt_performance\\Iris\\Iris_perf_per_var.csv",sep=',',header= T, na.strings="?")
                   # dataplot2=read.csv("opt_performance\\Iris\\Iris_perf_opt_disc_perclassifier.csv",sep=',',header= T, na.strings="?")
                   # dataplot3=read.csv("opt_performance\\Iris\\Iris_perf_opt_disc_summary.csv",sep=',',header= T, na.strings="?")
                   
                   
                   
                   dataplot1$opt_meth
                   
                   
                   dataplot1$id_disc=1
                   
                   
                   dataplotSW=dataplot1[which(dataplot1[,"var"]==2),]
                   dataplotSW=dataplotSW[order(dataplotSW$id_disc,decreasing = FALSE),]
                   table(dataplotSW$opt_meth,dataplotSW$id_disc)
                   
                   levDisc=unique(dataplotSW[order(dataplotSW$id_disc,decreasing = FALSE),"opt_meth"])
                   
                   summarydataplotSW=sqldf('select opt_meth,id_disc,
                          avg(opt_value) as mean_opt_value, 
                          stdev(opt_value) as std_opt_value,
                          min(opt_value) as min_opt_value,
                          max(opt_value) as max_opt_value,
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(Exe_t) as mean_Exe_t, 
                          stdev(Exe_t) as std_Exe_t
                          from dataplotSW group by opt_meth order by id_disc')
                   
                   
                   
                   discmeth<-factor(summarydataplotSW$opt_meth,levels=levDisc)
                   OptValue<-summarydataplotSW$mean_opt_value
                   IntervalNbr<-summarydataplotSW$mean_NB_interval
                   ExeTime<-summarydataplotSW$mean_Exe_t
                   
                   
                   #Define Margins. The trick is to use give as much space possible on the left margin (second value)
                   par(mar=c(8, 12, 4, 4) + 0.1)
                   layout(matrix(c(1), nrow = 1, ncol = 1, byrow = TRUE))
                   #Plot the first discmeth series.
                   plot(mean_opt_value~id_disc,data=summarydataplotSW, log = "y",
                        type="b",axes=FALSE, 
                        ylim=c(min(mean_opt_value)/10,max(mean_opt_value)*10),
                        cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                        col="blue",pch=15,lty=1,col.axis="blue")
                   # arrows(x0 =summarydataplotSW$id_disc, y0 = summarydataplotSW$min_opt_value, x1 = summarydataplotSW$id_disc, y1 = summarydataplotSW$max_opt_value, code=3, angle=90, length=0.1)
                   
                   axis(1, at = 1:length(summarydataplotSW$id_disc), labels = summarydataplotSW$opt_meth, font.axis=4,cex.axis=0.9, las=3)
                   
                   axis(2, col="blue", col.ticks="blue", col.axis="blue", cex.axis=0.8)
                   mtext("Optimum value", side=2, line=2, col="blue", cex=1)
                   
                   names(summarydataplotSW)
                   # Plot the second discmeth series. 
                   
                   par(new=T)
                   plot(mean_NB_interval~id_disc,data=summarydataplotSW, 
                        type="b",axes=FALSE, 
                        ylim=c(0 , max(mean_NB_interval)+1),  
                        cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                        col="dimgray", pch=16,lty=2, col.axis="dimgray")
                   
                   axis(2, col="dimgray", col.ticks="dimgray", col.axis="dimgray", 
                        cex.axis=0.8,line=3.5)
                   mtext("Intervals number", side=2, line=5.5, col="dimgray", cex=1)
                   
                   
                   #Plot the third discmeth series. Again the line parameter are both further increased.
                   
                   
                   par(new=T)
                   
                   plot(mean_Exe_t~id_disc,data=summarydataplotSW, 
                        type="b",axes=FALSE, 
                        ylim=c(min(mean_Exe_t) , max(mean_Exe_t)),  
                        cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                        col="firebrick", pch=17,lty=3, col.axis="firebrick")
                   
                   axis(2, col="firebrick", col.ticks="firebrick", col.axis="firebrick", 
                        cex.axis=0.8,line=7)
                   mtext("Execution time (s)", side=2, line=9, col="firebrick", cex=1)
                   
                   
                   
                   #And then plot the legend.
                   legend(cex=0.8,'topright',
                          c("Avg Opt Value","Avg Intervals Number","Avg Execution Time"), 
                          lty = c(1,2,3), bty="o",
                          col=c('blue','dimgray','firebrick'),ncol=1,
                          pch =c(15,16,17),horiz = F)
                   
                   
                   
                   
                   dataplot=read.csv("opt_performance\\Iris\\Iris_perf_opt_disc_iter.csv",sep=',',header= T, na.strings="?")
                   names(dataplot)
                   dataplot$disc_method=gsub("MMA_","",dataplot$disc_method)
                   dataplot$disc_method=gsub("cont","No_Discret",dataplot$disc_method)
                   dataplot$disc_method=gsub("directL","direct_L",dataplot$disc_method)
                   dataplot$disc_method=toupper(dataplot$disc_method)
                   unique(dataplot$disc_method)
                   unique(B_perf_all_iter$class_method)
                   dataplot$class_method=gsub("Boosting","BOOST",dataplot$class_method)
                   dataplot$class_method=gsub("NaiveBayes","NBC",dataplot$class_method)
                   dataplot$class_method=gsub("KNN_Class","KNNC",dataplot$class_method)
                   
                   
                   dataplot$id_disc=1
                   dataplot[which(dataplot$disc_method=="CRS2LM"),"id_disc"]=1
                   dataplot[which(dataplot$disc_method=="ISRES"),"id_disc"]=2                
                   dataplot[which(dataplot$disc_method=="DIRECT_ORG"),"id_disc"]=3    
                   dataplot[which(dataplot$disc_method=="DIRECT"),"id_disc"]=4    
                   dataplot[which(dataplot$disc_method=="DIRECT_NOSCAL"),"id_disc"]=5    
                   dataplot[which(dataplot$disc_method=="DIRECT_L"),"id_disc"]=6    
                   dataplot[which(dataplot$disc_method=="MLSL_MLSL"),"id_disc"]=7  
                   dataplot[which(dataplot$disc_method=="MLSL_LDS"),"id_disc"]=8    
                   dataplot[which(dataplot$disc_method=="STOGO"),"id_disc"]=9    
                   dataplot[which(dataplot$disc_method=="COBYLA"),"id_disc"]=10    
                   dataplot[which(dataplot$disc_method=="BOBYQA"),"id_disc"]=11   
                   dataplot[which(dataplot$disc_method=="NELDERMEAD"),"id_disc"]=12   
                   dataplot[which(dataplot$disc_method=="SBPLX"),"id_disc"]=13   
                   dataplot[which(dataplot$disc_method=="MMA"),"id_disc"]=14   
                   dataplot[which(dataplot$disc_method=="SLSQP"),"id_disc"]=15   
                   dataplot[which(dataplot$disc_method=="LBFGS"),"id_disc"]=16   
                   dataplot[which(dataplot$disc_method=="TNEWTON"),"id_disc"]=17
                   dataplot[which(dataplot$disc_method=="VARMETRIC"),"id_disc"]=18
                   dataplot[which(dataplot$disc_method=="AUGLAG_COBYLA"),"id_disc"]=19
                   dataplot[which(dataplot$disc_method=="AUGLAG_LBFGS"),"id_disc"]=20
                   dataplot[which(dataplot$disc_method=="AUGLAG_MMA"),"id_disc"]=21
                   dataplot[which(dataplot$disc_method=="AUGLAG_SLSQP"),"id_disc"]=22
                   dataplot[which(dataplot$disc_method=="NO_DISCRET"),"id_disc"]=23
                   dataplot[which(dataplot$disc_method=="FREQUENCY_DISC"),"id_disc"]=24
                   dataplot[which(dataplot$disc_method=="INTERVAL_DISC"),"id_disc"]=25
                   dataplot[which(dataplot$disc_method=="CLUSTER_DISC"),"id_disc"]=26
                   table( dataplot$id_disc,dataplot$disc_method)
                   
                   unique (dataplot$disc_method)
                   names(dataplot)
                   summary(dataplot)
                   dataplot$opt_value=as.numeric(as.character(dataplot$opt_value))
                   dataplot$NB_interval=as.numeric(as.character(dataplot$NB_interval))
                   # dataplot$exe_time=as.numeric(as.character(dataplot$exe_time_per_var))
                   # dataplot$exe_time_per_var=as.numeric(as.character(dataplot$exe_time_per_var))
                   dataplot=dataplot[-which(dataplot$disc_method %in% c("NO_DISCRET","FREQUENCY_DISC","INTERVAL_DISC","CLUSTER_DISC")),]
                   dataplot=dataplot[which(dataplot$data=="test"),]
                   summarydataplot=sqldf('select disc_method,id_disc,
                          avg(opt_value) as mean_opt_value, 
                          stdev(opt_value) as std_opt_value,
                          min(opt_value) as min_opt_value,
                          max(opt_value) as max_opt_value,
                          avg(NB_interval) as mean_NB_interval, 
                          stdev(NB_interval) as std_NB_interval,
                          avg(exe_time) as mean_Exe_t, avg(exe_time) as mean_Exe_t, 
                          stdev(exe_time) as std_Exe_t,
                          avg(exe_time_per_var) as mean_exe_time_per_var, 
                          stdev(exe_time_per_var) as std_exe_time_per_var,
                          avg(mean_acc) as mean_acc, 
                          stdev(mean_acc) as std_acc
                          from dataplot where data="test" group by disc_method order by id_disc')
                   
                   #Define Margins. The trick is to use give as much space possible on the left margin (second value)
                   # par(mar=c(8, 12, 2, 4) + 0.1,mfrow=c(2,1))
                   par(mar=c(0, 12, 0, 4) + 0.1,mfrow=c(2,1))
                   layout(matrix(c(1,1,2,2,2), nrow = 5, ncol = 1, byrow = TRUE))
                   # Plot the first panel
                   boxplot(mean_acc~id_disc ,data=dataplot, axes=F,  
                           ylim=c(min(dataplot$mean_acc),max(dataplot$mean_acc)+0.1), 
                           xlab="", ylab="",type="l",col="palegreen", main="",
                           boxwex=0.6,border="springgreen4")
                   
                   points(mean_acc~id_disc,data=summarydataplot, 
                          type="b",axes=FALSE, 
                          ylim=c(min(dataplot$mean_acc),max(dataplot$mean_acc)+0.1),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="springgreen4",pch=20,lty=3,col.axis="springgreen4")
                   axis(2, col="springgreen4", col.ticks="springgreen4", 
                        col.axis="springgreen4", 
                        cex.axis=1,line=3.5)
                   mtext("Accuracy", side=2, line=5.5, col="springgreen4", cex=1)
                   
                   # add legend
                   legend(cex=1.2,'bottomright',
                          c("Avg Accuracy","Avg Opt Value","Avg Intervals Number per Var.","Avg Execution Time per Var."), 
                          lty = c(3,3,3,3), bty="o",
                          col=c("springgreen4",'blue','dimgray','firebrick'),ncol=2,
                          pch =c(20,3,8,4),horiz = F)
                   
                   # Plot the second panel
                   #Plot the first discmeth series.
                   par(mar=c(10, 12, 0, 4))
                   boxplot(opt_value~id_disc ,data=dataplot, axes=F, log = "y", 
                           ylim=c(min(dataplot$opt_value)/10,max(dataplot$opt_value)*10), 
                           xlab="", ylab="",type="l",col="cornflowerblue", main="",boxwex=0.6,border="blue")
                   
                   points(mean_opt_value~id_disc,data=summarydataplot, log = "y",
                          type="b",axes=FALSE, 
                          ylim=c(min(dataplot$opt_value)/10,max(dataplot$opt_value)*10),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="blue",pch=3,lty=3,col.axis="blue")
                   
                   
                   # arrows(x0 =summarydataplotSW$id_disc, y0 = summarydataplotSW$min_opt_value, x1 = summarydataplotSW$id_disc, y1 = summarydataplotSW$max_opt_value, code=3, angle=90, length=0.1)
                   
                   
                   axis(1, at = 1:length(summarydataplot$id_disc), labels = summarydataplot$disc_method, font.axis=4,cex.axis=1.18, las=3)
                   axis(2, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1)
                   mtext("Optimum value", side=2, line=2, col="blue", cex=1)
                   
                   names(summarydataplot)
                   names(dataplot)
                   # Plot the second discmeth series. 
                   
                   par(new=T)
                   
                   boxplot(NB_interval~id_disc ,data=dataplot, axes=F,  
                           ylim=c(1,max(dataplot$NB_interval)+1), 
                           xlab="", ylab="",type="l",
                           col="snow3", main="",boxwex=0.6,border="dimgray")
                   
                   points(mean_NB_interval~id_disc,data=summarydataplot, 
                          type="b",axes=FALSE, 
                          ylim=c(1,max(dataplot$NB_interval)+1),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="dimgray",pch=8,lty=3,col.axis="dimgray")
                   
                   
                   
                   axis(2, col="dimgray", col.ticks="dimgray", col.axis="dimgray", 
                        cex.axis=1,line=3.5)
                   mtext("Intervals number", side=2, line=5.5, col="dimgray", cex=1)
                   
                   
                   #Plot the third discmeth series. Again the line parameter are both further increased.
                   
                   names(summarydataplot)
                   names(dataplot)
                   par(new=T)
                   
                   
                   boxplot(exe_time_per_var~id_disc ,data=dataplot, axes=F,
                           ylim=c(min(dataplot$exe_time_per_var),max(dataplot$exe_time_per_var)),
                           xlab="", ylab="",type="l",col="chocolate2", main="",boxwex=0.6,border="firebrick")
                   points(mean_exe_time_per_var~id_disc,data=summarydataplot,
                          type="b",axes=FALSE,
                          ylim=c(min(dataplot$exe_time_per_var),max(dataplot$exe_time_per_var)),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="firebrick",pch=4,lty=3,col.axis="firebrick")
                   
                   
                   
                   axis(2, col="firebrick", col.ticks="firebrick", col.axis="firebrick", 
                        cex.axis=1,line=7)
                   mtext("Execution time (s)", side=2, line=9, col="firebrick", cex=1)
                   
                   
                   
                   
                   
                   
                   # boxwhitout color
                   
                   #Define Margins. The trick is to use give as much space possible on the left margin (second value)
                   # par(mar=c(8, 12, 2, 4) + 0.1,mfrow=c(2,1))
                   par(mar=c(0, 12, 0, 4) + 0.1,mfrow=c(2,1))
                   layout(matrix(c(1,1,2,2,2), nrow = 5, ncol = 1, byrow = TRUE))
                   # Plot the first panel
                   boxplot(mean_acc~id_disc ,data=dataplot, axes=F,  
                           ylim=c(min(dataplot$mean_acc),max(dataplot$mean_acc)+0.1), 
                           xlab="", ylab="",type="l",col="palegreen", main="",
                           boxwex=0.6,border="springgreen4")
                   
                   points(mean_acc~id_disc,data=summarydataplot, 
                          type="b",axes=FALSE, 
                          ylim=c(min(dataplot$mean_acc),max(dataplot$mean_acc)+0.1),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="springgreen4",pch=20,lty=3,col.axis="springgreen4")
                   axis(2, col="springgreen4", col.ticks="springgreen4", 
                        col.axis="springgreen4", 
                        cex.axis=1,line=3.5)
                   mtext("Accuracy", side=2, line=5.5, col="springgreen4", cex=1)
                   
                   # add legend
                   legend(cex=1.2,'bottomright',
                          c("Avg Accuracy","Avg Opt Value","Avg Intervals Number per Var.","Avg Execution Time per Var."), 
                          lty = c(3,3,3,3), bty="o",
                          col=c("springgreen4",'blue','dimgray','firebrick'),ncol=2,
                          pch =c(20,3,8,4),horiz = F)
                   
                   # Plot the second panel
                   #Plot the first discmeth series.
                   par(mar=c(10, 12, 0, 4))
                   boxplot(opt_value~id_disc ,data=dataplot, axes=F, log = "y", 
                           ylim=c( min(dataplot$opt_value)/100, max(dataplot$opt_value)*1000), 
                           xlab="", ylab="",type="l",col=NULL, main="",boxwex=0.6,border="blue")
                   
                   points(mean_opt_value~id_disc,data=summarydataplot, log = "y",
                          type="b",axes=FALSE, 
                          ylim=c(min(dataplot$opt_value)/100,max(dataplot$opt_value)*1000),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="blue",pch=3,lty=3,col.axis="blue")
                   
                   
                   # arrows(x0 =summarydataplotSW$id_disc, y0 = summarydataplotSW$min_opt_value, x1 = summarydataplotSW$id_disc, y1 = summarydataplotSW$max_opt_value, code=3, angle=90, length=0.1)
                   
                   
                   axis(1, at = 1:length(summarydataplot$id_disc), labels = summarydataplot$disc_method, font.axis=4,cex.axis=1.18, las=3)
                   axis(2, col="blue", col.ticks="blue", col.axis="blue", cex.axis=1)
                   mtext("Optimum value", side=2, line=2, col="blue", cex=1)
                   
                   names(summarydataplot)
                   names(dataplot)
                   # Plot the second discmeth series. 
                   
                   par(new=T)
                   
                   boxplot(NB_interval~id_disc ,data=dataplot, axes=F,  
                           ylim=c(1,max(dataplot$NB_interval)+1), 
                           xlab="", ylab="",type="l",
                           col=NULL, main="",boxwex=0.6,border="dimgray")
                   
                   points(mean_NB_interval~id_disc,data=summarydataplot, 
                          type="b",axes=FALSE, 
                          ylim=c(1,max(dataplot$NB_interval)+1),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="dimgray",pch=8,lty=3,col.axis="dimgray")
                   
                   
                   
                   axis(2, col="dimgray", col.ticks="dimgray", col.axis="dimgray", 
                        cex.axis=1,line=3.5)
                   mtext("Intervals number", side=2, line=5.5, col="dimgray", cex=1)
                   
                   
                   #Plot the third discmeth series. Again the line parameter are both further increased.
                   
                   names(summarydataplot)
                   names(dataplot)
                   par(new=T)
                   
                   
                   boxplot(exe_time_per_var~id_disc ,data=dataplot, axes=F,
                           ylim=c(min(dataplot$exe_time_per_var),max(dataplot$exe_time_per_var)),
                           xlab="", ylab="",type="l",col=NULL, main="",boxwex=0.6,border="firebrick")
                   points(mean_exe_time_per_var~id_disc,data=summarydataplot,
                          type="b",axes=FALSE,
                          ylim=c(min(dataplot$exe_time_per_var),max(dataplot$exe_time_per_var)),
                          cex.lab=1,xaxt = "n",xlab="", ylab="",cex.axis=1.2,
                          col="firebrick",pch=4,lty=3,col.axis="firebrick")
                   
                   
                   
                   axis(2, col="firebrick", col.ticks="firebrick", col.axis="firebrick", 
                        cex.axis=1,line=7)
                   mtext("Execution time (s)", side=2, line=9, col="firebrick", cex=1)
                   
                   
                   
                   # plot Iris per classifier
                   names(Iris_perf_disc_summary_perclassifier)
                   unique(Iris_perf_disc_summary_perclassifier$disc_method)
                   Iris_perclass=Iris_perf_disc_summary_perclassifier[-which(Iris_perf_disc_summary_perclassifier$disc_method %in% c("FREQUENCY_DISC","INTERVAL_DISC","CLUSTER_DISC")),]
                   irislevDisc=c(levDisc,"NO_DISCRET")
                   Iris_perclass$disc_method = factor(Iris_perclass$disc_method,levels=irislevDisc)
                   Iris_perclass$class_method = factor(Iris_perclass$class_method,levels=unique(Iris_perclass$class_method)[c(2,6,5,4,1,3)])
                   unique(Iris_perclass$class_method)[c(2,6,5,4,1,3)]
                   gg <- ggplot(Iris_perclass, aes(x=disc_method, y=mean_acc, color=class_method)) + geom_point() + labs(title="", x="", y="Accuracy")  
                   print(gg)
                   names (df)
                   unique(df$data)
                   library(ggthemes)
                   # names(df)
                   # df1<-df[,c(1,2,9,11)]
                   # df1[,"metric"]<-"Accuracy"
                   # colnames(df1)<-c("data","disc_method","mean","SE","metric")
                   # df22<-df[,c(1,2,12,14)]
                   # df22[,"metric"]<-"F1 mesure"
                   # colnames(df22)<-c("data","disc_method","mean","SE","metric")
                   # df3<-df[,c(1,2,15,17)]
                   # df3[,"metric"]<-"Kappa"
                   # colnames(df3)<-c("data","disc_method","mean","SE","metric")
                   # df4=rbind(df1,df22)
                   # df5=rbind(df4,df3)
                   # gg=ggplot(df, aes(x=disc_method, y=mean_acc, color=data, group = data,pch=data)) + 
                   #   labs(title="", x="", y="Accuracy") +
                   #   geom_errorbar(aes(ymin=mean_acc-SE_acc, ymax=mean_acc+SE_acc),colour="black", width=.1) +
                   #   geom_line() +
                   #   geom_point() + 
                   #   # theme_hc()+ scale_colour_hc()+
                   #   theme_tufte()+
                   #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                   #         axis.title.y=element_text(size=18,face="bold"),
                   #         axis.title.x=element_text(face="bold"),
                   #         legend.position = "none")
                   # print(gg)
                   # gg + ylim(0.7, 1) + geom_line() + facet_wrap( ~ metric, ncol=2)  # columns defined by 'cut'
                   str(df5)
                   gg=ggplot(df5, aes(x=disc_method, y=mean, color=data, group = data,pch=data)) + 
                     labs(title="", x="", y="") +
                     geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE),colour="black", width=.1) +
                     geom_line() +
                     geom_point() + 
                     # theme_hc()+ scale_colour_hc()+
                     theme_tufte()+
                     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                           axis.title.y=element_text(size=18,face="bold"),
                           axis.title.x=element_text(face="bold"),
                           legend.position = "none")
                   print(gg)
                   gg + ylim(min(mean)-0.1,max(mean)+0.1) + geom_line() + facet_wrap( ~ metric, ncol=1,nrow=3)  # columns defined by 'cut'
                   
                   
                   # gg1 <- gg + theme(plot.title=element_text(size=30, face="bold"), 
                   #                   axis.text.x=element_text(size=15), 
                   #                   axis.text.y=element_text(size=15),
                   #                   axis.title.x=element_text(size=25),
                   #                   axis.title.y=element_text(size=25)) + 
                   #   scale_color_discrete(name="Cut of diamonds")  # add title and axis text, change legend title.
                   # print(gg1)  # print the plot
                   # geom_point(aes(color = cyl, size = qsec, shape = gear)) +
                   # the legend shows a shape attribute based on a factor variable, 
                   # you need to change it using scale_shape_discrete(name="legend title"). 
                   # Had it been a continuous variable, use scale_shape_continuous(name="legend title") instead.
                   # scale_shape_discrete(name="legend title")
                   # scale_shape_continuous(name="legend title")
                   
                   
gg + ylim(0.7, 1) + geom_line() +
                     facet_wrap( ~ class_method, ncol=2)  # columns defined by 'cut'
                   
                   
count_wilcox_accuracy_perclassifier