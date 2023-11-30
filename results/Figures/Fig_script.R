
direcs <- c("simpleTest_gtp3_5","selfCorrect_Test_gtp3_5",
            "fileCont_Test_gtp3_5","CoT_Test_gtp3_5","actAs_Test_gtp3_5")

for (i in direcs){
  assign(paste0(i,"_res"),read.table(paste0(i,"/results1_10.txt"),header=TRUE))
}

matr_plot <- matrix(ncol=5)
colnames(matr_plot) <- c("complexity_char","error","nchar","experiment","complexity_byhand")

for (i in direcs){

  curr_res <- eval(parse(text=paste0(i,"_res")))
  # create to be filled
  nchar <- c()
  for (j in 1:nrow(curr_res)){
    # get file path for specific task and cycle
    cyc = curr_res[j,"cycle"]
    task_nr = curr_res[j,"task"]
    file = paste0(i,"/cycle",cyc,"_task",task_nr,".rmd")
    # read nchars:
    cnt<-0
    flag=FALSE
    for(line in readLines(file)){
      if(grepl("response",line)){
        flag=TRUE
      }
      if (flag==TRUE){
        cnt <- cnt + nchar(line)
      }
    }
    nchar<-c(nchar,cnt)
  }
  # set error to 0 or 1:
  curr_res$error=ifelse(is.na(curr_res$error),0,1)
  exname <- strsplit(i,"_Test_gtp3_5")[[1]]
  exname <- strsplit(exname,"Test_gtp3_5")[[1]]

  resplot<- data.frame(error=curr_res$error,nchar = nchar, experiment = rep(exname,length(nchar)), complexity_byhand = curr_res$complexity)

  # create new difficulty levels based on nchar
  complexity_char<-cut(as.numeric(resplot$nchar), quantile(as.numeric(resplot$nchar), probs = c(0,0.2,0.4,0.6,0.8,1)), include.lowest=TRUE,
                  labels=1:5)

  # bind to dataframe:
  resplot<-cbind(complexity_char,resplot)

  #assign to variable
  matr_plot<-rbind(matr_plot,resplot)
}

# remove first row (has NA from creation in it)
matr_plot <- matr_plot[-1,]


#plot
require(ggplot2)
require(dplyr)
require(hrbrthemes)
require(patchwork)

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)


# plot generation:
# ---------------------------------------------------------------------------------------------------
# Submatrices

simple <- matr_plot[matr_plot$experiment=="simple",]
simple_CoT_actAs <- matr_plot[(matr_plot$experiment=="simple" | matr_plot$experiment=="CoT" |
                                 matr_plot$experiment=="actAs"),]
simple_filecont <- matr_plot[(matr_plot$experiment=="simple" | matr_plot$experiment=="fileCont"), ]


my_subs <- list(simple,simple_CoT_actAs,simple_filecont,matr_plot)
names <- c("simple","simple_CoT_actAs","simple_filecont", "all")




# extra function for saving the nested files, with text adjusting
plot.save <- function(plot, 
                      width = 800, 
                      height = 500, 
                      text.factor = 1, 
                      filename = paste0(
                        format(
                          Sys.time(), 
                          format = '%Y%m%d-%H%M%S'), '-Rplot.png'
                      )
) {
  
  dpi <- text.factor * 100
  width.calc <- width / dpi
  height.calc <- height / dpi
  
  ggsave(filename = filename,
         dpi = dpi,
         width = width.calc,
         height = height.calc,
         units = 'in',
         plot = plot)
}





for (i in 1:4){
  
  # get df:
  df <- as.data.frame(my_subs[i])
  
  # scatterplot error vs response length
  p1<-ggplot(df, aes(x=factor(error),color=factor(complexity_byhand),y=nchar)) +
    geom_beeswarm()+
    scale_colour_manual(values = my.colors(5),name="Complexity")+
    xlab("Error")+
    ylab("Response length") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 60,hjust=1,size=12),
          axis.text.y = element_text(angle = 60,hjust=1,size=12),
          axis.title=element_text(size=18))+
  facet_grid(~experiment,scales='free')
  
  # save p1:
  nm <- names[i]
  filename <- paste0(nm,"_scatter")
  ggsave(paste0(filename,".pdf"),width = 20, height = 20, units = "cm")
  ggsave(paste0(filename,".png"),width = 20, height = 20, units = "cm")
  
  
  resplot <- df
  bp <- dplyr::group_by(resplot,experiment,complexity_byhand) %>% summarise(execution=1-sum(error)/length(error))
  
  p2<-ggplot(bp, aes(x = complexity_byhand, y = execution,fill=experiment)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(
         y = 'Fraction of executable tasks',
         x= 'Task complexity') 
  
  # Customize the graphs with your company's color palette
  
  mycols <- my.colors(length(unique(df$experiment)))
  if (length(mycols)==1){
    mycols = "#ACAEAD"
  }
  

  p2<-p2  +scale_fill_manual(name = 'Selection Strategy',
                             values = mycols)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 60,hjust=1,size=12),
          axis.text.y = element_text(angle = 60,hjust=1,size=12),
          axis.title=element_text(size=18))
  
  # save p2:
  filename <- paste0(nm,"_bar")
  ggsave(paste0(filename,".pdf"),width = 20, height = 20, units = "cm")
  ggsave(paste0(filename,".png"),width = 20, height = 20, units = "cm")
  
  # nest together and save
  nested <- (p1 + theme(plot.tag = element_text(face = 'bold',size=20))|
               p2 + theme(plot.tag = element_text(face = 'bold',size=20)))+
    plot_annotation(tag_levels = "A")

  
  nested
  # save nested
  filename <- paste0(nm,"_nested")
  plot.save(nested, width = 1600, height = 900, text.factor = 1,filename = paste0(filename,".pdf"))
  plot.save(nested, width = 1600, height = 900, text.factor = 1,filename = paste0(filename,".png"))
  
}




