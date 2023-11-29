
direcs <- c("simpleTest_gtp3_5","selfCorrect_Test_gtp3_5",
            "fileCont_Test_gtp3_5","CoT_Test_gtp3_5","actAs_Test_gtp3_5")

for (i in direcs){
  assign(paste0(i,"_res"),read.table(paste0(i,"/results.txt"),header=TRUE))
  assign(paste0(i,"_pcpairs"),readRDS(paste0(i,"/pcpairs.rds")))
}

matr_plot <- matrix(ncol=4)
colnames(matr_plot) <- c("complexity","error","nchar","experiment")

for (i in direcs){

  curr_res <- eval(parse(text=paste0(i,"_res")))
  curr_pcpair <- eval(parse(text=paste0(i,"_pcpairs")))
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
  exname <- strsplit(i,"Test_gtp3_5")[[1]]


  resplot<- data.frame(error=curr_res$error,nchar = nchar, experiment = rep(exname,length(nchar)))

  # create new difficulty levels based on nchar
  complexity<-cut(as.numeric(resplot$nchar), quantile(as.numeric(resplot$nchar), probs = c(0,0.2,0.4,0.6,0.8,1)), include.lowest=TRUE,
                  labels=1:5)

  # bind to dataframe:
  resplot<-cbind(complexity,resplot)

  #assign to variable
  matr_plot<-rbind(matr_plot,resplot)
}

# remove first row
matr_plot <- matr_plot[-1,]


#plot
ggplot(matr_plot,aes(x=experiment,fill = factor(error),y=nchar)) +
  geom_dotplot(binaxis = "y", binwidth = 1, stackdir = "center", dotsize = 50,
               stackgroups=FALSE, binpositions="all", stackratio=1.2, alpha=0.8) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# group by exp
bp <- dplyr::group_by(matr_plot,experiment,complexity) %>% summarise(execution=1-sum(error)/length(error))


# plot barplot:
ggplot(bp,aes(x=complexity,y=execution,fill=factor(experiment)))+
  geom_bar(position="dodge", stat="identity")

