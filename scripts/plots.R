# script for plots

require(ggplot2)
require(hrbrthemes)

arrangeData<-function(x,prompt="actAs"){
  resplot<-x
  resplot$error=ifelse(is.na(resplot$error),1,0)
  
  resplot<-tapply(resplot$error,as.factor(resplot$complexity),function(x) sum(x)/length(x),simplify = TRUE)
  resplot<- data.frame(error=resplot,complexity=as.numeric(names(resplot)))
  cbind(resplot,prompt=prompt)
}
res.df2=read.table(file="../results/simpleTest/results.txt",sep="\t",header=T)

simple.df=arrangeData(res.df2,prompt="Simple")

res.df2=read.table(file="../results/actAs_Test/results.txt",sep="\t",header=T)
actas.df=arrangeData(res.df2,prompt="ActAs")

res.df2=read.table(file="../results/CoT_Test/results.txt",sep="\t",header=T)
cot.df=arrangeData(res.df2,prompt="CoT")

res.df2=read.table(file="../results/fileCont_Test/results.txt",sep="\t",header=T)
file.df=arrangeData(res.df2,prompt="FileCont")


res.df2=read.table(file="../results/selfCorrect_Test/results.txt",sep="\t",header=T)
selfc.df=arrangeData(res.df2,prompt="SelfCorrect")

# plot barplot for this experiment


# only simple and ActAs
p2<-ggplot(rbind(actas.df,simple.df), 
          aes(x = complexity, y = error,fill=prompt)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Fraction of executable code',
       subtitle = 'As task complexity increases LLM generated code becomes non-executable',
       y = 'Fraction of executable tasks',
       x= 'Task complexity') 

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)

# Customize the graphs with your company's color palette
p2<-p2  +scale_fill_manual(name = 'Prompt Strategy',
                           values = my.colors(2))+
  theme_ipsum() +
  theme(plot.title = element_text(color = "#315785"),
        plot.caption = element_text(color = "#315785", face = 'bold'),
        axis.text.x = element_text(angle = 60,hjust=1,size=8),      
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=13))

p2


# simple, actAs, CoT
p2<-ggplot(rbind(actas.df,simple.df,cot.df), 
           aes(x = complexity, y = error,fill=prompt)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Fraction of executable code',
       subtitle = 'As task complexity increases LLM generated code becomes non-executable',
       y = 'Fraction of executable tasks',
       x= 'Task complexity') 

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)

# Customize the graphs with your company's color palette
p2<-p2  +scale_fill_manual(name = 'Prompt Strategy',
                           values = my.colors(3))+
  theme_ipsum() +
  theme(plot.title = element_text(color = "#315785"),
        plot.caption = element_text(color = "#315785", face = 'bold'),
        axis.text.x = element_text(angle = 60,hjust=1,size=11),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=13))

p2

# simple, actAs, CoT,fileContent
p2<-ggplot(rbind(actas.df,simple.df,cot.df,file.df), 
           aes(x = complexity, y = error,fill=prompt)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Fraction of executable code',
       subtitle = 'As task complexity increases LLM generated code becomes non-executable',
       y = 'Fraction of executable tasks',
       x= 'Task complexity') 

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)

# Customize the graphs with your company's color palette
p2<-p2  +scale_fill_manual(name = 'Prompt Strategy',
                           values = my.colors(4))+
  theme_ipsum() +
  theme(plot.title = element_text(color = "#315785"),
        plot.caption = element_text(color = "#315785", face = 'bold'),
        axis.text.x = element_text(angle = 60,hjust=1,size=8),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=13))

p2



# simple, actAs, CoT,fileContent
p2<-ggplot(rbind(actas.df,simple.df,cot.df,file.df,selfc.df), 
           aes(x = complexity, y = error,fill=prompt)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(title = 'Fraction of executable code',
       subtitle = 'As task complexity increases LLM generated code becomes non-executable',
       y = 'Fraction of executable tasks',
       x= 'Task complexity') 

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)

# Customize the graphs with your company's color palette
p2<-p2  +scale_fill_manual(name = 'Prompt Strategy',
                           values = my.colors(5))+
  theme_ipsum() +
  theme(plot.title = element_text(color = "#315785"),
        plot.caption = element_text(color = "#315785", face = 'bold'),
        axis.text.x = element_text(angle = 60,hjust=1,size=8),
        axis.title.x= element_text(size=12),
        axis.title.y= element_text(size=13))

p2
