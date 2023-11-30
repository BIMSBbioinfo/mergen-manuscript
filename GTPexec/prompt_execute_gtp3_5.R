# this script converts clear text GPT fine-tuning prompts to JSON files
# needed for GPT


# open questions
#- how do i save the results with the code not only the result [done]
#   save the result as html and the prompt+response as rmd [done]
#- how do i keep track of succesful and unsuccesful executions [done]
#    have a dataframe to keep track of each cycle and task
#- how do i capture library commands and install missing libraries before execution
#   do this  before execution via regular expressions [done]
# HOW to deal with errors when GPT doesn't respond? [ ]

# general variables
# variables: output folder, context for the prompt, should error feedback be used


## libs
library(mergen)


##contexts:
simpleContext <- "Instruction: Provide R code for the following tasks. Provide the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response.\ntask:\n"

actAs<-"Instruction: Act as an expert bioformatician, biologist and R programmer. Complete the following tasks using your expertise and always provide relevant code. When providing the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response.\ntask:\n"

CoT<-"Instruction: Act as an expert bioformatician, biologist and R programmer. Complete the following tasks using your expertise and always provide code. When providing code, provide the code in triple backticks (``` and ```). Provide the code as a single block at the end of your response. Let's work this out in a step by step way to be sure we have the right answer.\ntask:\n"


## working directory
setwd("~/Dropbox/PAPERS/R-devel/mergen-manuscript/GTPexec")
readRenviron(".Renviron")


# what context is fed
context=simpleContext #actAs #CoT


## do you add file content example
fileContents=FALSE

## do you feed error back?
errorFeedback=FALSE

## output folder
output_folder="../results/simpleTest_gtp3_5/"

## input prompts
myPromptsFile="../scripts/mergen_prompts.Rmd"

## number of cycles: how many times each prompt should be run
cycles=10

### --- Being tests

# read the data
tdat=readLines(myPromptsFile)

# parse to find "prompt" keyword and "completion" keyword
prompts=which(tdat %in% "prompt:")
ends= which(tdat %in% "--- PROMPT END")

# get task types from the task list input file
taskTypes=tdat[grepl("^###",tdat)]
taskTypes=gsub("### Task `r i`,`r i<-i\\+1` ", "", taskTypes)
taskTypes=strsplit( taskTypes,",")
taskTypes=lapply(taskTypes, function(x) x[grep("taskType|numData|complexity", x)] )
taskTypes=lapply(taskTypes,function(x) gsub("taskType: | complexity: | numData: ","",x))
taskTypes=data.frame(do.call("rbind",taskTypes))
colnames(taskTypes)=c("taskType","numData","complexity")
taskTypes[,2]=as.numeric(taskTypes[,2])
taskTypes[,3]=as.numeric(taskTypes[,3])

if (length(prompts) != length(ends)){
  warning("number of prompts not equal to number of comps")
}

# resulting prompt completion pairs will be here
pcpairs=list()

for( i in 1:(length(prompts)-1)){

  pcpairs[[i]]=list(prompt=paste(tdat[(prompts[i]+1):(ends[i]-1)],collapse="\n"))


}

# add the last bit
pcpairs[[i+1]]=list(prompt=paste(tdat[(prompts[i+1]+1):(ends[i+1]-1)],collapse="\n"))



# make folders if it doesn't exist
# Check if the folder exists
if (!file.exists(output_folder)) {
  # If the folder doesn't exist, create it and any parent directories as needed
  dir.create(output_folder, recursive = TRUE)
}

# output the results on this data frame
results <- matrix(nrow=cycles,ncol=length(pcpairs))

# how many repetions per task defined by j
for(j in 1:cycles){
  # create agent
  require(mergen)
  myAgent<-mergen::setupAgent(name="openai",model="gpt-3.5-turbo",type="chat",ai_api_key=Sys.getenv("OPENAI_API_KEY"))

  message("cycle ",j, " starting...\n")

  # for each prompt
  for( i in 1:length(pcpairs)){

    message("responding to prompt ",i, "\n")

    # we can add file content samples to the prompt if this is true
    if(fileContents){
      filenames<-mergen::extractFilenames(pcpairs[[i]]$prompt)

      # if there are files add their content to the thingy
      if(!is.na(filenames)){
        addon<-mergen::fileHeaderPrompt(filenames)
      }

      # add to the prompt
      pcpairs[[i]]$prompt<-paste0(pcpairs[[i]]$prompt,addon)

    }


    # generate response
    response <- sendPrompt(myAgent, pcpairs[[i]]$prompt,context=context,return.type="text",
                           max_tokens = 1000)

    # sometimes error 200 is returned, if that's the case it should retry getting
    # the response until success, check the chat app by the indian boy

    #clear response of weird characters, otherwise this will return as error
    response <- mergen::clean_code_blocks(response)


    #write prompt and response to a file
    writeLines(paste0("prompt:\n",pcpairs[[i]]$prompt,
                      "\nresponse:\n",response),
               con=paste0(output_folder,"/cycle",j,"_task",i,".rmd") )

   # save response to the thingy
    pcpairs[[i]]$response <- response

    # parse code
    presponse<-extractCode(response, delimiter = "```")

    # check if any code is returned
    if(presponse$code==""){
      results[j,i]="no code returned"
      message("completed cycle ", j, " and task ",i,"\n")
      next
    }

    # Split the code into separate lines
    code_lines <- strsplit(presponse$code, "\n")[[1]]

    # for each line look for library call and install things if not installed
    extractInstallPkg(presponse$code)

    # output html
    full_path <- file.path(getwd(), paste0(output_folder,"/cycle",j,"_task",i,".html"))


    if(errorFeedback){

      # not implemented in this script
      # selfcorrect()

    }

    # execute response code
    htmlfile<-executeCode(presponse$code, output = "html",
                          output.file =full_path)

    # if error do sth else, save error results as well
    if( "error" %in% names(htmlfile)){

      results[j,i]=htmlfile$error
    }else{
      results[j,i]=NA
    }

   message("completed cycle ", j, " and task ",i,"\n")
  }

}

# save results
res.df=reshape2::melt(results,varnames=c("cycle","task"),value.name = "error")

res.df2<-cbind(res.df,taskTypes[res.df[,2],])
write.table(res.df2,file=paste0(output_folder,"/results.txt"),sep="\t",row.names = FALSE)



# plot barplot for this experiment
require(ggplot2)
require(hrbrthemes)

resplot<-res.df2
resplot$error=ifelse(is.na(resplot$error),1,0)

resplot<-tapply(resplot$error,as.factor(resplot$complexity),function(x) sum(x)/length(x),simplify = TRUE)
resplot<- data.frame(error=resplot,complexity=as.numeric(names(resplot)))
p2<-ggplot(resplot, aes(x = complexity, y = error)) +
  geom_bar(position="dodge", stat="identity")+
  labs(title = '% executible code',
       subtitle = 'As task complexity increases LLM generated code becomes non-executible',
       y = 'Fraction of executable tasks',
       x= 'Task complexity')

# make more colors
my.colors <-
  colorRampPalette(c('#272C63', '#4BBDD4', '#ACAEAD'))
my.colors(7)

# Customize the graphs with your company's color palette
p2<-p2  +scale_fill_manual(name = 'Selection Strategy',
                           values = my.colors(2))+
  theme_ipsum() +
  theme(plot.title = element_text(color = "#315785"),
        plot.caption = element_text(color = "#315785", face = 'bold'),
        axis.text.x = element_text(angle = 60,hjust=1,size=8))

p2


# count nr of characters for response, remove \n first.
for (j in 1:length(pcpairs)){
  pcpairs[[j]]$nchars <- nchar(gsub("[\n]"," ",pcpairs[[j]]$response))
}

# save pcpairs as RDS
saveRDS(pcpairs,file="pcpars.rds")
