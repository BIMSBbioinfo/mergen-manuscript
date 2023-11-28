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

# functions

#' extractCode
#' extract the code and text from from the text returned by LLM agent
#'
#' The function parses the code and can return the text and code as single blocks
#' This good for execution and might be useful for displaying purposes later on.
#'
#' @param text A character string containing the text with embedded code blocks.
#' @param delimiter A character string representing the delimiter used to enclose the code blocks (default: "```").
#'
#' @return A list with two elements: 'code' and 'text'. 'code' contains the concatenated code blocks, and
#'         'text' contains the remaining text with code blocks removed.
#' @examples
#' text <- "\n\nThe following, normalize the table and do PCA.
#' \n\n```\ndata <- read.table(\"test.txt\", header = TRUE, sep = \"\\t\")\n```"
#' result <- extractCode(text)
#' print(result$code)
#' print(result$text)

extractCode<-function(text,delimiter="```"){

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(text),
    assertthat::noNA(text)
  )

  assertthat::assert_that(
    assertthat::is.string(delimiter),
    assertthat::noNA(delimiter)
  )
  # -----------------------------------------------------------------------------

  # Split the text by the delimiter
  parts <- strsplit(text, delimiter, fixed = TRUE)[[1]]

  # get matches to delimiter
  mloc<-gregexpr(delimiter,text,fixed=T)[[1]]
  mdf<-cbind(mloc[-length(mloc)],mloc[-1]) # create a df with matches

  if(nrow(mdf)>1){
    mdf <- mdf[-seq(2, nrow(mdf), 2),, drop=FALSE ] # remove incorrect blocks
  }

  # extract code blocks
  code_blocks<-mapply(function(x,y) substr(text,x,y), mdf[,1]+3,mdf[,2]-1)

  # Concatenate the code blocks and remaining text
  concatenated_code <- paste(code_blocks, collapse = "\n")
  text_reg<-paste0(delimiter,".*?",delimiter)
  concatenated_remaining_text <-gsub(text_reg, "\n",text)

  return(list(code = concatenated_code,
              text = concatenated_remaining_text))
}


#' execute code
#'
#' The function executes a chunk of code either in the current working environment
#' or saves the output as an HTML file to be rendered as a part of a web page
#'
#' @param code code chunk as text, without any decoraters or HTML specific characters
#' @param output if the output is "eval" the code is executed as is if the output is "html"
#'               the code is not executed
#' @param output.file if the output is "html" user can provide a file name for the html,
#'                  if not provided a temporary file will be created
#'
#' @export
executeCode <- function(code, output = "eval",
                         output.file = NULL) {

  # Argument validation
  #------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(code),
    assertthat::noNA(code)
  )

  assertthat::assert_that(
    assertthat::is.string(output),
    assertthat::noNA(output)
  )

  if (!is.null(output.file)){
    assertthat::assert_that(
      assertthat::is.string(output.file),
      assertthat::noNA(output.file)
    )}

  #-------------------------------------------------------------------

  # Check if the output option is valid
  if (!output %in% c("eval", "html")) {
    stop("Invalid output option. Choose either 'eval' or 'html'.")
  }


  if (output == "eval") {

    expr<-str2expression(code)

  } else if (output == "html") {

    # Check if the output.file is provided
    if (is.null(output.file)) {
      # Create a temporary output file path
      output.file <- tempfile(fileext = ".html")
      message("Please provide an output.file path for the HTML output. Using temporary file:",
              output.file)
    }

    # Create a temporary R script file to store the parsed code
    temp_file <- tempfile(fileext = ".R")
    print(temp_file)
    writeLines(code, temp_file)

    wd<-getwd()
    expr <- quote({

      message("HTML file created at:", output.file,"\n")
      message("wdir is:", wd,"\n")

      # produce html fragment
      rmarkdown::render(temp_file, output_file = output.file,
                        knit_root_dir=wd,
                        output_format=rmarkdown::output_format(
                          knitr = rmarkdown::knitr_options(
                            opts_chunk = list(echo = FALSE)),
                          pandoc = rmarkdown::pandoc_options(to = "html",from = NULL),
                          clean_supporting = TRUE,
                          base_format=rmarkdown::html_fragment()
                        )
      )



      #rmarkdown::render(temp_file, output.file = output.file)

      message("HTML file created at:", output.file,"\n")

      # Read the HTML file content and return it
      #html_content <- readChar(output.file, file.info(output.file)$size)
      return(output.file)

    })
  }

  # create the error holder
  err<-list() # error and warning list

  # evalue the expression
  res<-tryCatch(
    withCallingHandlers(
      {

        eval(expr)

      },
      warning = function(w) {
        # Append the warning message to the list
        err[["warning"]] <<- c(err[["warning"]], w$message)

        # Return NULL to continue the program
        NULL
      }
    )
    ,error=function(e){
      #print(e$message)
      err[["error"]] <<- c(err[["error"]], e$message )
      NULL
    }

  )

  if(is.list(err) & ("error" %in% names(err) ) ){
    return(err)
  }else{
    # Return NULL when output is 'eval'
    return(res)
  }

}

#' Check and Install R Package
#'
#' This function checks if an R package is installed, and if not, attempts to install
#' it using either the standard CRAN repository or the Bioconductor repository.
#'
#' @param package_name A character string specifying the name of the package to be checked and installed.
#'
#' @importFrom BiocManager install
#' @importFrom BiocManager valid
#' @importFrom utils install.packages
#'
#' @examples
#' \dontrun{
#' # Check and install "dplyr" package
#' check_install("dplyr")
#' }
#'
#' @export
check_install<-function(package_name){

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(package_name),
    assertthat::noNA(package_name)
  )
  # -----------------------------------------------------------------------------

  if (!require(package_name, character.only = TRUE)) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }

    if (requireNamespace("BiocManager", quietly = TRUE)) {
      tryCatch(
        BiocManager::install(package_name, dependencies = TRUE,ask=FALSE,update=FALSE),
        error = function(e) {
          install.packages(package_name, dependencies = TRUE,ask=FALSE)
        }
      )
    }
  }
}

#' extract package names and install them
#'
#' This function extracts all package names that
#' are needed to run the code returned by the agent
#' and installs them as needed.
#' @param code code block returned by the agent.
#' @examples
#' \dontrun{
#' # check code for packages that need installing
#' extractInstallPkg(code)
#' }
#'
#' @export
extractInstallPkg<-function(code){

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(code),
    assertthat::noNA(code)
  )
  # -----------------------------------------------------------------------------

  # Split the code into separate lines
  code_lines <- strsplit(code, "\n")[[1]]

  # for each line look for library call and install things if not installed
  for(a in 1:length(code_lines)){

    if(grepl("library\\(", code_lines[a])){
      new_code <- gsub("library\\((.*)\\)", "check_install('\\1')", code_lines[a])
      message("trying to install ",new_code,"\n")
      eval(str2expression(new_code)) # execute install code

    }
  }
}

#' Extract file names from user prompt
#'
#' This function extracts file names from the user prompt
#' @param text user prompt
#'
#' @examples
#' \dontrun{
#' extractFilenames(text=user_prompt)
#' }
#' @export
extractFilenames <- function(text) {

  # argument validation
  # -----------------------------------------------------------------------------
  assertthat::assert_that(
    assertthat::is.string(text),
    assertthat::noNA(text)
  )
  # -----------------------------------------------------------------------------

  # Use regular expression to match common file extensions
  matches <- gregexpr("\\b\\S+\\.(txt|tsv|csv|xls|xlsx)\\b", text, ignore.case = TRUE)
  filenames <- regmatches(text, matches)[[1]]

  # Return filenames if found, otherwise return NA
  if (length(filenames) == 0) {
    return(NA)
  } else {
    return(filenames)
  }
}

#' Extract file headers from files in prompt
#'
#' This function extracts file headers  from
#' files mentioned in user prompts.
#' @param filenames list containing filenames mentioned in prompt
#'
#' @examples
#' \dontrun{
#' fileHeaderPrompt(filenames)
#' }
#' @export
fileHeaderPrompt<-function(filenames){

  output="\nhere are first few lines of the file(s).\n"
  for(filename in unique(filenames)){
    print(filename)
    output=paste0(output,"\n",filename,":\n")

    if(grepl("\\.(xls|xlsx)$", filename, ignore.case = TRUE)){
      requireNamespace("readxl",quietly = TRUE)

      data <- readxl::read_excel(filename, n_max=2)

      # Convert the data frame to a string with \n and \t separators
      data_string <- apply(data, 1, function(row) paste(row, collapse = "\t"))
      data_string <- paste(data_string, collapse = "\n")

      # add header
      header<-paste(colnames(data),collapse = "\t")
      data_string<-paste0(header,"\n",data_string)

      output<-paste0(output,data_string,"\n\n")

    }else{
      data_string<-paste(readLines(filename,n=2),collapse = "\n")
      output<-paste0(output,data_string,"\n\n")

    }

  }
  return(output)
}


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
output_folder="../results/simpleTest/"



## input prompts 
myPromptsFile="../scripts/mergen_prompts.Rmd"

## number of cycles: how many times each prompt should be run
cycles=5

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
      filenames<-extractFilenames(pcpairs[[i]]$prompt) 
  
      # if there are files add their content to the thingy
      if(!is.na(filenames)){ 
        addon<-fileHeaderPrompt(filenames)
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
    response<-gsub("```r", "```", response)
    response<-gsub("```R", "```", response)
    response<-gsub("```\\{r\\}", "```", response)
    response<-gsub("```\\{R\\}", "```", response)
    
    
    #write prompt and response to a file
    writeLines(paste0("prompt:\n",pcpairs[[i]]$prompt,
                      "\nresponse:\n",response),
               con=paste0(output_folder,"/cycle",j,"_task",i,".rmd") )

   # save response to the thingy 
    pcpairs[[i]]$response <- response
    
    # parse code 
    presponse<-extractCode2(response, delimiter = "```")
    
    # check if any code is returned
    if(presponse$code==""){
      results[j,i]="no code returned"
      message("completed cycle ", j, " and task ",i,"\n")
      next
    }
    
    # Split the code into separate lines
    code_lines <- strsplit( presponse$code, "\n")[[1]]
    
    # for each line look for library call and install things if not installed
    for(a in 1:length(code_lines)){
      
      if(grepl("library\\(", code_lines[a])){
        new_code <- gsub("library\\((.*)\\)", "check_install('\\1')", code_lines[a])
        eval(str2expression(new_code)) # xexecute install code
        
      }
    }

    # output html
    full_path <- file.path(getwd(), paste0(output_folder,"/cycle",j,"_task",i,".html"))
    
    
    if(errorFeedback){
      
      # not implemented in this script
      # selfcorrect()
      
    }
    
    # execute response code
    htmlfile<-executeCode2(presponse$code, output = "html",
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

