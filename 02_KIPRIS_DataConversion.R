#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KISDI Megatrend Project - Data Prep 01. Data Conversion
##  goal : Megatrend Tech Analysis
##  Data set: KIPRIS
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: 
##  Time-stamp: :  
##  Notice :
#######################################################################
 
library(data.table)
library(dplyr)
library(readr)

### Set directory path
directory.path <- "D:/downloads/KIPRIS_2024/" # Absolute path where you can find the KIPRIS patent set
directory.path <- "/Volumes/awe_passport/KIPRIS_2024/" # Absolute path where you can find the KIPRIS patent set

### Set country code
# For each country, folder starts with KIPRIS_ (ex. KIPRIS_KR, KIPRIS_JP, KIPRIS_US, KIPRIS_EU)
country.code <- "KR"

### Create new folder for each country
dir.create(paste0("R file/",country.code))

### Get file name list
file.list <- list.files(path=paste0(directory.path,"KIPRIS_",country.code,"_Patent"), pattern=NULL, 
                        all.files=FALSE, full.names=FALSE)
file.list <- file.list[!grepl("\\.zip$", file.list)]

file.name.set<-0
for (i in 1:length(file.list)){
  file.name <- list.files(path=paste0(directory.path,"KIPRIS_",country.code,"_Patent/",file.list[i]), 
                          pattern=NULL, all.files=FALSE, full.names=FALSE)
  file.name <- gsub(".txt","",file.name)
  file.name.set <- c(file.name.set,file.name)
}
file.name.set <- file.name.set[file.name.set!=0]
save(file.name.set, file=paste0("R file/",country.code,"/file.name.set.RData"))

### Load .txt file and convert it into Dataframe
# This takes long time....!
load(file=paste0("R file/",country.code,"/file.name.set.RData"))

i<-1
for (i in 1:length(file.list)){
  file.name <- list.files(path=paste0(directory.path,"KIPRIS_",country.code,"_Patent/",file.list[i]), 
                          pattern=NULL, all.files=FALSE, full.names=FALSE)
  print(paste("Reading files...",file.name))
  
  process_chunk <- function(lines, pos) {
    if (country.code =="EP"){
      split_lines <- strsplit(lines, "^|&", fixed = TRUE)
    } else {
      split_lines <- strsplit(lines, "@", fixed = TRUE)
    }
    max_cols <- max(sapply(split_lines, length))
    padded_split_lines <- lapply(split_lines, function(x) {
      length(x) <- max_cols
      return(x)
    })
    dt_chunk <- do.call(rbind, lapply(padded_split_lines, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))
    return(dt_chunk)
  }
  
  all_chunks <- list()
  
  callback <- DataFrameCallback$new(function(chunk, pos) {
    dt_chunk <- process_chunk(chunk, pos)
    all_chunks <<- append(all_chunks, list(dt_chunk))
    return(dt_chunk)
  })

  tryCatch({
    read_lines_chunked(
      paste0(directory.path,"KIPRIS_",country.code,"_Patent/", file.list[i], "/", file.name),
      callback = callback,
      locale = locale(encoding = "UTF-8")
    )
  }, error = function(e) {
    cat(paste("Error occurred while processing file", file.list[i], ":", e$message, "\n"))
  })

  data <- bind_rows(all_chunks)
  
  colnames(data) <- paste0("V", seq_len(ncol(data)))
  
  # data <- fread(paste0("D:/downloads/KIPRIS_KR_Patent/",file.list[i],"/",file.name),
  #               fill=TRUE, sep="@") #
  
  file.name <- gsub(".txt","",file.name)
  
  names(data) <- data[1,]
  data <- data[2:nrow(data),]
  
  assign(file.name, data) 
  rm(data, all_chunks)
  
  save(list = file.name, file = paste0("R file/",country.code,"/",file.name, ".RData"), envir = .GlobalEnv)

  print(i)
  rm(list = file.name)
  rm(file.name)
}

# Check
load(file="R file/file.name.set.RData")
i<-1
load(file=paste0("R file/",country.code,"/",file.name.set[i],".RData"))
JP_ABSTRACT %>% head(1)

JP_ABSTRACT[,which(is.na(names(data))==FALSE)]
JP_ABSTRACT[,4][is.na(JP_ABSTRACT[,4])==FALSE] %>% unique %>% length
