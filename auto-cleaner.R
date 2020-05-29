#!/usr/bin/env Rscript

#Include libraries
library("pdftools")
library("grid")
library("gridExtra")

args = commandArgs(trailingOnly=TRUE)
direc    <- ""
pattern  <- ""
out      <- ""

#Test if there is missing arguments
if(length(args) <= 1 || length(args) > 3){
	stop("Please specify the direc of document/documents and the regex pattern you are looking for !")
} else if (length(args) == 3){
	direc    <- args[1]
	pattern  <- args[2]
	out      <- args[3]
}

setwd(direc)
files <- grep(pattern=".pdf",list.files(),value=TRUE)
pdfs  <- lapply(files,pdf_text)
files <- substr(files,start=1,stop=nchar(files)-4)
coll <- data.frame("DocNames" = c(),"Text" = c())

#Data Filteration
for (i in seq_along(pdfs)){
	npages  <- length(pdfs[i][[1]])
	temp_df <- data.frame("DocNames"=rep(files[i],npages),"Text" = pdfs[i])
	colnames(temp_df) <- c("DocNames","Text")
	
	#Taking what matters based on the pattern
	temp_df       <- temp_df[which(grepl(pattern=pattern,x=temp_df$Text,ignore.case=TRUE)),names(temp_df) %in% c("DocNames","Text")]
	temp_df$Text  <- strsplit(temp_df$Text,split="\n\n")
	temp_df$Text  <- lapply(temp_df$Text,function(x) x[grep(pattern=pattern,x,ignore.case=TRUE)])
	coll    <- rbind(coll,temp_df)	
}

#Groubing and filtering on a pdf file
pdf(out,height=20,width=8.5)
grid.table(as.character(coll$Text))
dev.off()
