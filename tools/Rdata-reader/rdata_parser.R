#!/usr/bin/env Rscript
#Use a Rdata file and attributes to extract
#Get every argument and write a file with its values(s)

cat("Load rdata file\n")

#get the rdata file
args = commandArgs(trailingOnly=TRUE)
rdata<-load(args[1])
rdata<-get(rdata)
#sum<-summary(rdata) 

#get the selected attributes to explore
attributes_selected <- commandArgs(trailingOnly=TRUE)[2]
attributes<-strsplit(attributes_selected, ",") #List of elements

#write.table(sum,file = "summary.tabular",sep='\t',row.names=FALSE)
len<-length(attributes[[1]])
bind<-tail(args,n=1)

#file type definition
file_ext<-function(ext,attribute){
	file<-paste("attribute_",attribute,ext,sep="") #Filename definition
        file<-paste("outputs/",file,sep="")
	return(file)
}

cat("Write element(s) : ")
for (i in 1:len){
	attribute<-attributes[[1]][i] #Get the attribute i 
	if(! any(names(rdata)==attribute)){
		error<-paste(attribute, " doesn't exist in the RData. Check the inputs files")
		write(error, stderr())
	}

	attribute_val<-eval(parse(text=paste("rdata$",attribute,sep=""))) #Extract the value(s)

	if(is.null(attribute_val)){ #Galaxy can't produce output if NULL
		file<-file_ext(".txt",attribute)
                cat(paste(attribute,", ",sep=""))
		write("Return NULL value",file=file)
		next #Exit loop
	}

	if (typeof(attribute_val)=="list"){ #Need to be corrected, fail in galaxy but not in R
		if(length(attribute_val)=="0"){
			file<-file_ext(".txt",attribute)
			sink(file=file)
			print("Empty list :") #If the list is empty without element, file is empty and an error occur in galaxy
			print(attribute_val)
			sink()
			next
		}else{
			attribute_val<-as.data.frame(do.call(rbind, attribute_val))
			file<-file_ext(".tabular",attribute)
                        cat(paste(attribute,", ",sep=""))
			write.table(attribute_val,file=file,row.names=FALSE)
			next
		}
	}else if (typeof(attribute_val)=="language"){ #OK
		attribute_val<-toString(attribute_val,width = NULL)
		file<-file_ext(".txt",attribute)
                cat(paste(attribute,", ",sep=""))
		write(attribute_val,file=file)
		next
	}
        file<-file_ext(".tabular",attribute)
        dataframe<-as.data.frame(attribute_val)
        names(dataframe)<-attribute
        if(bind=="nobind"){
            cat(paste(attribute,", ",sep=""))
            write.table(dataframe,file=file,row.names=FALSE,sep="    ")
        }else{
            cat(paste(attribute,", ",sep=""))
            if(!exists("alldataframe")){
                alldataframe<-dataframe
            }else{
    	        alldataframe<-cbind(alldataframe, dataframe)
            }
        }
}

if(exists("alldataframe")&&bind=="bind"){
    write.table(alldataframe,file="outputs/all_attributes.tabular",row.names=FALSE,sep="	")
}
q('no')
