##07/06/2023 ##Genthon Tanguy
##update 15/11/2023 ##Seguineau Pauline

###make_eml

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  title <- args[1]
  start <- args[2]
  end <-args[3]
  data_table <- args[4]
  data_other <- args[5]
  destable <- args[6]
  desother <- args[7]
  quote <- args[8]
  table_url <- args[9]
  other_url <- args[10]
}
 

###Format data###

if (data_table == ""){
    table=NULL
}else{
    table =  strsplit(data_table," ")
    for (file in table){
         name_table = gsub("\\.[a-zA-Z]*", "", file)}
    }
    
    
if (quote != ""){
   quote = strsplit(quote,",")
   if (length(quote[[1]]) != length(table[[1]])){
   stop("Your number of quote(s) isn't equal to your number of data table file(s). Please enter the quote parameter as many time as the number of data tables you've input")}
}


tablequote=NULL
for (quote_table in quote[[1]]){
    if (quote_table=="quote"){
        quote_table = sub("quote",'"', quote_table)}
    else if (quote_table=="apostrophe"){
             quote_table = gsub("apostrophe","'",quote_table)}
    else if (quote_table=="none"){
             quote_table = gsub("none","",quote_table)}
    tablequote = c(tablequote, quote_table)
}

  
if (data_other == ""){
    other=NULL
}else{
    other =  strsplit(data_other," ")
    for (file in other){
         name_other = gsub("\\.[a-zA-Z]*", "", file)}
         }

if (data_table !=""){
   if (destable == ""){
       des_table = name_table
   }else{
       des_table =  strsplit(destable,",")}
}


if (data_other !=""){
   if (desother == ""){
       des_other = name_other
   }else{
       des_other =  strsplit(desother,",")}
}


if (data_table !=""){
   if (table_url == ""){
       urltable = ""
   }else{
       table_url = gsub("\\-" ,"", table_url)
       urltable =  strsplit(table_url,",")
   }
}

if (data_other !=""){
   if (other_url == ""){
       urlother = ""
   }else{
       other_url = gsub("\\-" ,"", other_url)
       urlother =  strsplit(other_url,",")
   }
}

###Make EML###
   
if (!is.null(table) && !is.null(other)){
   
   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      data.table=table[[1]],
                      data.table.name = name_table,
                      data.table.description = des_table[[1]],
                      data.table.quote.character = tablequote,
                      data.table.url = urltable[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]]
                      )
                      
}else if (is.null(table) && is.null(other)){
  
   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end))
   
}else if (!is.null(table) && is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      data.table=table[[1]],
                      data.table.name = name_table,
                      data.table.description = des_table[[1]],
                      data.table.quote.character = tablequote,
                      data.table.url = urltable[[1]])
                      

}else if (is.null(table) && !is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])}
                      


old.names <- list.files(path=".", pattern=".xml")
print(old.names)
file.rename(from=old.names, to="eml.xml")






