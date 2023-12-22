##07/06/2023 ##Genthon Tanguy
##update 15/11/2023 ## Seguineau Pauline

###make_eml

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  title <- args[1]
  start <- args[2]
  end <-args[3]
  data_table <- args[4]
  data_raster <- args[5]
  data_vector <- args[6]
  data_other <- args[7]
  destable <- args[8]
  desraster<- args[9]
  desvector<- args[10]
  desother <- args[11]
  quote <- args[12]
  table_url <- args[13]
  other_url <- args[14]
  raster_url <- args[15]
  vector_url <- args[16]
}

#Load package

library(EMLassemblyline)
sessionInfo()


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

  
if (data_raster == ""){
    raster=NULL
}else{
    raster =  strsplit(data_raster," ")
    for (file in raster){
         name_raster = gsub("\\.[a-zA-Z]*", "", file)}
    }
    
if (data_vector == ""){
    vector=NULL
}else{
    vector =  strsplit(data_vector," ")
    for (file in vector){
         name_vector = gsub("\\.[a-zA-Z]*", "", file)}
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

if (data_raster !=""){
   if (desraster == ""){
       des_raster = name_raster
   }else{
       des_raster =  strsplit(desraster,",")}
}

if (data_vector !=""){ 
   if (desvector == ""){
       des_vector = name_vector
   }else{
       des_vector =  strsplit(desvector,",")}
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

if (data_raster !=""){
   if (raster_url == ""){
       urlraster = ""
   }else{
       raster_url = gsub("\\-" ,"", raster_url)
       urlraster =  strsplit(raster_url,",")
   }
}

if (data_vector !=""){
   if (vector_url == ""){
       urlvector = ""
   }else{
       vector_url = gsub("\\-" ,"", vector_url)
       urlvector =  strsplit(vector_url,",")
   }
}



###Make EML###
   
if (!is.null(table) && !is.null(raster) && !is.null(vector) && !is.null(other)){
   
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
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])
                      
}else if (is.null(table) && is.null(raster) && is.null(vector) && is.null(other)){
  
   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end))
   
}else if (!is.null(table) && is.null(raster) && is.null(vector) && is.null(other)){

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
                      data.table.url = urltable[[1]]
                      )

}else if (!is.null(table) && !is.null(raster) && is.null(vector) && is.null(other)){
   
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
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]])
                      
}else if (!is.null(table) && !is.null(raster) && !is.null(vector) && is.null(other)){
  
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
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]])

}else if (is.null(table) && !is.null(raster) && is.null(vector) && is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]])
                      
}else if (is.null(table) && !is.null(raster) && !is.null(vector) && is.null(other)){
   
   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]])

}else if (is.null(table) && !is.null(raster) && !is.null(vector) && !is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])
                 
}else if (is.null(table) && is.null(raster) && !is.null(vector) && is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]])
                      
}else if (is.null(table) && is.null(raster) && !is.null(vector) && !is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])
                      
}else if (is.null(table) && is.null(raster) && is.null(vector) && !is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])
                      
}else if (!is.null(table) && is.null(raster) && !is.null(vector) && is.null(other)){

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
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]])

}else if (!is.null(table) && is.null(raster) && is.null(vector) && !is.null(other)){

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
                      other.entity.url= urlother[[1]])

}else if (is.null(table) && !is.null(raster) && is.null(vector) && !is.null(other)){

   EMLassemblyline::make_eml(
                      path="output_template",
                      data.path="data_files",
                      eml.path=".", 
                      dataset.title = title,
                      temporal.coverage = c(start,end),
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])

}else if (!is.null(table) && is.null(raster) && !is.null(vector) && !is.null(other)){
   
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
                      spatial.vector=vector[[1]],
                      spatial.vector.name = name_vector,
                      spatial.vector.description = des_vector[[1]],
                      spatial.vector.url = urlvector[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])

}else if (!is.null(table) && !is.null(raster) && is.null(vector) && !is.null(other)){

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
                      spatial.raster=raster[[1]],
                      spatial.raster.name = name_raster,
                      spatial.raster.description = des_raster[[1]],
                      spatial.raster.url = urlraster[[1]],
                      other.entity=other[[1]],
                      other.entity.name = name_other,
                      other.entity.description = des_other[[1]],
                      other.entity.url= urlother[[1]])}

issues()
old.names <- list.files(path=".", pattern=".xml")
file.rename(from=old.names, to="eml.xml")






