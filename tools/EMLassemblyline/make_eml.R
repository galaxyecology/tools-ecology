##07/06/2023
##Genthon Tanguy
###make_eml

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  title <- args[1]
}

EMLassemblyline::make_eml("output_template",eml.path=".", dataset.title = title)
old.names <- list.files(path=".", pattern=".xml")
print(old.names)
file.rename(from=old.names, to="eml.xml")
