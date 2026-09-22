library("httr2")
library("jsonlite")
library("getopt")

cat("start generic wrapper service \n")

remove_null_values <- function(x) {
  # Check if the input is a list
  if (is.list(x)) {
    # Remove NULL values and apply the function recursively to sublists
    x <- lapply(x, remove_null_values)
    x <- x[!sapply(x, is.null)]
  }
  return(x)
}

getParameters <- function() {
  con <- file("inputs.json", "r")
  lines <- readLines(con)
  close(con)
  
  json_string <- paste(lines, collapse = "\n")
  json_data <- fromJSON(json_string)
  
  # Remove NULL values from json_data
  cleaned_json_data <- remove_null_values(json_data)
  return(cleaned_json_data$conditional_process)
}

parseResponseBody <- function(body) {
  hex <- c(body)
  intValues <- as.integer(hex)
  rawVector <- as.raw(intValues)
  readableOutput <- rawToChar(rawVector)
  jsonObject <- jsonlite::fromJSON(readableOutput)
  return(jsonObject)
}

getOutputs <- function(inputs, output, server) {
  url <-
    paste(paste(server, "/processes/", sep = ""),
          inputs$select_process,
          sep = "")
  request <- request(url)
  response <- req_perform(request)
  responseBody <- parseResponseBody(response$body)
  outputs <- list()
  
  for (x in 1:length(responseBody$outputs)) {
    outputformatName <-
      paste(names(responseBody$outputs[x]), "_outformat", sep = "")
    output_item <- list()
    
    for (p in names(inputs)) {
      if (p == outputformatName) {
        format <- list("mediaType" = inputs[[outputformatName]])
        output_item$format <- format
      }
    }
    output_item$transmissionMode <- "reference"
    outputs[[x]] <- output_item
  }
  
  names(outputs) <- names(responseBody$outputs)
  return(outputs)
}

executeProcess <- function(url, process, requestBodyData, cookie) {
  url <-
    paste(paste(paste(url, "processes/", sep = ""), process, sep = ""), "/execution", sep = "")
  requestBodyData$inputs$cookie <- NULL
  requestBodyData$inputs$select_process <- NULL
  
  requestBodyData$inputs$s3_access_key <-
    requestBodyData$inputs$user_credentials$s3_access_key
  requestBodyData$inputs$s3_secret_key <-
    requestBodyData$inputs$user_credentials$s3_secret_key
  requestBodyData$inputs$user_credentials <- NULL
  if (process == "plot-image") {
    tmp <- requestBodyData$inputs$color_scale
    color_scale <- gsub("__ob__", "[", tmp)
    color_scale <- gsub("__cb__", "]", color_scale)
    requestBodyData$inputs$color_scale <- color_scale
    #print(requestBodyData$inputs$color_scale)
  }
  if (process == "calculate-band") {
    requestBodyData$inputs$name <- "output"
  }
  if (process == "reproject-image") {
    requestBodyData$inputs$output_name <- "output"
  }
  #requestBodyData$inputs$input_image$href <- "https://hirondelle.crim.ca/wpsoutputs/weaver/public/test-data/S2A_MSIL2A_20190701T110621_N0500_R137_T29SPC_20230604T023542_turbidity.tiff"
  
  body <- list()
  body$inputs <- requestBodyData$inputs
  #print(body$inputs)
  body$mode <- "async"
  body$response <- "document"
  #print(body$inputs)
  
  response <- request(url) %>%
    req_headers("Accept" = "application/json",
                "Content-Type" = "application/json",
                "Cookie" = cookie) %>%
    req_body_json(body) %>%
    req_perform()
  
  cat("\n Process executed")
  cat("\n status: ", response$status_code)
  cat("\n jobID: ", parseResponseBody(response$body)$jobID, "\n")
  
  jobID <- parseResponseBody(response$body)$jobID
  
  return(jobID)
}

checkJobStatus <- function(server, process, jobID, cookie) {
  url <- paste0(server, "processes/", process, "/jobs/", jobID)
  response <- request(url) %>%
    req_headers("Cookie" = cookie) %>%
    req_perform()
  jobStatus <- parseResponseBody(response$body)$status
  jobProgress <- parseResponseBody(response$body)$progress
  return(jobStatus)
}

getStatusCode <- function(server, process, jobID, cookie) {
  url <- paste0(server, "processes/", process, "/jobs/", jobID)
  response <- request(url) %>%
    req_headers("Cookie" = cookie) %>%
    req_perform()
  status_code <- response$status_code
  return(status_code)
}

getResult <- function (server, process, jobID, cookie) {
  response <-
    request(paste0(server, "processes/", process, "/jobs/", jobID, "/results")) %>%
    req_headers("Cookie" = cookie) %>%
    req_perform()
  return(response)
}

retrieveResults <-
  function(server, process, jobID, outputData, cookie) {
    status_code <- getStatusCode(server, process, jobID, cookie)
    if (status_code == 200) {
      status <- "running"
      while (status == "running") {
        jobStatus <- checkJobStatus(server, process, jobID, cookie)
        print(jobStatus)
        if (jobStatus == "succeeded") {
          status <- jobStatus
          result <- getResult(server, process, jobID, cookie)
          if (result$status_code == 200) {
            resultBody <- parseResponseBody(result$body)
            #print(resultBody)
            if (process == "select-products-sentinel2") {
              urls <- unname(unlist(lapply(resultBody, function(x)
                x$value)))
            } else if (process == "download-band-sentinel2-product-safe" ||
                       process == "calculate-band" ||
                       process == "plot-image" || process == "reproject-image") {
              urls <- unname(unlist(lapply(resultBody, function(x)
                x$href)))
            }
            urls_with_newline <- paste(urls, collapse = "\n")
            con <- file(outputData, "w")
            writeLines(urls_with_newline, con = con)
            close(con)
          }
        } else if (jobStatus == "failed") {
          status <- jobStatus
        }
        Sys.sleep(3)
      }
      cat("\n done \n")
    } else if (status_code1 == 400) {
      print("A query parameter has an invalid value.")
    } else if (status_code1 == 404) {
      print("The requested URI was not found.")
    } else if (status_code1 == 500) {
      print("The requested URI was not found.")
    } else {
      print(paste("HTTP", status_code1, "Error:", resp1$status_message))
    }
  }

is_url <- function(x) {
  grepl("^https?://", x)
}

server <- "https://hirondelle.crim.ca/weaver/"

print("--> Retrieve parameters")
inputParameters <- getParameters()
#print(inputParameters)
print("--> Parameters retrieved")

args <- commandArgs(trailingOnly = TRUE)
outputLocation <- args[2]

print("--> Retrieve outputs")
outputs <- getOutputs(inputParameters, outputLocation, server)
print("--> Outputs retrieved")

print("--> Parse inputs")
convertedKeys <- c()
for (key in names(inputParameters)) {
  if (is.character(inputParameters[[key]]) &&
      (endsWith(inputParameters[[key]], ".dat") ||
       endsWith(inputParameters[[key]], ".txt"))) {
    con <- file(inputParameters[[key]], "r")
    url_list <- list()
    #while (length(line <- readLines(con, n = 1)) > 0) {
    #  if (is_url(line)) {
    #    url_list <- c(url_list, list(list(href = trimws(line))))
    #  }
    #}
    con <- file(inputParameters[[key]], "r")
    lines <- readLines(con)
    print("--------------------------------------------------------------------1")
    print(length(lines))
    close(con)
    if (!length(lines) > 1 && endsWith(lines, ".jp2") && startsWith(lines, "https")) {
      print("--------------------------------------------------------------------2")
      tmp <- list()
      tmp$href <- lines
      tmp$type <- "image/jp2"
      inputParameters[[key]] <- tmp
    }
    else if (!length(lines) > 1 && endsWith(lines, ".SAFE") && startsWith(lines, "s3:")) {
      print("--------------------------------------------------------------------3")
      json_string <- paste(lines, collapse = "\n")
      inputParameters[[key]] <- json_string
    } else if (inputParameters$select_process == "plot-image" ||
               inputParameters$select_process == "reproject-image") {
      print("--------------------------------------------------------------------4")
      tmp <- list()
      tmp$href <- lines
      tmp$type <- "image/tiff; application=geotiff"
      if (inputParameters$select_process == "reproject-image") {
        tmp$type <- "image/tiff; subtype=geotiff"
      }
      inputParameters[[key]] <- tmp
    } else {
      print("-----------------------------------5")
      json_string <- paste(lines, collapse = "\n")
      json_data <- fromJSON(json_string)
      inputParameters[[key]] <- json_data
    }
    convertedKeys <- append(convertedKeys, key)
  }
  else if (grepl("_Array_", key)) {
    keyParts <- strsplit(key, split = "_")[[1]]
    type <- keyParts[length(keyParts)]
    values <- inputParameters[[key]]
    value_list <- strsplit(values, split = ",")
    convertedValues <- c()
    
    for (value in value_list) {
      if (type == "integer") {
        value <- as.integer(value)
      } else if (type == "numeric") {
        value <- as.numeric(balue)
      } else if (type == "character") {
        value <- as.character(value)
      }
      convertedValues <- append(convertedValues, value)
      
      convertedKey <- ""
      for (part in keyParts) {
        if (part == "Array") {
          break
        }
        convertedKey <-
          paste(convertedKey, paste(part, "_", sep = ""), sep = "")
      }
      convertedKey <- substr(convertedKey, 1, nchar(convertedKey) - 1)
    }
    
    inputParameters[[key]] <- convertedValues
    #print("-------------------------")
    #print(convertedValues)
    #print("-------------------------")
    convertedKeys <- append(convertedKeys, convertedKey)
  } else {
    #print("-------------------------")
    #print(key)
    #print(inputParameters[[key]])
    if (!is.null(inputParameters[[key]])) {
      convertedKeys <- append(convertedKeys, key)
    }
    #print("-------------------------")
    
  }
}
#print(inputParameters)
names(inputParameters) <- convertedKeys
#print(inputParameters)
print("--> Inputs parsed")

print("--> Prepare process execution")
jsonData <- list("inputs" = inputParameters,
                 "outputs" = outputs)

cookie <- inputParameters$cookie

print("--> Execute process")
jobID <-
  executeProcess(server, inputParameters$select_process, jsonData, cookie)
print("--> Process executed")

print("--> Retrieve results")
retrieveResults(server,
                inputParameters$select_process,
                jobID,
                outputLocation,
                cookie)
print("--> Results retrieved")