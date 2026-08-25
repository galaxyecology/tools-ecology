library("httr2")
library("jsonlite")
library("getopt")

cat("start generic wrapper service \n")

getParameters <- function(){
    con <- file("inputs.json", "r")
    line <- readLines(con, n = 1)
    json <- fromJSON(line)
    close(con)
    return(json)
}

parseResponseBody <- function(body) {
  hex <- c(body)
  intValues <- as.integer(hex)
  rawVector <- as.raw(intValues)
  readableOutput <- rawToChar(rawVector)
  jsonObject <- jsonlite::fromJSON(readableOutput)
  return(jsonObject)
}

getOutputs <- function(inputs, output, server, process) {
    url <- paste(paste(server, "processes/", sep = ""), process, sep = "")
    request <- request(url)
    response <- req_perform(request)
    responseBody <- parseResponseBody(response$body)
    outputs <- list()

    for (x in 1:length(responseBody$outputs)) {
        outputformatName <- paste(names(responseBody$outputs[x]), "_outformat", sep="")
        output_item <- list()

        for (p in names(inputs)) {
            if(p == outputformatName){
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

executeProcess <- function(url, process, requestBodyData, output) {
    url <- paste(paste(paste(url, "processes/", sep = ""), process, sep = ""), "/execution", sep = "")
    cookie <- requestBodyData$inputs$cookie
    requestBodyData$inputs$cookie <- NULL
    response <- request(url) %>%
    req_headers(
      "Prefer" = "respond-async",
      "Authorization" = cookie
    ) %>%
    req_body_json(requestBodyData) %>%
    req_perform()

    cat("\n Process executed")
    cat("\n status: ", response$status_code)
    cat("\n jobID: ", parseResponseBody(response$body), "\n")
    
    job_location <- response |> resp_headers("location")
    jobID <- sub(".*\\/([a-z0-9\\-]+)$", "\\1", job_location$location)

    return(jobID)
}

checkJobStatus <- function(server, jobID) {
  response <- request(paste0(server, "jobs/", jobID)) %>%
    req_headers(
        'accept' = 'application/json'
    ) %>%
    req_perform()
  jobResponse <- parseResponseBody(response$body) 
  jobStatus <- jobResponse$status
  jobProgress <- jobResponse$progress
  cat(paste0("\n status: ", jobStatus, ", progress: ", jobProgress))
  return(jobResponse)
}

getStatusCode <- function(server, jobID) {
  url <- paste0(server, "jobs/", jobID)
  response <- request(url) %>%
      req_headers(
        'accept' = 'application/json'
      ) %>%
      req_perform()
  return(response$status_code)
}

getResult <- function (server) {
  print("start getting result")
  print(server)
  response <- request(server) %>%
    req_headers(
      'accept' = 'application/json'
    ) %>%
    req_perform()
  res <- parseResponseBody(response$body)
  return(res)
}

retrieveResults <- function(server, jobID, outputData) {
  status_code <- getStatusCode(server, jobID)
  print(status_code)
  if (status_code == 200) {
    status <- "running"
    cat(status)
    while (status == "running") {
      job_res <- checkJobStatus(server, jobID)
      job_status <- job_res$status
      if (job_status == "successful") {
        status <- job_status
        result <- getResult(job_res$links$href[2])
        urls <- unname(unlist(result))
        con <- file(outputData, "w")
        print(outputData)
        writeLines(urls, con = con)
        close(con)
      } else if (job_status == "failed") {
        status <- job_status
      }
      Sys.sleep(10)
    }
    cat("\n done \n")
  } else if (status_code == 400) {
    print("A query parameter has an invalid value.")
  } else if (status_code == 404) {
    print("The requested URI was not found.")
  } else if (status_code == 500) {
    print("The requested URI was not found.")
  } else {
    print(paste("HTTP", status_code, "Error:", job_res$status_message))
  }
}


is_url <- function(x) {
  grepl("^https?://", x)
}

print("--> Retrieve parameters")
inputParameters <- getParameters()
print("--> Parameters retrieved")

args <- commandArgs(trailingOnly = TRUE)
server <- "https://processing.terrabyte.lrz.de/"
process <- "water-quality-eo-app-pkg"
outputLocation <- args[2]

print("--> Retrieve outputs")
outputs <- getOutputs(inputParameters, outputLocation, server, process)
print("--> Outputs retrieved")

print("--> Parse inputs")
convertedKeys <- c()
for (key in names(inputParameters)) {
  if (is.character(inputParameters[[key]]) && (endsWith(inputParameters[[key]], ".dat") || endsWith(inputParameters[[key]], ".txt"))) { 
    con <- file(inputParameters[[key]], "r")
    url_list <- list()
    while (length(line <- readLines(con, n = 1)) > 0) {
      if (is_url(line)) {
        url_list <- c(url_list, list(list(href = trimws(line))))
      }
    }
    close(con)
    inputParameters[[key]] <- url_list
    convertedKeys <- append(convertedKeys, key)
  }
  else if (grepl("_Array_", key)) {
    keyParts <- strsplit(key, split = "_")[[1]]
    type <- keyParts[length(keyParts)]
    values <- inputParameters[[key]]
    value_list <- strsplit(values, split = ",")

    convertedValues <- c()

    for (value in value_list) {
      if(type == "integer") {
        value <- as.integer(value)
      } else if (type == "numeric") {
        value <- as.numeric(balue)
      } else if (type == "character") {
        value <- as.character(value)
      }
    convertedValues <- append(convertedValues, value)

    convertedKey <- ""
    for (part in keyParts) {
      if(part == "Array") {
        break
      }
      convertedKey <- paste(convertedKey, paste(part, "_", sep=""), sep="")
    }
    convertedKey <- substr(convertedKey, 1, nchar(convertedKey)-1)
}

    inputParameters[[key]] <- convertedValues
    convertedKeys <- append(convertedKeys, convertedKey)
  } else {
    convertedKeys <- append(convertedKeys, key)
  }
}
print(convertedKeys)
names(inputParameters) <- convertedKeys
print("--> Inputs parsed")

print("--> Prepare process execution")
jsonData <- list(
  "inputs" = inputParameters,
  "outputs" = outputs
)

print("--> Execute process")
jobID <- executeProcess(server, process, jsonData, outputLocation)
print("--> Process executed")

print("--> Retrieve results")
retrieveResults(server, jobID, outputLocation)
print("--> Results retrieved")