library("httr2")
library("jsonlite")
library("getopt")

cat("start generic wrapper service")

getParameters <- function(){
    args <- commandArgs(trailingOnly = TRUE)

    names <- list()
    params <- list()

    for (x in 1:length(args)) {
        if(x %% 2 == 0) {
            param <- gsub("--", "", args[x-1])
            value <- args[x]
            params <- append(params, value)
            names <- append(names, param)
        }
    }
    names(params) <- names
    return(params)
}

parseResponseBody <- function(body) {
  hex <- c(body)
  intValues <- as.integer(hex)
  rawVector <- as.raw(intValues)
  readableOutput <- rawToChar(rawVector)
  jsonObject <- jsonlite::fromJSON(readableOutput)
  return(jsonObject)
}

getOutputs <- function(inputs, output) {
    url <- paste(paste(inputs$server, "/processes/", sep = ""), inputs$process, sep = "")
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
    response <- request(url) %>%
    req_headers(
      "accept" = "/*",
      "Prefer" = "respond-async;return=representation",
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(requestBodyData) %>%
    req_perform()

    cat("\n Process executed")
    cat("\n status: ", response$status_code)
    cat("\n jobID: ", parseResponseBody(response$body)$jobID, "\n")

    jobID <- parseResponseBody(response$body)$jobID

    return(jobID)
}

checkJobStatus <- function(server, jobID) {
  response <- request(paste0(server, "jobs/", jobID)) %>%
    req_headers(
        'accept' = 'application/json'
    ) %>%
    req_perform()
  jobStatus <- parseResponseBody(response$body)$status
  jobProgress <- parseResponseBody(response$body)$progress
  cat(paste0("\n status: ", jobStatus, ", progress: ", jobProgress))
  return(jobStatus)
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

getResult <- function (server, jobID) {
  response <- request(paste0(server, "jobs/", jobID, "/results")) %>%
    req_headers(
      'accept' = 'application/json'
    ) %>%
    req_perform()
  return(response)
}

retrieveResults <- function(server, jobID, outputData) {
    status_code <- getStatusCode(server, jobID)
    if(status_code == 200){
        status <- "running"
        cat(status)
        while(status == "running"){
            jobStatus <- checkJobStatus(server, jobID)
            if (jobStatus == "successful") {
                status <- jobStatus
                result <- getResult(server, jobID)
                if (result$status_code == 200) {
                  resultBody <- parseResponseBody(result$body)
                  urls <- unname(unlist(lapply(resultBody, function(x) x$href)))
                  urls_with_newline <- paste(urls, collapse = "\n")
                  sink(paste0(outputData, "_result_urls.txt"))
                    cat(urls_with_newline, "\n")
                  sink()
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

inputs <- getParameters()

inputParameters <- inputs[3:length(inputs)]

outputLocation <- inputParameters$outputData

outputs <- getOutputs(inputs, outputLocation)

for (key in names(inputParameters)) {
  if (endsWith(inputParameters[[key]], ".dat") || endsWith(inputParameters[[key]], ".txt")) { 
    con <- file(inputParameters[[key]], "r")
    url_list <- list()
    while (length(line <- readLines(con, n = 1)) > 0) {
      if (is_url(line)) {
        url_list <- c(url_list, list(list(href = trimws(line))))
      }
    }
    close(con)
    inputParameters[[key]] <- url_list
  }
}

jsonData <- list(
  "inputs" = inputParameters,
  "outputs" = outputs
)

jobID <- executeProcess(inputs$server, inputs$process, jsonData, outputLocation)

retrieveResults(inputs$server, jobID, outputLocation)