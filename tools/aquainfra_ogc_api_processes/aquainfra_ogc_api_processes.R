library("httr2")
library("jsonlite")
library("getopt")

cat("START GENERIC WRAPPER SERVICE \n")

remove_null_values <- function(x) {
  if (is.list(x)) {
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

executeProcess <- function(url, process, requestBodyData) {
  url <-
    paste(paste(paste(url, "processes/", sep = ""), process, sep = ""), "/execution", sep = "")
  requestBodyData$inputs$select_process <- NULL
  
  body <- list()
  body$inputs <- requestBodyData$inputs
  
  response <- request(url) %>%
    req_headers("Content-Type" = "application/json",
                "Prefer" = "respond-async") %>%
    req_body_json(body) %>%
    req_perform()
  
  cat("\n 3.1: Process executed")
  cat("\n 3.1: Status code: ", response$status_code)
  jobId <- parseResponseBody(response$body)$jobID
  cat("\n 3.1: Job ID: ", jobId, "\n")
  
  return(jobId)
}

checkJobStatus <- function(server, process, jobID) {
  url <- paste0(server, "jobs/", jobID)
  response <- request(url) %>%
    req_perform()
  jobStatus <- parseResponseBody(response$body)$status
  jobProgress <- parseResponseBody(response$body)$progress
  return(jobStatus)
}

getStatusCode <- function(url) {
  response <- request(url) %>%
    req_perform()
  status_code <- response$status_code
  return(status_code)
}

getResult <- function (server, process, jobID) {
  response <-
    request(paste0(server, "jobs/", jobID, "/results?f=json")) %>%
    req_perform()
  return(response)
}

findHref <- function(obj) {
  hrefs <- c()
  if (is.list(obj)) {
    for (name in names(obj)) {
      element <- obj[[name]]
      if (is.list(element)) {
        hrefs <- c(hrefs, findHref(element))
      } else if (name == "href") {
        hrefs <- c(hrefs, element)
      }
    }
  }
  return(hrefs)
}

retrieveResults <- function(server, process, jobID, outputData) {
  url <- paste0(server, "jobs/", jobID)
  cat(" 4.1: Job URL: ", url)
  status_code <- getStatusCode(url)
  cat("\n 4.2: Status code: ", status_code, "\n")
  
  if (status_code == 200) {
    status <- "running"
    
    while (status == "running") {
      jobStatus <- checkJobStatus(server, process, jobID)
      cat(" 4.3: Job status: ", jobStatus, "\n")
      
      if (jobStatus == "successful") {
        status <- jobStatus
        result <- getResult(server, process, jobID)
        
        if (result$status_code == 200) {
          resultBody <- parseResponseBody(result$body)
          cat("\n 4.4 Outputs: \n")
          print(resultBody)
          hrefs <- findHref(resultBody)
          
          if (length(hrefs) > 0) {
            urls_with_newline <- paste(hrefs, collapse = "\n")
            con <- file(outputData, "w")
            writeLines(urls_with_newline, con = con)
            close(con)
          } else {
            stop(paste0("Job failed. No hrefs found. See details at: ", server, "jobs/", jobID))
          }
        }
      } else if (jobStatus == "failed") {
        stop(paste0("Job failed. See details at: ", server, "jobs/", jobID))
      }
      Sys.sleep(3)
    }
    
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

saveResult <- function(href, outputData) {
  con <- file(outputData, "w")
    writeLines(href, con = con)
  close(con)
}

is_url <- function(x) {
  grepl("^https?://", x)
}

server <- "https://aquainfra.ogc.igb-berlin.de/pygeoapi/"

cat("\n1: START RETRIEVING PARAMETERS\n\n")
inputParameters <- getParameters()
print(inputParameters)
cat("1: END RETRIEVING PARAMETERS\n")

args <- commandArgs(trailingOnly = TRUE)
outputLocation <- args[2]

cat("\n2: START PARSING INPUTS\n\n")
convertedKeys <- c()

for (key in names(inputParameters)) {
  if (is.character(inputParameters[[key]]) &&
      (endsWith(inputParameters[[key]], ".dat") ||
       endsWith(inputParameters[[key]], ".txt"))) {
    con <- file(inputParameters[[key]], "r")
    url_list <- list()

    con <- file(inputParameters[[key]], "r")
    lines <- readLines(con)
    close(con)

    json_string <- paste(lines, collapse = ",")
    inputParameters[[key]] <- json_string

    convertedKeys <- append(convertedKeys, key)
  } else {
    if (!is.null(inputParameters[[key]])) {
      convertedKeys <- append(convertedKeys, key)
    }
  }
}
names(inputParameters) <- convertedKeys
print(inputParameters)
cat("2: END PARSING INPUTSs\n")

cat("\n3: START EXECUTING PROCESS\n")
jsonData <- list("inputs" = inputParameters)
jobId <- executeProcess(server, inputParameters$select_process, jsonData)
cat("\n3: END EXECUTING PROCESS\n")

cat("\n4: START RETRIEVING RESULTS\n\n")
retrieveResults(server, inputParameters$select_process, jobId, outputLocation)
cat("4: END RETRIEVING RESULTS\n")

cat("\n5: DONE.")