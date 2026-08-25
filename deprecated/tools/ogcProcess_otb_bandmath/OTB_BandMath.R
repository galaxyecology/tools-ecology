#Run with Rscript ./OTB_BandMath.R --file otb_band_math_test_input.txt --processingMemory 256 --mathExpression im1b3,im1b2,im1b1 --outputType png --outputFormat download --outputImage float --outputData otb_band_math_test_output.png

library("httr2")
library("jsonlite")
library("getopt")

args <- commandArgs(trailingOnly = TRUE)
option_specification <- matrix(c(
  'file', 'i1', 1, 'character',
  'processingMemory', 'i2', 2, 'integer',
  'mathExpression', 'i3', 2, 'character',
  'outputType', 'i4', 2, 'character',
  'outputFormat', 'i5', 1, 'character',
  'outputImage', 'i6', 1, 'character',
  'outputData', 'o', 1, 'character'
), byrow = TRUE, ncol = 4)
options <- getopt(option_specification)

file <- options$file
processingMemory <- options$processingMemory
mathExpression <-options$mathExpression
outputType <- paste0("image/", options$outputType)
outputFormat <- options$outputFormat
outputImage <- options$outputImage
outputData <- options$outputData

cat("\n file: ", file)
cat("\n ram: ", processingMemory)
cat("\n exp: ", mathExpression)
cat("\n outputType: ", outputType)
cat("\n outputFormat: ", outputFormat)
cat("\n outputImage: ", outputImage)

baseUrl <- "https://ospd.geolabs.fr:8300/ogc-api/"
execute <- "processes/OTB.BandMath/execution"
getStatus <- "jobs/"
getResult <- "/results"

file_urls <- readLines(file, warn = FALSE)

il_list <- lapply(file_urls, function(url) {
  list("href" = url)
})

json_data <- list(
  "inputs" = list(
    "il" = il_list,
    "out" = outputImage,
    "exp" = mathExpression,
    "ram" = processingMemory
  ),
  "outputs" = list(
    "out" = list(
      "format" = list(
        "mediaType" = outputType
      ),
      "transmissionMode" = "reference"
    )
  )
)

makeResponseBodyReadable <- function(body) {
  hex <- c(body)
  int_values <- as.integer(hex)
  raw_vector <- as.raw(int_values)
  readable_output <- rawToChar(raw_vector)
  json_object <- jsonlite::fromJSON(readable_output)
  return(json_object)
}

tryCatch({
  #Request 1
  resp1 <- request(paste0(baseUrl, execute)) %>%
    req_headers(
      'accept' = '/*',
      'Prefer' = 'respond-async;return=representation',
      'Content-Type' = 'application/json'
    ) %>%
    req_body_json(json_data) %>%
    req_perform()

  response <- makeResponseBodyReadable(resp1$body)
  status_code1 <- resp1$status_code

  if (status_code1 == 201) {
    status <- "running"
    attempt = 1
    while (status == "running") {
      #Request 2
      #cat("\n",response$jobID)
      resp2 <- request(paste0(baseUrl,getStatus,response$jobID)) %>%
        req_headers(
          'accept' = 'application/json'
        ) %>%
        req_perform()
      status_code2 <- resp2$status_code
      #cat("\n", status_code2)
      if (status_code2 == 200) {
        response2 <- makeResponseBodyReadable(resp2$body)
        cat("\n", response2$status)
        if (response2$status=="successful") {
          status <- "successful"
          #Request 3
          resp3 <- request(paste0(baseUrl,getStatus, response2$jobID, getResult)) %>%
            req_headers(
              'accept' = 'application/json'
            ) %>%
            req_perform()
          status_code3 <- resp3$status_code
          if (status_code3 == 200) {
            response3 <- makeResponseBodyReadable(resp3$body)
            if (outputFormat == "download") {
              options(timeout=300)
              download.file(response3$out$href, destfile = outputData, mode = "wb")              
            } else if (outputFormat == "getUrl") {
              writeLines(response3$out$href, con = outputData)
            }
          } else if (status_code3 == 404) {
            print("The requested URI was not found.")
          } else if (status_code3 == 500) {
            print("A server error occurred.")
          } else {
            print(paste("HTTP", status_code3, "Error:", resp3$status_message))
          }
        } else if (response2$status=="failed") {
          status <- "failed"
          message("An error occurred. For further details, check OGC Job status through https://ospd.geolabs.fr:8300/ogc-api/jobs/", response2$jobID)
          q(status = 1)
        }      
      } else {
        status <- "failed"
        print(paste("HTTP", status_code2, "Error:", resp2$status_message, "An error occurred. For further details, check OGC Job status through https://ospd.geolabs.fr:8300/ogc-api/jobs/", response2$jobID))
        q(status = 1)
      }
      Sys.sleep(3)
    }
    print(status)
  } else if (status_code1 == 400) {
    print("A query parameter has an invalid value.")
  } else if (status_code1 == 404) {
    print("The requested URI was not found.")
  } else if (status_code1 == 500) {
    print("The requested URI was not found.")
  } else {
    print(paste("HTTP", status_code1, "Error:", resp1$status_message))
  }
}, error = function(e) {
  message("An error occurred:", e)
  # Exit with code 1
  q(status = 1)
})