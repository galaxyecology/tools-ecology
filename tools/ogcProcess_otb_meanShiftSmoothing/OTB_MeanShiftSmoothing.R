#Run with Rscript ./OTB_MeanShiftSmoothing.R
#--file otb_band_math_test_input.txt
#--fOut float --fOutpos float --processingMemory 1024 --spatialR 5 --rangeR 15
#--thresHold 0.1 --maxIter 100 --rangeRamp 0 --modeSearch False
#--outputType png
#--outputFormat download --outputData test1.png

library("httr2")
library("jsonlite")
library("getopt")

args <- commandArgs(trailingOnly = TRUE)
option_specification <- matrix(c(
  "file", "i1", 1, "character",
  "fOut", "i2", 1, "character",
  "fOutpos", "i3", 1, "character",
  "processingMemory", "i4", 1, "integer",
  "spatialR", "i5", 2, "integer",
  "rangeR", "i6", 2, "double",
  "thresHold", "i7", 2, "double",
  "maxIter", "i8", 2, "integer",
  "rangeRamp", "i9", 2, "double",
  "modeSearch", "i10", 1, "character",
  "outputType", "i11", 1, "character",
  "outputFormat", "i12", 1, "character",
  "outputData", "o", 1, "character"
), byrow = TRUE, ncol = 4)
options <- getopt(option_specification)

file <- options$file
fout <- options$fOut
foutpos <- options$fOutpos
processingMemory <- options$processingMemory
spatialr <- options$spatialR
ranger <- options$rangeR
threshold <- options$thresHold
maxiter <- options$maxIter
rangeramp <- options$rangeRamp
modesearch <- options$modeSearch
outputType <- paste0("image/", options$outputType)
outputFormat <- options$outputFormat
outputData <- options$outputData

cat("\n file: ", file)
cat("\n fout: ", fout)
cat("\n foutpos: ", foutpos)
cat("\n processingMemory: ", processingMemory)
cat("\n spatialr: ", spatialr)
cat("\n ranger: ", ranger)
cat("\n threshold: ", threshold)
cat("\n maxiter: ", maxiter)
cat("\n rangeramp: ", rangeramp)
cat("\n modesearch: ", modesearch)
cat("\n outputType: ", outputType)
cat("\n outputFormat: ", outputFormat)

baseUrl <- "https://ospd.geolabs.fr:8300/ogc-api/"
execute <- "processes/OTB.MeanShiftSmoothing/execution"
getStatus <- "jobs/"
getResult <- "/results"

file_urls <- readLines(file, warn = FALSE)

il_list <- lapply(file_urls, function(url) {
  list("href" = url)
})

json_data <- list(
  "inputs" = list(
    "in" = il_list,
    "fout" = fout,
    "foutpos" = foutpos,
    "ram" = processingMemory,
    "spatialr" = spatialr,
    "ranger" = ranger,
    "thres" = threshold,
    "maxiter" = maxiter,
    "rangeramp" = rangeramp,
    "modesearch" = modesearch
  ),
  "outputs" = list(
    "fout" = list(
      "format" = list(
        "mediaType" = outputType
      ),
      "transmissionMode" = "reference"
    ),
    "foutpos" = list(
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
      "accept" = "/*",
      "Prefer" = "respond-async;return=representation",
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(json_data) %>%
    req_perform()
  response <- makeResponseBodyReadable(resp1$body)
  status_code1 <- resp1$status_code
  if (status_code1 == 201) {
    status <- "running"
    attempt <- 1
    while (status == "running") {
      #Request 2
      resp2 <- request(paste0(baseUrl,getStatus,response$jobID)) %>%
        req_headers(
          "accept" = "application/json"
        ) %>%
        req_perform()
      status_code2 <- resp2$status_code
      if (status_code2 == 200) {
        response2 <- makeResponseBodyReadable(resp2$body)
        cat("\n", response2$status )
        if (response2$status=="successful") {
          status <- "successful"
          #Request 3
          resp3 <- request(paste0(baseUrl,getStatus, response2$jobID, getResult)) %>%
            req_headers(
              "accept" = "application/json"
            ) %>%
            req_perform()
          status_code3 <- resp3$status_code
          if (status_code3 == 200) {
            response3 <- makeResponseBodyReadable(resp3$body)
            if (outputFormat == "download") {
              options(timeout=600)
              download.file(response3$fout$href, destfile = paste0("output1.", options$outputType), mode = "wb")
              download.file(response3$foutpos$href, destfile = paste0("output2.", options$outputType), mode = "wb")
            } else if (outputFormat == "getUrl") {
              writeLines(paste(response3$fout$href, response3$foutpos$href, sep = "\n"), con = "output.txt")
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
        }
        #else {
        # attempt <- attempt +1
        # if (attempt == 200) {
        #   status <- "failed"
        # }
        #}
      } else {
        status <- "failed"
        print(paste("HTTP", status_code2, "Error:", resp2$status_message))
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
})