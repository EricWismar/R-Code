library(httr)
library(XML)

sharePointUrl <- 'https://cnet.centene.com/sites/HealthcareAnalytics/'
folderUrl <- '/sites/HealthcareAnalytics/Clinical%20Analytics/COVID-19/Data%20and%20Reporting/LOB%20Data'

r <- POST(
  paste0(sharePointUrl, "/_api/contextinfo"),
  authenticate(Sys.getenv("EDW_UID"), Sys.getenv("EDW_PWD"), "ntlm"),
  content_type_json(),
  accept_json() # This is getting ignored and I don't know why.
)

formDigestValue <- xmlToList(xmlParse(content(r, "parsed")))$FormDigestValue

file_name <- "teapot.txt"

# Second request
request_url <- paste0(
  sharePointUrl,
  "/_api/web/GetFolderByServerRelativeUrl('",
  folderUrl,
  "')/Files/add(url='",
  file_name,
  "',overwrite='true')"
)

upload_r <- POST(
  request_url,
  authenticate(Sys.getenv("EDW_UID"), Sys.getenv("EDW_PWD"), "ntlm"),
  content_type_json(),
  accept_json(),
  add_headers(`X-RequestDigest` = formDigestValue,
              `binaryStringRequestBody` = "true"),
  body = upload_file("teapot.txt")
)