##############################################################################
# Migrate Data from Streak to ProsperWorks
# 
# EXPORT TASKS - Extract via Streak API
#
# VERSION: 1.0
# AUTHOR: Bob
# (C)Kami 2017
##############################################################################


library(httr)
library(jsonlite)
library(lubridate)

options(stringsAsFactors = FALSE)

APIkey <- "78a3e9ca7b3246d68a7a28ea03502483"
url  <- "https://www.streak.com"
endpoint <-  "/api/v1"
# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- "/users/me"
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    this.content <- fromJSON(this.raw.content)
	print(this.content$email) # returns "bob@kamihq.com"
} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}


# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- "/pipelines"
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	pipelines.df <- fromJSON(this.raw.content)
	
	pipelineKey <- pipelines.df$key[pipelines.df$name=="Master Sales Pipeline"]
	print(paste("Master Sales Pipeline key:",pipelineKey))

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}

# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- paste0("/pipelines/",pipelineKey,"/boxes")
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	boxes.df <- fromJSON(this.raw.content)
	
	boxKey <- boxes.df$key[boxes.df$name=="PWC Global"]
	print( paste("PWC Global - boxKey:",boxKey))

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}
# ----------------------------------------------------------------------------
endpoint <-  "/api/v2"
api <- paste0("/boxes/",boxKey,"/tasks")
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	tasks <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
	tasks.df<- as.data.frame(tasks)

	print(ifelse(tasks.df$results.status != "DONE", 
			paste(tasks.df$results.text,
				tasks.df$results.dueDate,
				tasks.df$creatorSharingEntry$fullName),
			""))

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- paste0("/boxes/",boxKey,"/newsfeed")
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	newsfeed <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
	newsfeed.df<- as.data.frame(newsfeed)

	#timestamp<- newsfeed.df$specificVariables$MEETING_START_TIMESTAMP
    #meetingTime<- as.POSIXlt(timestamp/1000, origin = "1970-01-01")  

	print("MEETING_NOTES")
	print(
		paste(
			as.POSIXlt(as.integer(
				       substr(newsfeed.df$specificVariables$MEETING_START_TIMESTAMP[!is.na(newsfeed.df$specificVariables$MEETING_NOTES)],1,10)), origin = "1970-01-01") ,
			":",
			newsfeed.df$specificVariables$MEETING_NOTES[!is.na(newsfeed.df$specificVariables$MEETING_NOTES)]
		)
	)

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}
# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- paste0("/boxes/",boxKey,"/threads")
path <- paste0(endpoint,api)
print(path)

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	threads <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
	threads.df<- as.data.frame(threads)

	emails <- unlist(threads.df$emailAddresses)
	outside_emails <- emails[substr(emails,nchar(emails)-10,nchar(emails)) != "@kamihq.com"]
	print(outside_emails)
	names<- unlist(threads.df$names)
	outside_names<- names[substr(emails,nchar(emails)-10,nchar(emails)) != "@kamihq.com"]
	print(outside_names)

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#nchar(this.raw.content)
# Let’s look at the first 100 characters:
#class(this.content.df) 
#dim(this.content.df)   #with  rows and  variables



