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
# extract the countth word from a sentence 
# ----------------------------------------------------------------------------
extractWord <- function(sentence, count){
    return (sapply(strsplit(sentence, "\\s+"), `[`, count))
}
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
getUserName <- function(userKey) {
    endpoint <-  "/api/v1"
    api <- "/users/"
    path <- paste0(endpoint,api,userKey)
    print(path)

# Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)
        users <- fromJSON(this.raw.content)
        userName <- users$displayName

    	View(users.df) # returns "bob@kamihq.com"

    } else{
        if(  raw.result$status_code == 400 ) {
    	   print("getUserName: status code 400: Bad Request")
        }else{
        	print(paste("status code", raw.result$status_code ))
        }
    }   
}

# ----------------------------------------------------------------------------
endpoint <-  "/api/v1"
api <- "/pipelines"
path <- paste0(endpoint,api)
print(path)

pipelineSales <- "Master Sales Pipeline"

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	pipelines <- fromJSON(this.raw.content)

	pipelineKey <- pipelines$key[pipelines$name==pipelineSales]
	print(paste(pipelineSales," key:",pipelineKey))

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
    
    # write full box dump to JSON file
    write_json(boxes, path="~/Downloads/streak_all_boxes_raw_api.json")

     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	boxes <- fromJSON(this.raw.content)

	boxes.df<- data.frame(boxes$key, boxes$name)

    assignedTo.list<- sapply(boxes$assignedToSharingEntries, `[[`, "displayName")
#    assignedTo.list<- sapply(ifelse(length(boxes$assignedToSharingEntries)==0,"",boxes$assignedToSharingEntries, `[[`, "displayName"))
    assignedTo.vec<- as.character(assignedTo.list)
    assignedTo.df<- as.data.frame(assignedTo.vec)
    colnames(assignedTo.df)<- "assigned_to"

    boxes.df$assignedTo <- extractWord(assignedTo.df$assigned_to,1)

    colnames(boxes.df)<- c("key","name","assigned_to")

# TODO $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#	put a for loop here over the boxes.df to dump activities from all boxes
#
# TODO $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

	boxName <- "PWC Global"
	boxKey <- boxes$key[boxes$name == boxName]
	print( paste("Box Name", boxName, " Key:", boxKey))

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
}

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

	specificVariables.df<- newsfeed.df$specificVariables

	#timestamp<- newsfeed.df$specificVariables$MEETING_START_TIMESTAMP
    #meetingTime<- as.POSIXlt(timestamp/1000, origin = "1970-01-01")  

    # PIPELINE_NAME # "Master Sales Pipeline"
    # boxName <- str_trim(specificVariables.df$BOX_NAME) # "PWC GLobal"

    print ("TASKS")

    # TASK_TEXT
    # DUE_DATE # "1510081200000"
    # STATUS # "Done"
    # STORY_AUTHOR_DISPLAY_NAME # "Bob D"

	tasks<- specificVariables.df[!is.na(specificVariables.df$TASK_TEXT),]
	tasks.df<- data.frame(
		str_trim(tasks$BOX_NAME),
		sapply(strsplit(tasks$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
		tasks$TASK_TEXT,
		as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01"),
		tasks$STATUS)
	colnames(tasks.df) <- c("box_name","owner","task_text","due_date","status")

    # change all box names for this box key to the latest name given
    tasks.df$box_name<- tasks.df$box_name[1]

    # filter out tasks without a due date 
    tasks.df<- tasks.df[!is.na(tasks.df$due_date),]

	View(tasks.df)

	print("MEETING_NOTES")

	# MEETING_TYPE # "CALL_LOG"  
	# MEETING_START_TIMESTAMP # "1510081200000"
	# MEETING_NOTES # "Blah"
    # STORY_AUTHOR_DISPLAY_NAME # "Bob D"

	meetingNotes<- specificVariables.df[!is.na(specificVariables.df$MEETING_TYPE),]
	meetingNotes.df<- data.frame(
		str_trim(meetingNotes$BOX_NAME),
		sapply(strsplit(meetingNotes$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
		meetingNotes$MEETING_TYPE,
		  as.POSIXlt(as.integer(
				       substr(meetingNotes$MEETING_START_TIMESTAMP,1,10)), 
					   origin = "1970-01-01"),
		  meetingNotes$MEETING_NOTES)
	colnames(meetingNotes.df) <- c("box_name","owner","meeting_type","meeting_timestamp","meeting_notes")

    # change all box names for this box key to the latest name given
    meetingNotes.df$box_name<- meetingNotes.df$box_name[1]

	View(meetingNotes.df)

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
    
     # So the result is a single character string that contains a JSON file. 
     # Let’s tell R to parse it into something R can work with.
	threads.df <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
	#threads.df<- as.data.frame(threads)

	emails <- unlist(threads.df$emailAddresses)
	names<- unlist(threads.df$names)

	contacts.df<- data.frame(names,emails)
	contacts.df$box_name <- boxName

	colnames(contacts.df)<- c("name","email","box_name")

	# filter out kamihq.com and wufoo emails and duplicates
	contacts.df<- contacts.df[contacts.df$name != "Kami",]
	contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-10,nchar(contacts.df$email)) != "@kamihq.com",]
    contacts.df <- contacts.df[!duplicated(contacts.df$email),]


	View(contacts.df)

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



