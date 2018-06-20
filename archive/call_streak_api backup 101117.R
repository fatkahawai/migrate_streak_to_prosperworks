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
library(stringr)

options(stringsAsFactors = FALSE)

APIkey <- "78a3e9ca7b3246d68a7a28ea03502483"
url  <- "https://www.streak.com"
endpoint <-  "/api/v1"

# ----------------------------------------------------------------------------
# FUNCTION: extract the countth word from a sentence 
# ----------------------------------------------------------------------------
extractWord <- function(sentence, count){
    return (sapply(strsplit(sentence, "\\s+"), `[`, count))
}
# ----------------------------------------------------------------------------
# FUNCTION: get a user's name from a user key
# ----------------------------------------------------------------------------
getUserName <- function(userKey) {
    endpoint <-  "/api/v1"
    api <- "/users/"
    path <- paste0(endpoint,api,userKey)
#    print(path)

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
    return(userName)
}

# ----------------------------------------------------------------------------
# FUNCTION
# ----------------------------------------------------------------------------
getTasksAndMeetingNotes <- function(boxKey){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/newsfeed")
    path <- paste0(endpoint,api)
#    print(path)

    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)
    
         # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

        newsfeed <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
        newsfeed.df<- as.data.frame(newsfeed)
        
        View(newsfeed.df)

        specificVariables.df<- newsfeed.df$specificVariables

#        print ("TASKS")

    # TASK_TEXT
    # DUE_DATE # "1510081200000"
    # STATUS # "Done"
    # STORY_AUTHOR_DISPLAY_NAME # "Bob D"

#        tasks<- specificVariables.df[!is.na(specificVariables.df$TASK_TEXT),]
        tasks<- specificVariables.df[specificVariables.df$TASK_TEXT!= NULL,]

        if(nrow(tasks) > 0 ){

            tasks$BOX_NAME <- ifelse(is.na(tasks$BOX_NAME),"",tasks$BOX_NAME)
            tasks$STORY_AUTHOR_DISPLAY_NAME <- ifelse(is.na(tasks$STORY_AUTHOR_DISPLAY_NAME),"",tasks$STORY_AUTHOR_DISPLAY_NAME)
            tasks$TASK_TEXT <- ifelse(is.na(tasks$TASK_TEXT),"",tasks$TASK_TEXT)
            tasks$DUE_DATE <- ifelse(is.na(tasks$DUE_DATE),"",tasks$DUE_DATE)
            tasks$STATUS <- ifelse(is.na(tasks$STATUS),"",tasks$STATUS)

         
            boxTasks.df<- data.frame(
                str_trim(tasks$BOX_NAME),
                sapply(strsplit(tasks$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
                tasks$TASK_TEXT,
                as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01"),
                tasks$STATUS)

                colnames(boxTasks.df) <- c("box_name","owner","task_text","due_date","status")
            
            # change all box names for this box key to the latest name given
            boxTasks.df$box_name<- boxTasks.df$box_name[1]

            # filter out tasks without a due date 
            boxTasks.df<- boxTasks.df[!is.na(boxTasks.df$due_date),]

#            View(tasks.df)

#            readline(prompt="task done. next...")
        }

#        print("MEETING_NOTES")

    # MEETING_TYPE # "CALL_LOG"  
    # MEETING_START_TIMESTAMP # "1510081200000"
    # MEETING_NOTES # "Blah"
    # STORY_AUTHOR_DISPLAY_NAME # "Bob D"

        meetingNotes<- specificVariables.df[specificVariables.df$MEETING_TYPE!=NULL,]

        if(nrow(meetingNotes) > 0) {
           
            meetingNotes$BOX_NAME <- ifelse(is.na(meetingNotes$BOX_NAME),"",meetingNotes$BOX_NAME)
            meetingNotes$STORY_AUTHOR_DISPLAY_NAME <- ifelse(is.na(meetingNotes$STORY_AUTHOR_DISPLAY_NAME),"",meetingNotes$STORY_AUTHOR_DISPLAY_NAME)
            meetingNotes$MEETING_TYPE <- ifelse(is.na(meetingNotes$MEETING_TYPE),"",meetingNotes$MEETING_TYPE)
            meetingNotes$MEETING_START_TIMESTAMP <- ifelse(is.na(meetingNotes$MEETING_START_TIMESTAMP),"",meetingNotes$MEETING_START_TIMESTAMP)
            meetingNotes$MEETING_NOTES <- ifelse(is.na(meetingNotes$MEETING_NOTES),"",meetingNotes$MEETING_NOTES)

            boxMeetings.df<- data.frame(
                  str_trim(meetingNotes$BOX_NAME),
                  sapply(strsplit(meetingNotes$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
                  meetingNotes$MEETING_TYPE,
                  as.POSIXlt(as.integer(
                           substr(meetingNotes$MEETING_START_TIMESTAMP,1,10)), 
                           origin = "1970-01-01"),
                  meetingNotes$MEETING_NOTES)

            colnames(boxMeetings.df) <- c("box_name","owner","meeting_type","meeting_timestamp","meeting_notes")

            # change all box names for this box key to the latest name given
            boxMeetings.df$box_name<- boxMeetings.df$box_name[1]

        }

# $$$ TODO

#        readline(prompt="meetings done for a box. next...")

# $$$$$

        # write.csv(meetingNotes.df, append=TRUE)

    } else{
        if(  raw.result$status_code == 400 ) {
           print("status code 400: Bad Request")
        }else{
           print(paste("status code", raw.result$status_code ))
        }
    }

    stopifnot(raw.result$status_code == 200 )
}

# ----------------------------------------------------------------------------
# FUNCTION
# ----------------------------------------------------------------------------
getContacts <- function(boxKey, boxName){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/threads")
    path <- paste0(endpoint,api)
#    print(path)

    contacts.df <- NULL

    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)

         # So the result is a single character string that contains a JSON file. 
         # Let’s tell R to parse it into something R can work with.
        threads.df <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
        #threads.df<- as.data.frame(threads)

#    print(paste("Threads:",threads.df))

#    print(paste("nrow Threads:",nrow(threads.df)))

#        if(threads.df != NULL) {
          if(nrow(threads.df) > 0) {

            emails <- NULL
            names <- NULL

#    print(paste("length emailAddresses:",length(threads.df$emailAddresses)))

            if(length(threads.df$emailAddresses) >0){

#    print(paste("emailAddresses:",threads.df$emailAddresses))
#    print(paste("class(emailAddresses):",threads.df$emailAddresses))

                emails <- unlist(threads.df$emailAddresses)

#    print(paste("emails:",emails))

                names<- unlist(threads.df$names)
                contacts.df<- data.frame(names,emails)
                contacts.df$box_name <- boxName

                colnames(contacts.df)<- c("name","email","box_name")

                # filter out kamihq.com and wufoo emails and duplicates
                contacts.df<- contacts.df[contacts.df$name != "Kami",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@wufoo.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-10,nchar(contacts.df$email)) != "@kamihq.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-14,nchar(contacts.df$email)) != "@notablepdf.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-10,nchar(contacts.df$email)) != "@notable.ac",]
                contacts.df<- contacts.df[substr(contacts.df$email,1,14) != "mailer-daemon@",]
                contacts.df <- contacts.df[!duplicated(contacts.df$email),]

            }

# $$$


# $$$
          }
#        }

    } else{
        if(  raw.result$status_code == 400 ) {
           print("status code 400: Bad Request")
        }else{
           print(paste("status code", raw.result$status_code ))
        }
    }
    stopifnot(raw.result$status_code == 200 )

    return(contacts.df)
}    

# ----------------------------------------------------------------------------
#
# MAIN
#
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

    stopifnot(raw.result$status_code == 200 )
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

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#	the primary FOR loop here over the boxes.df to dump activities from all boxes
#

	# boxName <- "PWC Global"
	# boxKey <- boxes$key[boxes$name == boxName]
    allTasks.df <- NULL
    allMeetings.df <- NULL
    allContacts.df <- NULL

    boxTasks.df <- NULL
    boxMeetings.df <- NULL
    boxContacts.df <- NULL


    appendToFile <- FALSE

    for(i in 1:nrow(boxes.df)) {
#        row <- boxes.df[i,]
        # do stuff with row
#    for( box in boxes.df ) {
#	   print( paste("Box Name", box$name, " Key:", box$key))

#        print(paste("getting TASKS & MEETING_NOTES from newsfeed for", boxes.df[i,]$name))

        boxTasks.df <- NULL
        boxMeetings.df <- NULL
        boxContacts.df <- NULL

        getTasksAndMeetingNotes(boxes.df[i,]$key)
        allTasks.df <- rbind(allTasks.df, boxTasks.df)
        allMeetings.df <- rbind(allMeetings.df, boxMeetings.df)

        write.table(boxTasks.df, 
            file="~/Downloads/streak_tasks.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = TRUE,
            appendToFile)

        write.table(boxMeetings.df, 
            file="~/Downloads/streak_meeting_notes.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = TRUE,
            appendToFile)


#        print("getting CONTACTS from threads")
# $$$ TODO

#       readline(prompt=paste("got tasks and meetings for [",boxes.df[i,]$name,"] get contacts?..."))

# $$$$$

        boxContacts.df <- getContacts(boxes.df[i,]$key, boxes.df[i,]$name)
        allContacts.df <- rbind(allContacts.df, boxContacts.df)

        write.table(boxContacts.df, 
            file="~/Downloads/streak_thread_contacts.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = TRUE,
            appendToFile)

        appendToFile <- TRUE

# $$$ TODO

        readline(prompt=paste("done [",boxes.df[i,]$name,"] next?..."))

# $$$$$

    }
    View(allTasks.df)
    View(allMeetings.df)
    View(allContacts.df)

    print("Done")

# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

} else{
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }

    stopifnot(raw.result$status_code == 200 )    
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#nchar(this.raw.content)
# Let’s look at the first 100 characters:
#class(this.content.df) 
#dim(this.content.df)   #with  rows and  variables



