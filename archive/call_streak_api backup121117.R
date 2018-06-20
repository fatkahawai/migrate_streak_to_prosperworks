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
getTasks <- function(boxKey){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/newsfeed")
    path <- paste0(endpoint,api)
#    print(path)

    boxTasks.df <- NULL
    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)
    
         # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

        newsfeed <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
        newsfeed.df<- as.data.frame(newsfeed)
        
        if(!is.null(newsfeed.df)){

#$$$ DEBUG
if(nrow(newsfeed.df) >45) { print(paste("getTasks:",as.integer(nrow(newsfeed.df)),"rows in a newsfeed for box",boxKey))}           
#$$$
#        write.table(newsfeed.df, 
#            file="~/Downloads/newsfeed.csv", 
#            sep = ",",
#            na="",
#            col.names = TRUE,
#            row.names = FALSE,
#            append = TRUE)

#$$$ DEBUG
if(nrow(newsfeed.df) ==0) { print("getTasks: no rows in newsfeed")}           
#$$$
          taskFeed<- newsfeed.df[newsfeed.df$newsfeedEntrySpecific == "ADD_TASK",]

#$$$ DEBUG
#          print(paste("ADD_TEXT events",as.character(nrow(taskFeed)),"rows"))
#$$$$
          specificVariables.df <- taskFeed$specificVariables
#        View(specificVariables.df)

          # filter in any entries with something in TASK_TEXT  
          tasks<- specificVariables.df[!is.null(specificVariables.df$DUE_DATE),]
          tasks<- tasks[!is.na(specificVariables.df$DUE_DATE),]

#$$$ DEBUG
#          print(paste("ADD_TASK with DUE_DATE",as.character(nrow(tasks)),"rows"))
#          print(tasks)
#$$$$
          # filter out any where due date is in past
          tasks$dueDate <- as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01")
          tasks <- tasks[tasks$dueDate > today(),]

          #allTasks<- allTasks.df[allTasks.df$due_date > today(),]

#$$$ DEBUG
#          print(paste("Tasks due > today",as.character(nrow(tasks)),"rows"))
#          print(tasks)
#$$$$
#        tasks<- specificVariables.df[!is.na(specificVariables.df$TASK_TEXT),]
#        tasks<- specificVariables.df[specificVariables.df$TASK_TEXT!= NULL,]

          if( nrow(tasks) > 0 ) {

#$$$ DEBUG
#            View(tasks)
#          print(tasks)
            print(paste("saving Tasks",as.character(nrow(tasks)),"rows"))
#            print(tasks$TASK_TEXT)
#$$$$
            tasks$BOX_NAME <- ifelse(is.na(tasks$BOX_NAME),"",tasks$BOX_NAME)
            tasks$STORY_AUTHOR_DISPLAY_NAME <- ifelse(is.na(tasks$STORY_AUTHOR_DISPLAY_NAME),"",tasks$STORY_AUTHOR_DISPLAY_NAME)
            tasks$TASK_TEXT <- ifelse(is.na(tasks$TASK_TEXT),"",tasks$TASK_TEXT)
            tasks$DUE_DATE <- ifelse(is.na(tasks$DUE_DATE),"",tasks$DUE_DATE)
            tasks$STATUS <- ifelse(is.na(tasks$STATUS),"",tasks$STATUS)

         
            boxTasks.df<- data.frame(
                boxKey,
                str_trim(tasks$BOX_NAME),
                sapply(strsplit(tasks$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
                tasks$TASK_TEXT,
                tasks$dueDate,
#                as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01"),
                tasks$STATUS)

            colnames(boxTasks.df) <- c("box_key","box_name","owner","task_text","due_date","status")
            
            # change all box names for this box key to the latest name given
            boxTasks.df$box_name<- boxTasks.df$box_name[1]

            # filter out tasks without a due date 
            boxTasks.df<- boxTasks.df[!is.na(boxTasks.df$due_date),]

#            View(tasks.df)

#            readline(prompt="task done. next...")
          }
        }
    } else{
        if(  raw.result$status_code == 400 ) {
           print("status code 400: Bad Request")
        }else{
           print(paste("status code", raw.result$status_code ))
        }
    }

    stopifnot(raw.result$status_code == 200 )

    return(boxTasks.df)
}

# ----------------------------------------------------------------------------
# FUNCTION
# ----------------------------------------------------------------------------
getMeetingNotes <- function(boxKey){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/newsfeed")
    path <- paste0(endpoint,api)
#    print(path)

    boxMeetings.df <- NULL

    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(APIkey,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)
    
         # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

        newsfeed <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
        newsfeed.df<- as.data.frame(newsfeed)
        
#        View(newsfeed.df)

        if(!is.null(newsfeed.df)){

          meetingFeed<- newsfeed.df[newsfeed.df$newsfeedEntrySpecific == "ADD_MEETING",]
# $$$ DEBUG
#        View(meetingFeed)
#
#         print(paste("ADD_MEETING entries",as.character(nrow(meetingFeed)),"rows"))
# $$$ DEBUG

          if(nrow(meetingFeed) > 0) {
            specificVariables.df <- meetingFeed$specificVariables
#        View(specificVariables.df)

#        print("MEETING_NOTES")

    # MEETING_TYPE # "CALL_LOG"  
    # MEETING_START_TIMESTAMP # "1510081200000"
    # MEETING_NOTES # "Blah"
    # STORY_AUTHOR_DISPLAY_NAME # "Bob D"

            meetingNotes<- specificVariables.df[!is.na(specificVariables.df$MEETING_TYPE),]

# $$$ DEBUG
#        View(meetingNotes)
#            print(paste("MEETING_TYPE",as.character(nrow(meetingNotes)),"rows"))
# $$$ DEBUG

            if(nrow(meetingNotes) > 0) {
           
# $$$ DEBUG
              print(paste("MEETING_TYPE",meetingNotes$MEETING_TYPE,"Box",meetingNotes$BOX_NAME))
# $$$ DEBUG
              meetingNotes$BOX_NAME <- ifelse(is.na(meetingNotes$BOX_NAME),"",meetingNotes$BOX_NAME)
              meetingNotes$STORY_AUTHOR_DISPLAY_NAME <- ifelse(is.na(meetingNotes$STORY_AUTHOR_DISPLAY_NAME),"",meetingNotes$STORY_AUTHOR_DISPLAY_NAME)
              meetingNotes$MEETING_TYPE <- ifelse(is.na(meetingNotes$MEETING_TYPE),"",meetingNotes$MEETING_TYPE)
              meetingNotes$MEETING_START_TIMESTAMP <- ifelse(is.na(meetingNotes$MEETING_START_TIMESTAMP),"",meetingNotes$MEETING_START_TIMESTAMP)
              meetingNotes$MEETING_NOTES <- ifelse(is.na(meetingNotes$MEETING_NOTES),"",meetingNotes$MEETING_NOTES)

              boxMeetings.df<- data.frame(
                  boxKey,
                  str_trim(meetingNotes$BOX_NAME),
                  sapply(strsplit(meetingNotes$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
                  meetingNotes$MEETING_TYPE,
                  as.POSIXlt(as.integer(
                           substr(meetingNotes$MEETING_START_TIMESTAMP,1,10)), 
                           origin = "1970-01-01"),
                  meetingNotes$MEETING_NOTES)

              colnames(boxMeetings.df) <- c("box_key","box_name","owner","meeting_type","meeting_timestamp","meeting_notes")

              # change all box names for this box key to the latest name given
              boxMeetings.df$box_name<- boxMeetings.df$box_name[1]

            }
          }
      }
# $$$ DEBUG

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

    return(boxMeetings.df)
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
          if(!is.null(threads.df)) {

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
                contacts.df$box_key <- boxKey

                colnames(contacts.df)<- c("name","email","box_name","box_key")

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

    write.table(NULL, 
            file="~/Downloads/newsfeed.csv", 
            sep = ",",
            na="",
            col.names = FALSE,
            row.names = FALSE,
            append = FALSE)


# $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#
#	the primary FOR loop here over the boxes.df to dump activities from all boxes
#

	# boxName <- "PWC Global"
	# boxKey <- boxes$key[boxes$name == boxName]
    allTasks.df <- NULL
    allMeetings.df <- NULL
    allContacts.df <- NULL

    appendToFile <- FALSE


    for(i in 1:nrow(boxes.df)) {

        cat(i)
# $$$ DEBUG
#        row <- boxes.df[i,]
#        # do stuff with row
#	   print( paste("Box Name", box$name, " Key:", box$key))
#
#        print(paste("getting TASKS & MEETING_NOTES from newsfeed for", boxes.df[i,]$name))
# $$$

#        boxTasks.df <- NULL
#        boxMeetings.df <- NULL
#        boxContacts.df <- NULL

        boxTasks.df <- getTasks(boxes.df[i,]$key)
        boxMeetings.df <- getMeetingNotes(boxes.df[i,]$key)


        allTasks.df <- rbind(allTasks.df, boxTasks.df)
        allMeetings.df <- rbind(allMeetings.df, boxMeetings.df)

        write.table(boxTasks.df, 
            file="~/Downloads/streak_tasks.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = FALSE,
            append = appendToFile)

        write.table(boxMeetings.df, 
            file="~/Downloads/streak_meeting_notes.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = FALSE,
            append = appendToFile)


# $$$ DEBUG

#       print("getting CONTACTS from threads")
#       readline(prompt=paste("got tasks and meetings for [",boxes.df[i,]$name,"] get contacts?..."))

# $$$$$

        boxContacts.df <- getContacts(boxes.df[i,]$key, boxes.df[i,]$name)
        allContacts.df <- rbind(allContacts.df, boxContacts.df)

        write.table(boxContacts.df, 
            file="~/Downloads/streak_thread_contacts.csv", 
            sep = ",",
            na="",
            col.names = !appendToFile,
            row.names = FALSE,
            append = appendToFile)

        appendToFile <- TRUE

# $$$ DEBUG

#        readline(prompt=paste("done [",boxes.df[i,]$name,"] next?..."))
    
#        stopifnot( i < 500)
# $$$$$

    }
    View(allTasks.df)
    View(allMeetings.df)
    View(allContacts.df)

    print("Done")

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



