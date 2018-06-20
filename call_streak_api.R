##############################################################################
#' Migrate Data from Streak to ProsperWorks
#'
#' EXPORT TASKS - Extract via Streak API
#' 
#' @see [\url{http://streak.com}
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

# Libraries
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)
library(dplyr)         # group, filter and manipulate dataframes


options(stringsAsFactors = FALSE)

# INPUT
url      <- "https://www.streak.com"
endpoint <-  "/api/v1"


# OUTPUT
outputBoxesFileName <- "output/streak_boxes.csv"
logFile <- "log/call_streak_api.log"

#
# GLOBAL VARS
#
newsfeed.df <- as.data.frame(NULL)
threads.df <- as.data.frame(NULL)
stages <- NULL
pipelines <- NULL
fields <- NULL

cred     <- NULL

startBoxIndex <- 1

# ----------------------------------------------------------------------------
#' @function extractWord 
#' extract the countth word from a sentence 
#' @param sentence string
#' @param count int
#' @notes
#' @returns a string: the word extracted
# ----------------------------------------------------------------------------
extractWord <- function(sentence, count){
    return (sapply(strsplit(sentence, "\\s+"), `[`, count))
}
# ----------------------------------------------------------------------------
#' @function getUserName 
#' get a user's name from a user key
#' 
#' @param userKey key
#' @notes
#' @returns string: user name
# ----------------------------------------------------------------------------
getUserName <- function(userKey) {
    endpoint <-  "/api/v1"
    api <- "/users/"
    path <- paste0(endpoint,api,userKey)
#    print(path)

# Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

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
#' @function getNewsfeed 
#' get newsfeed via API
# ----------------------------------------------------------------------------
getNewsfeed <- function(boxKey){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/newsfeed")
    path <- paste0(endpoint,api)
#$$$ DEBUG
    print(paste("getNewsfeed:",boxKey))
#    print(path)
# $$$ DEBUG

    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

    if(raw.result$status_code == 200){

        this.raw.content <- rawToChar(raw.result$content)
    
         # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

        anewsfeed <- fromJSON(this.raw.content, simplifyDataFrame=TRUE)

        if(!is.null(anewsfeed)){

          anewsfeed.df<<- as.data.frame(anewsfeed)
#$$$ DEBUG
          print(paste("newsfeed.df contains",nrow(anewsfeed.df),"events"))
# $$$ DEBUG
        }
    } else{
        if(  raw.result$status_code == 400 ) {
           print("status code 400: Bad Request")
        }else{
           print(paste("status code", raw.result$status_code ))
        }
    }

    stopifnot(raw.result$status_code == 200 )

    return(anewsfeed.df)
}

# ----------------------------------------------------------------------------
#' @function getTasks
#' get tasks via API
# ----------------------------------------------------------------------------
getTasks <- function(newsfeed.df){
# $$$ DEBUG
    specs <- newsfeed.df$specificVariables
    print(paste("getTasks:",specs$BOX_NAME[1]))
#    print(path)
# $$$ DEBUG

    boxTasks.df <- NULL

    taskFeed<- newsfeed.df[newsfeed.df$newsfeedEntrySpecific == "ADD_TASK" | newsfeed.df$newsfeedEntrySpecific == "EDIT_TASK_DUE_DATE",]

    tasks<- data.frame(NULL)

    if( nrow(taskFeed) >0 ){
#$$$ DEBUG
            print(paste("Tasks:",as.character(nrow(taskFeed)),"ADD_TASK or EDIT_TASK_DUE_DATE events"))
#$$$$
            specificTaskVariables.df <- taskFeed$specificVariables

#$$$ DEBUG

            View(specificTaskVariables.df)
#$$$$

          # filter in any entries with something in TASK_TEXT  
            tasks<- specificTaskVariables.df[!is.null(specificTaskVariables.df$DUE_DATE),]
            tasks<- tasks[!is.na(tasks$DUE_DATE),]

#$$$ DEBUG
            print(paste("Tasks with a DUE_DATE",as.character(nrow(tasks)),"rows"))
#          print(tasks)
#$$$$
          # filter out any where due date is in past
            tasks$dueDate <- as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01")
            tasks <- tasks[tasks$dueDate > today(),]


#$$$ DEBUG
          print(paste("Tasks with future due date",as.character(nrow(tasks))))
          print(paste(tasks$BOX_NAME,tasks$TASK_TEXT,tasks$DUE_DATE))
#$$$$

            if( nrow(tasks) > 0 ) {

#$$$ DEBUG
#            View(tasks)
#          print(tasks)
#            print(paste("saving Tasks",as.character(nrow(tasks)),"rows"))
#            print(tasks$TASK_TEXT)
#$$$$
              tasks$BOX_NAME <- ifelse(is.na(tasks$BOX_NAME),"",tasks$BOX_NAME)
              tasks$STORY_AUTHOR_DISPLAY_NAME <- ifelse(is.na(tasks$STORY_AUTHOR_DISPLAY_NAME),"",tasks$STORY_AUTHOR_DISPLAY_NAME)
              tasks$TASK_TEXT <- ifelse(is.na(tasks$TASK_TEXT),"",tasks$TASK_TEXT)
              tasks$DUE_DATE <- ifelse(is.na(tasks$DUE_DATE),"",tasks$DUE_DATE)
              tasks$STATUS <- ifelse(is.null(tasks$STATUS),"",tasks$STATUS)
              tasks$STATUS <- ifelse(is.na(tasks$STATUS),"",tasks$STATUS)
              tasks$STATUS <- ifelse(tasks$STATUS=="","Open",tasks$STATUS)

# replace any semi-colons with commas bc we will use ; as our CSV separator
              tasks$TASK_TEXT <- sub(",",";",tasks$TASK_TEXT)
              tasks$TASK_TEXT <- sub("’","'",tasks$TASK_TEXT)

              # change all box names for this box key to the latest name given
              tasks$BOX_NAME<- tasks$BOX_NAME[1]
         
              boxTasks.df<- data.frame(
                  ####040418
                  #boxKey, 
                  newsfeed.df$creatorKey,
                  #####
                str_trim(tasks$BOX_NAME),
                sapply(strsplit(tasks$STORY_AUTHOR_DISPLAY_NAME, "\\s+"), `[`, 1), # change "Bob D" to "Bob"
                tasks$TASK_TEXT,
                tasks$dueDate,
#                as.POSIXlt(as.integer(substr(tasks$DUE_DATE,1,10)), origin = "1970-01-01"),
                tasks$STATUS)

              colnames(boxTasks.df) <- c("box_key","box_name","owner","task_text","due_date","status")
            
            # filter out tasks without a due date 
#            boxTasks.df<- boxTasks.df[!is.na(boxTasks.df$due_date),]
#$$$ DEBUG
#            View(tasks.df)
#            readline(prompt="task done. next...")
#$$$ DEBUG
            }
          }
    return(boxTasks.df)
}

# ----------------------------------------------------------------------------
#' @function getMeetingNotes 
#' get Meeting Notes via API
# ----------------------------------------------------------------------------
getMeetingNotes <- function(newsfeed.df){
# $$$ DEBUG
    specs <- newsfeed.df$specificVariables
    print(paste("getMeetingNotes: for",specs$BOX_NAME[1],"boxkey:",newsfeed.df$boxKey[1]))
#    print(path)
# $$$ DEBUG

    boxMeetings.df <- NULL

    if(!is.null(newsfeed.df)){

          meetingFeed<- newsfeed.df[newsfeed.df$newsfeedEntrySpecific == "ADD_MEETING",]
# $$$ DEBUG
#        View(meetingFeed)
#
         print(paste("ADD_MEETING entries",nrow(meetingFeed),"rows"))
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

# convert the HTML into plain text by stripping out the tags
              meetingNotes$MEETING_NOTES <- gsub("[\\<].*?[\\>]","",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- gsub("&nbsp;","",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- gsub("&gt;","",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- gsub("&lt;","",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- gsub("\\n"," ",meetingNotes$MEETING_NOTES)

              meetingNotes$MEETING_NOTES <- gsub("\"","\"\"",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- gsub("'","''",meetingNotes$MEETING_NOTES)

              meetingNotes$MEETING_NOTES <- sub(",",";",meetingNotes$MEETING_NOTES)
              meetingNotes$MEETING_NOTES <- sub("’","'",meetingNotes$MEETING_NOTES)

              boxMeetings.df<- data.frame(
                  ####040418
                  #boxKey, 
                  newsfeed.df$boxKey[1],
                  #####

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
    return(boxMeetings.df)
}

# ----------------------------------------------------------------------------
#' @function getContacts 
#' get Contacts via API
# ----------------------------------------------------------------------------
getContacts <- function(boxKey, boxName){
    endpoint <-  "/api/v1"
    api <- paste0("/boxes/",boxKey,"/threads")
    path <- paste0(endpoint,api)

# $$$ DEBUG
    print(paste("getContacts:",boxName))
#    print(path)
# $$$ DEBUG

    contacts.df <- NULL

    # Executing an API call with the GET flavor is done using the GET() function.

    raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

    if(raw.result$status_code == 200){
        this.raw.content <- rawToChar(raw.result$content)

         # So the result is a single character string that contains a JSON file. 
         # Let’s tell R to parse it into something R can work with.
        threads.df <<- fromJSON(this.raw.content, simplifyDataFrame=TRUE)
        #threads.df<- as.data.frame(threads)

#    print(paste("Threads:",threads.df))

#    print(paste("nrow Threads:",nrow(threads.df)))

#        if(threads.df != NULL) {
          if(!is.null(threads.df)) {

            emails <- NULL
            names <- NULL

#    print(paste("length emailAddresses:",length(threads.df$emailAddresses)))

            if(length(threads.df$emailAddresses[[1]]) >0){

#    print(paste("emailAddresses:",threads.df$emailAddresses))
#    print(paste("class(emailAddresses):",threads.df$emailAddresses))

                emails <- unlist(threads.df$emailAddresses)

#    print(paste("emails:",emails))

                names<- unlist(threads.df$names)
                contacts.df<- data.frame(names,emails)
                contacts.df$box_name <- boxName
                contacts.df$box_key <- boxKey

                colnames(contacts.df)<- c("name","email","box_name","box_key")

                # filter out common email providers domains and duplicates
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@outlook.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@hotmail.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@wufoo.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@edlio.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,1,14) != "mailer-daemon@",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-9,nchar(contacts.df$email)) != "@gmail.com",]
                contacts.df<- contacts.df[substr(contacts.df$email,nchar(contacts.df$email)-14,nchar(contacts.df$email)) != "@googlemail.com",]
                contacts.df <- contacts.df[!duplicated(contacts.df$email),]

            }
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
# ----------------------------------------------------------------------------
#
#' @function MAIN 
#
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# 'Tee' stdout console output to a logile. 
#
sink( file=logFile, append=FALSE, type= c("output","message"), split=TRUE)
print("call_streak_api.R: entry")

# ----------------------------------------------------------------------------
# GET Pipeline Key
# ----------------------------------------------------------------------------

endpoint <-  "/api/v1"
api <- "/pipelines"
path <- paste0(endpoint,api)
#$$$ DEBUG
# print(path)
#$$$ DEBUG

# load credentials from config.yml 
  cred <- config::get("streak") # e.g.
  print(paste("loaded Streak API key",cred$api_key))


pipelineSales <- "Master Sales Pipeline"

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	pipelines <<- fromJSON(this.raw.content)

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
# GET Stages
# ----------------------------------------------------------------------------

endpoint <-  "/api/v1"
api <- paste0("/pipelines/",pipelineKey,"/stages")
path <- paste0(endpoint,api)
#$$$ DEBUG
# print(path)
#$$$ DEBUG

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

  stages <<- fromJSON(this.raw.content)

  allStages.df <- NULL

  for( i in 1:length(stages)) {

    stages.df <- data.frame(stages[i])
    colnames(stages.df) <- c("name","key","fcolor","bcolor","count")

    allStages.df <- rbind(allStages.df,stages.df)

    print("Stages XRef")
    print(stages.df)
  }

  write.csv(allStages.df, file="output/streak_stages.csv") 

} else{
    if(  raw.result$status_code == 400 ) {
      print("status code 400: Bad Request")
    }else{
      print(paste("status code", raw.result$status_code ))
    }

    stopifnot(raw.result$status_code == 200 )
}

# ----------------------------------------------------------------------------
# GET Fields
# ----------------------------------------------------------------------------

endpoint <-  "/api/v1"
api <- paste0("/pipelines/",pipelineKey,"/fields")
path <- paste0(endpoint,api)
#$$$ DEBUG
# print(path)
#$$$ DEBUG

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

  fields <<- fromJSON(this.raw.content)

  allFields.df <- cbind(fields$name,fields$key)
  colnames(allFields.df) <- c("name","key")   

  print("Fields XRef")
  print(allFields.df)

  write.csv(allFields.df, file="output/streak_fields.csv") 

} else{
    if(  raw.result$status_code == 400 ) {
      print("status code 400: Bad Request")
    }else{
      print(paste("status code", raw.result$status_code ))
    }

    stopifnot(raw.result$status_code == 200 )
}


# ----------------------------------------------------------------------------
# GET BOXES
# ----------------------------------------------------------------------------

endpoint <-  "/api/v1"
api <- paste0("/pipelines/",pipelineKey,"/boxes")
path <- paste0(endpoint,api)
#$$$ DEBUG
# print(path)
#$$$ DEBUG

# Executing an API call with the GET flavor is done using the GET() function.

raw.result <- GET(url = url, path = path, authenticate(cred$api_key,"",type="basic"))

if(raw.result$status_code == 200){
    this.raw.content <- rawToChar(raw.result$content)
    
    # write full box dump to JSON file
    write_json(boxes, path="streak_all_boxes_raw_api.json")

     # So the result is a single character string that contains a JSON file. Let’s tell R to parse it into something R can work with.

	boxes <- fromJSON(this.raw.content)

  if( !is.null(boxes$assignedToSharingEntries) ) { 
    boxes$assignedTo.list<- sapply(boxes$assignedToSharingEntries, `[[`, "displayName")
  
#    assignedTo.list<- sapply(ifelse(length(boxes$assignedToSharingEntries)==0,"",boxes$assignedToSharingEntries, `[[`, "displayName"))
    boxes$assignedTo.first.list<- sapply(boxes$assignedTo.list, `[[`, 1) #as.character(boxes$assignedTo.list[[1]])
    boxes$assignedTo.vec <- as.character(boxes$assignedTo.first.list)
#    boxes$assignedTo.df<- as.data.frame(assignedTo.vec)
#    colnames(assignedTo.df)<- "assigned_to"
    boxes$assignedTo <- extractWord(boxes$assignedTo.vec,1)
  } else {
    boxes$assignedTo <- ""
  }

	boxes.df<- data.frame(
              boxes$boxKey, 
              boxes$name,
              boxes$assignedTo,
              boxes$notes,
              boxes$stageKey,
              NA)
#              boxes$fields.1058)

    colnames(boxes.df)<- c("key","name","assigned_to","notes","stageKey","emailDomain") #,"details")

    write.csv(boxes.df, file=outputBoxesFileName) 

# ----------------------------------------------------------------------------
# GET NEWSFEED FOR EACH BOX, and Extract TASKS, MEETINGS, CONTACTS from it
# ----------------------------------------------------------------------------

#	the primary FOR loop here over the boxes.df to get and store activities/events 
# from all boxes
#
	# boxName <- "PWC Global"
	# boxKey <- boxes$key[boxes$name == boxName]
    allTasks.df <- NULL
    allMeetings.df <- NULL
    allContacts.df <- NULL
    allNewsFeed.df <- NULL

# $$$ DEBUG
    print(paste("dumping activities for",nrow(boxes.df),"boxes"))
# $$$
    startBoxIndex <- 1

    for(i in startBoxIndex:nrow(boxes.df)) {

        cat(i,":")

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

        if( boxes.df[i,]$key == "5014") {
            print(paste("ignoring Prospect - not getting tasks or meetings for ",boxes.df[i,]$name))

        } else{

          newsfeed.df <- getNewsfeed(boxes.df[i,]$key)
# ACHTUNG - seems we cant append newfeeds using rbind as they seem to have different structure each call.
# $$$ DEBUG
print(paste("returned from getNewsfeed with",nrow(newsfeed.df),"events"))
# $$$ DEBUG
#          if(!is.null(newsfeed.df)){
#            if(is.null(allNewsFeed.df)) {
#              allNewsFeed.df <- newsfeed.df 
#            } else {
#              allNewsFeed.df <- rbind(allNewsFeed.df, newsfeed.df) # THIS FAILS!!!!
#            }
#          }

          boxTasks.df <- getTasks(newsfeed.df)
# $$$ DEBUG
print("returned from getTasks")
# $$$ DEBUG
          if(!is.null(boxTasks.df)){
            if(is.null(allTasks.df)) {
              allTasks.df <- boxTasks.df 
            } else {
              allTasks.df <- rbind(allTasks.df, boxTasks.df)
            }
          }

          boxMeetings.df <- getMeetingNotes(newsfeed.df)
# $$$ DEBUG
print("returned from getMeetings")
# $$$ DEBUG
          if(!is.null(boxMeetings.df)){
            if(is.null(allMeetings.df)) {
              allMeetings.df <- boxMeetings.df 
            } else {
              allMeetings.df <- rbind(allMeetings.df, boxMeetings.df)
            }
          }
# $$$ DEBUG
       print("getting CONTACTS from threads")
#       readline(prompt=paste("got tasks and meetings for [",boxes.df[i,]$name,"] get contacts?..."))
# $$$$$

        boxContacts.df <- getContacts(boxes.df[i,]$key, boxes.df[i,]$name)
# $$$ DEBUG
print(paste("returned from getContacts with",boxContacts.df$email))
# $$$ DEBUG

        if(!is.null(boxContacts.df)) {
          if(nrow(boxContacts.df) >0) {
            if (!is.null(boxContacts.df$email)) {
              if ( !is.null(boxContacts.df$email[1]) & !is.na(boxContacts.df$email[1]) ) {
                emailDom <- ifelse(is.na(boxContacts.df$email[1]),"", boxContacts.df$email[1])
                emailDom <- strsplit(emailDom,"@")
                emailDom <- sapply(emailDom, function (x) x[2])
#                emailDom <- ifelse(is.na(boxes.df[i,]$emailDomain),"", boxes.df[i,]$emailDomain)
                boxes.df[i,]$emailDomain <- emailDom
# $$$ DEBUG
                print(paste("set emailDomain from getContacts with",emailDom))
# $$$ DEBUG
              }
            }
          }  
        }

        allContacts.df <- rbind(allContacts.df, boxContacts.df)
# $$$ DEBUG
print("added to allContacts.df")
# $$$ DEBUG
      } # else !prospect
    } # for

    write.csv(allContacts.df, 
            file= "output/streak_thread_contacts.csv", 
#            sep = ",",
            na="",
            col.names = TRUE,
            row.names = FALSE,
            fileEncoding="latin1",
            append = FALSE)
# $$$ DEBUG
    print("wrote to contacts file")
# $$$ DEBUG

# after first entry, append subsequent entries to the output files
#        appendToFile <- TRUE

# $$$ DEBUG
#
#        readline(prompt=paste("done [",boxes.df[i,]$name,"] next?..."))
# $$$ DEBUG   
# $$$ DEBUG
#        stopifnot( i < 500)
# $$$$$

#    View(allNewsFeed.df)
    View(allTasks.df)
    View(allMeetings.df)
    View(allContacts.df)

        if(nrow(allTasks.df) >0){
          write.csv(allTasks.df, 
            file="output/streak_tasks.csv", 
#            sep = ",",
            na="",
            col.names = TRUE,
            row.names = FALSE,
#            qmethod = 'escape',
 #           quote = TRUE,
            fileEncoding="latin1",
            append = FALSE)
        }

        if(nrow(allMeetings.df) >0){
          write.csv(allMeetings.df, 
            file="output/streak_meeting_notes.csv", 
#            sep = ",",
#            na="",
            col.names = TRUE,
            row.names = FALSE,
            fileEncoding="latin1",
            append = FALSE)
        }# $$$ DEBUG

        if(nrow(allMeetings.df) >0){
          write.csv(allMeetings.df, 
            file="output/streak_meeting_notes_utf8.csv", 
#            sep = ",",
#            na="",
            col.names = TRUE,
            row.names = FALSE,
            fileEncoding="UTF-8",
            append = FALSE)
        }# $$$ DEBUG
        print("wrote to 3 newsfeed files")
# $$$ DEBUG
        print("Done")
      
  } else{ 
    if(  raw.result$status_code == 400 ) {
    	print("status code 400: Bad Request")
    }else{
    	print(paste("status code", raw.result$status_code ))
    }
  }

print("call_streak_api.R: exit")
sink() # terminate teeing stdout to the logFile

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

#nchar(this.raw.content)
# Let’s look at the first 100 characters:
#class(this.content.df) 
#dim(this.content.df)   #with  rows and  variables



