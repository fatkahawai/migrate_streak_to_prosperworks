##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD MEETINGNOTES, from emails attached to boxes - add company and opp name
#' 
#' first, run export_companies.R to create the import files from the Streak download.
#' then run call_streak_api to create streak_boxes.csv, with all the email addresses
#' then run add_missing and add_email_domain.. 
#' then run this 
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(lubridate)
library(stringr)
library(dplyr)

# INPUT
inputFileName <- "data/streak_meeting_notes.csv"
# allMeetingsReopened.df - the data frame created by call_streak_api.R

# OUTPUT
outputFileName <- "output/import_meeting_notes.csv"

# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------

print("add_company_and_opp_to_meetingnotes.R: entry")

allMeetingsReopened.df <- allMeetings.df
print(paste("loaded",nrow(allMeetingsReopened.df),"meeting notes"))

importMeetings <- allMeetingsReopened.df


#initialise a new Company column
importMeetings$Company<- ""
importMeetings$Opportunity<- ""
invalidMeetingsDeleted <- NULL

for( i in 1:nrow(importMeetings)){

#$$$DEBUG
#	cat(i,":")
#$$$DEBUG

	boxName<- importMeetings$box_name[i]

	#find entry in export data frame
	exportBox <- export[na.omit(match(boxName,export$orig_box_name)),] 
	if(nrow(exportBox)==0){

		if(is.null(invalidMeetingsDeleted)) {
			invalidMeetingsDeleted <- data.frame(importMeetings[i,])
		} else {
			invalidMeetingsDeleted <- rbind(invalidMeetingsDeleted,importMeetings[i,])
		}

		importMeetings$owner[i] <- NA
	}
	else if(nrow(exportBox)==1){
		importMeetings$Company[i]<- exportBox$Company
		importMeetings$Opportunity[i]<- exportBox$Opportunity
	}
	else{
		print(paste("Achtung: ",nrow(exportBox),"maches for box name",boxName))
	}
}

importMeetings <- dplyr::filter(importMeetings,!is.na(owner))

print(paste("filtered out",nrow(invalidMeetingsDeleted),"invalid contacts from boxes we are not importing"))
print(paste("saved",nrow(importMeetings),"contacts in final list of meetings to import"))

importMeetings$activity_type <- 'Meeting'
importMeetings$activity_type <- ifelse(importMeetings$meeting_type=='CALL_LOG','Phone Call',importMeetings$activity_type)

importMeetings$owner <- ifelse(importMeetings$owner=="Kami","Hengjie",importMeetings$owner)
importMeetings$owner <- ifelse(importMeetings$owner=="Abhi","Hengjie",importMeetings$owner)
importMeetings$owner <- ifelse(importMeetings$owner=="Jeffery","Hengjie",importMeetings$owner)
importMeetings$owner <- ifelse(importMeetings$owner=="Marshall","Bob",importMeetings$owner)

#importMeetings$meeting_notes <- str_trim(iconv(importMeetings$meeting_notes, from = "UTF-8", to = "Latin1", sub = "_"))
#importMeetings$meeting_notes <- gsub("\\n","\\\n",importMeetings$meeting_notes)

pwImportMeetingTable <- data.frame(
								importMeetings$Company, # X ref between our list of tasks and Pw Opps list
								"opportunity",
								importMeetings$activity_type,
								importMeetings$meeting_notes,
								importMeetings$owner,
								importMeetings$meeting_timestamp,
								stringsAsFactors=FALSE)

colnames(pwImportMeetingTable) <- c('Parent ID','Parent Type','Activity Type','Details','User Name','Activity Date')

View(pwImportMeetingTable)

# ----------------------------------------------------------------------------
#write to CSV file
# ----------------------------------------------------------------------------
write.csv(pwImportMeetingTable,outputFileName,row.names=FALSE,fileEncoding="UTF-8")

print("add_company_and_opp_to_meetingnotes.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


