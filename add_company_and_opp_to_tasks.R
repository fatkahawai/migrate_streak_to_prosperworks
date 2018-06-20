##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD TASKS, from emails attached to boxes - add company and opp name
#' 
#' first, run export_companies.R to create the import files from the Streak download.
#' then run call_streak_api to create streak_boxes.csv, with all the email addresses
#' then run add_missing and add_email_domain.. 
#' then run this 
#'
#' 
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################


library(lubridate)
library(stringr)
library(dplyr)

# INPUT
inputFileName <- "data/streak_tasks.csv"
# allTasks.df - an output of  call_streak_api.R

# OUTPUT
outputFileName <- "output/import_tasks.csv"

# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
print("add_company_and_opp_to_tasks.R: entry")

allTasks.df <- read.csv(inputFileName, 
	check.names=FALSE, 
	header = TRUE, 
	stringsAsFactors = FALSE,
	blank.lines.skip = TRUE,
	strip.white = TRUE)

print(paste("loaded",nrow(allTasks.df),"tasks"))

tasks <- allTasks.df

#initialise a new Company column
tasks$Company<- ""
tasks$Opportunity<- ""
invalidTasksDeleted <- NULL

for( i in 1:nrow(tasks)){
#$$$DEBUG
#	cat(i,":")
#$$$DEBUG

	boxName<- tasks$box_name[i]

	#find entry in export data frame
	exportBox <- export[na.omit(match(boxName,export$orig_box_name)),] 
	if(nrow(exportBox)==0){

		if(is.null(invalidTasksDeleted)) {
			invalidTasksDeleted <- data.frame(tasks[i,])
		} else {
			invalidTasksDeleted <- rbind(invalidTasksDeleted,tasks[i,])
		}

		tasks$owner[i] <- NA
	}
	else if(nrow(exportBox)==1){
		tasks$Company[i]<- exportBox$Company
		tasks$Opportunity[i]<- exportBox$Opportunity
	}
	else{
		print(paste("Achtung: ",nrow(exportBox),"matches for box name",boxName))
	}
}

tasks <- dplyr::filter(tasks,!is.na(owner))

print(paste("filtered out",nrow(invalidTasksDeleted),"invalid contacts from boxes we are not importing"))
print(paste("saved",nrow(tasks),"contacts in final list of tasks to import"))

# save original task list, and assign new one
allTasks.orig.df <- allTasks.df
allTasks.df <- tasks

pwImportTaskTable <- allTasks.df
pwImportTaskTable <- data.frame(substr(allTasks.df$task_text,1,80),  # use first 80 chars for name
								allTasks.df$Company, # X ref between our list of tasks and Pw Opps list
								"opportunity",
								allTasks.df$owner,
								allTasks.df$status,
								allTasks.df$task_text,
								allTasks.df$due_date,
								stringsAsFactors=FALSE)

colnames(pwImportTaskTable) <- c('Task Name','Related ID','Related Type','User Name','Status','Description','Due Date')

View(pwImportTaskTable)
# ----------------------------------------------------------------------------
# write to CSV file
# ----------------------------------------------------------------------------
write.csv(pwImportTaskTable,outputFileName,row.names=FALSE,na="",fileEncoding="latin1")
print("add_company_and_opp_to_tasks.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


