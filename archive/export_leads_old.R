##############################################################################
# Migrate Data from Streak to ProsperWorks
# 
# VERSION: 1.0
# AUTHOR: Bob
# (C)Kami 2017
##############################################################################


# ----------------------------------------------------------------------------
# FUNCTION: 
#
# ARGS:
# metric, - char
# fromDate,toDate, - POSIX dates
#
# RETURNS: a data frame with 2 columns - date and values
# ----------------------------------------------------------------------------

exportFileName <- "~/Downloads/Streak Export- Master Sales Pipeline (11-1-17 9-08 PM) - Boxes (Master Sales Pipeline).csv"
leadOwner <- "Carmela"


boxes <- read.csv(exportFileName, 
	check.names=FALSE, 
	header = TRUE, 
	stringsAsFactors = FALSE,
	blank.lines.skip = TRUE,
	strip.white = TRUE)

View(boxes)

# ----------------------------------------------------------------------------
main <- data.frame(Updated=boxes$'Date Last Updated')

main$'First Name' <- sapply(strsplit(boxes$'Main Contact (Decision Maker)', "\\s+"), `[`, 1)
main$'Middle Name' <- ""
main$'Last Name' <- sapply(strsplit(boxes$'Main Contact (Decision Maker)', "\\s+"), `[`, 2)
main$Salutation <- ""
main$Suffix <- ""
main$Title <- boxes$'Contact (Champion) Position'
main$Details <- paste(ifelse(is.na(boxes$Details),"", boxes$Details), ifelse(is.na(boxes$Notes),"", boxes$Notes))
main$Value <- ""
main$Company <- boxes$Name
main$'Owned By' <- leadOwner # Boxes$'Assigned To'
main$Source <- ifelse(is.na(boxes$'Lead Source'),"Outbound",boxes$'Lead Source')
main$Street <- ""
main$City <- ""
main$State <- boxes$State
main$'Postal Code' <- ""
main$Country <- "United States"
main$'Work Phone' <- substr(boxes$'Main Contact Phone',0,29)
main$'Mobile Phone' <- ""
main$Email <- boxes$'Main Contact Email'
main$'Work Website' <- boxes$'Domain 1'
main$'Personal Website' <- ""
main$'Status' <- "New"
main$'Organization Type' <- ifelse(is.na(boxes$Type),"School",boxes$Type)
main$LinkedIn <- ""
main$Facebook <- ""
main$Tag <- "Buyer"


#clean up - remove rows with no contact name
main<- main[!is.na(main$Email),]

# ----------------------------------------------------------------------------

champion <- data.frame(Updated=boxes$'Date Last Updated')

champion$'First Name' <- sapply(strsplit(boxes$'Contact (Champion)', "\\s+"), `[`, 1)
champion$'Middle Name' <- ""
champion$'Last Name' <- sapply(strsplit(boxes$'Contact (Champion)', "\\s+"), `[`, 2)
champion$Salutation <- ""
champion$Suffix <- ""
champion$Title <- boxes$'Contact (Champion) Position'
champion$Details <- paste(ifelse(is.na(boxes$Details),"", boxes$Details), ifelse(is.na(boxes$Notes),"", boxes$Notes))
champion$Value <- ""
champion$Company <- boxes$Name
champion$'Owned By' <- leadOwner # Boxes$'Assigned To'
champion$Source <- ifelse(is.na(boxes$'Lead Source'),"Outbound",boxes$'Lead Source')
champion$Street <- ""
champion$City <- ""
champion$State <- boxes$State
champion$'Postal Code' <- ""
champion$Country <- "United States"
champion$'Work Phone' <- substr(boxes$'Contact (Champion) Phone',0,29)
champion$'Mobile Phone' <- ""
champion$Email <- boxes$'Contact (Champion) Email'
champion$'Work Website' <- boxes$'Domain 1'
champion$'Personal Website' <- ""
champion$'Status' <- "New"
champion$'Organization Type' <- ifelse(is.na(boxes$Type),"School",boxes$Type)
champion$LinkedIn <- ""
champion$Facebook <- ""
champion$Tag <- "Champion"

#clean up - remove rows with no contact name
champion<- champion[!is.na(champion$Email),]
# ----------------------------------------------------------------------------

#concatenate the data frames
prosperWorksImportLeads<-rbind(main,champion)

View(prosperWorksImportLeads)

#write to CSV file
write.csv(prosperWorksImportLeads, file="~/Downloads/prospects.csv")



