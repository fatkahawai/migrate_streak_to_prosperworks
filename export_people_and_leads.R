##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' extract the PEOPLE (Main and Champion) from the export table
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(stringr)

# INPUT
# companies - which have main and champion contact details
# allContacts.df - from the threads extracted by the streak api call 

# ----------------------------------------------------------------------------
#' @function mapToProsperWorks 
#
#' @param boxes dataframe
#' @param name 
#' @param position 
#' @param phone 
#' @param email 
#' @param tag 
#' @notes
#' @returns dataframe of people
# ----------------------------------------------------------------------------
mapPeopleToProsperWorks <- function(boxes,name,position,phone,email,tag){

print(paste("mapPeopleToProsperWorks: processing",nrow(boxes),"boxes"))

name <- ifelse(is.na(name), "", str_trim(name))
position <- ifelse(is.na(position), "", str_trim(position))
phone <- ifelse(is.na(phone), "", str_trim(phone))
email <- ifelse(is.na(email), "", str_trim(email))
 
#$$$ DEBUG
#print("init")
#$$$
people <- data.frame(Name=name, stringsAsFactors=FALSE)

#$$$ DEBUG
#print(paste("have a people table with",nrow(people),"rows"))
#$$$

#people$Name <- name

people$Prefix <- ""
people$Suffix <- ""
people$Title <- position
#$$$ DEBUG
#print("set name")
#View(people)
#$$$

# combine details and notes into details column
people$Details <- ""

people$Domain <- boxes$'Email Domain'
people$Company <- boxes$Company
people$Opportunity<- boxes$Opportunity
people$box_name <- boxes$box_name

people$'Owned By' <- boxes$'Owned By' # Boxes$'Assigned To'

people$Source <- boxes$Source

people$Street <- boxes$Street
people$City <- boxes$City
people$State <- boxes$State
people$`Postal Code` <- boxes$`Postal Code`
people$Country <- boxes$Country

people$`Work Phone` <- substr(phone,0,29)
people$`Mobile Phone` <- ""
people$Email <- email
people$`Work Email` <- email

people$`Personal Website` <- ""

people$Details <- boxes$Details # only need box details for leads not people

people$Tag <- tag

people$Stage <- boxes$Stage
people$Status <- boxes$Status

#clean up - remove rows with no contact email, 
people<- people[!is.na(people$Email),]
people<- people[people$Email != "",]

return(people)
}
# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------

print("export_people_and_leads.R: entry")

print("main contacts:")

main <- mapPeopleToProsperWorks(companiesAndOpps,
	companiesAndOpps$'Main Contact (Decision Maker)',
	companiesAndOpps$'Main Contact Position',
	companiesAndOpps$'Main Contact Phone',
	companiesAndOpps$'Main Contact Email',
	"Buyer")

print("champions:")

champion <- mapPeopleToProsperWorks(companiesAndOpps,
	companiesAndOpps$'Contact (Champion)',
	companiesAndOpps$'Contact (Champion) Position',
	companiesAndOpps$'Contact (Champion) Phone',
	companiesAndOpps$'Contact (Champion) Email',
	"Champion")


#concatenate the data frames
downloadPeople<-rbind(main,champion)

print(paste("extracted total of",nrow(downloadPeople),"main and champion people from companies table"))


contactsToAdd <- allContacts.df
print(paste("Now adding",nrow(contactsToAdd),"people from allContacts.df table (extracted from streak threads)"))

colnames(contactsToAdd) <- c("Name","Email","box_name","box_key","Domain","Company","Opportunity","Stage","Status")

# add missing columns so we can rbind it
contactsToAdd$`Owned By` <- ""
contactsToAdd$Prefix <- ""
contactsToAdd$Suffix <- ""
contactsToAdd$Title <- ""
contactsToAdd$pos <- ""
contactsToAdd$Details <- ""
contactsToAdd$Street <- ""
contactsToAdd$City <- ""
contactsToAdd$State <- ""
contactsToAdd$`Postal Code` <- ""
contactsToAdd$Country <- ""
contactsToAdd$`Work Phone` <- ""
contactsToAdd$`Mobile Phone` <- ""
contactsToAdd$`Work Email` <- contactsToAdd$Email
contactsToAdd$`Personal Website` <- ""
contactsToAdd$Details <- ""
contactsToAdd$Tag <- "streak_thread_contact"
contactsToAdd$Source <- ""

contactsToAdd$box_key<-NULL

rawImportPeople<-rbind(downloadPeople,contactsToAdd)

print(paste("Now we have combined total of",nrow(rawImportPeople),"people"))

importPeople <- rawImportPeople

# clean up rogue entries - emails that are not for People we want to import
importPeople <- dplyr::filter(importPeople,!grepl("@groovehq.com$",Email))
importPeople <- dplyr::filter(importPeople,!grepl("@streak.com$",Email))
importPeople <- dplyr::filter(importPeople,!grepl("^notifications@",Email))
importPeople <- dplyr::filter(importPeople,!grepl("^noreply@",Email))


importPeople <- dplyr::filter(importPeople,Name!="<DO_NOT_REPLY>")
importPeople <- dplyr::filter(importPeople,Name!="noreply")
importPeople <- dplyr::filter(importPeople,Name!="donotreply")
importPeople <- dplyr::filter(importPeople,Name!="Mail Delivery System")

# filter out contact where the email starts with a digit
importPeople <- dplyr::filter(importPeople, !grepl("^[0-9]+.*",Email))

# filter out contact where the email is invalid 
importPeople <- dplyr::filter(importPeople, grepl("^.+@[a-zA-Z][a-zA-Z]+",Email))


importPeople$Email <- tolower(importPeople$Email)
importPeople$Email <- gsub("[\\>]","",importPeople$Email)
importPeople$Email <- gsub("[\\<]","",importPeople$Email)
importPeople$Email <- gsub("mailto:","",importPeople$Email)

# remove any trailing info after the initial email address 
importPeople$Email <- gsub("([^ ]+) *.*","\\1",importPeople$Email)

importPeople$`Work Email` <- importPeople$Email

print(paste("cleaned up entries with invaid email addresses, now leaving",nrow(importPeople),"people"))

# delete any that have neither a name nor email address 
importPeople <- dplyr::filter(importPeople, Name!="" | Email != "")
print(paste("cleaned up entries with neither a name nor an email, now leaving",nrow(importPeople),"people"))

# if no name, use the user part of the email address as an alias to put into the name
importPeople$orig_name <- importPeople$Name
importPeople$aliassplit <- strsplit(importPeople$Email,"@")
importPeople$aliasusr <- sapply(importPeople$aliassplit, function (x) x[1])

#importPeople$alias <- ifelse(!is.null(importPeople$aliasusr),importPeople$aliasusr,importPeople$Name)
importPeople$Name <- ifelse(importPeople$Name=="",importPeople$aliasusr,importPeople$Name)

importPeople$aliassplit <- NULL
importPeople$aliasusr <- NULL

importPeople <- dplyr::filter(importPeople, Name!="postmaster")
importPeople <- dplyr::filter(importPeople, Name!="Guest User")

#print(paste("created total of",nrow(importPeople),"people"))

# harder than it looks to remove dups. And one dup may have phone number or other info 
# I tried to let prosperworks handle dups, but it imports dups that are not identical in all fields

dupPeople <- importPeople[duplicated(importPeople$Email),]
print(paste("removing unrequired extra entries for",nrow(dupPeople),"people duplicated - with same email address"))

# ignore entries id'd as dups because Email is empty or NA
dupPeople <- dupPeople[!is.na(dupPeople$Email),]
dupPeople <- dupPeople[dupPeople$Email!="",]
print(paste("removed empty email entries. still going to remove unrequired extra entries for",nrow(dupPeople),"people duplicated - with same email address"))

# can't just call unique() - we want to retain the dup with most fields filled.
bestEntries<-data.frame(NULL)

for(i in 1:nrow(dupPeople)){
	dupPersonEmail <- dupPeople$Email[i]
	dupEntries <- dplyr::filter(importPeople,Email==dupPersonEmail)

	# delete all matches with this dup in people list.
	# we will later add back the best one we want to keep
	importPeople <- dplyr::filter(importPeople,Email!=dupPersonEmail)

	if(nrow(dupEntries)<2){
		# just means this is a dup entry in dup list so has already been processed
		# ignore it
#		print(paste(i,": Achtung: huh?",dupPersonEmail,"is a dup but can't find ANY matching dup entries"))

#		stopifnot(nrow(dupEntries)>1)
	} else{ 
		# find best entry to keep, and delete the others
		# start with the 1st entry as default
		bestEntry <- dupEntries[1,]		
		found<- FALSE
		for(j in 1:nrow(dupEntries)){
			if( dupEntries$`Work Phone`[j] != "") {
				bestEntry <- dupEntries[j,]
				break
			}
		} # for
		if(!found){
			for(j in 1:nrow(dupEntries)){
				if( dupEntries$Title[j] != "") {
					bestEntry <- dupEntries[j,]
					break
				}
			} # for
		} # if
		if(!found){
			for(j in 1:nrow(dupEntries)){
				if( dupEntries$`Owned By`[j] != "") {
					bestEntry <- dupEntries[j,]
					break
				}
			} # for
		} # if
		bestEntries <- rbind(bestEntries,bestEntry)
	} # else
} # for dupPeople

# add back in the best entries we found for each email in the dups list 
if(nrow(bestEntries) > 0 ){
	importPeopleFull <- rbind(importPeople,bestEntries)
} else { 
	importPeopleFull <- importPeople
}
print(paste("finally have total of",nrow(importPeopleFull),"people"))


# separate Leads from Contacts/People
prosperWorksImportPeople<- importPeopleFull[importPeopleFull$Stage != "Prospect",]
print(paste("split into",nrow(prosperWorksImportPeople),"People"))

# don't need details for people
prosperWorksImportPeople$Details <- NULL


View(prosperWorksImportPeople)

prosperWorksImportLeads<- importPeopleFull[importPeopleFull$Stage == "Prospect",]
print(paste("and",nrow(prosperWorksImportLeads),"Leads"))
prosperWorksImportLeads$Source <- ifelse(prosperWorksImportLeads$Source=="","Outbound",prosperWorksImportLeads$Source)
prosperWorksImportLeads$Status <- ifelse(prosperWorksImportLeads$Status=="","Unqualified",prosperWorksImportLeads$Status)

View(prosperWorksImportLeads)

#write to CSV file
write.csv(prosperWorksImportPeople,"output/import_people.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
write.csv(prosperWorksImportLeads,"output/import_leads.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")

print(paste("Now dumping",nrow(dplyr::filter(companiesAndOpps,Stage=="Prospect")),"prospects we no longer need from companiesAndOpps table"))
print(paste("and dumping",nrow(dplyr::filter(companiesAndOpps,Stage=="")),"with empty Stage from companiesAndOpps table"))

# Now dump Prospects from companiesAndOpps table for processing from now on.
companiesAndOpps <- dplyr::filter(companiesAndOpps,Stage!="Prospect")
companiesAndOpps <- dplyr::filter(companiesAndOpps,Stage!="")

print(paste("leaving a total for next steps of",nrow(companiesAndOpps),"companies and opps"))

print("export_people.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


