##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD COMPANIES, where a Main Contact is missing, ADD a Contact taken from emails 
#' attached to box threads from API
#' 
#' first, run export_companies.R to create the import files from the Streak download.
#' then run call_streak_api to create streak_thread_contacts.csv, with all the email addresses
#' then run this to extract the domain from the emails and attach to company.
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(lubridate)
library(stringr)

# INPUT
# the companiesAndOpps data frame

# OUTPUT
# modified companiesAndOpps data frame

# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
# 'Tee' stdout console output to a logile. 
#
#sink( file=logFile, append=FALSE, type= c("output","message"), split=TRUE)
print("add_missing_maincontact_to_companies.R: entry")

print(paste("add_missing_maincontact_to_companies: loaded",nrow(companiesAndOpps),"companies"))
print(paste("of which",nrow(companiesAndOpps[companiesAndOpps$'Main Contact (Decision Maker)'=="",]), "have no Main Contact Name"))
print(paste("and of which",nrow(companiesAndOpps[companiesAndOpps$'Main Contact Email'=="",]), "have no Main Contact Email"))

print(paste("loaded",nrow(prosperWorksImportPeople),"streak thread contacts. Now using these to find a main contact for companies showing no main contact"))


companiesAndOpps$'Main Contact (Decision Maker)' <- ifelse(is.na(companiesAndOpps$'Main Contact (Decision Maker)'), "" ,companiesAndOpps$'Main Contact (Decision Maker)')
companiesAndOpps$'Main Contact Email' <- ifelse(is.na(companiesAndOpps$'Main Contact Email'), "" ,companiesAndOpps$'Main Contact Email')

for( i in 1:nrow(companiesAndOpps)){
	
	companyName<- companiesAndOpps$Company[i]
#	cat(i,":")

	# if we have contacts from streak API for this company, we'll use those
	contactsForThisCompany <- dplyr::filter(prosperWorksImportPeople, Company == companyName)
	contactsForThisCompany <- dplyr::filter(contactsForThisCompany, !is.na(Name))
	contactsForThisCompany <- dplyr::filter(contactsForThisCompany, !is.na(Email))
	contactsForThisCompany <- dplyr::filter(contactsForThisCompany, Name!="")
	contactsForThisCompany <- dplyr::filter(contactsForThisCompany, Email!="")

	# if no main contact defined for this company
	if(companiesAndOpps$'Main Contact (Decision Maker)'[i] == "" ){

		if( nrow(contactsForThisCompany) >0){
			firstContactFound <- contactsForThisCompany[1,]
#$$$ DEBUG
#			print(paste("adding inferred contact Name and Email",firstContactFound$name,"into missing Main contact for",companyName))
#$$$
			companiesAndOpps$'Main Contact (Decision Maker)'[i] <- firstContactFound$Name
			companiesAndOpps$'Main Contact Email'[i] <- firstContactFound$Email[1]
		}
	#else if a Name was in there, but no email address, lets try find that
	} else if(companiesAndOpps$'Main Contact Email'[i] == "" ){

		thisContactEmail <- "" # init

		# if we have contacts from streak API for this company, we'll use those
		if( nrow(contactsForThisCompany) >0){
			thisContact <- dplyr::filter(contactsForThisCompany, Name == companiesAndOpps$'Main Contact (Decision Maker)'[i])
			if(nrow(thisContact)>0){
				# found it
				thisContact <- thisContact[1,]
				thisContactEmail <- as.character(thisContact$Email)
			} else{
				# couldnt find that one so use the first valid one
				thisContactEmail <- as.character(contactsForThisCompany$Email[1])
		}
#$$$ DEBUG
#				print(paste("adding inferred contact Email",thisContactEmail,"into missing Main contact email for",companyName))
#$$$ 
		companiesAndOpps$'Main Contact Email'[i] <- thisContactEmail
			
		} else { 
			print(paste("can't find an email address for this Company",companyName)) 
		}
	} # else if main name but no email defined
} # for

# if DOmain is empty extract email domain from the email address of the main contact 
companiesAndOpps$'Email Domain' <- ifelse(is.na(companiesAndOpps$'Email Domain'), "", companiesAndOpps$'Email Domain')

xemailDomain <- ifelse(is.na(companiesAndOpps$`Main Contact Email`),"", companiesAndOpps$`Main Contact Email`)
xemailDomain <- strsplit(xemailDomain,"@")
xemailDomain <- sapply(xemailDomain, function (x) x[2])
xemailDomain <- ifelse(is.na(xemailDomain),"", xemailDomain)

companiesAndOpps$`Email Domain` <- ifelse(companiesAndOpps$`Email Domain`!="", companiesAndOpps$`Email Domain`, xemailDomain)
xemailDomain <- NULL # remove temp column

View(companiesAndOpps)
print("===============================================================================")
print(paste("After xref with streak thread contacts, we now have ",nrow(companiesAndOpps[companiesAndOpps$'Main Contact (Decision Maker)'=="",]), " companies still with no main contact"))
print(paste("and",nrow(companiesAndOpps[companiesAndOpps$'Main Contact Email'=="",]), "still have no Main Contact Email"))
print(paste("and",nrow(companiesAndOpps[companiesAndOpps$'Email Domain'=="",]), "still have no Email Domain"))

# remove any with no domain that at closed lost or disqualified
print("removing any entries with no Email Domain that are disqualified or closed-lost")

companiesAndOpps <- dplyr::filter(companiesAndOpps,!(`Email Domain`=="" & (Stage=="Closed - Lost" | Stage=="Disqualified")))
print(paste("now only",nrow(companiesAndOpps[companiesAndOpps$'Email Domain'=="",]), "have no Email Domain"))
print(companiesAndOpps$Company[companiesAndOpps$'Email Domain'==""])
print("===============================================================================")

print("add_missing_maincontact_to_companies.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


