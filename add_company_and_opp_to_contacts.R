##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD CONTACTS, from emails attached to boxes - add company and opp name
#' 
#' first, run export_companies.R to create the import files from the Streak download.
#' then run call_streak_api to create streak_boxes.csv, with all the email addresses
#' then run add_missing and add_email_domain.. 
#' then run this to add company and opp name to contacts
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(lubridate)
library(stringr)
library(dplyr)

# INPUT
inputFileName <- "data/streak_thread_contacts.csv"
# allContacts.df - an output of  call_streak_api.R

# OUTPUT
outputFileName <- "data/streak_thread_contacts_final.csv"
outputCompaniesMissingDomainFileName <-  "output/companies_domain_missing.csv"


# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
print("add_company_and_opp_to_contacts.R: entry")

allContacts.df <- read.csv(inputFileName, 
	check.names=FALSE, 
	header = TRUE, 
	stringsAsFactors = FALSE,
	blank.lines.skip = TRUE,
	strip.white = TRUE)

print(paste("loaded",nrow(allContacts.df),"contacts"))

contacts <- allContacts.df

#initialise a new Company column
contacts$Domain<- ""
contacts$Company<- ""
contacts$Opportunity<- ""
contacts$Stage<- ""
contacts$Status<- ""
invalidContactsDeleted <- NULL

# 1st, find emaildomain from the contact, us that to find Company name

print(paste(nrow(contacts[contacts$Domain=="",]), "contacts have no Domain"))
print(paste(nrow(contacts[contacts$Company=="",]), "contacts have no Company"))
print("first pass using domain from email address of contact to find company")

for( i in 1:nrow(contacts)){

#	cat(i,":")

	thisEmailDomain1 <- ifelse(is.na(contacts$email[i]),"", contacts$email[i])
	thisEmailDomain2 <- strsplit(thisEmailDomain1,"@")
	thisEmailDomain3 <- sapply(thisEmailDomain2, function (x) x[2])
	contacts$Domain[i] <- ifelse(is.na(thisEmailDomain3),"", thisEmailDomain3)

	#if we have extracted a domain for the contact, use it tof ind a 
	# matching entry in companiesAndOpps data frame
	if( contacts$Domain[i] !=""){ 

		companyAndOppList <- companiesAndOpps[na.omit(match(contacts$Domain[i],companiesAndOpps$'Email Domain')),] 

		# if we didn't find the company matching this contact, move on
		if(nrow(companyAndOppList)==0){
#			print(paste("Achtung: no match found in companiesAndOpps for email domain",contacts$Domain[i])	)	
		}
		# else if exactly one match - the normal case, copy the Company and Opp name over to the contact record.
		else if(nrow(companyAndOppList)>=1){
			contacts$Company[i]<- companyAndOppList$Company[1]
			contacts$Opportunity[i]<- companyAndOppList$Opportunity[1]
			contacts$Stage[i]<- companyAndOppList$Stage[1]
			contacts$Status[i]<- companyAndOppList$Status[1]

		 	cat(i,":")
		}
	}
}

# 2nd, for the remainder still not matched, use box name to find company name 
print("after 1st pass")
print(paste(nrow(contacts[contacts$Domain=="",]), "contacts have no Domain"))
print(paste(nrow(contacts[contacts$Company=="",]), "contacts have no Company"))
print("2nd pass using box name")

for( i in 1:nrow(contacts)){

#	cat(i,":")
  # if we havent set a Company for this contact yet, 
  if( contacts$Company==""){

	# we will use its boxname to find a company name in companiesAndOpps
	boxName<- contacts$box_name[i]

	#find entry in companiesAndOpps data frame
	companyAndOppList <- companiesAndOpps[na.omit(match(boxName,companiesAndOpps$box_name)),] 

	# if we didn't find the company matching this contact, then its one we have filtered out to not import
	# so move it to invalidContactsDeleted list, and set fields to NA for later cleanup below
	if(nrow(companyAndOppList)==0){

		if(is.null(invalidContactsDeleted)) {
			invalidContactsDeleted <- data.frame(contacts[i,])
		} else {
			invalidContactsDeleted <- rbind(invalidContactsDeleted,contacts[i,])
		}

		contacts$name[i] <- NA
	}
	# else if exactly one match - the normal case, copy the Company and Opp name over to the contact record.
	else if(nrow(companyAndOppList)==1){
		contacts$Company[i]<- companyAndOppList$Company
		contacts$Opportunity[i]<- companyAndOppList$Opportunity
		contacts$Stage[i]<- companyAndOppList$Stage
		contacts$Status[i]<- companyAndOppList$Status
		if( contacts$Domain[i] == "") {
			contacts$Domain[i]<- companyAndOppList$'Email Domain'

		 	cat(i,":")
		} 
	}
	# else we found multiple matches for this company in export dataframe - so thats a fail somewehere
	else{
#		print(paste("Achtung: ",nrow(companyAndOppList),"matches found in companiesAndOpps for one box name in contacts called",boxName))
	}
  }
}

print("after 2nd pass")
print(paste(nrow(contacts[contacts$Domain=="",]), "contacts have no Domain"))
print(paste(nrow(contacts[contacts$Company=="",]), "contacts have no Company"))

# filter out contacts that have a numerical-only username (prob students)
#xemailSplit <- strsplit(contacts$name,"@")
#xemailName <- sapply(xemailSplit, function (x) x[1])
#xemailName <- ifelse(is.na(xemailName),"", xemailName)

# filter out the rows where we found invalid 
contacts <- dplyr::filter(contacts,!is.na(name))
print(paste("filtered out",nrow(invalidContactsDeleted),"invalid contacts from boxes we are not importing"))

print(paste("saving",nrow(contacts),"contacts in final list of contacts to import, of which"))
print(paste(nrow(contacts[contacts$Domain=="",]), "contacts have no Domain"))
print(paste(nrow(contacts[contacts$Company=="",]), "contacts have no Company"))

# save original meetings list, and assign new one
allContacts.orig.df <- allContacts.df
allContacts.df <- contacts

colnames(allContacts.df) <- c("Name","Email","box_name","box_key","Domain","Company","Opportunity","Stage","Status")

View(allContacts.df)
View(invalidContactsDeleted)

# ----------------------------------------------------------------------------
# record missing domains
domainMissing <- companiesAndOpps[companiesAndOpps$`Email Domain` == "",c('Company','Owned By','Email Domain','Stage')]

#print(paste("After xref with streak boxes, we now have ",nrow(companies[companies$'Email Domain'=="",]), " companies still with no email domain"))
if( nrow(domainMissing) > 0) {
  print(paste("Achtung: still have",nrow(domainMissing),"companies with domains missing after second pass. see list in",outputCompaniesMissingDomainFileName))
}
# find out a list of companies are sharing the same email domain with at least 1 other company
# We need to fix those in the source Streak data

# ----------------------------------------------------------------------------
#write to CSV file
# ----------------------------------------------------------------------------
write.table(allContacts.df,outputFileName,sep=",",col.names=TRUE,fileEncoding="latin1")
write.csv( domainMissing, file = outputCompaniesMissingDomainFileName)

print("add_company_and_opp_to_contacts.R: exit")
print("===============================================================================")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


