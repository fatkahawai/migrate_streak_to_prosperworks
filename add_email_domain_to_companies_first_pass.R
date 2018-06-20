##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD COMPANIES, ADD THEIR EMAIL DOMAINS from emails attached to boxes
#' 
#' first, run export_companies.R to create the import files from the Streak download.
#' then run call_streak_api to create streak_boxes.csv, with all the email addresses
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
# the allContacts.df data frame

# OUTPUT
outputCompaniesMissingDomainFileName <-  "output/companies_domain_missing_first_pass.csv"
# modified companiesAndOpps data frame


# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
print("add_email_domain_to_companies_first_pass.R: entry")


print(paste("loaded",nrow(companiesAndOpps),"companies"))
print(paste("of which",nrow(companiesAndOpps[companiesAndOpps$'Email Domain'=="",]), "have no email domain"))

print(paste("loaded",nrow(allContacts.df),"allContacts.df Now using these details to infer email domain for companiesAndOpps showing no email domain"))
#print(paste("of which",nrow(boxes[is.na(boxes$emailDomain)]), " have no email domain"))

#companiesAndOpps<- as.data.frame(companiesAndOpps)
#companies$'Email Domain' <-  ifelse(is.null(companies$'Email Domain'), "", companies$'Email Domain')
companiesAndOpps$'Email Domain' <-  ifelse(is.na(companiesAndOpps$'Email Domain'), "", companiesAndOpps$'Email Domain')

for( i in 1:nrow(companiesAndOpps)){

#	cat(i,":")

	thisCompanyBoxName<- companiesAndOpps$box_name[i]
	if(is.na(companiesAndOpps$'Email Domain'[i])){
		companiesAndOpps$'Email Domain'[i] <- ""
	}

	if(companiesAndOpps$'Email Domain'[i] == ""){
		thisCompanyDomain <- allContacts.df$Domain[na.omit(match(thisCompanyBoxName,allContacts.df$box_name))]
		if( length(thisCompanyDomain) > 0){
			companiesAndOpps$'Email Domain'[i] <- thisCompanyDomain
		 	cat(i,"=matched,")
		}
	}
	companiesAndOpps$'Email Domain'[i] <- str_trim(companiesAndOpps$'Email Domain'[i]) 

	if(companiesAndOpps$'Email Domain'[i] == "gmail.com"){ # clearly not their true domain
		companiesAndOpps$'Email Domain'[i] <- ""
	}
	if(companiesAndOpps$'Email Domain'[i] == "edlio.com"){ # this is the old wufoo domain
		companiesAndOpps$'Email Domain'[i] <- ""
	}
}
View(companiesAndOpps)
print("")


# find a list of remaining companies we haven't been able to infer an email domain fors
#print(paste("reduced to",nrow(companiesAndOpps),"companies after deleting disqualified and lost"))

domainMissing <- companiesAndOpps[companiesAndOpps$`Email Domain` == "",c('Company','Owned By','Email Domain','Stage')]

print("") # newline
print("===============================================================================")
#print(paste("After xref with streak boxes, we now have ",nrow(companies[companies$'Email Domain'=="",]), " companies still with no email domain"))
if( nrow(domainMissing) > 0) {
  print(paste("Achtung: still have",nrow(domainMissing),"companies with domains missing after second pass"))
}

# ----------------------------------------------------------------------------
#write to CSV file
write.csv( domainMissing, file = outputCompaniesMissingDomainFileName)
 
print("add_email_domain_to_companies_first_pass.R: exit") 

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


