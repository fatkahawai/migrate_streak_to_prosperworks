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

# OUTPUT
outputDomainDupsFileName <-  "output/companies_domain_dups.csv"
outputChildrenFileName <- "output/child_parent_map.csv"
# modified companiesAndOpps data frame


# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
print("add_email_domain_to_companies_list_dups.R: entry")

print(paste("loaded",nrow(companiesAndOpps),"companies"))
print(paste("of which",nrow(companiesAndOpps[companiesAndOpps$'Email Domain'=="",]), "have no email domain"))

# $$$$$$$$
# NO NEED TO XFEF WITH CONTACTS - I NOW DO THIS IN ADD_DOMAIN_TO_COMPANIES.R
# $$$$$$$$


# find out a list of companies are sharing the same email domain with at least 1 other company
# We need to fix those in the source Streak data

duplicates <- companiesAndOpps[duplicated(companiesAndOpps$`Email Domain`),] 
duplicates <- duplicates[!is.na(duplicates$`Email Domain`),]
duplicates <- duplicates[duplicates$`Email Domain`!="",]

nonEmptyCompanies <- companiesAndOpps[companiesAndOpps$`Email Domain`!="",]

#print(paste("list of",nrow(duplicates),"Companies with already used emailDomains"))
#print(paste(duplicates$'Email Domain',":",duplicates$Name))

uniqueDups <- unique(duplicates$`Email Domain`)
uniquesDups.df<- data.frame(uniqueDups)

print("===============================================================================")
if(nrow(uniquesDups.df)<=0){
	print("no dups found")
}else{
	print(paste("list of",nrow(uniquesDups.df),"duplicated emailDomains used in multiple Streak boxes"))
	print(uniquesDups.df)

	print("===============================================================================")
	print("Streak Boxes where each duplicate domain is used:")
	dups <- NULL

	for( i in 1:nrow(uniquesDups.df)){
  	if(!is.null(uniquesDups.df[i,])){
	  if(!is.na(uniquesDups.df[i,])){
		dom <- as.character(uniquesDups.df[i,])
		print(paste("duplicate domain",i,":",dom))
		print(nonEmptyCompanies$Company[nonEmptyCompanies$'Email Domain' == dom])
		print("-------------------------------------------------------------")

		dupdom <- cbind(dom,
			            nonEmptyCompanies$box_name[nonEmptyCompanies$'Email Domain' == dom],
			            nonEmptyCompanies$District[nonEmptyCompanies$'Email Domain' == dom],
			            nonEmptyCompanies$Source[nonEmptyCompanies$'Email Domain' == dom],
			            nonEmptyCompanies$`Owned By`[nonEmptyCompanies$'Email Domain' == dom]) 
		dupdom <- as.data.frame(dupdom) 
#		dups <- rbind( dups, as.data.frame(dom))
		dups <- rbind( dups, dupdom)
  		}
  	  }
	}
	colnames(dups) <- c("Domain","box_name","District","Source","Owned by")
}

# Workaround: Prosprworks doesn't have parent/child relationship for companies, 
# and uses the email domain as a unique key for companies. 
# We have some schools and districts that share an email domain. 
# So...
# find the parent (District) of each group of companies with a shared domain.
# That will be the company in Prosperworks, the others will be just opportunities
# attached to the parent company
# And also - check there is exactly one Parent (District) for each email domain ?

# ALSO if a box name contains RENEWAL or UPSELL, we should treat it like a child.

print("===============================================================================")

# make a list of parents only
parents <- dups[
				dups$District==TRUE &
				dups$Source!="Renewal" &
				dups$Source!="Upsell",]
children <- dups[
				dups$District==FALSE |
				dups$Source=="Renewal" |
				dups$Source=="Upsell",]

print("List of Parents: -------------------------------------------------------------")
print(parents$box_name)
print("List of Children: -------------------------------------------------------------")
print(children$box_name)
print("-------------------------------------------------------------")

# create a map of child to parent. if we have one parent and 1:n children sharing an email domain, 
# we will be creating a company for the parent in PW, and attaching the child opps at the parent company. 
#
for(i in 1:nrow(children)){
	myDomain <- as.character(children$Domain[i])
	myParent <- as.character(parents$box_name[parents$Domain==myDomain])
	if( length(myParent) == 0) {
		print(paste("Achtung: child",children$box_name[i],"has no parent defined"))

		# maybe its an orignal and a renewal for a school level.
		originalDealName <- children$box_name[children$Domain==myDomain & children$Source != "Renewal" & children$Source != "Upsell"]
	    if(length(originalDealName) != 1) {
	    	print("Achtung: and I can't find a non-district, non-renewal/upsell box to treat as its parent either")
	    }else{
	    	print(paste("Self-corrected:  I found a non-district, non-renewal/upsell box",originalDealName,"to treat as its parent"))
	    	children$Parent[i] <- originalDealName
		}
	}
	else children$Parent[i] <- myParent
}
# check if any parents have same domain
dupParents <- parents[duplicated(parents$Domain),] 
if(nrow(dupParents) != 0) {
	print("Achtung: can't have multiple parents on same domain:")
	print(dupParents)
}

# in companiesAndOpps data frame, replace company name with parent company name, so export_opps.R will use that
for( i in 1:nrow(children)) {
  childName <- as.character(children$box_name[i])
  parentName <- as.character(children$Parent[i])
  companiesAndOpps$Company[companiesAndOpps$Company == childName] <- parentName
#  companies$Name[companies$Name ==childName] <- parentName
}

# in the companies dataframe, delete the children (bc we dont need in list of companies - 
# we only need them in export dataframe still for the opps)

print(paste("marking",nrow(children),"children from companies list that we don't want to create companies from in Pw (only opps)"))

companiesAndOpps$Child <- FALSE
for( i in 1:nrow(children)) {
	childName <- as.character(children$box_name[i])
	companiesAndOpps$Child[companiesAndOpps$box_name == childName]<- TRUE
}


# ----------------------------------------------------------------------------
#write to CSV file
# ----------------------------------------------------------------------------
write.csv( dups, file = outputDomainDupsFileName)
write.csv( children, file = outputChildrenFileName)
 
print("add_email_domain_to_companies_list_dups.R: exit") 
print("===============================================================================")

#sink() # terminate teeing stdout to the logFile

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


