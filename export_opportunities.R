##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' EXPORT OPPS
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(stringr)
library(dplyr)


# ----------------------------------------------------------------------------
# FUNCTION mapToProsperWorks
# ----------------------------------------------------------------------------
mapOppToProsperWorks <- function(boxes){

opps <- data.frame(Name=boxes$'Opportunity')

opps$Opportunity <- boxes$Opportunity
opps$Company <- boxes$Company

# combine details and notes into details column
opps$Details <- boxes$CompanyDetails

opps$'Primary Person Contact' <- boxes$'Primary Person Contact'

opps$Status <- boxes$Status
opps$'Owned By' <- boxes$'Owned By' 

opps$Value <- boxes$Value
opps$Priority <- boxes$Priority

opps$Pipeline <- boxes$Pipeline
opps$Stage <- boxes$Stage
opps$'Win Probability' <- boxes$'Win Probability'

opps$'Exp. to Close' <- boxes$'Exp. to Close'

opps$Source <- boxes$Source

opps$'Loss Reason' <- boxes$'Loss Reason'

opps$Tag <- boxes$Tag
opps$'Vertical' <- boxes$'Vertical'
opps$'Users to License' <- boxes$'Users to License'

opps$Renewal <- boxes$Renewal
opps$Upsell <- boxes$Upsell

opps$'Parent Domain' <- boxes$'Parent Domain'
opps$'Users Domain' <- boxes$'Users Domain'


#clean up - dump any rows that are just leads
opps<- opps[opps$Stage != "Prospect",]

# IF Primary person is empty, find a contact from the prosperWorksImportPeople table
for(i in 1:nrow(opps)) {
	candidates<- dplyr::filter(prosperWorksImportPeople,Opportunity==opps$Opportunity[i])
	if(nrow(candidates)>0){
		opps$`Primary Person Contact`[i] <- ifelse(opps$`Primary Person Contact`[i]=="",candidates$Name[1],opps$`Primary Person Contact`[i])
#$$$ DEBUG
#		print(paste("used",candidates$Name[1],"for missing Primary Person Contact for",opps$Opportunity[i]))
#$$$
	} # if nrow
} # for
return(opps)
}
# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------
print("export_opportunities.R: entry")

# dump any in Prospect stage - those are just leads
reducedExport<- companiesAndOpps[companiesAndOpps$Stage != "Prospect",]
print(paste("removed Prospects (will be imported separately as Leads). leaving",nrow(reducedExport),"companies and opps"))

prosperWorksImportOpps <- mapOppToProsperWorks(reducedExport)

View(prosperWorksImportOpps)

#write to CSV file
write.csv(prosperWorksImportOpps,"output/import_opps.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
print("export_opportunities.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


