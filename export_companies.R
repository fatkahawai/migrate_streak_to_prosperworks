##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' EXPORT COMPANIES
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

# Libraries
library(stringr)
library(dplyr)

# INPUT
# companiesAndOpps 

# ----------------------------------------------------------------------------
# FUNCTION mapToProsperWorksCompany
# ----------------------------------------------------------------------------
mapToProsperWorksCompany <- function(boxes){

print(paste("entry mapToProsperWorksCompany with",nrow(boxes),"boxes"))

company <- data.frame(Name=boxes$Company)

company$Company <- boxes$Company
company$'Owned By' <- boxes$'Owned By' # Boxes$'Assigned To'
company$Source <- boxes$Source
company$Street <- boxes$Street
company$City <- boxes$City
company$State <- boxes$State
company$'Postal Code' <- boxes$'Postal Code'
company$Stage <- boxes$Stage


company$'Contact type' <- boxes$'Contact type'
company$Country <- boxes$Country
company$'Work Phone' <- boxes$'Work Phone'
company$'Email Domain' <- boxes$'Email Domain'
company$'District' <- boxes$District
company$'Users Domain' <- boxes$'Users Domain'
company$'Vertical' <- boxes$Vertical


company$'Main Contact (Decision Maker)' <- boxes$'Main Contact (Decision Maker)'
company$'Main Contact Position' <- boxes$'Main Contact Position'
company$'Main Contact Phone' <- boxes$'Main Contact Phone'
company$'Main Contact Email' <- boxes$'Main Contact Email'

company$'Contact (Champion)' <- boxes$'Contact (Champion)'
company$'Contact (Champion) Position' <- boxes$'Contact (Champion) Position'
company$'Contact (Champion) Phone' <- boxes$'Contact (Champion) Phone'
company$'Contact (Champion) Email' <- boxes$'Contact (Champion) Email'

print(paste("we have",nrow(company),"companies"))

# filter out children dups (where we have one company and multiple opps)
company <- company[boxes$Child==FALSE,]
print(paste("after filtered child entries, now",nrow(company),"companies"))

return(company)
}
# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------

print("export_companies.R: entry")

# dump any in Prospect stage - those are just leads
reducedExport<- companiesAndOpps[companiesAndOpps$Stage != "Prospect",]
print(paste("removed Prospects (will be imported separately as Leads). leaving",nrow(reducedExport),"companies and opps"))

companies <- mapToProsperWorksCompany(reducedExport)

View(companies)

#write to CSV file
write.csv(companies,"output/import_companies.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
print("export_companies.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


