##############################################################################
#' mini export
#' create a subset of each import CSV for a quicker test cycle 
#' 
#' @see [\url{http://prosperworks.com}
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(dplyr)

print("mini_export.R: entry")

# filter a subset of the data by some criteria that will filter out ca 90% of the data 
mini_companies <- dplyr::filter(companies,`Owned By`=="Bob")
mini_opps <- dplyr::filter(prosperWorksImportOpps,`Owned By`=="Bob")
mini_contacts <- dplyr::filter(prosperWorksImportPeople,`Owned By`=="Bob")
mini_tasks <- dplyr::filter(allTasks.df,owner=="Bob")
mini_meetings <- dplyr::filter(allMeetings.df,owner=="Bob")

mini_leads <- prosperWorksImportLeads[]
#View(mini_companies)
#View(mini_opps)
#View(mini_contacts)
#View(mini_tasks)
#View(mini_meetings)

# save subset datasets to CSV files
write.csv(mini_companies,"output/mini_companies.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
write.csv(mini_opps,"output/mini_opps.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
write.csv(mini_contacts,"output/mini_contacts.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
write.csv(mini_tasks,"output/mini_tasks.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")
write.csv(mini_meetings,"output/mini_meetings.csv",col.names=TRUE,row.names=FALSE,na="",fileEncoding="latin1")

print("mini_export.R: exit")
