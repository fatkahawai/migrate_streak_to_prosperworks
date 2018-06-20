##############################################################################
#'  Migrate CRM data from Streak to ProsperWorks
#' 
#' 
#' @see [\url{http://streak.com}
#' @see [\url{http://prosperworks.com}
#' 
#' For importing activity data into ProsperWorks using their Google Sheet macro:
#' @see [\url{https://support.prosperworks.com/hc/en-us/articles/115002177803-How-do-I-import-activities-}
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

sink( file="log/migrate_all.log", append=FALSE, type= c("output","message"), split=TRUE)

print("DID YOU OPEN STREAK DOWNLOAD IN CSV EDITOR AND SAVE AS LATIN WITH QUOTE ALL FIELDS checkbox??")

source("process_export.R")

command <- readline("Enter 'Y' to call streak API (allow several hours). Or 'X' to stop execution here. Or just hit Enter to skip this step...") 
if(command	== "X") 
	stop("terminated by user")
if(command == "Y")
	source("call_streak_api.R")


source("add_company_and_opp_to_contacts.R") # and stats stage, and also parsed domain from contacts email address
source("add_email_domain_to_companies_first_pass.R") # use domains from streak contacts list


source("export_people_and_leads.R")

source("add_missing_maincontact_to_companies.R")

source("add_email_domain_to_companies_list_dups.R")


source("export_companies.R")
source("export_opportunities.R")

source("add_company_and_opp_to_tasks.R")

source("add_company_and_opp_to_meetingnotes.R")

# While testing, create a mini subset of all output files you can quickly upload and verify in ProsperWorks, and then roll back. 
# Once this is working, you can test an upload of the full dataset. 

source("mini_export.R")

print("Next Steps: Now do this..")
print("1. import Companies, Opps, Contacts, Leads into ProsperWorks, then")
print("2. import Opps from PW into the Google sheet for the activites upload")
print("Make a copy of the Sheet as described here https://support.prosperworks.com/hc/en-us/articles/115002177803-How-do-I-import-activities-")
print("3. download the Opps table from the google sheet, call it 'PW Import Activities - opportunities -header.csv'")
print("4. run 'source(add_streak_xref_to_pw_opps_table.R)' to add the Company Name to the prosperworks opps table")
print("5. reimport Opps into google sheet with streak xref column, and run import activities")
#source("add_streak_xref_to_pw_opps_table.R")

sink() # terminate teeing stdout to the logFile

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


