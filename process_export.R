##############################################################################
#' process export
#' Migrating Data from Streak to ProsperWorks
#' process the Streak export File - clean data, mutate , add helper columns
#' 
#' @see [\url{http://google.com}
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

# Libraries
library(dplyr)
library(stringr)

# --- INPUT
# the CSV file containing the Pipeline exported from Streak
exportFileName <- "data/Streak_Export_Boxes.csv"

# ---- OUTPUT
# export - data frame, and 
# companiesAndOpps - data frame
# no files are written 

# ----------------------------------------------------------------------------
#' @function mapToProsperWorks 
#' @param boxes - a data frame. The boxes from Streak
#' @notes
#' Clean, add helper columns, and columns required by PW. Convert formats  
#' @returns companyAndOppData - a data frame with the columns Pw Needs
# ----------------------------------------------------------------------------
mapToProsperWorks <- function(boxes){

print(paste("entry: mapToProsperWorks - processing",nrow(boxes),"boxes"))

companyAndOppData <- data.frame(Updated=boxes$'Date Last Updated')

companyAndOppData$box_name <- boxes$box_name
companyAndOppData$Company <- boxes$Company
companyAndOppData$Opportunity <- boxes$Opportunity

companyAndOppData$'Owned By' <- boxes$Owner # Boxes$'Assigned To'

people$Source <- ifelse(people$Source=="","Outbound",people$Source)

companyAndOppData$Street <- ""
companyAndOppData$City <- ""
companyAndOppData$State <- boxes$State
companyAndOppData$'Postal Code' <- ""

# map stages defined in Streak to Stages defined in Pw
companyAndOppData$Status <- "Open"
companyAndOppData$Status <- ifelse(boxes$Stage == "Closed - Lost", "Lost", companyAndOppData$Status)
companyAndOppData$Status <- ifelse(boxes$Stage == "PO Received / Waiting Payment", "Won", companyAndOppData$Status)
companyAndOppData$Status <- ifelse(boxes$Stage == "Check/Payment Received - WON", "Won", companyAndOppData$Status)
companyAndOppData$Status <- ifelse(boxes$Stage == "Prospect", "Unqualified", companyAndOppData$Status)
companyAndOppData$Status <- ifelse(boxes$Stage == "Responded", "Calling (engaged)", companyAndOppData$Status)


companyAndOppData$Stage <- boxes$Stage

# eliminate the responded stage we don't need
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Responded", "Prospect", companyAndOppData$Stage)

companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Qualified (MQL)", "Qualified", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "In Conversation (SQL)" , "In Discussion", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Verbal Quote", "In Discussion", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Teacher Plan", "In Discussion", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "PO Received / Waiting Payment", "PO Received", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Negotiating", "Negotiation", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Check/Payment Received - WON", "Closed", companyAndOppData$Stage)
companyAndOppData$Stage <- ifelse(companyAndOppData$Stage == "Closed - Lost", "Closed", companyAndOppData$Stage)

# combine details and notes into details column (MAX 250 Chars PW will import)
companyAndOppData$Details <- paste(boxes$Details, boxes$Notes)
companyAndOppData$Details <- substr(companyAndOppData$Details,1,249)
companyAndOppData$Details <- sub(pattern,"",companyAndOppData$Details)

companyAndOppData$Value <- str_extract(boxes$'Deal Size US$', "\\d+")
companyAndOppData$Value <- ifelse(is.na(companyAndOppData$Value), "0", companyAndOppData$Value)

companyAndOppData$Priority <- "None"
companyAndOppData$Priority <- ifelse(companyAndOppData$Value >= 2000, "High", companyAndOppData$Priority)

companyAndOppData$'Win Probability' <- 0
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "Qualified", 5, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "On Hold", 5, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "In Discussion", 10, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "Trial", 20, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "Negotiation", 30, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "Quote Sent", 40, companyAndOppData$'Win Probability' )
companyAndOppData$'Win Probability' <- ifelse(companyAndOppData$Stage == "PO Received", 99, companyAndOppData$'Win Probability' )


companyAndOppData$'Exp. to Close' <- ifelse(companyAndOppData$Stage== "Closed", boxes$'Date of Last Email', "12/31/2018")

# Map lead sources from Streak defs to Pw defs
companyAndOppData$Source <- boxes$'Lead Source'
companyAndOppData$Source <- ifelse(is.na(companyAndOppData$Source),"Outbound",companyAndOppData$Source)
companyAndOppData$Source <- ifelse(companyAndOppData$Source=="","Outbound",companyAndOppData$Source)

companyAndOppData$Source <- ifelse(tolower(companyAndOppData$Source)=="conference","Conference",companyAndOppData$Source)

companyAndOppData$'Contact type' <- "Potential Customer"
companyAndOppData$'Contact type' <- ifelse(companyAndOppData$Renewal==TRUE,"Current Customer",companyAndOppData$'Contact type')
companyAndOppData$'Contact type' <- ifelse(companyAndOppData$Upsell==TRUE,"Current Customer",companyAndOppData$'Contact type')
companyAndOppData$'Contact type' <- ifelse(companyAndOppData$Stage=="Closed","Current Customer",companyAndOppData$'Contact type')


companyAndOppData$Country <- "United States"
companyAndOppData$Country <- ifelse(boxes$State == "United Kingdom", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "France", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Sweden", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Germany", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Belgium", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Norway", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Ireland", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Netherlands", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Thailand", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Egypt", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Taiwan", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Australia", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "New Zealand", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Canada", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Brazil", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Argentina", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Japan", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "South Korea", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "India", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Pakistan", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Israel", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Italy", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Poland", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Switzerland", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Denmark", boxes$State, companyAndOppData$Country)
companyAndOppData$Country <- ifelse(boxes$State == "Mexico", boxes$State, companyAndOppData$Country)


companyAndOppData$'Work Phone' <- substr(boxes$'Main Contact Phone',1,29)

companyAndOppData$'Email Domain' <- boxes$tidyEmailDomain
companyAndOppData$'Loss Reason' <- ""

companyAndOppData$Tag <- ""


# columns required for the Custom Fields we have defined in Prosperworks
companyAndOppData$'Users to License' <- "0"

companyAndOppData$'School Year' <- ifelse(companyAndOppData$Stage!= "Closed", "SY2018-19", "SY2017-18")

companyAndOppData$Renewal <- FALSE
companyAndOppData$Renewal <- ifelse(boxes$'Account Age' =="Second Year", TRUE, companyAndOppData$Renewal)
companyAndOppData$Renewal <- ifelse(boxes$'Account Age' =="Third Year", TRUE, companyAndOppData$Renewal)
companyAndOppData$Renewal <- ifelse(companyAndOppData$Source== "Renewal", TRUE, companyAndOppData$Renewal)

companyAndOppData$Upsell <- ifelse(companyAndOppData$Source== "Upsell", TRUE, FALSE)
companyAndOppData$'Users Domain' <- companyAndOppData$'Email Domain'

companyAndOppData$'District' <- ifelse(toupper(boxes$Type)=='DISTRICT',TRUE,FALSE)

companyAndOppData$'Vertical' <- "Education" # default
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="district","Education",companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="school","Education",companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="channel partner",boxes$Type,companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="partner",boxes$Type,companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="api",boxes$Type,companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="non-profit",boxes$Type,companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="sme",boxes$Type,companyAndOppData$'Vertical' )
companyAndOppData$'Vertical' <- ifelse(tolower(boxes$Type)=="enterprise",boxes$Type,companyAndOppData$'Vertical' )

companyAndOppData$'School Year' <- ifelse(companyAndOppData$Vertical!= "Education", "", companyAndOppData$'School Year')

companyAndOppData$Pipeline <- "Sales"
companyAndOppData$Pipeline <- ifelse(tolower(companyAndOppData$Vertical)=="partner","Business Development",companyAndOppData$Pipeline)
companyAndOppData$Pipeline <- ifelse(tolower(companyAndOppData$Vertical)=="api","Business Development",companyAndOppData$Pipeline)
companyAndOppData$Pipeline <- ifelse(tolower(companyAndOppData$Vertical)=="channel partner","Business Development",companyAndOppData$Pipeline)

companyAndOppData$Stage <- ifelse(companyAndOppData$Pipeline=="Business Development","First Meeting",companyAndOppData$Stage)

companyAndOppData$'Main Contact (Decision Maker)' <- boxes$'Main Contact (Decision Maker)'
companyAndOppData$'Main Contact Position' <- boxes$'Main Contact Position'
companyAndOppData$'Main Contact Phone' <- boxes$'Main Contact Phone'
companyAndOppData$'Main Contact Email' <- boxes$'Main Contact Email'

companyAndOppData$'Contact (Champion)' <- boxes$'Contact (Champion)'
companyAndOppData$'Contact (Champion) Position' <- boxes$'Contact (Champion) Position'
companyAndOppData$'Contact (Champion) Phone' <- boxes$'Contact (Champion) Phone'
companyAndOppData$'Contact (Champion) Email' <- boxes$'Contact (Champion) Email'

companyAndOppData$'Primary Person Contact' <- boxes$'Main Contact (Decision Maker)'
companyAndOppData$'Primary Person Contact' <- ifelse(companyAndOppData$'Primary Person Contact'=="",boxes$'Contact (Champion)',companyAndOppData$'Primary Person Contact')

print(paste("after contacts, now",nrow(companyAndOppData),"companies"))

#clean up - remove rows with no name
companyAndOppData<- companyAndOppData[!is.na(companyAndOppData$Company),]
companyAndOppData<- companyAndOppData[companyAndOppData$Company != "",]

print(paste("Exit: mapToProsperWorks - returning",nrow(companyAndOppData),"companies"))

return(companyAndOppData)
}

# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------

print("process_export.R: entry")

exportraw <- read.table(exportFileName, 
                      check.names=FALSE, 
                      header = TRUE, 
                      sep=",",
                      quote="\"",
                      fill = TRUE,
                      stringsAsFactors = FALSE,
                      blank.lines.skip = TRUE,
                      strip.white = TRUE, skipNul=TRUE,
                      fileEncoding="Latin1")
export<- exportraw
export$orig_box_name <- export$Name
export$box_name <- export$Name
export$simplifiedName <- export$Name
export$Name <- NULL

# replace any commas with semi-colons   bc we  use , as our CSV separator
export$simplifiedName <- str_trim(export$simplifiedName)
export$simplifiedName <- sub(",",";",export$simplifiedName)
#export$'Details' <- sub("\"","'",export$'Details')
#export$'Notes' <- sub("\"","'",export$'Notes')

# if the box name has "DUP" prefix manually added, we can ignore it
export<- export[grepl("^DUP .*",export$simplifiedName,ignore.case=TRUE)==FALSE,]
export<- export[grepl("^DUPLICATE .*",export$simplifiedName,ignore.case=TRUE)==FALSE,]

export <- export[export$`Stage` != 'Disqualified',]
export <- export[export$`Stage` != 'Closed - Lost',]
print(paste("removed boxes marked 'DUP*' and any in stage 'Disqualifed' or 'Closed - Lost'. exit mapToProsperWorks with",nrow(export),"companies remaining to import"))

export$'Details' <- sub(",",";",export$'Details')
export$'Notes' <- sub(",",";",export$'Notes')

export$simplifiedName <- sub("’","'",export$simplifiedName)
export$'Details' <- sub("’","'",export$'Details')
export$'Notes' <- sub("’","'",export$'Notes')

export$simplifiedName <- sub("\"","\"\"",export$simplifiedName)

#remove intl chars
export$'Main Contact (Decision Maker)' <- str_trim(export$'Main Contact (Decision Maker)')
export$'Main Contact Position' <- str_trim(export$'Main Contact Position')
export$'Main Contact Phone' <- str_trim(export$'Main Contact Phone')
export$'Main Contact Email' <- str_trim(export$'Main Contact Email')
#export$'Main Contact (Decision Maker)' <- iconv(export$'Main Contact (Decision Maker)', from = "UTF-8", to = "ASCII", sub = " ")
export$'Contact (Champion)' <- str_trim(export$'Contact (Champion)')
export$'Contact (Champion) Position' <- str_trim(export$'Contact (Champion) Position')
export$'Contact (Champion) Phone' <- str_trim(export$'Contact (Champion) Phone')
export$'Contact (Champion) Email' <- str_trim(export$'Contact (Champion) Email')


# ----------------------------------------------------------------------------
# Tidy up Names
# ----------------------------------------------------------------------------

# COMMON ABBREVS

export$simplifiedName <- gsub("incorporated","Inc",export$simplifiedName, ignore.case=TRUE)
export$simplifiedName <- gsub("limited","Ltd",export$simplifiedName, ignore.case=TRUE)

# -----------------------------

export$Company <- export$simplifiedName

# -----------------------------
export$Opportunity <- export$simplifiedName

# add which year to the opp name
export$Opportunity <- paste(export$Opportunity, "-", ifelse(export$'Account Age'=="","First Year", export$'Account Age'))

export$Opportunity <- substr(export$Opportunity,1,249) # max 250 chars for PW

# -----------------------------
# Streak uses the email address for the owner, Pw uses the Name. so strip the name from the email
export$Ownerlower <- strsplit(export$'Assigned To',"@")
export$Ownerlower <- sapply(export$Ownerlower, function (x) x[1])
export$Owner <- paste0(toupper(substr(export$Ownerlower, 1, 1)), substr(export$Ownerlower, 2, nchar(export$Ownerlower)))
export$Ownerlower <- NULL # remove temp helper column

# extract email domain from DOmain 1 field or if empty, extract from the email address of the main contact 
export$tidyEmailDomain <- ifelse(is.na(export$'Domain 1'), "", export$'Domain 1')

xemailDomain1 <- ifelse(is.na(export$'Main Contact Email'),"", export$'Main Contact Email')
xemailDomain2 <- strsplit(xemailDomain1,"@")
xemailDomain3 <- sapply(xemailDomain2, function (x) x[2])
xemailDomain <- ifelse(is.na(xemailDomain3),"", xemailDomain3)

export$tidyEmailDomain <- ifelse(export$tidyEmailDomain!="", export$tidyEmailDomain, xemailDomain)

xemailDomain1 <- ifelse(is.na(export$'Contact (Champion) Email'),"", export$'Contact (Champion) Email')
xemailDomain2 <- strsplit(xemailDomain1,"@")
xemailDomain3 <- sapply(xemailDomain2, function (x) x[2])
xemailDomain <- ifelse(is.na(xemailDomain3),"", xemailDomain3)

export$tidyEmailDomain <- ifelse(export$tidyEmailDomain!="", export$tidyEmailDomain, xemailDomain)

xemailDomain <- NULL
xemailDomain1 <- NULL
xemailDomain2 <- NULL
xemailDomain3 <- NULL

export$tidyEmailDomain <- sub("https://api\\.notablepdf\\.com.*","",export$tidyEmailDomain)

export$tidyEmailDomain <- sub("http://", "",export$tidyEmailDomain)
export$tidyEmailDomain <- sub("https://", "",export$tidyEmailDomain)
export$tidyEmailDomain <- str_trim(export$tidyEmailDomain)

View(export)

companiesAndOpps <- mapToProsperWorks(export)

print("process_export.R: exit")

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


