##############################################################################
# Migrate Data from Streak to ProsperWorks
# 
# EXPORT LEADS
#
# VERSION: 1.0
# AUTHOR: Bob
# (C)Kami 2017
##############################################################################

library(stringr)

#exportFileName <- paste0(
#	"~/Downloads/",
#	"Prospects ",
#	"Streak Export_ Master Sales Pipeline () - Boxes (Master Sales Pipeline).csv"
#	)

# ----------------------------------------------------------------------------
# FUNCTION mapToProsperWorks
# ----------------------------------------------------------------------------
mapLeadToProsperWorks <- function(boxes,firstname,lastname,position,phone,email,tag){

firstname <- ifelse(is.na(firstname), "", str_trim(firstname))
lastname <- ifelse(is.na(lastname), "", str_trim(lastname))
position <- ifelse(is.na(position), "", str_trim(position))
phone <- ifelse(is.na(phone), "", str_trim(phone))
email <- ifelse(is.na(email), "", str_trim(email))

lead <- data.frame(Updated=boxes$'Date Last Updated')

lead$`First Name` <- firstname
lead$'Middle Name' <- ""
lead$'Last Name' <- lastname
lead$Prefix <- ""
lead$Suffix <- ""
lead$Title <- position

# if there's a kami admin panel link in Details, copy it to the new custom column
lead$'Open Kami Admin'<- ifelse( substr(boxes$Details,1,12) == "https://api.", boxes$Details, "")

# combine details and notes into details column
lead$Details <- paste(ifelse(is.na(boxes$Details),"", boxes$Details), ifelse(is.na(boxes$Notes),"", boxes$Notes))
# but remove any admin panel link
# at start
#lead$Details <- ifelse(substr(lead$Details,1,39)=="https://api.notablepdf.com/admin/users/", substr(lead$Details,48,nchar(lead$Details)),lead$Details)
# or anywhere
pos <- regexpr("https://api.notablepdf.com/admin/users/", lead$Details)
lead$Details <- ifelse(pos<1, lead$Details, paste0( substr(lead$Details, 1, pos-1), substr(lead$Details, pos+46, nchar(lead$Details))) )
lead$Details <- str_trim(lead$Details)


lead$Company <- str_trim(boxes$Company)
lead$'Email Domain' <- boxes$'Domain 1'


lead$'Owned By' <- boxes$Owner # Boxes$'Assigned To'
lead$'Owned By' <- ifelse(lead$'Owned By'=="Sanjana","Ayushi",lead$'Owned By')
lead$'Owned By' <- ifelse(lead$'Owned By'=="Sales","Carmela",lead$'Owned By')

lead$Source <- boxes$'Lead Source'
lead$Source <- ifelse(lead$Source=="","Outbound",lead$Source)

lead$Street <- ""
lead$City <- ""
lead$'Postal Code' <- ""

lead$Country <- "United States"
lead$Country <- ifelse(boxes$State == "United Kingdom", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "France", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Sweden", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Germany", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Belgium", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Norway", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Ireland", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Netherlands", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Thailand", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Egypt", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Taiwan", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Australia", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "New Zealand", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Canada", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Brazil", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Argentina", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Japan", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "South Korea", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "India", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Pakistan", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Israel", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Italy", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Poland", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Switzerland", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Denmark", boxes$State, lead$Country)
lead$Country <- ifelse(boxes$State == "Mexico", boxes$State, lead$Country)

# lead$State <- ifelse(lead$Country == "United States", lead$State, "")

lead$'Work Phone' <- substr(phone,0,29)
lead$'Mobile Phone' <- ""
lead$Email <- email

lead$'Personal Website' <- ""
lead$'Lead Status' <- "Unqualified"
lead$'District' <- ifelse(toupper(boxes$Type)=='DISTRICT',TRUE,FALSE)
lead$Tag <- tag

lead$'Vertical' <- "Education"
lead$'Parent Company' <- ""
lead$'Users to License' <- "0"
lead$'Total Population' <- "0"
lead$'School Year' <- "SY2017-18"
lead$Renewal <- FALSE
lead$Upsell <- FALSE

#lead$'Parent Domain' <- ""

lead$'User Domain' <- boxes$'Domain 1'
lead$'User Domain' <- sub("^https://api\\.notablepdf\\.com.*","deleted",lead$'User Domain')

lead$'User Domain' <- ifelse(substr(boxes$'Domain 1',1,7)=="http://", substr(boxes$'Domain 1',8,nchar(boxes$'Domain 1')),boxes$'Domain 1')
lead$'User Domain' <- ifelse(substr(lead$'User Domain',1,8)=="https://", substr(boxes$'Domain 1',9,nchar(lead$'User Domain')),lead$'User Domain')
lead$'User Domain' <- str_trim(lead$'User Domain')

# take boxes in Stage "Prospects" only
lead <- lead[boxes$Stage == "Prospect",]

# remove duplicates - look only for same email address 
lead <- lead[!duplicated(lead$Email),]

#clean up - remove rows with no contact name
#lead<- lead[!is.na(lead$Email),]
#lead<- lead[lead$Email == "",]

return(lead)
}
# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------


# export <- read.table(exportFileName, 
# 	check.names=FALSE, 
# 	header = TRUE, 
# 	encoding = "UTF-8",
# 	sep=",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
# 	stringsAsFactors = FALSE,
# 	blank.lines.skip = TRUE,
# 	strip.white = TRUE)

#remove intl chars
#export$'Contact (Champion)' <- str_trim(export$'Contact (Champion)')
#export$'Contact (Champion)' <- iconv(export$'Contact (Champion)', from = "UTF-8", to = "ASCII", sub = " ")
#export$'Main Contact (Decision Maker)' <- str_trim(export$'Main Contact (Decision Maker)')
#export$'Main Contact (Decision Maker)' <- iconv(export$'Main Contact (Decision Maker)', from = "UTF-8", to = "ASCII", sub = " ")
#export$'Name' <- str_trim(export$'Name')
#export$'Name' <- iconv(export$'Name', from = "UTF-8", to = "ASCII", sub = " ")

# add helper columns
export$'Contact (Champion) First Name' <- sapply(strsplit(export$'Contact (Champion)', "\\s+"), `[`, 1)
export$'Contact (Champion) Last Name' <- sapply(strsplit(export$'Contact (Champion)', "\\s+"), `[`, 2)
export$'Main Contact First Name' <- sapply(strsplit(export$'Main Contact (Decision Maker)', "\\s+"), `[`, 1)
export$'Main Contact Last Name' <- sapply(strsplit(export$'Main Contact (Decision Maker)', "\\s+"), `[`, 2)

# tidy up assigned Tos
#export$Ownerlower <- export$'Assigned To'
#export$Ownerlower <- ifelse(is.na(export$Ownerlower),"bob@kamihq.com", export$Ownerlower )
#export$Ownerlower <- ifelse(export$Ownerlower=="","bob@kamihq.com", export$Ownerlower )
#export$Ownerlower <- ifelse(export$Ownerlower=="sales@kamihq.com","alliv@kamihq.com", export$Ownerlower)
#export$Ownerlower <- ifelse(export$Ownerlower=="sanjana@kamihq.com","ayushi@kamihq.com", export$Ownerlower)
#export$Ownerlower <- sapply(strsplit(export$Ownerlower,"@"), `[`, 1)
#export$Owner <- paste0(toupper(substr(export$Ownerlower, 1, 1)), substr(export$Ownerlower, 2, nchar(export$Ownerlower)))

#View(export)


main <- mapLeadToProsperWorks(export,
	export$'Main Contact First Name',
	export$'Main Contact Last Name',
	export$'Main Contact Position',
	export$'Main Contact Phone',
	export$'Main Contact Email',
	"Buyer")

champion <- mapLeadToProsperWorks(export,
	export$'Contact (Champion) First Name',
	export$'Contact (Champion) Last Name',
	export$'Contact (Champion) Position',
	export$'Contact (Champion) Phone',
	export$'Contact (Champion) Email',
	"Champion")


#concatenate the data frames
prosperWorksImportLeads<-rbind(main,champion)

View(prosperWorksImportLeads)

#write to CSV file
write.csv(prosperWorksImportLeads, 
		file="import_leads.csv")
#write.table(prosperWorksImportLeads, 
#		file="~/Downloads/import_leads.csv",
#		header = TRUE,
#		sep=",", 
#		quote= TRUE,
#		na = ""
#		)

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


