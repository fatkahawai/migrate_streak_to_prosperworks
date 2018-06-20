##############################################################################
#' Migrate Data from Streak to ProsperWorks
#' 
#' LOAD PW Activity/task/meetingnotes import GoogleSheet, and add a streak box key as XRef 
#' 
#' then we can paste the table from streak_meeting_notes.csv into the 
#' PW Activity import Sheet, to import those meeting records and link to the right opportunities
#'
#' 
#' @see [\url{http://streak.com}
#' @see [\url{http://prosperworks.com}
#' 
#' For importing activity data into ProsperWorks using their Google Sheet macro:
#' @see [\url{https://support.prosperworks.com/hc/en-us/articles/115002177803-How-do-I-import-activities-}
#' @see [\url{https://support.prosperworks.com/hc/en-us/articles/115002878432-How-to-import-tasks}
#'
#' @version 1.0
#' @author Robert Drummond
#' (C) 2017
##############################################################################

library(lubridate)
library(stringr)

# INPUT
prosperWorksInputFileName <- "data/PW Import Activities - opportunities -header.csv"
streakBoxesFileName <- 	"data/streak_boxes.csv"

#OUTPUT
prosperWorksOutputFileName <- "output/PW Import Activities - with Streak XREF - opportunities.csv"
missingFileName <- "output/missing.csv"
multiplesFileName <- "output/multiples.csv"

# ----------------------------------------------------------------------------
# MAIN
# ----------------------------------------------------------------------------

pwOpps.df <- read.csv(prosperWorksInputFileName, 
	check.names=FALSE, 
	header = TRUE, 
	stringsAsFactors = FALSE,
	blank.lines.skip = TRUE,
	strip.white = TRUE)

print(paste("loaded",nrow(pwOpps.df),"pwOpps.df from", prosperWorksInputFileName))

boxes <- read.csv(streakBoxesFileName, 
	check.names=FALSE, 
	header = TRUE, 
	stringsAsFactors = FALSE,
	blank.lines.skip = TRUE,
	strip.white = TRUE)

print(paste("loaded",nrow(boxes),"boxes from",streakBoxesFileName))
#print(paste("of which",nrow(boxes[is.na(boxes$emailDomain)]), " have no email domain"))

# pwOpps.df <- data.frame(pwOpps)

# insert a column for the streak Key, initially blank

pwOpps.df$streakKey <- ""

multiples <- data.frame(NULL)
missing <- data.frame(NULL)

boxes$name <- str_trim(iconv(boxes$name, from = "UTF-8", to = "Latin1", sub = " "))


# for each row in the list of Opportunites downloaded from PW
for( i in 1:nrow(pwOpps.df)){

# $$$ DEBUG
	if((i %% 100) == 0) {cat(i,":")}
# $$$ DEBUG

	# get the opp name in PW opps list 
#	opName<- str_trim(iconv(pwOpps.df$primary_organization[i], from = "UTF-8", to = "ASCII", sub = " "))
	opName<- str_trim(iconv(pwOpps.df$Name[i], from = "UTF-8", to = "Latin1", sub = " "))
	coName<- str_trim(iconv(pwOpps.df$Company[i], from = "UTF-8", to = "ASCII", sub = "_"))
# $$$ DEBUG
#	print(name)
# $$$ DEBUG

	if( length(boxes$key[boxes$name == opName]) == 0 ){
		print(paste("no match in streak box found for",opName))
		missing <- rbind(missing,opName)
	} else {
		if(length(boxes$key[boxes$name == opName]) > 1){
			print(paste("multiple matches found for",opName,"using",boxes$key[boxes$name == opName][1]))
			multiples <- rbind(multiples,opName)
			pwOpps.df$streakKey[i] <- boxes$key[boxes$name == opName][1]
		} else{
			pwOpps.df$streakKey[i] <- boxes$key[boxes$name == opName] 
		} # else
	} # else
} # for

colnames(multiples) <- "names"
colnames(missing) <- "names"


View(pwOpps.df)

#write to CSV file
write.csv(pwOpps.df, file=prosperWorksOutputFileName)

write.csv(multiples, file=multiplesFileName)
write.csv(missing, file=missingFileName)

# ----------------------------------------------------------------------------
# END OF FILE
# ----------------------------------------------------------------------------


