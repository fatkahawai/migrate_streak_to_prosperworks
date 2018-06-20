## What's this?

This is a project in R for migrating table-based data :
 * From the Streak CRM prodcut,
 * To the ProsperWorks CRM product 


## Quick start

- Make sure all libraries are installed in RStudio, using the install.packages() command. 
- This project was developed on RStudio v1.1.383


## App structure

The application has a structure like this:
- the idepedendently executable source files are in the project root
- source files containing utility functions only are in '~/util'
- input data exported from Streak is stored in '~/data'
- logs are written into their own '~/logs' folder
- any test source code and results are in '~/test'
- output files for import into Prosperworks are stored to '~/output'
-

## Inputs (Export from Streak)
There are 2 ways to get data out of Streak, and we will need to use both:
- Export static table data to a CSV file
- the API for temporal data - call logs, notes, etc

## Outputs (Import to Prosperworks)
Similarly, aside from manual data entry, there are three ways to get data into ProsperWorks:
- upload CSV files to static tables of People, Companies, Opportunities, and Leads
- the API for call logs, meeting notes etc
- a Google Sheet macro provided by the Prosperworks team, which wraps the API and enables relatively simple import of structured temporal data

## Custom Fields defined in Prosperworks
We have created these csutom fields in Pw, for which we will need to create columns in the import File:
- Renewal
- Upsell
- Users Domain
- Vertical
- Year
- Admin Panel 


## Styleguide for the project

- 2 spaces for indentation
- Line length should be 80 (that's a soft limit, 82-83 for example is ok provided these are just a few exceptions)
- Braces go on the same line as the statement
- Vars should always be declared at the top
- Variables and properties should use lower camel case capitalization

## References
- http://www.streak.com
- http://www.prosperworks.com
