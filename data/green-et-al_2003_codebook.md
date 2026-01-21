# Codebook: GreenGerberNickerson_JP_2003-1_EDITED

| Variable | Label | Class | Value labels |
| --- | --- | --- | --- |
| city | City of residence | character |  |
| precinct | Name of precinct in city where subject resides | character |  |
| zip | Zipcode for subject's residence | numeric |  |
| race | Subject's race (as indicated on voter file) | character |  |
| party | Subject's party affiliation (as indicated on voter file) | character |  |
| sex | Subject's gender (as indicated on voter file) | character |  |
| age | Subject's age in years at time of experiment | numeric |  |
| turf | Aggregation of households for use of applying treatment | numeric |  |
| voted01 | Dependent variable | haven_labelled | Abstained = 0; Voted = 1 |
| voted00 | Turnout in 2000 general election | haven_labelled | Abstained = 0; Voted = 1 |
| voted99 | Turnout in 1999 general election | haven_labelled | Abstained = 0; Voted = 1 |
| family | Identifier of people sharing address | numeric |  |
| famsize | Number of registered voters residing at address | numeric |  |
| represen | Randomly selected to be listed as a subject for randomization & walksheets | haven_labelled | Not listed = 0; Listed = 1 |
| reached | Contacted by campaign | haven_labelled | No = 0; Yes = 1 |
| other | Number of other cohabitants canvasser spoke with | numeric |  |
| goaway | Canvasser told to go away | haven_labelled | No = 0; Yes = 1 |
| nothome | No answer at door | haven_labelled | No = 0; Yes = 1 |
| bad | Current resident confirmed that person no longer lives at address | haven_labelled | No = 0; Yes = 1 |
| cant | Canvasser unable to reach the door for some reason | haven_labelled | No = 0; Yes = 1 |
| nothing | Canvasser did not indicated disposition of visit -- most likley did not attempt | haven_labelled | No = 0; Yes = 1 |
| contact | Any contact made (either 'reached' or 'other' ==1) | haven_labelled | No = 0; Yes = 1 |
| treatmen | Treatment indicator | haven_labelled | Control = 0; Treatment = 1 |
| primary | Voted in 2001 primary election | haven_labelled | Abstained = 0; Voted = 1 |
