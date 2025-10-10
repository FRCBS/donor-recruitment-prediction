# donor-recruitment-prediction
Code for predicting future donation amounts with from historical donation data.

# Spreadsheet tool
## Participating blood establishments
The followin codes are used for the blood establishment:
- au: Australian Red Cross Lifeblood (Australia)
- ct: Blood and Tissue Bank (BST, Catalonia, Spain)
- fi: Finnish Red Cross Blood establishment (FRCBS, Finland)
- fr: Établissement français du sang (France)
- nc: Banco de Sangre (Navarre, Spain)
- nl: Sanquin Blood Supply Foundation (Netherlands)
  
## Source data 
### Predicted donation activity
The spreadsheet tool contains the predicted donation activity as a function of time since the year of first donation estimated for: 
- three different models: **log.separately**, **log.lump**, and **power.lump**, which correspond to the models (d), (a) and (b) in Figure 2 of the manuscript, respectively
- each participating blood establishment
- O-negative donors (**Oneg**) and all donors (**all**)
This data is on sheet **main** but hidden by default and it is not recommended to manipulated directly.
### Number of new donors
The sheet **nr of new donors** contains the number of new donors per blood establishment and year. This sheet is protected by default and it is not recommended to be edited. Formulas on sheet **main** (column *number of new donors*) link to this data based on the selections made in the paremeters section of that sheet.
### Coefficients
This sheet contains the coefficients *multiplier* and *exponent* and their confidence intervals, explained in detail in the manuscript, for each combination of blood group and establishment.
This sheet is protected by default and it is not recommended to be edited.
The coefficients can be used to estimate the relative donation activity (as reflected by *cdon50* values) between blood establishments, eg. to assess what level activity could be reached by altering the management process; the ratio between the maximum achievable activity and current activity can then be entered in the *activity multiplier* column.
### Actual donations
This sheet contains the number of actual donations per year for each blood establishment and blood group.
Please note that sum of actual donations on a year is *not equal to* the number of donations recorded in the raw data for that year. Instead, for each donor, the number of donations during one year since the donor's date of first donation is recorded for the year of first donation, and similarly for other years.

## Functionality
