"""
Read in sanquin eprogesa data and export to .csv for project of Timo asikainen from finland timo.o.asikainen@helsinki.fi
data_dir should point to eprogesa files as exported to .sav (which are normally on the L disk).
Should be YYYYDonatiesPlus.sav (multiple yearly files), YYYYDonorsTotaal.sav (only 1 file of latest year) and YYYYDonorAfkeur.sav (only 1 file of latest year)
"""

import os
import numpy as np
import pandas as pd

data_dir = "/mnt/c/Users/potha01m/data/Donaties/"  # data are saved in same dir
np.random.seed(42213998)
# to create fake data or just a sample of the data (CAVE not sure this still works, so keep to False and None)
SHUFFLE = False
NMAX = None  # 100000

START_YEAR = 2009
END_YEAR = 2024

ONLY_VB = True  # only whole blood donations

ONLY_SUCCESSFUL = True  # only donations with volume >200ml

PARQUET = True  # assume .parquet files instead of .spss

# map columns to names as expected by Timo
map_col_names = {
    "KeyID": "releaseID",
    "Donatiedatum": "DonationDate",
    "Donatiecentrumcode": "DonationPlaceCode",
    "Donatiesoortcode": "BloodDonationTypeKey",
    "Geslacht": "Sex",
    "Geboortedatum": "DateOfBirth",
    "Bloedgroep": "BloodGroup",
    "VanafDatum": "DeferralStartDate",
    "TotDatum": "DeferralEndDate",
    "Donatie_Tijd_Start": "DonationTimeStart",
}


def shuffle(df):
    """shuffle each column by rows"""
    for key in df.keys():
        df[key] = df[key].sample(frac=1)
    return df


def process_donations(donations):
    """Process donations file"""
    donations = donations.rename(map_col_names, axis=1)
    # only whole blood is used
    donations["BloodDonationTypeKey"] = donations["BloodDonationTypeKey"].map(
        {
            "Volbloed": "Whole Blood (K)",
            "Plasmaferese": "Plasma",
            "Omniplasma": "Plasma",
            "V": "Whole Blood (K)",
            "P": "Plasma",
            "O": "Plasma",
            "N": "New",

        }
    )

    if ONLY_VB:
        donations = donations.query('BloodDonationTypeKey in ["Whole Blood (K)"]').copy()

    donations['SuccessfulDonation'] = (donations['AfgenomenVolume'] > 400).astype(int)
    if ONLY_SUCCESSFUL:
        donations = donations[donations["SuccessfulDonation"] == 1]
    donations["Sex"] = donations["Sex"].map({"vrouw": "Female", "man": "Male", "M": "Male", "F": "Female"})
    # donations["DonationDate"] = pd.to_datetime(donations["DonationDate"]) # no need
    # set everything that is not ML to office is that correct?
    # donations["DonationPlaceType"] = (
    #     donations["DonationPlaceCode"].str[:2].map({"ML": "Mobile"})
    # )
    # donations["DonationPlaceType"] = (
    #     donations["DonationPlaceCode"].str[1].map({"M": "Mobile"})
    # )
    # donations['DonationPlaceType'] = donations["DonationPlaceType"].fillna("Office")
    #TODO: not sure how to get MAL locations, so set everything to office for now
    donations["DonationPlaceType"] = "Office" 
    #Convert to datetime and then strftime to coerce and find na's
    donations['DonationTimeStart'] = pd.to_datetime(donations['DonationTimeStart'], format='%H:%M', errors='coerce').dt.strftime('%H:%M')
    donations = donations[
        [
            "releaseID",
            "Sex",
            "DateOfBirth",
            "BloodDonationTypeKey",
            "DonationDate",
            "DonationPlaceType",
            "DonationPlaceCode",
            "Hb",
            "DonationTimeStart",
        ]
    ]

    donations['DateOfBirth'] = donations.groupby('releaseID')['DateOfBirth'].transform('first') 
    donations['Sex'] = donations.groupby('releaseID')['Sex'].transform('first')

    if SHUFFLE:
        donations = shuffle(donations)
        donations["releaseID"] += 1284
        donations["DonationDate"] = "2020-01-01"
        donations["DonationPlaceType"] = "Office"
        donations["DonationPlaceCode"] = np.nan
    if NMAX is not None:
        return donations.sample(NMAX)
    return donations


def process_donor(donor):
    """process donorsfile"""
    donor = donor.rename(map_col_names, axis=1)
    donor["Sex"] = donor["Sex"].map({"vrouw": "Female", "man": "Male", "M": "Male", "F": "Female"})
    donor["PostalCode"] = np.nan
    donor["PermissionToInvite"] = donor["Donor_Oproepbaar_Ind"].map({0: "No", 1: "Yes"})
    # donor['DateOfBirth'] = pd.to_datetime(donor['DateOfBirth'])
    donor["BloodGroup"] = donor["BloodGroup"].map(
        {
            "O positief": "O+",
            "A positief": "A+",
            "O negatief": "O-",
            "B positief": "B+",
            "A negatief": "A-",
            "B negatief": "B-",
            "AB positief": "AB+",
            "AB negatief": "AB-",
        }
    )
    #not select these rows with missing bloodgroup
    donor = donor[donor['BloodGroup'].notna()]

    donor = donor[
        [
            "releaseID",
            "Sex",
            "PostalCode",
            "PermissionToInvite",
            "DateOfBirth",
            "BloodGroup",
        ]
    ]
    if SHUFFLE:
        donor = shuffle(donor)
        donor["releaseID"] += 1284
        donor["DateOfBirth"] = "1990-01-01"
    if NMAX is not None:
        return donor.sample(NMAX)
    return donor


def process_deferral(deferral):
    """process deferral file"""
    deferral = deferral.rename(map_col_names, axis=1)
    # deferral['DeferralStartDate'] = pd.to_datetime(deferral['DeferralStartDate'], errors='coerce')
    # deferral['DeferralEndDate'] = pd.to_datetime(deferral['DeferralEndDate'], errors='coerce')
    deferral["DonorAdverseReactionType"] = (
        np.nan
    )  # could get this form donorafkeur, but there are a lot of reasons
    deferral = deferral[
        [
            "releaseID",
            "DeferralStartDate",
            "DeferralEndDate",
            "DonorAdverseReactionType",
        ]
    ]
    if SHUFFLE:
        deferral = shuffle(deferral)
        deferral["releaseID"] += 1284
        deferral["DeferralStartDate"] = "2023-01-01"
        deferral["DeferralEndDate"] = "2023-02-01"
    if NMAX is not None:
        return deferral.sample(NMAX)
    return deferral


def create_contact_df():
    """Create contact df (we don't have this data so empty)"""
    contact = pd.DataFrame(
        {
            "releaseID": [0],
            "ContactChannel": ["Email"],
            "ContactType": ["Call"],
            "DateSent": ["2023-01-01"],
            "DonationSiteCode": ["AL Amsterdam"],
        }
    )
    return contact


print("reading and processing deferral")
try:
    deferral = pd.read_spss(
        os.path.join(data_dir, f"{END_YEAR}DonorAfkeur.sav"),
        usecols=["KeyID", "VanafDatum", "TotDatum"],
    )
    deferral = process_deferral(deferral)
    print(deferral.shape)
    print(deferral.head())
except Exception as e:
    print(e)
    print("Could not get deferrals")
    deferral = pd.DataFrame({'releaseID': [], 'DeferralStartDate': [], 'DeferralEndDate': [], 'DonorAdverseReactionType': []})

if SHUFFLE:
    deferral.to_csv(os.path.join(data_dir, "deferral_fake.csv"), index=False)
else:
    deferral.to_csv(os.path.join(data_dir, "deferral.csv"), index=False)
del deferral

contact = create_contact_df()

contact.to_csv(os.path.join(data_dir, "contact.csv"), index=False)

print("reading and processing donations")
donations = []
for year in range(START_YEAR, END_YEAR + 1):
    print(year)
    if PARQUET:
        donations_year = pd.read_parquet(
            os.path.join(data_dir, f"{year}Donatiesplus.parquet")
        )
    else:
        donations_year = pd.read_spss(
            os.path.join(data_dir, f"{year}Donatiesplus.sav"),
        )
    # make sure all cols start with upper. hb->Hb
    new_cols = []
    for col in donations_year.columns:
        col = col[0].upper() + col[1:]
        new_cols.append(col)
    donations_year.columns = new_cols
    donations_year = donations_year[
        [
            "KeyID",
            "Donatiedatum",
            "Donatiecentrumcode",
            "Donatiesoortcode",
            "Geslacht",
            "Hb",
            "Geboortedatum",
            "Donatie_Tijd_Start",
            "AfgenomenVolume"
        ]
    ]

    donations_year = process_donations(donations_year)
    donations.append(donations_year)

donations = pd.concat(donations, ignore_index=True)

print(donations.shape)
print(donations.head())

donations_output_filename = 'donations'

if ONLY_VB:
    donations_output_filename += '_only_vb'
if ONLY_SUCCESSFUL:
    donations_output_filename += '_only_successful'

donations_output_filename += '.csv'

if SHUFFLE:
    donations.to_csv(os.path.join(data_dir, "donations_fake.csv"), index=False)
else:
    donations.to_csv(os.path.join(data_dir, donations_output_filename), index=False)

print("reading and processing donors")
if PARQUET:
    donor = pd.read_parquet(
        os.path.join(data_dir, f"DonorsTotaal.parquet"),
        columns=[
            "KeyID",
            "Geslacht",
            "Geboortedatum",
            "Bloedgroep",
            "Donor_Oproepbaar_Ind",
        ],
    )
else:
    donor = pd.read_spss(
        os.path.join(data_dir, f"DonorsTotaal.sav"),
        usecols=[
            "KeyID",
            "Geslacht",
            "Geboortedatum",
            "Bloedgroep",
            "Donor_Oproepbaar_Ind",
        ],
    )

donor = process_donor(donor)
print(donor.shape)
print(donor.head())
if SHUFFLE:
    donor.to_csv(os.path.join(data_dir, "donor_fake.csv"), index=False)
else:
    donor.to_csv(os.path.join(data_dir, "donor.csv"), index=False)
donor.head()

donor['DateOfBirth'] = donor['releaseID'].map(
    donations.groupby('releaseID')['DateOfBirth'].first()
)
donor['Sex'] = donor['releaseID'].map(
    donations.groupby('releaseID')['Sex'].first()
)
print(f"Saving at {data_dir}")
print('...done')