from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

#############
# Functions #
#############

# gp_contact_date_X: Creates n columns for each consecutive GP consulation date
def gp_contact_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_gp_consultations(
                    returning="date",
                    between=[on_or_after, gp_end_date],
                    date_format="YYYY-MM-DD",
                    find_first_match_in_period=True,
                    return_expectations={
                        "date": {"earliest": gp_start_date, "latest": gp_end_date},
                        "rate": "uniform",
                        "incidence": 0.2
                        }
                    ),
        }
     
    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("gp_contact_date_1", gp_start_date)
        else:
            variables.update(var_signature(f"gp_contact_date_{i}", f"gp_contact_date_{i-1} + 1 day"))
    return variables


####################
# Study Definition #
####################

# Import global-variables.json
with open("./analysis/global_variables.json") as f:
    gbl_vars = json.load(f)

# Define study date variables
start_date = gbl_vars["start_date"]
end_date   = gbl_vars["end_date"]

# Define date range for GP appointments
gp_start_date = "2022-01-01"
gp_end_date   = "2022-05-01"


# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_gp      = gbl_vars["n_gp"]
n_gp_high = gbl_vars["n_gp_high"]

# Study definition
study = StudyDefinition(

    index_date=start_date,

    default_expectations={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.5,
    },
    # Study population: Registered as of study start, between ages 1 and 18, alive at study start, 
    population=patients.satisfying(
        """
        registered
        AND
        (age < 18) AND (age > 1)
        AND
        (NOT has_died)
        AND
        (gp_contact_count > 0) AND (gp_contact_count < 26)
        """,
        registered=patients.registered_as_of(
            "index_date",
        ),
        has_died=patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
        ),
        age=patients.age_as_of(
            "index_date",
        ),
        gp_contact_count=patients.with_gp_consultations(
            returning="number_of_matches_in_period",
            between=[gp_start_date, gp_end_date],
        ),
    ),

    ##############
    # GP Contact #
    ##############

    # GP contact X: n columns of date of GP consultations
    **gp_contact_date_X(
       n=n_gp
    ),   

)
