from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

#############
# Functions #
#############

# outpatient_date_X: Creates n columns for each consecutive outpatient appointment
def outpatient_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.outpatient_appointment_date(
                    returning="date",
                    attended=True,
                    find_first_match_in_period=True,
                    between=[on_or_after, outpatient_end_date],
                    date_format="YYYY-MM-DD",
                    return_expectations={
                        "date": {"earliest": outpatient_start_date, "latest": outpatient_end_date},
                        "rate": "uniform",
                        "incidence": 0.2
                        }
                    ),
        }
     
    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("outpatient_date_1", outpatient_start_date)
        else:
            variables.update(var_signature(f"outpatient_date_{i}", f"outpatient_date_{i-1} + 1 day"))
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
outpatient_start_date = "2021-01-01"
outpatient_end_date   = "2021-12-31"

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_outpatient      = gbl_vars["n_outpatient"]
n_outpatient_high = gbl_vars["n_outpatient_high"]

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
        (outpatient_count > 10)
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
        outpatient_count=patients.outpatient_appointment_date(
            returning="number_of_matches_in_period",
            attended=True,
            between=[outpatient_start_date, outpatient_end_date],
        ),
    ),

    ###########################
    # Outpatient appointments #
    ###########################

    # Oupatient appointments X: n columns of date of admissions, date of discharge, admission method
    **outpatient_date_X(
        n=n_outpatient_high
    ),

)
