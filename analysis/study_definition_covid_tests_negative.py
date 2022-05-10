from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

#############
# Functions #
#############

# covid_negative_test_date_X: Creates n columns for each consecutive negative covid test
def covid_negative_test_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_test_result_in_sgss(
                pathogen="SARS-CoV-2",
                test_result="negative",
                between=[on_or_after, "last_day_of_month(index_date)"],
                find_first_match_in_period=True,
                returning="date",
                date_format="YYYY-MM-DD",
                return_expectations={
                    "date": {"earliest": "2020-03-01", "latest": end_date},
                    "rate": "exponential_increase",
                    "incidence": 0.2
                },
            ),
        }
     
    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("covid_negative_test_date_1", "first_day_of_month(index_date)")
        else:
            variables.update(var_signature(f"covid_negative_test_date_{i}", f"covid_negative_test_date_{i-1} + 1 day"))
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

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_negative_test      = gbl_vars["n_negative_test"]

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
        (covid_negative_test_count > 0)
        """,
        registered=patients.registered_as_of(
            start_date,
        ),
        has_died=patients.died_from_any_cause(
            on_or_before=start_date,
            returning="binary_flag",
        ),
        age=patients.age_as_of(
            start_date,
        ),
    ),

    #############################
    # Negative COVID Test Dates #
    #############################
    
    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **covid_negative_test_date_X(
        n=n_negative_test
    ),

    # Number of negative covid tests during period
    covid_negative_test_count=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="negative",
        between=["first_day_of_month(index_date)", "last_day_of_month(index_date)"],
        returning="number_of_matches_in_period",
        restrict_to_earliest_specimen_date=False,
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
            "incidence": 1,
        },
    ),

)
