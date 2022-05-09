from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

#############
# Functions #
#############

# admitted_to_hospital_X: Creates n columns for each consecutive event of hospital admission/discharge dates, admission method
def admitted_to_hospital_X(n):
    def var_signature(name, returning, on_or_after, return_expectations):
        return {
            name: patients.admitted_to_hospital(
                    returning=returning,
                    between=[on_or_after, admission_end_date],
                    date_format="YYYY-MM-DD",
                    find_first_match_in_period=True,
                    return_expectations=return_expectations
                    ),
        }

    def var_signature2(name, returning, on_or_after, return_expectations):
        return {
            name: patients.admitted_to_hospital(
                    returning=returning,
                    on_or_after=on_or_after,
                    date_format="YYYY-MM-DD",
                    find_first_match_in_period=True,
                    return_expectations=return_expectations
                    ),
        }

    # Expectation for admission method
    return_expectations_method={
        "category": {"ratios": {"11": 0.1, "12": 0.1, "13": 0.1, "21": 0.1, "22": 0.1, "23": 0.1,
                                "24": 0.05, "25": 0.05, "2A": 0.05, "2B": 0.05, "2C": 0.05, "2D": 0.05, "28": 0.05,
                                "31": 0.01, "32": 0.01, "82": 0.01, "83": 0.01, "81": 0.01}},
                     "incidence": 1}
                     
    # Expections for admission dates
    return_expectations_date_adm={
        "date": {"earliest": admission_start_date, "latest": admission_end_date},
        "rate": "uniform",
        "incidence": 0.5}
        
    # Expections for discharge dates
    return_expectations_date_dis={
        "date": {"earliest": admission_start_date, "latest": admission_end_date},
        "rate": "uniform",
        "incidence": 0.5}

    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("admission_date_1", "date_admitted", admission_start_date, return_expectations_date_adm)
            variables.update(var_signature2("discharge_date_1", "date_discharged", "admission_date_1", return_expectations_date_dis))
            variables.update(var_signature2("admission_method_1", "admission_method", "admission_date_1", return_expectations_method))
        else:
            variables.update(var_signature(f"admission_date_{i}", "date_admitted", f"admission_date_{i-1} + 1 day", return_expectations_date_adm))
            variables.update(var_signature2(f"discharge_date_{i}", "date_discharged", f"admission_date_{i}", return_expectations_date_dis))
            variables.update(var_signature2(f"admission_method_{i}", "admission_method", f"admission_date_{i}", return_expectations_method))
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

# Define date range for hospital admissions
admission_start_date = "2020-04-01"
admission_end_date   = "2020-06-30"

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_admission  = gbl_vars["n_admission_high"]

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
        (admission_count > 5)
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
        admission_count=patients.admitted_to_hospital(
            returning="number_of_matches_in_period",
            between=[admission_start_date, admission_end_date],
        ),
    ),

    #######################
    # Hospital Admissions #
    #######################
    
    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **admitted_to_hospital_X(
        n=n_admission
    ),

)