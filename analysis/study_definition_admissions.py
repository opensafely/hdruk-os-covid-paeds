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
                    between=[on_or_after, "index_date + 6 days"],
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
                     
    # Expections for admission dates
    return_expectations_date_adm={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.8}
        
    # Expections for discharge dates
    return_expectations_date_dis={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.8}

    # Expectation for admission method
    return_expectations_method={
        "category": {"ratios": {"11": 0.1, "12": 0.1, "13": 0.1, "21": 0.1, "22": 0.1, "23": 0.1,
                                "24": 0.05, "25": 0.05, "2A": 0.05, "2B": 0.05, "2C": 0.05, "2D": 0.05, "28": 0.05,
                                "31": 0.01, "32": 0.01, "82": 0.01, "83": 0.01, "81": 0.01}},
                     "incidence": 0.95}

    # Expectation for primary diagnosis
    return_expectations_diagnosis={
        "category": {"ratios": {"J45": 0.25, "E10": 0.25, "C91": 0.25, "I50": 0.25}},
                     "incidence": 0.95}

    # Expectation for admission treatment function code
    return_expectations_diagnosis={
        "category": {"ratios": {"100": 0.25, "173": 0.25, "212": 0.25, "I50": 0.25}},
                     "incidence": 0.95}

    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("admission_date_1", "date_admitted", "index_date", return_expectations_date_adm)
            variables.update(var_signature2("discharge_date_1", "date_discharged", "admission_date_1", return_expectations_date_dis))
            variables.update(var_signature2("admission_method_1", "admission_method", "admission_date_1", return_expectations_method))
            variables.update(var_signature2("primary_diagnosis_1", "primary_diagnosis", "admission_date_1", return_expectations_diagnosis))
            variables.update(var_signature2("treatment_function_1", "admission_treatment_function_code", "admission_date_1", return_expectations_diagnosis))
        else:
            variables.update(var_signature(f"admission_date_{i}", "date_admitted", f"admission_date_{i-1} + 1 day", return_expectations_date_adm))
            variables.update(var_signature2(f"discharge_date_{i}", "date_discharged", f"admission_date_{i}", return_expectations_date_dis))
            variables.update(var_signature2(f"admission_method_{i}", "admission_method", f"admission_date_{i}", return_expectations_method))
            variables.update(var_signature2(f"primary_diagnosis_{i}", "primary_diagnosis", f"admission_date_{i}", return_expectations_diagnosis))
            variables.update(var_signature2(f"treatment_function_{i}", "admission_treatment_function_code", f"admission_date_{i}", return_expectations_diagnosis))
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
n_admission  = gbl_vars["n_admission"]

# Study definition
study = StudyDefinition(

    index_date=start_date,

    default_expectations={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.5,
    },
    # Study population: Alive and registered as of study start, registered at study end or died during study, age between 1 and 18 years
    population=patients.satisfying(
        """
        (NOT died_before_start_date) AND registered_at_start_date
        AND (registered_at_end_date OR died_during_study)
        AND (age_on_start_date > 1) AND (age_on_start_date < 18)
        AND (admission_count > 0)
        """,
        registered_at_start_date=patients.registered_as_of(
            start_date,
        ),
        registered_at_end_date=patients.registered_as_of(
            end_date,
        ),
        died_before_start_date=patients.died_from_any_cause(
            on_or_before=start_date,
            returning="binary_flag",
        ),
        died_during_study=patients.died_from_any_cause(
            between=[start_date, end_date],
            returning="binary_flag",
        ),
        age_on_start_date=patients.age_as_of(
            start_date,
        ),
    ),

    #######################
    # Hospital Admissions #
    #######################
    
    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **admitted_to_hospital_X(
        n=n_admission
    ),

    # Number of admissions during period
    admission_count=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["index_date", "index_date + 6 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

)