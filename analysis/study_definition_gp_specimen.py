from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

############
# Codelist #
############

# Import Codelists
from codelists import *

#############
# Functions #
#############

# gp_contact_date_X: Creates n columns for each consecutive GP consulation date
def gp_contact_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_these_clinical_events(
                    codelist = snomed_specimen,
                    returning="date",
                    between=[on_or_after, "index_date + 6 days"],
                    date_format="YYYY-MM-DD",
                    find_first_match_in_period=True,
                    return_expectations={
                        "date": {"earliest": start_date, "latest": end_date},
                        "rate": "uniform",
                        "incidence": 0.2
                        }
                    ),
        }
     
    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("gp_contact_date_1", "index_date")
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

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_gp      = gbl_vars["n_gp"]

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
        AND (gp_contact_count > 0)
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

    ##############
    # GP Contact #
    ##############

    # GP contact X: n columns of date of GP consultations
    **gp_contact_date_X(
       n=n_gp
    ),

    # Number of GP contacts during period
    gp_contact_count=patients.with_these_clinical_events(
        codelist = snomed_specimen,
        returning="number_of_matches_in_period",
        between=["index_date", "index_date + 6 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

)
