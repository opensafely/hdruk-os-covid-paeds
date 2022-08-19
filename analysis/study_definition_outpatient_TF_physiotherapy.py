from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

# Combine codelists
treatment_function_codelist = combine_codelists(
    treatment_function_physiotherapy
)

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
n_outpatient = gbl_vars["n_outpatient"]

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
        AND (age_on_start_date > 0) AND (age_on_start_date < 18)
        AND (outpatient_count_week > 0)
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

    ###########################
    # Outpatient appointments #
    ###########################

    # Number of weekly outpatient appointments
    outpatient_count_week=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date", "index_date + 6 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    # Number of daily outpatient appointments
    outpatient_count_1=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date", "index_date"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_2=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 1 days", "index_date + 1 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_3=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 2 days", "index_date + 2 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_4=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 3 days", "index_date + 3 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_5=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 4 days", "index_date + 4 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_6=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 5 days", "index_date + 5 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    outpatient_count_7=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        with_these_treatment_function_codes = treatment_function_codelist,
        between=["index_date + 6 days", "index_date + 6 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

)
