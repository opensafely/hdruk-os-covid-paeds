from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

####################
# Study Definition #
####################

# Import global-variables.json
with open("./analysis/global_variables.json") as f:
    gbl_vars = json.load(f)

# Define variables explicitly
start_date = gbl_vars["start_date"]
end_date   = gbl_vars["end_date"] 

# Study definition
study = StudyDefinition(

    index_date=start_date,

    default_expectations={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.5,
    },
    # Study population: Registered as of study start, under age of 19, male or female, not died prior to study start
    population=patients.satisfying(
        """
        registered
        AND
        (age < 18) AND (age > 1)
        AND
        (NOT has_died)
        """,
        registered=patients.registered_as_of(
            "index_date",
        ),
        has_died=patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
        ),
        age=patients.age_as_of(
            start_date,
        ),
    ),

    #######################
    # Hospital Admissions #
    #######################

    # Number of hospital admissions between 2018-09-01 and 2019-12-31
    admission_count_2018_Q4=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2018-10-01", "2018-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2019_Q1=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2019-01-01", "2019-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2019_Q2=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2019-04-01", "2019-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2019_Q3=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2019-07-01", "2019-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2019_Q4=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2019-10-01", "2019-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2020_Q1=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2020-01-01", "2020-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2020_Q2=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2020-04-01", "2020-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2020_Q3=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2020-07-01", "2020-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2020_Q4=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2020-10-01", "2020-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2021_Q1=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2021-01-01", "2021-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2021_Q2=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2021-04-01", "2021-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2021_Q3=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2021-07-01", "2021-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2021_Q4=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2021-10-01", "2021-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2022_Q1=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2022-01-01", "2022-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    admission_count_2022_Q2=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["2022-04-01", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),
    
    ###########################
    # Outpatient appointments #
    ###########################

    outpatient_count_2019_Q1=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2019-01-01", "2019-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2019_Q2=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2019-04-01", "2019-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2019_Q3=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2019-07-01", "2019-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2019_Q4=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2019-10-01", "2019-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2020_Q1=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2020-01-01", "2020-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2020_Q2=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2020-04-01", "2020-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2020_Q3=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2020-07-01", "2020-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2020_Q4=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2020-10-01", "2020-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2021_Q1=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2021-01-01", "2021-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2021_Q2=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2021-04-01", "2021-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2021_Q3=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2021-07-01", "2021-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2021_Q4=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2021-10-01", "2021-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2022_Q1=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2022-01-01", "2022-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    outpatient_count_2022_Q2=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["2022-04-01", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    ###################
    # GP interactions #
    ###################
    
    # Number of GP-patient interactions in period
    gp_contact_count_2019_Q1=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2019-01-01", "2019-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2019_Q2=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2019-04-01", "2019-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2019_Q3=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2019-07-01", "2019-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2019_Q4=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2019-10-01", "2019-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2020_Q1=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2020-01-01", "2020-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2020_Q2=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2020-04-01", "2020-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2020_Q3=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2020-07-01", "2020-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2020_Q4=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2020-10-01", "2020-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2021_Q1=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2021-01-01", "2021-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2021_Q2=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2021-04-01", "2021-06-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2021_Q3=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2021-07-01", "2021-09-30"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2021_Q4=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2021-10-01", "2021-12-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2022_Q1=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2022-01-01", "2022-03-31"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    gp_contact_count_2022_Q2=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["2022-04-01", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    #################
    # Covid Testing #
    #################

    # Number of positive covid tests
    covid_positive_test_count=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        between=["index_date", end_date],
        returning="number_of_matches_in_period",
        restrict_to_earliest_specimen_date=False,
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

    # Number of negative covid tests
    covid_negative_test_count=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="negative",
        between=["index_date", end_date],
        returning="number_of_matches_in_period",
        restrict_to_earliest_specimen_date=False,
        return_expectations={
            "int": {"distribution": "poisson", "mean": 2},
            "incidence": 1,
        },
    ),

)
