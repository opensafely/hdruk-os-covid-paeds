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

    # Study population: Alive and registered as of study start, registered at study end or died during study, age between 1 and 18 years
    population=patients.satisfying(
        """
        (NOT died_before_start_date) AND registered_at_start_date
        AND (registered_at_end_date OR died_during_study)
        AND (age > 0) AND (age < 18)
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
    ),

    age=patients.age_as_of(
        start_date,
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "normal", "mean": 12, "stddev": 2.5},
            "incidence": 1
        },
    ),

    date_of_birth=patients.date_of_birth(
        "YYYY-MM",
        return_expectations={
            "date": {"earliest": "2001-01-01", "latest": "2018-05-01"},
            "rate": "uniform",
            "incidence": 1
        }
    ),

    admission_2019=patients.admitted_to_hospital(
        between=["2019-01-01", "2019-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.1},
            "incidence": 1
        }
    ),

    admission_2020=patients.admitted_to_hospital(
        between=["2020-01-01", "2020-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.1},
            "incidence": 1
        }
    ),

    admission_2021=patients.admitted_to_hospital(
        between=["2021-01-01", "2021-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.1},
            "incidence": 1
        }
    ),

    admission_2022=patients.admitted_to_hospital(
        between=["2019-01-01", "2019-04-30"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.1},
            "incidence": 1
        }
    ),

    beddays_2019=patients.admitted_to_hospital(
        between=["2019-01-01", "2019-12-31"],
        returning="total_bed_days_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.3},
            "incidence": 1
        }
    ),

    beddays_2020=patients.admitted_to_hospital(
        between=["2020-01-01", "2020-12-31"],
        returning="total_bed_days_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.3},
            "incidence": 1
        }
    ),

    beddays_2021=patients.admitted_to_hospital(
        between=["2021-01-01", "2021-12-31"],
        returning="total_bed_days_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.3},
            "incidence": 1
        }
    ),

    beddays_2022=patients.admitted_to_hospital(
        between=["2022-01-01", "2022-04-30"],
        returning="total_bed_days_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.3},
            "incidence": 1
        }
    ),

    outpatient_2019=patients.outpatient_appointment_date(
        between=["2019-01-01", "2019-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.7},
            "incidence": 1
        }
    ),

    outpatient_2020=patients.outpatient_appointment_date(
        between=["2020-01-01", "2020-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.7},
            "incidence": 1
        }
    ),

    outpatient_2021=patients.outpatient_appointment_date(
        between=["2021-01-01", "2021-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.7},
            "incidence": 1
        }
    ),

    outpatient_2022=patients.outpatient_appointment_date(
        between=["2022-01-01", "2022-04-30"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 0.7},
            "incidence": 1
        }
    ),

    gp_2019=patients.with_gp_consultations(
        between=["2019-01-01", "2019-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1
        }
    ),


    gp_2020=patients.with_gp_consultations(
        between=["2020-01-01", "2020-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1
        }
    ),

    gp_2021=patients.with_gp_consultations(
        between=["2021-01-01", "2021-12-31"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1
        }
    ),

    gp_2022=patients.with_gp_consultations(
        between=["2022-01-01", "2022-04-30"],
        returning="number_of_matches_in_period",
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1
        }
    ),
 

)
