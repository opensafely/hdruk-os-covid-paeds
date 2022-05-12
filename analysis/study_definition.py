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
            start_date,
        ),
        has_died=patients.died_from_any_cause(
            on_or_before=start_date,
            returning="binary_flag",
        )
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

    # https://github.com/opensafely/risk-factors-research/issues/46
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),

    # Ethnicity in 6 categories
    ethnicity=patients.with_these_clinical_events(
        ethnicity_codes,
        returning="category",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75,
        },
    ),

    # New ethnicity variable that takes data from SUS
    ethnicity_6_sus=patients.with_ethnicity_from_sus(
        returning="group_6",
        use_most_frequent_code=True,
        return_expectations={
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.3,
            },
    ),

    # NHS administrative region
    region_2019=patients.registered_practice_as_of(
        "2019-01-01",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.2,
                    "Yorkshire and the Humber": 0.2,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.1,
                    "South East": 0.09,
                    "": 0.01
                },
            },
        },
    ),

    region_2020=patients.registered_practice_as_of(
        "2020-01-01",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.2,
                    "Yorkshire and the Humber": 0.2,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.1,
                    "South East": 0.09,
                    "": 0.01
                },
            },
        },
    ),

    region_2021=patients.registered_practice_as_of(
        "2021-01-01",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.2,
                    "Yorkshire and the Humber": 0.2,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.1,
                    "South East": 0.09,
                    "": 0.01
                },
            },
        },
    ),

    # IMD - index of multiple deprivation
    imd_2019=patients.address_as_of(
        "2019-01-01",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"3300": 0.2, "9900": 0.2, "16400": 0.2, "23000": 0.2, "30000": 0.2}},
        },
    ),

    imd_2020=patients.address_as_of(
        "2020-01-01",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"3300": 0.2, "9900": 0.2, "16400": 0.2, "23000": 0.2, "30000": 0.2}},
        },
    ),

    imd_2021=patients.address_as_of(
        "2021-01-01",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"3300": 0.2, "9900": 0.2, "16400": 0.2, "23000": 0.2, "30000": 0.2}},
        },
    ),

    # Rural/Urban Classification
    rural_urban_2019=patients.address_as_of(
        "2019-01-01",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": 
                {"ratios": {
                    "1": 0.1,
                    "2": 0.1,
                    "3": 0.1,
                    "4": 0.1,
                    "5": 0.1,
                    "6": 0.1,
                    "7": 0.2,
                    "8": 0.2,
                }
            },
        },
    ),

    rural_urban_2020=patients.address_as_of(
        "2020-01-01",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": 
                {"ratios": {
                    "1": 0.1,
                    "2": 0.1,
                    "3": 0.1,
                    "4": 0.1,
                    "5": 0.1,
                    "6": 0.1,
                    "7": 0.2,
                    "8": 0.2,
                }
            },
        },
    ),

    rural_urban_2021=patients.address_as_of(
        "2021-01-01",
        returning="rural_urban_classification",
        return_expectations={
            "rate": "universal",
            "category": 
                {"ratios": {
                    "1": 0.1,
                    "2": 0.1,
                    "3": 0.1,
                    "4": 0.1,
                    "5": 0.1,
                    "6": 0.1,
                    "7": 0.2,
                    "8": 0.2,
                }
            },
        },
    ),

    #########
    # Death #
    #########
    # ONS date of death
    death_date=patients.died_from_any_cause(
        between=[start_date, end_date],
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        },
    ),

    ##############
    # Conditions #
    ##############

    # CURRENT ASTHMA
    asthma_date=patients.with_these_clinical_events(
        current_asthma_codes,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # DIABETES
    diabetes_date=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    ########################
    # Covid-19 Vaccination #
    ########################

    # Date of 1st covid-19 vaccination
    vax_covid_date_1=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["index_date", end_date],
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "exponential_increase",
            "incidence": 0.2
        },
    ),

    # Date of 2nd covid-19 vaccination
    vax_covid_date_2=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["vax_covid_date_1 + 1 day", end_date],
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "exponential_increase",
            "incidence": 0.2
        },
    ),

    # Date of 3rd covid-19 vaccination
    vax_covid_date_3=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["vax_covid_date_2 + 1 day", end_date],
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "exponential_increase",
            "incidence": 0.2
        },
    ),

)
