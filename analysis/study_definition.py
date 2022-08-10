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

    # https://github.com/opensafely/risk-factors-research/issues/46
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),

    # Ethnicity in 6 categories
    ethnicity_gp=patients.with_these_clinical_events(
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

    # ASTHMA
    asthma_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_asthma
        ),
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

    asthma_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_asthma
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # CANCER
    cancer_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cancer
        ),
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

    cancer_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cancer
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # CYSTIC FIBROSIS
    cystic_fibrosis_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cystic_fibrosis
        ),
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

    cystic_fibrosis_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cystic_fibrosis
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # DIABETES
    diabetes_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_diabetes
        ),
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

    diabetes_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_diabetes
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # EPILEPSY
    epilepsy_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_epilepsy
        ),
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

    epilepsy_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_epilepsy
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # SEVERE METNAL ILLNESS
    severe_mental_illness_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_severe_mental_illness
        ),
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

    severe_mental_illness_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_severe_mental_illness
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # CEREBRAL PALSY
    cerebral_palsy_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cerebral_palsy
        ),
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

    cerebral_palsy_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_cerebral_palsy
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # CHRONIC INFECTIONS
    chronic_infections_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_chronic_infections
        ),
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

    chronic_infections_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_chronic_infections
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # DEVICES AND STOMAS
    devices_and_stomas_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_devices_and_stomas
        ),
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

    devices_and_stomas_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_devices_and_stomas
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # ENDOCRINE 
    endocrine_disorders_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_endocrine
        ),
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

    endocrine_disorders_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_endocrine
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # GASTROINTESTINAL DISORDERS 
    gastrointestinal_disorders_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_gastrointestinal
        ),
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

    gastrointestinal_disorders_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_gastrointestinal
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # HAEMATOLOGICAL DISORDERS 
    haematological_disorders_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_haematology
        ),
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

    haematological_disorders_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_haematology
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # IMMUNOLOGICAL DISORDERS 
    immunological_disorders_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_immunological
        ),
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

    immunological_disorders_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_immunological
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # LEARNING AND BEHAVIOUR DIFFICULTIES 
    learning_and_behaviour_difficulties_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_learning_difficulties_and_behaviour
        ),
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

    learning_and_behaviour_difficulties_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_learning_difficulties_and_behaviour
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # MENTAL ILLNESS
    mental_illness_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_mental_illness
        ),
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

    mental_illness_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_mental_illness
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # MUSCOLOKELETAL AND RHEUMATIC DISEASES
    musculoskeletal_and_rheum_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_musculoskeletal_and_rheum
        ),
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

    musculoskeletal_and_rheum_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_musculoskeletal_and_rheum
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations={
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.05
        }
    ),

    # TRANSPLANT
    transplant_first_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_transplant
        ),
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

    transplant_last_date=patients.with_these_clinical_events(
        combine_codelists(
            snomed_transplant
        ),
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
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
