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
        AND (age > 1) AND (age < 18)
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
    asthma_diagnosis_date=patients.with_these_clinical_events(
        asthma_diagnosis_codelist,
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

    current_asthma_date=patients.with_these_clinical_events(
        current_asthma_codelist,
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
    

    # CANCER
    cancer_excl_lung_and_haem_date=patients.with_these_clinical_events(
        cancer_excl_lung_and_haem_codelist,
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

    cancer_haem_date=patients.with_these_clinical_events(
        cancer_haem_codelist,
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

    cancer_lung_date=patients.with_these_clinical_events(
        cancer_lung,
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

    # CHRONIC CARDIAC DISEASE
    chronic_cardiac_disease_date=patients.with_these_clinical_events(
        chronic_cardiac_disease_codelist,
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

    heart_disease_other_date=patients.with_these_clinical_events(
        heart_disease_other_codelist,
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


    # CHRONIC KIDNEY DISEASE
    chronic_kidney_disease_date=patients.with_these_clinical_events(
        chronic_kidney_disease_codelist,
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

    # CHRONIC LIVER DISEASE
    chronic_liver_disease_date=patients.with_these_clinical_events(
        chronic_liver_disease_codelist,
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

    # CHRONIC RESPIRATORY DIESASE
    chronic_respiratory_disease_date=patients.with_these_clinical_events(
        chronic_respiratory_disease_codelist,
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
        diabetes_codelist,
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

    # HIV
    hiv_date=patients.with_these_clinical_events(
        hiv_codelist,
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

    # INTELLECTUAL AND LEARNING DISABILITY
    intellectual_disability_date=patients.with_these_clinical_events(
        intellectual_disability_codelist,
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

    learning_disabilities_date=patients.with_these_clinical_events(
        learning_disabilities_codelist,
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

    # SEVERE MENTAL ILLNESS
    severe_mental_illness_date=patients.with_these_clinical_events(
        severe_mental_illness_codelist,
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

    # NEUROLOGICAL DISEASES
    neurological_diseases_date=patients.with_these_clinical_events(
        neurological_diseases_codelist,
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

    other_neurological_conditions_date=patients.with_these_clinical_events(
        other_neurological_conditions_codelist,
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

    cerebral_palsy_date=patients.with_these_clinical_events(
        cerebral_palsy_codelist,
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

    # PERMANENT IMMUNOSUPPRESSION
    permanent_immunosuppression_date=patients.with_these_clinical_events(
        permanent_immunosuppression_codelist,
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

    # PREGNENCY OR DELIVERY (INCLUDING MISCARRIAGE AND TERMINATION)
    pregnency_delivery_date=patients.with_these_clinical_events(
        pregnency_delivery_codelist,
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

    # SEVERE OBESITY
    severe_obesity_date=patients.with_these_clinical_events(
        severe_obesity_codelist,
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

    # SICKLE CELL DISEASE
    sickle_cell_disease_date=patients.with_these_clinical_events(
        sickle_cell_disease_codelist,
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
