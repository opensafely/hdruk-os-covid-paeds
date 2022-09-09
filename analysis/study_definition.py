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

    #############
    # Shielding #
    #############

    shielding_first_date=patients.with_these_clinical_events(
        combine_codelists(
            shielding_codes
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

    ##############
    # Conditions #
    ##############

    condition_expectations = {
            "date": {"earliest": "2015-01-01", "latest": end_date},
            "rate": "uniform",
            "incidence": 0.2
        }

    # ASTHMA
    asthma_first_date=patients.with_these_clinical_events(
        snomed_asthma,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    asthma_last_date=patients.with_these_clinical_events(
        snomed_asthma,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Behavioural and developmental conditions including autism
    behavioural_and_developmental_including_autism_first_date=patients.with_these_clinical_events(
        snomed_behavioural_and_developmental_including_autism,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    behavioural_and_developmental_including_autism_last_date=patients.with_these_clinical_events(
        snomed_behavioural_and_developmental_including_autism,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cancer
    cancer_first_date=patients.with_these_clinical_events(
        snomed_cancer,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cancer_last_date=patients.with_these_clinical_events(
        snomed_cancer,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cardiovascular - congenital
    cardiovascular_congenital_first_date=patients.with_these_clinical_events(
        snomed_cardiovascular_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cardiovascular_congenital_last_date=patients.with_these_clinical_events(
        snomed_cardiovascular_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cardiovascular - devices
    cardiovascular_devices_first_date=patients.with_these_clinical_events(
        snomed_cardiovascular_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cardiovascular_devices_last_date=patients.with_these_clinical_events(
        snomed_cardiovascular_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cardiovascular - non-congenital
    cardiovascular_non_congenital_first_date=patients.with_these_clinical_events(
        snomed_cardiovascular_non_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cardiovascular_non_congenital_last_date=patients.with_these_clinical_events(
        snomed_cardiovascular_non_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cerebral palsy and paralysis
    cerebral_palsy_paralysis_first_date=patients.with_these_clinical_events(
        snomed_cerebral_palsy_paralysis,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cerebral_palsy_paralysis_last_date=patients.with_these_clinical_events(
        snomed_cerebral_palsy_paralysis,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Chronic infections
    chronic_infections_first_date=patients.with_these_clinical_events(
        snomed_chronic_infections,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    chronic_infections_last_date=patients.with_these_clinical_events(
        snomed_chronic_infections,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Congenital endocrine
    congenital_endocrine_first_date=patients.with_these_clinical_events(
        snomed_congenital_endocrine,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    congenital_endocrine_last_date=patients.with_these_clinical_events(
        snomed_congenital_endocrine,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Congenital malformation syndromes and chromosomal
    congenital_malformation_syndromes_and_chromosomal_first_date=patients.with_these_clinical_events(
        snomed_congenital_malformation_syndromes_and_chromosomal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    congenital_malformation_syndromes_and_chromosomal_last_date=patients.with_these_clinical_events(
        snomed_congenital_malformation_syndromes_and_chromosomal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Congenital neurological
    congenital_neuro_first_date=patients.with_these_clinical_events(
        snomed_congenital_neuro,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    congenital_neuro_last_date=patients.with_these_clinical_events(
        snomed_congenital_neuro,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Congenital renal
    congenital_renal_first_date=patients.with_these_clinical_events(
        snomed_congenital_renal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    congenital_renal_last_date=patients.with_these_clinical_events(
        snomed_congenital_renal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Congenital urogenital
    congenital_urogenital_first_date=patients.with_these_clinical_events(
        snomed_congenital_urogenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    congenital_urogenital_last_date=patients.with_these_clinical_events(
        snomed_congenital_urogenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Cystic fibrosis
    cystic_fibrosis_first_date=patients.with_these_clinical_events(
        snomed_cystic_fibrosis,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    cystic_fibrosis_last_date=patients.with_these_clinical_events(
        snomed_cystic_fibrosis,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Diabetes
    diabetes_first_date=patients.with_these_clinical_events(
        snomed_diabetes,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    diabetes_last_date=patients.with_these_clinical_events(
        snomed_diabetes,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Endocrine no dm
    endocrine_no_dm_first_date=patients.with_these_clinical_events(
        snomed_endocrine_no_dm,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    endocrine_no_dm_last_date=patients.with_these_clinical_events(
        snomed_endocrine_no_dm,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Epilepsy
    epilepsy_first_date=patients.with_these_clinical_events(
        snomed_epilepsy,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    epilepsy_last_date=patients.with_these_clinical_events(
        snomed_epilepsy,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Gastrointestinal - devices
    gastrointestinal_devices_first_date=patients.with_these_clinical_events(
        snomed_gastrointestinal_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    gastrointestinal_devices_last_date=patients.with_these_clinical_events(
        snomed_gastrointestinal_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Gastrointestinal
    gastrointestinal_first_date=patients.with_these_clinical_events(
        snomed_gastrointestinal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    gastrointestinal_last_date=patients.with_these_clinical_events(
        snomed_gastrointestinal,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Gastrointestinal - non-congenital
    genitourinary_non_congenital_first_date=patients.with_these_clinical_events(
        snomed_genitourinary_non_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    genitourinary_non_congenital_last_date=patients.with_these_clinical_events(
        snomed_genitourinary_non_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Haematology
    haematology_first_date=patients.with_these_clinical_events(
        snomed_haematology,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    haematology_last_date=patients.with_these_clinical_events(
        snomed_haematology,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Headaches
    headaches_first_date=patients.with_these_clinical_events(
        snomed_headaches,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    headaches_last_date=patients.with_these_clinical_events(
        snomed_headaches,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Immunological
    immunological_first_date=patients.with_these_clinical_events(
        snomed_immunological,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    immunological_last_date=patients.with_these_clinical_events(
        snomed_immunological,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Mental illness
    mental_illness_first_date=patients.with_these_clinical_events(
        snomed_mental_illness,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    mental_illness_last_date=patients.with_these_clinical_events(
        snomed_mental_illness,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Metabolic
    metabolic_first_date=patients.with_these_clinical_events(
        snomed_metabolic,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    metabolic_last_date=patients.with_these_clinical_events(
        snomed_metabolic,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Neuro devices
    neuro_devices_first_date=patients.with_these_clinical_events(
        snomed_neuro_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    neuro_devices_last_date=patients.with_these_clinical_events(
        snomed_neuro_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Neurological no epilepsy or cp headaches
    neurological_no_epilepsy_or_cp_headaches_first_date=patients.with_these_clinical_events(
        snomed_neurological_no_epilepsy_or_cp_headaches,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    neurological_no_epilepsy_or_cp_headaches_last_date=patients.with_these_clinical_events(
        snomed_neurological_no_epilepsy_or_cp_headaches,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Obesity
    obesity_first_date=patients.with_these_clinical_events(
        snomed_obesity,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    obesity_last_date=patients.with_these_clinical_events(
        snomed_obesity,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Palliative care
    palliative_care_first_date=patients.with_these_clinical_events(
        snomed_palliative_care,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    palliative_care_last_date=patients.with_these_clinical_events(
        snomed_palliative_care,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Renal devices
    renal_devices_first_date=patients.with_these_clinical_events(
        snomed_renal_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    renal_devices_last_date=patients.with_these_clinical_events(
        snomed_renal_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Resp congenital
    resp_congenital_first_date=patients.with_these_clinical_events(
        snomed_resp_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    resp_congenital_last_date=patients.with_these_clinical_events(
        snomed_resp_congenital,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Resp devices
    resp_devices_first_date=patients.with_these_clinical_events(
        snomed_resp_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    resp_devices_last_date=patients.with_these_clinical_events(
        snomed_resp_devices,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Respiratory not asthma or cf
    respiratory_not_asthma_or_cf_first_date=patients.with_these_clinical_events(
        snomed_respiratory_not_asthma_or_cf,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    respiratory_not_asthma_or_cf_last_date=patients.with_these_clinical_events(
        snomed_respiratory_not_asthma_or_cf,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Rheumatology
    rheumatology_first_date=patients.with_these_clinical_events(
        snomed_rheumatology,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    rheumatology_last_date=patients.with_these_clinical_events(
        snomed_rheumatology,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Severe mental illness
    severe_mental_illness_first_date=patients.with_these_clinical_events(
        snomed_severe_mental_illness,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    severe_mental_illness_last_date=patients.with_these_clinical_events(
        snomed_severe_mental_illness,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
    ),

    # Transplant
    transplant_first_date=patients.with_these_clinical_events(
        snomed_transplant,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_first_match_in_period=True,
        return_expectations=condition_expectations
    ),

    transplant_last_date=patients.with_these_clinical_events(
        snomed_transplant,
        on_or_before=end_date,
        returning = "date",
        date_format = "YYYY-MM-DD",
        find_last_match_in_period=True,
        return_expectations=condition_expectations
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
        between=["vax_covid_date_1 + 19 days", end_date],
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
        between=["vax_covid_date_2 + 56 day", end_date],
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
