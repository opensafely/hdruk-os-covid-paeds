from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# import json and datetime module
import json
import datetime

# Import Codelists
from codelists import *

#############
# FUNCTIONS #
#############

# admitted_to_hospital_X: Creates n columns for each consecutive event of hospital admission/discharge dates, admission method
def admitted_to_hospital_X(n):
    def var_signature(name, returning, on_or_after, return_expectations):
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
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.5}
        
    # Expections for discharge dates
    return_expectations_date_dis={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 0.5}

    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("admission_date_1", "date_admitted", "index_date", return_expectations_date_adm)
            variables.update(var_signature("discharge_date_1", "date_discharged", "admission_date_1", return_expectations_date_dis))
            variables.update(var_signature("admission_method_1", "admission_method", "admission_date_1", return_expectations_method))
        else:
            variables.update(var_signature(f"admission_date_{i}", "date_admitted", f"admission_date_{i-1} + 1 day", return_expectations_date_adm))
            variables.update(var_signature(f"discharge_date_{i}", "date_discharged", f"admission_date_{i}", return_expectations_date_dis))
            variables.update(var_signature(f"admission_method_{i}", "admission_method", f"admission_date_{i}", return_expectations_method))
    return variables

# outpatient_date_X: Creates n columns for each consecutive outpatient appointment
def outpatient_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.outpatient_appointment_date(
                    returning="date",
                    attended=True,
                    find_first_match_in_period=True,
                    on_or_after="index_date",
                    date_format="YYYY-MM-DD",
                    return_expectations={
                        "date": {"earliest": start_date, "latest": end_date},
                        "rate": "uniform",
                        "incidence": 0.2
                        }
                    ),
        }
     
    for i in range(1, n+1):
        if i == 1:
            variables = var_signature("outpatient_date_1", "index_date")
        else:
            variables.update(var_signature(f"outpatient_date_{i}", f"outpatient_date_{i-1} + 1 day"))
    return variables

# gp_contact_date_X: Creates n columns for each consecutive GP consulation date
def gp_contact_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_gp_consultations(
                    returning="date",
                    between=[on_or_after, end_date],
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

# Define variables explicitly
start_date = gbl_vars["start_date"]
end_date   = gbl_vars["end_date"] 

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_admission     = gbl_vars["n_admission"]
n_outpatient    = gbl_vars["n_outpatient"]
n_gp            = gbl_vars["n_gp"]
n_positive_test = gbl_vars["n_positive_test"]
n_negative_test = gbl_vars["n_negative_test"]

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

    # ONS death - covid mentioned on certificate
    death_covid_flag_any=patients.with_these_codes_on_death_certificate(
        covid_codelist,
        on_or_after="index_date",
        match_only_underlying_cause=False,
        return_expectations={"incidence": 0.33},
    ),

    # ONS death - covid as underlying cause
    death_covid_flag_underlying=patients.with_these_codes_on_death_certificate(
        covid_codelist,
        on_or_after="index_date",
        match_only_underlying_cause=True,
        return_expectations={"incidence": 0.33},
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

    #######################
    # Hospital Admissions #
    #######################

    # Number of hospital admissions in period
    admission_count=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["index_date", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),
    
    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **admitted_to_hospital_X(
        n=n_admission
    ),
    
    ###########################
    # Outpatient appointments #
    ###########################

    outpatient_count=patients.outpatient_appointment_date(
        returning="number_of_matches_in_period",
        attended=True,
        between=["index_date", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    # Oupatient appointments X: n columns of date of admissions, date of discharge, admission method
    **outpatient_date_X(
        n=n_outpatient
    ),

    ###################
    # GP interactions #
    ###################
    
    # Number of GP-patient interactions in period
    gp_contact_count=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["index_date", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 3},
            "incidence": 1,
        },
    ),

    # GP contact X: n columns of date of GP consultations
    **gp_contact_date_X(
       n=n_gp
    ),    

    #################
    # Covid Testing #
    #################

    # 1st positive covid test date
    covid_positive_test_date_1=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
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

    # 1st negative covid test date
    covid_negative_test_date_1=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="negative",
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

    # 1st negative covid test date on or before 1st positive covid test
    covid_negative_test_date_before_positive=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="negative",
        on_or_before="covid_positive_test_date_1",
        find_last_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "exponential_increase",
            "incidence": 0.2
        },
    ),

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
