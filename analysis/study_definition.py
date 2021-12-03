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
     
    # Calculate days in study
    date_format = "%Y-%m-%d"
    a = datetime.datetime.strptime(start_date, date_format)
    b = datetime.datetime.strptime(end_date, date_format)
    delta = b - a

    for i in range(1, n+1):
        # Create staged date ranges for admission and discharge dates
        date_1 = a + datetime.timedelta(days = delta.days*(i-1)/n+1)
        date_2 = a + datetime.timedelta(days = delta.days*(i-0.5)/n)
        date_3 = a + datetime.timedelta(days = delta.days*(i)/n)
        date_1_str = date_1.strftime(date_format)
        date_2_str = date_2.strftime(date_format)
        date_3_str = date_3.strftime(date_format)
        # Expections for admission dates
        return_expectations_date_adm={
            "date": {"earliest": date_1_str, "latest": date_2_str},
            "rate": "uniform",
            "incidence": 0.3
            }
        # Expections for discharge dates
        return_expectations_date_dis={
            "date": {"earliest": date_2_str, "latest": date_3_str},
            "rate": "uniform",
            "incidence": 0.3
            }
        if i == 1:
            variables = var_signature("admission_date_1", "date_admitted", "index_date + 1 day", return_expectations_date_adm)
            variables.update(var_signature("discharge_date_1", "date_discharged", "admission_date_1", return_expectations_date_dis))
            variables.update(var_signature("admission_method_1", "admission_method", "admission_date_1", return_expectations_method))
        else:
            variables.update(var_signature(f"admission_date_{i}", "date_admitted", f"admission_date_{i-1} + 1 day", return_expectations_date_adm))
            variables.update(var_signature(f"discharge_date_{i}", "date_discharged", f"admission_date_{i-1} + 1 day", return_expectations_date_dis))
            variables.update(var_signature(f"admission_method_{i}", "admission_method", f"admission_date_{i-1} + 1 day", return_expectations_method))
    return variables


# gp_consultation_date_X: Creates n columns for each consecutive GP consulation date
def gp_consultation_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_gp_consultations(
                    returning="date",
                    on_or_after=on_or_after,
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
            variables = var_signature("gp_consultation_date_1", "index_date + 1 day")
        else:
            variables.update(var_signature(f"gp_consultation_date_{i}", f"gp_consultation_date_{i-1} + 1 day"))
    return variables

####################
# Study Definition #
####################


# import global-variables.json
with open("./analysis/global_variables.json") as f:
    gbl_vars = json.load(f)

# define variables explicitly
start_date = gbl_vars["start_date"] # change this in global-variables.json if necessary
end_date = gbl_vars["end_date"] # change this in global-variables.json if necessary

# Number of hospital admissions and GP interactions to query
n_admission = 5
n_gp = 5

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
        (age < 19)
        AND
        (sex = "M" OR sex = "F")
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

    # https://github.com/opensafely/risk-factors-research/issues/49
    age=patients.age_as_of(
        "2020-03-31",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
            "incidence": 1
        },
    ),

    # https://github.com/opensafely/risk-factors-research/issues/46
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),

    # https://github.com/opensafely/risk-factors-research/issues/51
    bmi=patients.categorised_as(
        {
            "Not obese": "DEFAULT",
            "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
            "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
            "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
            # set maximum to avoid any impossibly extreme values being
            # classified as obese
        },
        bmi_value=patients.most_recent_bmi(
            on_or_after="2015-12-01",
            minimum_age_at_measurement=16
            ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "Not obese": 0.7,
                    "Obese I (30-34.9)": 0.1,
                    "Obese II (35-39.9)": 0.1,
                    "Obese III (40+)": 0.1,
                }
            },
        },
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
            "incidence": 0.8,
            },
    ),

    # NHS administrative region
    region=patients.registered_practice_as_of(
        "index_date",
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

    # IMD - quintile
    imd=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >=1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 """,
        },
        index_of_multiple_deprivation=patients.address_as_of(
            "index_date",
            returning="index_of_multiple_deprivation",
            round_to_nearest=100,
        ),
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.01,
                    "1": 0.20,
                    "2": 0.20,
                    "3": 0.20,
                    "4": 0.20,
                    "5": 0.19,
                }
            },
        },
    ),

    #########
    # Death #
    #########
    death_date=patients.died_from_any_cause(
        between=[start_date, end_date],
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": start_date, "latest": end_date},
            "rate": "uniform"
        },
    ),

    ##############
    # Conditions #
    ##############

    # CURRENT ASTHMA
    asthma=patients.with_these_clinical_events(
        current_asthma_codes,
        on_or_before=start_date,
        returning = "binary_flag",
        find_first_match_in_period=True,
        return_expectations = {"incidence": 0.05}
    ),

    # DIABETES
    diabetes=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before=start_date,
        returning = "binary_flag",
        find_first_match_in_period=True,
        return_expectations = {"incidence": 0.05}
    ),

    #######################
    # Hospital Admissions #
    #######################

    # Number of hospital admissions in period
    hospital_admissions_total=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["index_date", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 5},
            "incidence": 1,
        },
    ),

    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **admitted_to_hospital_X(
        n=n_admission
    ),
    
    ###################
    # GP interactions #
    ###################
    
    # Number of GP-patient interactions in period
    gp_consultations_total=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["index_date", end_date],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 5},
            "incidence": 1,
        },
    ),

    # Hospital admission X: n columns of date of admissions, date of discharge, admission method
    **gp_consultation_date_X(
        n=n_gp
    ),
    

    #################
    # Covid Testing #
    #################

    # 1st positive covid test date
    covid_positive_test_date_1=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        on_or_after="2020-02-01",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-02-01"},
            "rate": "exponential_increase",
            "incidence": 0.2
        },
    ),
)
