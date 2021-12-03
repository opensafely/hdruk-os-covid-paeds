from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import Codelists
from codelists import *

#############
# FUNCTIONS #
#############

# admitted_to_hospital_date_X: Creates n columns for hospital admission/discharge dates
def admitted_to_hospital_date_X(n):
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

    return_expectations_date={
        "date": {"earliest": start_date, "latest": end_date},
        "rate": "uniform",
        "incidence": 1
        }
    return_expectations_method={
        "category": {"ratios": {"cat1": 0.1, "cat2": 0.2, "cat3": 0.7}}, "incidence": 1}
    variables = var_signature("admission_date_1", "date_admitted", "index_date + 1 day", return_expectations_date)
    variables.update(var_signature("discharge_date_1", "date_discharged", "admission_date_1", return_expectations_date))
    variables.update(var_signature("admission_method_1", "admission_method", "admission_date_1", return_expectations_method))
    for i in range(2, n+1):
        variables.update(var_signature(f"admission_date_{i}", "date_admitted", f"admission_date_{i-1} + 1 day", return_expectations_date))
        variables.update(var_signature(f"discharge_date_{i}", "date_discharged", f"admission_date_{i-1} + 1 day", return_expectations_date))
        variables.update(var_signature(f"admission_method_{i}", "admission_method", f"admission_date_{i-1} + 1 day", return_expectations_method))
    return variables


####################
# Study Definition #
####################

# import json module
import json

# import global-variables.json
with open("./analysis/global_variables.json") as f:
    gbl_vars = json.load(f)

# define variables explicitly
start_date = gbl_vars["start_date"] # change this in global-variables.json if necessary
end_date = gbl_vars["end_date"] # change this in global-variables.json if necessary

# start_date is currently set to the start of the vaccination campaign
# end_date depends on the most reent data coverage in the database, and the particular variables of interest


n_max = 5

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
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": start_date}},
    ),

    # DIABETES
    diabetes=patients.with_these_clinical_events(
        diabetes_codes,
        on_or_before=start_date,
        return_first_date_in_period=True,
        include_month=True,
        return_expectations={"date": {"latest": start_date}},
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

    # Hospital admission X: X columns of all date of admissions
    **admitted_to_hospital_date_X(
        n=n_max
    ),
    

    #################
    # Covid Testing #
    #################

    # First positive covid test date
    first_positive_test_date=patients.with_test_result_in_sgss(
        pathogen="SARS-CoV-2",
        test_result="positive",
        on_or_after="2020-02-01",
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-02-01"},
            "rate": "exponential_increase"
        },
    ),
)
