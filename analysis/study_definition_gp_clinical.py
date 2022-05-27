from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA

# Import json and datetime module
import json
import datetime

############
# Codelist #
############

# Import Codelists
from codelists import *

combined_clinical_codes = combine_codelists(
    central_nervous_system_codes,
    pregnancy_childbirth_complication_codes,
    congenital_disease_codes,
    auditory_system_codes,
    cardiovascular_system_codes,
    cellular_component_of_blood_codes,
    connective_tissue_codes,
    digestive_system_codes,
    endocrine_system_codes,
    fetus_newborn_codes,
    hematopoietic_structure_codes,
    immune_function_codes,
    labor_delivery_codes,
    musculoskeletal_system_codes,
    nervous_system_codes,
    puerperium_codes,
    respiratory_system,
    skin_subcutaneous_tissue_codes,
    genitourinary_system_codes,
    infectious_disease_codes,
    mental_disorder_codes,
    metabolic_disease_codes,
    neoplastic_disease_codes,
    nutritional_disorder_codes,
    poisoning_codes,
    traumatic_non_traumatic_injury_codes,
    visual_system_codes
)


#############
# Functions #
#############

# gp_contact_date_X: Creates n columns for each consecutive GP consulation date
def gp_contact_date_X(n):
    def var_signature(name, on_or_after):
        return {
            name: patients.with_these_clinical_events(
                    codelist = combined_clinical_codes,
                    returning="date",
                    between=[on_or_after, "index_date + 6 days"],
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

# Define study date variables
start_date = gbl_vars["start_date"]
end_date   = gbl_vars["end_date"]

# Number of hospital admissions, outpatient appointments, GP interactions, covid tests to query
n_gp      = gbl_vars["n_gp"]

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
        AND (registered_at_end_date OR died_after_start_date)
        AND (age > 1) AND (age < 18)
        AND (gp_contact_count > 0)
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
        died_after_start_date=patients.died_from_any_cause(
            on_or_before=end_date,
            returning="binary_flag",
        ),
        age=patients.age_as_of(
            start_date,
        ),
    ),

    ##############
    # GP Contact #
    ##############

    # GP contact X: n columns of date of GP consultations
    **gp_contact_date_X(
       n=n_gp
    ),

    # Number of GP contacts during period
    gp_contact_count=patients.with_these_clinical_events(
        codelist = combined_clinical_codes,
        returning="number_of_matches_in_period",
        between=["index_date", "index_date + 6 days"],
        return_expectations={
            "int": {"distribution": "poisson", "mean": 1},
            "incidence": 1,
        },
    ),

)
