from cohortextractor import (codelist, codelist_from_csv, combine_codelists)

covid_codelist = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

current_asthma_codes = codelist_from_csv(
    "codelists/opensafely-current-asthma.csv",
    system="ctv3",
    column="CTV3ID",
)

diabetes_codes = codelist_from_csv(
    "codelists/opensafely-diabetes.csv",
    system="ctv3",
    column="CTV3ID",
)

ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

# Snowmed

central_nervous_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-central-nervous-system-finding-all-descendants.csv",
    system="snomed",
    column="code",
)

pregnancy_childbirth_complication_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-complication-of-pregnancy-childbirth-andor-the-puerperium-all-descendants.csv",
    system="snomed",
    column="code",
)

congenital_disease_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-congenital-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

auditory_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-auditory-system-all-descendants.csv",
    system="snomed",
    column="code",
)

cardiovascular_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-cardiovascular-system-all-descendants.csv",
    system="snomed",
    column="code",
)

cellular_component_of_blood_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-cellular-component-of-blood-all-descendants.csv",
    system="snomed",
    column="code",
)

connective_tissue_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-connective-tissue-all-descendants.csv",
    system="snomed",
    column="code",
)

digestive_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-digestive-system-all-descendants.csv",
    system="snomed",
    column="code",
)

endocrine_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-endocrine-system-all-descendants.csv",
    system="snomed",
    column="code",
)

fetus_newborn_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-fetus-or-newborn-all-descendants.csv",
    system="snomed",
    column="code",
)

hematopoietic_structure_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-hematopoietic-structure-all-descendants.csv",
    system="snomed",
    column="code",
)

hematopoietic_structure_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-immune-function-all-descendants.csv",
    system="snomed",
    column="code",
)

labor_delivery_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-labor-delivery-all-descendants.csv",
    system="snomed",
    column="code",
)

musculoskeletal_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-musculoskeletal-system-all-descendants.csv",
    system="snomed",
    column="code",
)

nervous_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-nervous-system-all-descendants.csv",
    system="snomed",
    column="code",
)

puerperium_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-puerperium-all-descendants.csv",
    system="snomed",
    column="code",
)

respiratory_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-respiratory-system-all-descendants.csv",
    system="snomed",
    column="code",
)

skin_subcutaneous_tissue_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-skin-andor-subcutaneous-tissue-all-descendants.csv",
    system="snomed",
    column="code",
)

genitourinary_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-the-genitourinary-system-all-descendants.csv",
    system="snomed",
    column="code",
)

infectious_disease_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-infectious-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

mental_disorder_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-mental-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)

metabolic_disease_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-metabolic-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

neoplastic_disease_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-neoplastic-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

nutritional_disorder_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-nutritional-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)

poisoning_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-poisoning-all-descendants.csv",
    system="snomed",
    column="code",
)

traumatic_non_traumatic_injury_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-traumatic-andor-non-traumatic-injury-all-descendants.csv",
    system="snomed",
    column="code",
)

visual_system_codes = codelist_from_csv(
    "codelists/user-kate-mansfield-visual-system-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)