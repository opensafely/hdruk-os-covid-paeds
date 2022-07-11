from cohortextractor import (codelist, codelist_from_csv, combine_codelists)

covid_codelist = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

snomed_disorder = codelist_from_csv(
    "analysis/codelists/snomed-disorder.csv",
    system="snomed",
    column="code",
)

snomed_finding = codelist_from_csv(
    "analysis/codelists/snomed-finding.csv",
    system="snomed",
    column="code",
)

snomed_observable_entity = codelist_from_csv(
    "analysis/codelists/snomed-observable-entity.csv",
    system="snomed",
    column="code",
)

snomed_procedure = codelist_from_csv(
    "analysis/codelists/snomed-procedure.csv",
    system="snomed",
    column="code",
)

snomed_regime_therapy = codelist_from_csv(
    "analysis/codelists/snomed-regime-therapy.csv",
    system="snomed",
    column="code",
)

snomed_specimen = codelist_from_csv(
    "analysis/codelists/snomed-specimen.csv",
    system="snomed",
    column="code",
)

snomed_asthma = codelist_from_csv(
    "analysis/codelists/SNOMED_athma_disorders.csv",
    system="snomed",
    column="code",
)

snomed_cancer = codelist_from_csv(
    "analysis/codelists/SNOMED_cancer_disorders.csv",
    system="snomed",
    column="code",
)

snomed_cystic_fibrosis = codelist_from_csv(
    "analysis/codelists/SNOMED_CF_disorders.csv",
    system="snomed",
    column="code",
)

snomed_diabetes = codelist_from_csv(
    "analysis/codelists/SNOMED_diabetes_disorders.csv",
    system="snomed",
    column="code",
)

snomed_epilepsy = codelist_from_csv(
    "analysis/codelists/SNOMED_epilepsy_disorders.csv",
    system="snomed",
    column="code",
)

snomed_severe_mental_illness = codelist_from_csv(
    "analysis/codelists/SNOMED_severe_mental_illness_disorders.csv",
    system="snomed",
    column="code",
)

snomed_cerebral_palsy = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_cerebral_palsy.csv",
    system="snomed",
    column="code",
)

snomed_chronic_infections = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_chronic_infections.csv",
    system="snomed",
    column="code",
)

snomed_devices_and_stomas = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_devices_and_stomas.csv",
    system="snomed",
    column="code",
)

snomed_endocrine = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_endocrine.csv",
    system="snomed",
    column="code",
)

snomed_gastrointestinal = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_gastrointestinal.csv",
    system="snomed",
    column="code",
)

snomed_haematology = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_haematology.csv",
    system="snomed",
    column="code",
)

snomed_immunological = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_immunological.csv",
    system="snomed",
    column="code",
)

snomed_learning_difficulties_and_behaviour = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_learning_difficulties_and_behaviour.csv",
    system="snomed",
    column="code",
)

snomed_mental_illness = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_mental_illness.csv",
    system="snomed",
    column="code",
)

snomed_musculoskeletal_and_rheum = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_musculoskeletal_and_rheum.csv",
    system="snomed",
    column="code",
)

snomed_transplant = codelist_from_csv(
    "analysis/codelists_draft/SNOMED_transplant.csv",
    system="snomed",
    column="code",
)