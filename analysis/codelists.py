from cohortextractor import (codelist, codelist_from_csv, combine_codelists)

asthma_diagnosis_codelist = codelist_from_csv(
    "codelists/opensafely-asthma-diagnosis-snomed.csv",
    system="snomed",
    column="id",
)

cancer_excl_lung_and_haem_codelist = codelist_from_csv(
    "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
    system="ctv3",
    column="CTV3ID",
)

cerebral_palsy_codelist = codelist_from_csv(
    "codelists/opensafely-cerebral-palsy.csv",
    system="ctv3",
    column="code",
)

chronic_cardiac_disease_codelist = codelist_from_csv(
    "codelists/opensafely-chronic-cardiac-disease.csv",
    system="ctv3",
    column="CTV3ID",
)

chronic_kidney_disease_codelist = codelist_from_csv(
    "codelists/opensafely-chronic-kidney-disease.csv",
    system="ctv3",
    column="CTV3ID",
)

chronic_liver_disease_codelist = codelist_from_csv(
    "codelists/opensafely-chronic-liver-disease.csv",
    system="ctv3",
    column="CTV3ID",
)

chronic_respiratory_disease_codelist = codelist_from_csv(
    "codelists/opensafely-chronic-respiratory-disease-snomed.csv",
    system="snomed",
    column="id",
)

covid_codelist = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    system="icd10",
    column="icd10_code",
)

current_asthma_codelist = codelist_from_csv(
    "codelists/opensafely-current-asthma.csv",
    system="ctv3",
    column="CTV3ID",
)

diabetes_codelist = codelist_from_csv(
    "codelists/opensafely-diabetes-snomed.csv",
    system="snomed",
    column="id",
)

ethnicity_codes = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    system="ctv3",
    column="Code",
    category_column="Grouping_6",
)

cancer_haem_codelist = codelist_from_csv(
    "codelists/opensafely-haematological-cancer.csv",
    system="ctv3",
    column="CTV3ID",
)

hiv_codelist = codelist_from_csv(
    "codelists/opensafely-hiv.csv",
    system="ctv3",
    column="CTV3ID",
)

intellectual_disability_codelist = codelist_from_csv(
    "codelists/opensafely-intellectual-disability-including-downs-syndrome.csv",
    system="ctv3",
    column="CTV3ID",
) 

cancer_lung = codelist_from_csv(
    "codelists/opensafely-lung-cancer.csv",
    system="ctv3",
    column="CTV3ID",
)

heart_disease_other_codelist = codelist_from_csv(
    "codelists/opensafely-other-heart-disease.csv",
    system="ctv3",
    column="CTV3ID",
)

other_neurological_conditions_codelist = codelist_from_csv(
    "codelists/opensafely-other-neurological-conditions-snomed.csv",
    system="snomed",
    column="id",
)

permanent_immunosuppression_codelist = codelist_from_csv(
    "codelists/opensafely-permanent-immunosuppression.csv",
    system="ctv3",
    column="CTV3ID",
)

sickle_cell_disease_codelist = codelist_from_csv(
    "codelists/opensafely-sickle-cell-disease.csv",
    system="ctv3",
    column="CTV3ID",
)

neurological_diseases_codelist = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cns_cov_cod.csv",
    system="snomed",
    column="code",
)

learning_disabilities_codelist = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-learndis_cod.csv",
    system="snomed",
    column="code",
)

pregnency_delivery_codelist = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-pregdel_cod.csv",
    system="snomed",
    column="code",
)

severe_mental_illness_codelist = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_mental.csv",
    system="snomed",
    column="code",
)

severe_obesity_codelist = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-sev_obesity.csv",
    system="snomed",
    column="code",
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