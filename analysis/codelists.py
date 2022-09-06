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

shielding_codes = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    system="snomed",
    column="code",
)

snomed_mapped_0_occupations = codelist_from_csv(
    "analysis/codelists/snomed_mapped_0_occupations.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_1_history_symptoms = codelist_from_csv(
    "analysis/codelists/snomed_mapped_1_history_symptoms.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_2_examination_signs = codelist_from_csv(
    "analysis/codelists/snomed_mapped_2_examination_signs.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_3_diagnostic_procedures = codelist_from_csv(
    "analysis/codelists/snomed_mapped_3_diagnostic_procedures.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_4_laboratory_procedures = codelist_from_csv(
    "analysis/codelists/snomed_mapped_4_laboratory_procedures.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_5_radiology_physics_in_medicine = codelist_from_csv(
    "analysis/codelists/snomed_mapped_5_radiology_physics_in_medicine.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_6_preventive_procedures = codelist_from_csv(
    "analysis/codelists/snomed_mapped_6_preventive_procedures.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_7_operations_procedures_sites = codelist_from_csv(
    "analysis/codelists/snomed_mapped_7_operations_procedures_sites.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_8_other_therapeutic_procedures = codelist_from_csv(
    "analysis/codelists/snomed_mapped_8_other_therapeutic_procedures.csv",
    system="snomed",
    column="conceptId",
)

snomed_mapped_9_administration = codelist_from_csv(
    "analysis/codelists/snomed_mapped_9_administration.csv",
    system="snomed",
    column="conceptId",
)



snomed_disorder = codelist_from_csv(
    "analysis/codelists/snomed-disorder.csv",
    system="snomed",
    column="code",
)

snomed_finding = codelist_from_csv(
    "analysis/codelists/snomed-finding_no_vacc.csv",
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



snomed_KM_central_nervous_system = codelist_from_csv(
    "codelists/user-kate-mansfield-central-nervous-system-finding-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_pregnancy_childbirth = codelist_from_csv(
    "codelists/user-kate-mansfield-complication-of-pregnancy-childbirth-andor-the-puerperium-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_congenital_disease = codelist_from_csv(
    "codelists/user-kate-mansfield-congenital-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_auditory_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-auditory-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_cardiovascular_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-cardiovascular-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_cellular_component_blood = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-cellular-component-of-blood-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_connective_tissue = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-connective-tissue-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_digestive_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-digestive-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_endocrine_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-endocrine-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_fetus_newborn = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-fetus-or-newborn-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_hematopoietic_structure = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-hematopoietic-structure-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_immune_function = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-immune-function-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_labor_delivery = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-labor-delivery-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_musculoskeletal_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-musculoskeletal-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_nervous_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-nervous-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_puerperium = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-puerperium-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_respiratory_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-respiratory-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_skin_subcutaneous_tissue = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-skin-andor-subcutaneous-tissue-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_genitourinary_system = codelist_from_csv(
    "codelists/user-kate-mansfield-disorder-of-the-genitourinary-system-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_infectious_disease = codelist_from_csv(
    "codelists/user-kate-mansfield-infectious-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_mental_disorder = codelist_from_csv(
    "codelists/user-kate-mansfield-mental-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_metabolic_disease = codelist_from_csv(
    "codelists/user-kate-mansfield-metabolic-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_neoplastic_disease = codelist_from_csv(
    "codelists/user-kate-mansfield-neoplastic-disease-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_nutritional_disorder = codelist_from_csv(
    "codelists/user-kate-mansfield-nutritional-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_poisoning = codelist_from_csv(
    "codelists/user-kate-mansfield-poisoning-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_traumatic_injury = codelist_from_csv(
    "codelists/user-kate-mansfield-traumatic-andor-non-traumatic-injury-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_visual_system = codelist_from_csv(
    "codelists/user-kate-mansfield-visual-system-disorder-all-descendants.csv",
    system="snomed",
    column="code",
)

snomed_KM_self_harm = codelist_from_csv(
    "analysis/codelists/SNOMED_self_harm.csv",
    system="snomed",
    column="code",
)


treatment_function_cardiology = codelist_from_csv(
    "analysis/codelists/treatment_function_cardiology.csv",
    system=None,
    column="code",
)

treatment_function_community_paediatrics = codelist_from_csv(
    "analysis/codelists/treatment_function_community_paediatrics.csv",
    system=None,
    column="code",
)

treatment_function_dermatology = codelist_from_csv(
    "analysis/codelists/treatment_function_dermatology.csv",
    system=None,
    column="code",
)
treatment_function_dietetics = codelist_from_csv(
    "analysis/codelists/treatment_function_dietetics.csv",
    system=None,
    column="code",
)

treatment_function_endocrinology = codelist_from_csv(
    "analysis/codelists/treatment_function_endocrinology.csv",
    system=None,
    column="code",
)

treatment_function_gastrointestinal = codelist_from_csv(
    "analysis/codelists/treatment_function_gastrointestinal.csv",
    system=None,
    column="code",
)

treatment_function_general = codelist_from_csv(
    "analysis/codelists/treatment_function_general.csv",
    system=None,
    column="code",
)

treatment_function_haematology = codelist_from_csv(
    "analysis/codelists/treatment_function_haematology.csv",
    system=None,
    column="code",
)

treatment_function_immunology_and_allergy = codelist_from_csv(
    "analysis/codelists/treatment_function_immunology_and_allergy.csv",
    system=None,
    column="code",
)

treatment_function_infectious_disease = codelist_from_csv(
    "analysis/codelists/treatment_function_infectious_disease.csv",
    system=None,
    column="code",
)

treatment_function_mental_health = codelist_from_csv(
    "analysis/codelists/treatment_function_mental_health.csv",
    system=None,
    column="code",
)

treatment_function_metabolic = codelist_from_csv(
    "analysis/codelists/treatment_function_metabolic.csv",
    system=None,
    column="code",
)

treatment_function_neurology = codelist_from_csv(
    "analysis/codelists/treatment_function_neurology.csv",
    system=None,
    column="code",
)

treatment_function_occupational_therapy = codelist_from_csv(
    "analysis/codelists/treatment_function_occupational_therapy.csv",
    system=None,
    column="code",
)

treatment_function_oncology = codelist_from_csv(
    "analysis/codelists/treatment_function_oncology.csv",
    system=None,
    column="code",
)

treatment_function_pain_management_and_palliative_care = codelist_from_csv(
    "analysis/codelists/treatment_function_pain_management_and_palliative_care.csv",
    system=None,
    column="code",
)

treatment_function_physiology = codelist_from_csv(
    "analysis/codelists/treatment_function_physiology.csv",
    system=None,
    column="code",
)

treatment_function_physiotherapy = codelist_from_csv(
    "analysis/codelists/treatment_function_physiotherapy.csv",
    system=None,
    column="code",
)

treatment_function_post_covid_clinic = codelist_from_csv(
    "analysis/codelists/treatment_function_post_covid_clinic.csv",
    system=None,
    column="code",
)

treatment_function_rehabilitation = codelist_from_csv(
    "analysis/codelists/treatment_function_rehabilitation.csv",
    system=None,
    column="code",
)

treatment_function_renal = codelist_from_csv(
    "analysis/codelists/treatment_function_renal.csv",
    system=None,
    column="code",
)

treatment_function_respiratory = codelist_from_csv(
    "analysis/codelists/treatment_function_respiratory.csv",
    system=None,
    column="code",
)

treatment_function_rheumatology = codelist_from_csv(
    "analysis/codelists/treatment_function_rheumatology.csv",
    system=None,
    column="code",
)

treatment_function_speech_and_language_therapy = codelist_from_csv(
    "analysis/codelists/treatment_function_speech_and_language_therapy.csv",
    system=None,
    column="code",
)

