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


snomed_asthma = codelist_from_csv(
    "analysis/codelists/snomed_asthma.csv",
    system="snomed",
    column="code",
)

snomed_behavioural_and_developmental_including_autism = codelist_from_csv(
    "analysis/codelists/snomed_behavioural_and_developmental_including_autism.csv",
    system="snomed",
    column="code",
)

snomed_cancer = codelist_from_csv(
    "analysis/codelists/snomed_cancer.csv",
    system="snomed",
    column="code",
)

snomed_cardiovascular_congenital = codelist_from_csv(
    "analysis/codelists/snomed_cardiovascular_congenital.csv",
    system="snomed",
    column="code",
)

snomed_cardiovascular_devices = codelist_from_csv(
    "analysis/codelists/snomed_cardiovascular_devices.csv",
    system="snomed",
    column="code",
)

snomed_cardiovascular_non_congenital = codelist_from_csv(
    "analysis/codelists/snomed_cardiovascular_non_congenital.csv",
    system="snomed",
    column="code",
)

snomed_cerebral_palsy_paralysis = codelist_from_csv(
    "analysis/codelists/snomed_cerebral_palsy_paralysis.csv",
    system="snomed",
    column="code",
)

snomed_chronic_infections = codelist_from_csv(
    "analysis/codelists/snomed_chronic_infections.csv",
    system="snomed",
    column="code",
)

snomed_congenital_endocrine = codelist_from_csv(
    "analysis/codelists/snomed_congenital_endocrine.csv",
    system="snomed",
    column="code",
)

snomed_congenital_malformation_syndromes_and_chromosomal = codelist_from_csv(
    "analysis/codelists/snomed_congenital_malformation_syndromes_and_chromosomal.csv",
    system="snomed",
    column="code",
)

snomed_congenital_neuro = codelist_from_csv(
    "analysis/codelists/snomed_congenital_neuro.csv",
    system="snomed",
    column="code",
)

snomed_congenital_renal = codelist_from_csv(
    "analysis/codelists/snomed_congenital_renal.csv",
    system="snomed",
    column="code",
)

snomed_congenital_urogenital = codelist_from_csv(
    "analysis/codelists/snomed_congenital_urogenital.csv",
    system="snomed",
    column="code",
)

snomed_cystic_fibrosis = codelist_from_csv(
    "analysis/codelists/snomed_cystic_fibrosis.csv",
    system="snomed",
    column="code",
)

snomed_diabetes = codelist_from_csv(
    "analysis/codelists/snomed_diabetes.csv",
    system="snomed",
    column="code",
)

snomed_endocrine_no_dm = codelist_from_csv(
    "analysis/codelists/snomed_endocrine_no_dm.csv",
    system="snomed",
    column="code",
)

snomed_epilepsy = codelist_from_csv(
    "analysis/codelists/snomed_epilepsy.csv",
    system="snomed",
    column="code",
)

snomed_gastrointestinal_devices = codelist_from_csv(
    "analysis/codelists/snomed_gastrointestinal_devices.csv",
    system="snomed",
    column="code",
)

snomed_gastrointestinal = codelist_from_csv(
    "analysis/codelists/snomed_gastrointestinal.csv",
    system="snomed",
    column="code",
)

snomed_genitourinary_non_congenital = codelist_from_csv(
    "analysis/codelists/snomed_genitourinary_non_congenital.csv",
    system="snomed",
    column="code",
)

snomed_haematology = codelist_from_csv(
    "analysis/codelists/snomed_haematology.csv",
    system="snomed",
    column="code",
)

snomed_headaches = codelist_from_csv(
    "analysis/codelists/snomed_headaches.csv",
    system="snomed",
    column="code",
)

snomed_immunological = codelist_from_csv(
    "analysis/codelists/snomed_immunological.csv",
    system="snomed",
    column="code",
)

snomed_mental_illness = codelist_from_csv(
    "analysis/codelists/snomed_mental_illness.csv",
    system="snomed",
    column="code",
)

snomed_metabolic = codelist_from_csv(
    "analysis/codelists/snomed_metabolic.csv",
    system="snomed",
    column="code",
)

snomed_neuro_devices = codelist_from_csv(
    "analysis/codelists/snomed_neuro_devices.csv",
    system="snomed",
    column="code",
)

snomed_neurological_no_epilepsy_or_cp_headaches = codelist_from_csv(
    "analysis/codelists/snomed_neurological_no_epilepsy_or_cp_headaches.csv",
    system="snomed",
    column="code",
)

snomed_obesity = codelist_from_csv(
    "analysis/codelists/snomed_obesity.csv",
    system="snomed",
    column="code",
)

snomed_palliative_care = codelist_from_csv(
    "analysis/codelists/snomed_palliative_care.csv",
    system="snomed",
    column="code",
)

snomed_renal_devices = codelist_from_csv(
    "analysis/codelists/snomed_renal_devices.csv",
    system="snomed",
    column="code",
)

snomed_resp_congenital = codelist_from_csv(
    "analysis/codelists/snomed_resp_congenital.csv",
    system="snomed",
    column="code",
)

snomed_resp_devices = codelist_from_csv(
    "analysis/codelists/snomed_resp_devices.csv",
    system="snomed",
    column="code",
)

snomed_respiratory_not_asthma_or_cf = codelist_from_csv(
    "analysis/codelists/snomed_respiratory_not_asthma_or_cf.csv",
    system="snomed",
    column="code",
)

snomed_rheumatology = codelist_from_csv(
    "analysis/codelists/snomed_rheumatology.csv",
    system="snomed",
    column="code",
)

snomed_self_harm = codelist_from_csv(
    "analysis/codelists/snomed_self_harm.csv",
    system="snomed",
    column="code",
)

snomed_severe_mental_illness = codelist_from_csv(
    "analysis/codelists/snomed_severe_mental_illness.csv",
    system="snomed",
    column="code",
)

snomed_transplant = codelist_from_csv(
    "analysis/codelists/snomed_transplant.csv",
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

