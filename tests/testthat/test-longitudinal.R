## "Longitudinal data"

test_that("CSV export matches reference", {
  file_paths <- vapply(
    c(
      records = "WARRIORtestForSoftwa_DATA_2018-06-21_1431.csv",
      metadata = "WARRIORtestForSoftwareUpgrades_DataDictionary_2018-06-21.csv"
    ), get_data_location,
    FUN.VALUE = "character"
  )

  redcap <- lapply(file_paths, read.csv, stringsAsFactors = FALSE)
  redcap[["metadata"]] <- with(redcap, metadata[metadata[, 1] > "", ])
  redcap_output <- with(redcap, REDCap_split(records, metadata))


  # expect_known_hash(redcap_output, "0934bcb292")
  expect_identical(redcap_output
    ,
    list(
      structure(list(record_id = c(
        "806-1", "806-1", "806-2",
        "806-2"
      ), redcap_event_name = c(
        "baseline_arm_1", "followup_month_3_arm_1",
        "baseline_arm_1", "followup_month_3_arm_1"
      ), redcap_data_access_group = c(
        "uf_test",
        "uf_test", "uf_test", "uf_test"
      ), redcap_survey_identifier = c(
        NA,
        NA, NA, NA
      ), signed_consent_1 = c(
        "[document]", "", "[document]",
        ""
      ), signed_consent_2 = c(NA, NA, NA, NA), signed_consent_3 = c(
        NA,
        NA, NA, NA
      ), signed_addendum1 = c(
        "[document]", "", "[document]",
        ""
      ), signed_addendum2 = c(NA, NA, NA, NA), signed_addendum3 = c(
        NA,
        NA, NA, NA
      ), upload_of_signed_icfs_complete = c(2L, NA, 2L, NA), demo_date = c("2018-05-08", "", "2018-05-08", ""), demo_name_first = c(
        "Philip",
        "", "afadgs", ""
      ), demo_name_init = c("B", "", "afd", ""), demo_name_last = c(
        "Chase",
        "", "afdsgfd", ""
      ), demo_date_birth = c(
        "1964-04-09", "", "1977-06-26",
        ""
      ), demo_street_ad = c("5959 NW 13th Ave", "", "24325543", ""), demo_city_ad = c("Gainesville", "", "2352453", ""), demo_state_ad = c(
        "FL",
        "", "fwef", ""
      ), demo_zip = c(32605L, NA, 32601L, NA), demo_daytime = c(
        "(352) 555-0760",
        "", "(352) 294-5299", ""
      ), demo_email = c(
        "bobsyouruncle@example.org",
        "", "", ""
      ), demo_ethnic = c(2L, NA, 2L, NA), demo_racial = c(
        5L,
        NA, 89L, NA
      ), demo_racial_oth = c(NA, NA, NA, NA), demo_military_mrn = c(
        2L,
        NA, NA, NA
      ), demo_ssn = c("111-22-3333", "", "123-45-6789", ""), demographics_complete = c(2L, NA, 2L, NA), elig_icf = c(
        1L,
        NA, 1L, NA
      ), elig_ischem = c(1L, NA, 1L, NA), elig_signs___1 = c(
        1L,
        NA, 0L, NA
      ), elig_signs___2 = c(0L, NA, 1L, NA), elig_signs___3 = c(
        0L,
        NA, 0L, NA
      ), elig_signs___4 = c(0L, NA, 0L, NA), elig_card_cath = c(
        1L,
        NA, 0L, NA
      ), elig_card_cath_details = c(1L, NA, NA, NA), elig_cath_disease_severity = c(
        NA,
        NA, NA, NA
      ), elig_cath_vessel = c(NA, NA, NA, NA), elig_ejection_fraction = c(
        60L,
        NA, NA, NA
      ), elig_cath_ffr = c(1L, NA, NA, NA), elig_ccta = c(
        1L,
        NA, 0L, NA
      ), elig_card_cath_details_2 = c(1L, NA, NA, NA), elig_cath_disease_severity_2 = c(
        NA,
        NA, NA, NA
      ), elig_ejection_fraction_2 = c(60L, NA, NA, NA), elig_cta_score = c(
        24L,
        NA, NA, NA
      ), elig_nocom_med = c(0L, NA, 0L, NA), elig_ischemia_dilated = c(
        0L,
        NA, 0L, NA
      ), elig_doc_acs = c(0L, NA, 0L, NA), elig_lvef = c(
        0L,
        NA, 0L, NA
      ), elig_nyha_class = c(0L, NA, 0L, NA), elig_hos_hfref = c(
        0L,
        NA, 0L, NA
      ), elig_stroke = c(0L, NA, 0L, NA), elig_carnial_hemo = c(
        0L,
        NA, 0L, NA
      ), elig_renal = c(0L, NA, 0L, NA), elig_valvular = c(
        0L,
        NA, 0L, NA
      ), elig_life_expect = c(0L, NA, 0L, NA), elig_enroll_clinic = c(
        0L,
        NA, 0L, NA
      ), elig_intol_ace = c(0L, NA, 0L, NA), elig_intol_arb = c(
        0L,
        NA, 0L, NA
      ), elig_intol_statin = c(0L, NA, 0L, NA), elig_intol_pcsk = c(
        NA,
        NA, NA, NA
      ), elig_preg = c(0L, NA, 0L, NA), elig_liver_dis = c(
        0L,
        NA, 0L, NA
      ), elig_hist_rhab = c(0L, NA, 0L, NA), elig_high_dose = c(
        0L,
        NA, 0L, NA
      ), elig_study_yes = c(1L, NA, 1L, NA), elig_date = c(
        "2018-05-08",
        "", "2018-05-08", ""
      ), elig_study_no = c(NA, NA, NA, NA), eligibility_complete = c(
        2L,
        NA, 2L, NA
      )), row.names = c(1L, 2L, 7L, 8L), class = "data.frame"),
      informed_consent = structure(list(record_id = c(
        "806-1",
        "806-2"
      ), redcap_event_name = c("baseline_arm_1", "baseline_arm_1"), redcap_repeat_instrument = c("informed_consent", "informed_consent"), redcap_repeat_instance = c(1L, 1L), redcap_data_access_group = c(
        "uf_test",
        "uf_test"
      ), redcap_survey_identifier = c(NA, NA)), row.names = c(
        3L,
        9L
      ), class = "data.frame"), informed_consent_and_addendum = structure(list(
        record_id = c("806-1", "806-1", "806-1", "806-2"), redcap_event_name = c(
          "baseline_arm_1",
          "baseline_arm_1", "baseline_arm_1", "baseline_arm_1"
        ),
        redcap_repeat_instrument = c(
          "informed_consent_and_addendum",
          "informed_consent_and_addendum", "informed_consent_and_addendum",
          "informed_consent_and_addendum"
        ), redcap_repeat_instance = c(
          1L,
          2L, 3L, 1L
        ), redcap_data_access_group = c(
          "uf_test",
          "uf_test", "uf_test", "uf_test"
        ), redcap_survey_identifier = c(
          NA,
          NA, NA, NA
        ), informed_consent_and_addendum_timestamp = c(
          "2018-05-08 21:15:12",
          "", "", "2018-05-08 21:02:39"
        ), icf_first_name = c(
          "Philip",
          "Bobs", "Bobs", "test"
        ), icf_last_name = c(
          "Chase", "Youruncle",
          "Youruncle", "test"
        ), icf_date = c(
          "2018-05-08", "2018-06-21",
          "2018-06-21", "2018-05-08"
        ), icf_sign = c(
          "[document]",
          "[document]", "[document]", "[document]"
        ), icf_consenter_name = c(
          "Philip B Chase",
          "Yo Mama", "zsdf", "taryn"
        ), icf_consentee_info = c(
          "UF",
          "Anywhere she wants", "DF", "stoffs"
        ), icf_consentee_sign = c(
          "[document]",
          "[document]", "[document]", "[document]"
        ), icf_consentee_date = c(
          "2018-05-08",
          "2018-06-21", "2018-06-21", "2018-05-08"
        ), informed_consent_and_addendum_complete = c(
          2L,
          2L, 2L, 2L
        )
      ), row.names = c(4L, 5L, 6L, 10L), class = "data.frame")
    )
  )

})
