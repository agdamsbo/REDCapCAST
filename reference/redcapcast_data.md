# Data set for demonstration

This is a small dataset from a REDCap database for demonstrational
purposes. Contains only synthetic data.

## Usage

``` r
data(redcapcast_data)
```

## Format

A data frame with 22 variables:

- record_id:

  ID, numeric

- redcap_event_name:

  Event name, character

- redcap_repeat_instrument:

  Repeat instrument, character

- redcap_repeat_instance:

  Repeat instance, numeric

- cpr:

  CPR number, character

- inclusion:

  Inclusion date, Date

- inclusion_time:

  Inclusion time, hms

- dob:

  Date of birth, Date

- age:

  Age decimal, numeric

- age_integer:

  Age integer, numeric

- sex:

  Legal sex, character

- cohabitation:

  Cohabitation status, character

- con_calc:

  con_calc

- con_mrs:

  con_mrs

- consensus_complete:

  consensus_complete

- hypertension:

  Hypertension, character

- diabetes:

  diabetes, character

- region:

  region, character

- baseline_data_start_complete:

  Completed, character

- mrs_assessed:

  mRS Assessed, character

- mrs_date:

  Assesment date, Date

- mrs_score:

  Categorical score, numeric

- mrs_complete:

  Complete, numeric

- event_datetime:

  Event datetime, POSIXct

- event_age:

  Age at time of event, numeric

- event_type:

  Event type, character

- new_event_complete:

  Completed, character
