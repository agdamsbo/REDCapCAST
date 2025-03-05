# REDCapCAST 25.3.1

* FIX: `as_factor()` now interprets empty variables with empty levels attribute as logicals to avoid returning factors with empty levels.

* NEW: `as_logical()`: interprets vectors with two levels as logical if values matches supplied list of logical pairs like "TRUE"/"FALSE", "Yes"/"No" or 1/2. Eases interpretation of data from databases with minimal metadata. Works on vectors and for data.frames. Interprets vectors with single value also matching to any of supplied levels (Chooses first match pair if several matches).

* NEW: `easy_redcap()`: new parameter `data_format` to specify data format as c("wide", "list", "redcap", "long"). For now "redcap" and "long" is treated equally. This was added to ease MMRM analyses. In that case, missing baseline values can be carried forward as "last observation carried forward" using the `tidyr::fill()` function specifying variables to fill. Interesting discussion on filling data [here on Stackoverflow](https://stackoverflow.com/a/13810615). `redcap_read_tables()` now has the option "none" for the `split_forms` parameter to allow not splitting the data.

* FIX: `ds2dd_detailed()`: The `convert_logicals` parameter has been turned off by default and logicals are now interpreted as field type "truefalse". Converting logicals to factors would result in the numeric values being 1 for FALSE and 2 for TRUE, which is opposite of the traditional notation and could lead to serous problems if not handled correctly. This should solve it.

# REDCapCAST 25.1.1

The newly introduced extension of `forcats::fct_drop()` has been corrected to work as intended as a method.

Conversion of column names to `field_names` are aligning better with REDCap naming.

Shorten variable names above 100 characters (REDCap criteria; note recommended variable name length is <26)

Fixed a params conflict in easy_redcap() when specifying raw_or_label.

# REDCapCAST 24.12.1

This release attempts to solve problems hosting the shiny_cast app, while also implementing functions to preserve as much meta data as possible from the REDCap database when exporting data.

The hosting on shinyapps.io has given a lot of trouble recently. Modified package structure a little around the `shiny_cast()`, to accommodate an alternative hosting approach with all package functions included in a script instead of requiring the package.

* NEW: A new option to `raw_or_label` in `read_redcap_tables()` has been added: "both". Get raw values with REDCap labels applied as labels. Use `as_factor()` to format factors with original labels and use the `gtsummary` package to easily get beautiful tables with original labels from REDCap. Use `fct_drop()` to drop empty levels.

* NEW: fct_drop() has been added with an extension to `forcats::fct_drop()`, that works across data.frames. Use as `fct_drop()`.

* CHANGE: the default data export method of `easy_redcap()` has been changed to use the new labelled data export with `read_redcap_tables()`.

# REDCapCAST 24.11.3

* BUG: shiny_cast() fails to load as I missed loading REDCapCAST library in ui.r. Fixed. Tests would be great.


# REDCapCAST 24.11.2

24.11.1 was rejected on CRAN based on wrong title capitalisation. This was an opportunity to extend the package overhaul. And this actually turned out to be a major step towards a very usable shiny app which have received most of the focus.

I have implemented option to specify categorical variables to factorize, but doing this with a modified version of {forcats} and {haven}'s `as_factor()`, that will preserve any attributes applied to the data to be able to upload and cast REDCap meta data from richly formatted data (use .rds). No matter the input type, all input is parsed using the default options from  the {readr} package. Also to avoid mis-labelling, logicals are converted to factors as REDCap truefalse class follows different naming conversion compared to R. Also correct support for variable labels as field labels (use .rds formatted data and label with labelled::var_label())

Vignettes and documentation have been restructured.

This package has been detached from the REDCapRITS, which it was originally forked from. The data split function will be kept, while testing will be rewritten. This projects has evolved away from the original fork.

# REDCapCAST 24.11.1

Revised tests.

Documentation has been slightly updated to highlight the shiny app for casting REDCap metadata. I am working on hosting my own Shiny Server.

### Functions:

* Bug: 'form.name' specified to 'ds2dd_detailed()' was ignored. Corrected to only be ignored if 'form.sep' is specified. Added handling of re-occurring `form.sep` pattern.

* New: `export_redcap_instrument()` is a new version of `create_instrument_meta()`, that will only export a single instrument. Multiple instrument export can be done with `lapply()` or `purrr::map()`. This allows for inclusion of this functionality in the Shiny implementation and is easier to handle. `create_instrument_meta()` is deprecated.

* Improved: `shiny_cast()` app has been updated to actually work if you install the package and not clones the whole repository. 

### Shiny:

* New: Major overhaul of the app interface with the introduction of `bslib` for building the page. Also Detailed documentation added for the app workflow.

* New: Export a REDCap instrument ready to add to your database based on an uploaded spreadsheet. This is thanks to the `export_redcap_instrument()` function. This functionality is intended for projects in production and adding instruments should be handled manually and not by API upload.

* Bug: Export datadictionary with "" instead of "NA" for NAs. Upload to REDCap failed. Not anymore.

The shiny implementation is included with this package. Implementing in shinylive may be looked into again later.

# REDCapCAST 24.10.3

Updated links and spelling.

# REDCapCAST 24.10.1

Minor changes to pass tests and renv is out. `rhub` is really not running as smooth as previously.

# REDCapCAST 24.6.1

### Functions

* Fix: `read_redcap_tables()`: field names testing allows to include "[form_name]_complete" fields.

* Fix: `ds2dd_detailed()`: default record ID name is now "record_id", the REDCap default. Default is still to use the first column name. Support was added to interpret column name prefix or suffix as instrument names. See the examples.

* New: `create_instrument_meta()`: creates zip with instrument files to allow adding new instruments to project in production. Takes data dictionary as input and creates a zip for each instrument specified by the `form_name` column.

* New: `doc2dd()`: function to convert document table to data dictionary. This allows to specify instrument or whole data dictionary in text document, which for most is easier to work with and easily modifiable. The generic case is a data frame with variable names as values in a column. This is a format like the REDCap data dictionary, but gives a few options for formatting. Has a few related functions for data handling and formatting. One interesting function is `case_match_regex_list()`, which allows for a dynamic `dplyr::case_when()`-like approach for regex-matching. I think it is neat at least.


### Documentation and more

* Dependencies: In order to deploy `shiny_cast()` with `shinylive`, I need to remove `curl` as a dependency. To accomplish this, the `shiny_deploy()` helper functions has been moved to the package [`project.aid`](https://github.com/agdamsbo/project.aid). This was before realising that `REDCapR` has `curl` as dependency, which is the culprit. `REDCapCAST` is not going to be a `shinylive` web-app without removing `REDCapR` dependency or any other REDCap database interaction, which would defy the purpose. I'll stick to hosted Shiny app instead.

# REDCapCAST 24.2.1

### Functions

* Fix: `ds2dd()`: uses correct default dd column names. Will be deprecated.

* Fix: `easy_redcap()`: fixed to actually allow project naming. also specifically asks for uri. widening updated to work.

* Fix: `redcap_wider()`: updated to accept more formats and allow handling of simple projects without repeating instruments and not longitudinal.

* Fix: `read_redcap_tables()`: now handles non-longitudinal project without repeatable instruments.

* NEW: `ds2dd_detailed()`: extension of the `ds2dd()`, which serves to preserve as much metadata as possible automatically. Depends on a group of helper functions also introduced. Of special note is the `guess_time_only_filter()`, which will try to guess which columns/variables should be formatted as time only formats. Supports hms time format. DETAILED INSTRUCTION AND VIGNETTE IS PENDING.

* NEW: `read_redcap_instrument()`: convenience function to retrieve complete instrument. Goes a little against the focused approach. With `REDCapR::redcap_read()` you can specify a form to download. You have to also specify the record id variable though. This is done for you with `read_redcap_instrument()`. Nothing fancy.

* NEW: `shiny_cast()`: [Shiny](https://shiny.posit.co/) application to ease the process of converting a spreadsheet/data set to a REDCap database. The app runs locally and data is transferred securely. You can just create and upload the data dictionary, but you can also transfer the given data in the same process. I plan to host the app with shinyapps.io, but for now you can run it locally.

### Other

I believe `renv` has now been added and runs correctly. After clone, do `renv::restore()` to install all necessary package to modify the package. This seems to always be back and forth. `renv` may be on its way out again.

Added a Code of Conduct.

# REDCapCAST 24.1.1

### Functions

* Fix: `read_redcap_tables()`: checking form names based on data dictionary to allow handling of non-longitudinal projects. Prints invalid form names and invalid event names. If invalid form names are supplied to `REDCapR::redcap_read()` (which is the backbone), all forms are exported, which is not what we want with a focused approach. Invalid event names will give an output with a rather peculiar formatting. Checking of field names validity is also added.

# REDCapCAST 23.12.1

One new function to ease secure dataset retrieval and a few bug fixes.

### Functions

* New: `easy_redcap()` function to ease the retrieval of a dataset with `read_redcap_tables()` with `keyring`-package based key storage, which handles secure API set, storage and retrieval. Relies on a small helper function, `get_api_key()`, which wraps relevant `keyring`-functions. Includes option to cast the data in a wide format with flag `widen.data`.
* Fix: `REDCap_split()`: when using this function on its own, supplying a data set with check boxes would fail if metadata is supplied as a tibble. Metadata is now converted to data.frame. Fixed.
* Fix: `read_redcap_tables()`: fixed bug when supplying events.

# REDCapCAST 23.6.2

This version marks the introduction of a few helper functions to handle database creation.

### Functions

* New: `ds2dd()` function migrating from the `stRoke`-package. Assists in building a data dictionary for REDCap from a dataset.

* New: `strsplitx()` function to ease the string splitting as an extension of `base::strsplit()`. Inspiration from https://stackoverflow.com/a/11014253/21019325 and https://www.r-bloggers.com/2018/04/strsplit-but-keeping-the-delimiter/. 

* New: `d2n()` function converts single digits to written numbers. Used to sanitize variable and form names in REDCap database creation. For more universal number to word I would suggest `english::word()` or `xfun::numbers_to_words()`, though I have not been testing these.


# REDCapCAST 23.6.1

### Documentation:

* Updated description.
* Look! A hex icon!
* Heading for CRAN.

# REDCapCAST 23.4.1

### Documentation:

* Aiming for CRAN

# REDCapCAST 23.3.2

### Documentation:

* Page added. Vignettes to follow.

* GithubActions tests added and code coverage assessed. Badge galore..

# REDCapCAST 23.3.1

### New name: REDCapCAST

To reflect new functions and the limitation to only working in R, I have changed the naming of the fork, while still, of course, maintaining the status as a fork.

The versioning has moved to a monthly naming convention.

The main goal this package is to keep the option to only export a defined subset of the whole dataset from the REDCap server as is made possible through the `REDCapR::redcap_read()` function, and combine it with the work put into the REDCapRITS package and the handling of longitudinal projects and/or projects with repeated instruments.

### Functions:

* `read_redcap_tables()` **NEW**: this function is mainly an implementation of the combined use of `REDCapR::redcap_read()` and `REDCap_split()` to maintain the focused nature of `REDCapR::redcap_read()`, to only download the specified data. Also implements tests of valid form names and event names. The usual fall-back solution was to get all data.

* `redcap_wider()` **NEW**: this function pivots the long data frames from `read_redcap_tables()` using `tidyr::pivot_wider()`.

* `focused_metadata()` **NEW**: a hidden helper function to enable a focused data acquisition approach to handle only a subset of metadata corresponding to the focused dataset.

### Notes:

* metadata handling **IMPROVED**: improved handling of different column names in matadata (DataDictionary) from REDCap dependent on whether it is acquired thorugh the api og downloaded from the server. 
