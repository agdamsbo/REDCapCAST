# Changelog

## REDCapCAST 26.1.1

25.11.1 was never pushed to CRAN, so this fills in for that.

- FIX: spelling

- FIX: Suggested dependencies were trimmed including {styler}.

- FIX: export_redcap_instrument was updated to include default file name
  for the instrument zip. Ensures custom names has correct file
  extension.

## REDCapCAST 25.11.1

- FIX: logicals exported as numeric for data upload

- FIX: improved name cleaning

- FIX: use the more robust readr::write_csv() to export data.

## REDCapCAST 25.3.2

CRAN release: 2025-03-10

- BUG: The
  [`redcap_wider()`](https://agdamsbo.github.io/REDCapCAST/reference/redcap_wider.md)
  function would attempt to pivot empty selection of columns from list,
  and failing, causing all functions relying on this to fail. Fixed by
  filtering out data.frames in list with no additional columns than the
  “generics”.

## REDCapCAST 25.3.1

CRAN release: 2025-03-05

- FIX:
  [`as_factor()`](https://agdamsbo.github.io/REDCapCAST/reference/as_factor.md)
  now interprets empty variables with empty levels attribute as logicals
  to avoid returning factors with empty levels.

- NEW:
  [`as_logical()`](https://agdamsbo.github.io/REDCapCAST/reference/as_logical.md):
  interprets vectors with two levels as logical if values matches
  supplied list of logical pairs like “TRUE”/“FALSE”, “Yes”/“No” or 1/2.
  Eases interpretation of data from databases with minimal metadata.
  Works on vectors and for data.frames. Interprets vectors with single
  value also matching to any of supplied levels (Chooses first match
  pair if several matches).

- NEW:
  [`easy_redcap()`](https://agdamsbo.github.io/REDCapCAST/reference/easy_redcap.md):
  new parameter `data_format` to specify data format as c(“wide”,
  “list”, “redcap”, “long”). For now “redcap” and “long” is treated
  equally. This was added to ease MMRM analyses. In that case, missing
  baseline values can be carried forward as “last observation carried
  forward” using the
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)
  function specifying variables to fill. Interesting discussion on
  filling data [here on
  Stackoverflow](https://stackoverflow.com/a/13810615).
  `redcap_read_tables()` now has the option “none” for the `split_forms`
  parameter to allow not splitting the data.

- FIX:
  [`ds2dd_detailed()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd_detailed.md):
  The `convert_logicals` parameter has been turned off by default and
  logicals are now interpreted as field type “truefalse”. Converting
  logicals to factors would result in the numeric values being 1 for
  FALSE and 2 for TRUE, which is opposite of the traditional notation
  and could lead to serous problems if not handled correctly. This
  should solve it.

## REDCapCAST 25.1.1

CRAN release: 2025-01-29

The newly introduced extension of
[`forcats::fct_drop()`](https://forcats.tidyverse.org/reference/fct_drop.html)
has been corrected to work as intended as a method.

Conversion of column names to `field_names` are aligning better with
REDCap naming.

Shorten variable names above 100 characters (REDCap criteria; note
recommended variable name length is \<26)

Fixed a params conflict in easy_redcap() when specifying raw_or_label.

## REDCapCAST 24.12.1

CRAN release: 2024-12-02

This release attempts to solve problems hosting the shiny_cast app,
while also implementing functions to preserve as much meta data as
possible from the REDCap database when exporting data.

The hosting on shinyapps.io has given a lot of trouble recently.
Modified package structure a little around the
[`shiny_cast()`](https://agdamsbo.github.io/REDCapCAST/reference/shiny_cast.md),
to accommodate an alternative hosting approach with all package
functions included in a script instead of requiring the package.

- NEW: A new option to `raw_or_label` in
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md)
  has been added: “both”. Get raw values with REDCap labels applied as
  labels. Use
  [`as_factor()`](https://agdamsbo.github.io/REDCapCAST/reference/as_factor.md)
  to format factors with original labels and use the `gtsummary` package
  to easily get beautiful tables with original labels from REDCap. Use
  [`fct_drop()`](https://agdamsbo.github.io/REDCapCAST/reference/fct_drop.md)
  to drop empty levels.

- NEW: fct_drop() has been added with an extension to
  [`forcats::fct_drop()`](https://forcats.tidyverse.org/reference/fct_drop.html),
  that works across data.frames. Use as
  [`fct_drop()`](https://agdamsbo.github.io/REDCapCAST/reference/fct_drop.md).

- CHANGE: the default data export method of
  [`easy_redcap()`](https://agdamsbo.github.io/REDCapCAST/reference/easy_redcap.md)
  has been changed to use the new labelled data export with
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md).

## REDCapCAST 24.11.3

- BUG: shiny_cast() fails to load as I missed loading REDCapCAST library
  in ui.r. Fixed. Tests would be great.

## REDCapCAST 24.11.2

CRAN release: 2024-11-22

24.11.1 was rejected on CRAN based on wrong title capitalisation. This
was an opportunity to extend the package overhaul. And this actually
turned out to be a major step towards a very usable shiny app which have
received most of the focus.

I have implemented option to specify categorical variables to factorize,
but doing this with a modified version of {forcats} and {haven}’s
[`as_factor()`](https://agdamsbo.github.io/REDCapCAST/reference/as_factor.md),
that will preserve any attributes applied to the data to be able to
upload and cast REDCap meta data from richly formatted data (use .rds).
No matter the input type, all input is parsed using the default options
from the {readr} package. Also to avoid mis-labelling, logicals are
converted to factors as REDCap truefalse class follows different naming
conversion compared to R. Also correct support for variable labels as
field labels (use .rds formatted data and label with
labelled::var_label())

Vignettes and documentation have been restructured.

This package has been detached from the REDCapRITS, which it was
originally forked from. The data split function will be kept, while
testing will be rewritten. This projects has evolved away from the
original fork.

## REDCapCAST 24.11.1

Revised tests.

Documentation has been slightly updated to highlight the shiny app for
casting REDCap metadata. I am working on hosting my own Shiny Server.

#### Functions:

- Bug: ‘form.name’ specified to ‘ds2dd_detailed()’ was ignored.
  Corrected to only be ignored if ‘form.sep’ is specified. Added
  handling of re-occurring `form.sep` pattern.

- New:
  [`export_redcap_instrument()`](https://agdamsbo.github.io/REDCapCAST/reference/export_redcap_instrument.md)
  is a new version of
  [`create_instrument_meta()`](https://agdamsbo.github.io/REDCapCAST/reference/create_instrument_meta.md),
  that will only export a single instrument. Multiple instrument export
  can be done with [`lapply()`](https://rdrr.io/r/base/lapply.html) or
  [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html). This
  allows for inclusion of this functionality in the Shiny implementation
  and is easier to handle.
  [`create_instrument_meta()`](https://agdamsbo.github.io/REDCapCAST/reference/create_instrument_meta.md)
  is deprecated.

- Improved:
  [`shiny_cast()`](https://agdamsbo.github.io/REDCapCAST/reference/shiny_cast.md)
  app has been updated to actually work if you install the package and
  not clones the whole repository.

#### Shiny:

- New: Major overhaul of the app interface with the introduction of
  `bslib` for building the page. Also Detailed documentation added for
  the app workflow.

- New: Export a REDCap instrument ready to add to your database based on
  an uploaded spreadsheet. This is thanks to the
  [`export_redcap_instrument()`](https://agdamsbo.github.io/REDCapCAST/reference/export_redcap_instrument.md)
  function. This functionality is intended for projects in production
  and adding instruments should be handled manually and not by API
  upload.

- Bug: Export datadictionary with “” instead of “NA” for NAs. Upload to
  REDCap failed. Not anymore.

The shiny implementation is included with this package. Implementing in
shinylive may be looked into again later.

## REDCapCAST 24.10.3

CRAN release: 2024-10-03

Updated links and spelling.

## REDCapCAST 24.10.1

Minor changes to pass tests and renv is out. `rhub` is really not
running as smooth as previously.

## REDCapCAST 24.6.1

CRAN release: 2024-06-07

#### Functions

- Fix:
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md):
  field names testing allows to include “\[form_name\]\_complete”
  fields.

- Fix:
  [`ds2dd_detailed()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd_detailed.md):
  default record ID name is now “record_id”, the REDCap default. Default
  is still to use the first column name. Support was added to interpret
  column name prefix or suffix as instrument names. See the examples.

- New:
  [`create_instrument_meta()`](https://agdamsbo.github.io/REDCapCAST/reference/create_instrument_meta.md):
  creates zip with instrument files to allow adding new instruments to
  project in production. Takes data dictionary as input and creates a
  zip for each instrument specified by the `form_name` column.

- New:
  [`doc2dd()`](https://agdamsbo.github.io/REDCapCAST/reference/doc2dd.md):
  function to convert document table to data dictionary. This allows to
  specify instrument or whole data dictionary in text document, which
  for most is easier to work with and easily modifiable. The generic
  case is a data frame with variable names as values in a column. This
  is a format like the REDCap data dictionary, but gives a few options
  for formatting. Has a few related functions for data handling and
  formatting. One interesting function is
  [`case_match_regex_list()`](https://agdamsbo.github.io/REDCapCAST/reference/case_match_regex_list.md),
  which allows for a dynamic
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)-like
  approach for regex-matching. I think it is neat at least.

#### Documentation and more

- Dependencies: In order to deploy
  [`shiny_cast()`](https://agdamsbo.github.io/REDCapCAST/reference/shiny_cast.md)
  with `shinylive`, I need to remove `curl` as a dependency. To
  accomplish this, the `shiny_deploy()` helper functions has been moved
  to the package
  [`project.aid`](https://github.com/agdamsbo/project.aid). This was
  before realising that `REDCapR` has `curl` as dependency, which is the
  culprit. `REDCapCAST` is not going to be a `shinylive` web-app without
  removing `REDCapR` dependency or any other REDCap database
  interaction, which would defy the purpose. I’ll stick to hosted Shiny
  app instead.

## REDCapCAST 24.2.1

CRAN release: 2024-02-28

#### Functions

- Fix:
  [`ds2dd()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd.md):
  uses correct default dd column names. Will be deprecated.

- Fix:
  [`easy_redcap()`](https://agdamsbo.github.io/REDCapCAST/reference/easy_redcap.md):
  fixed to actually allow project naming. also specifically asks for
  uri. widening updated to work.

- Fix:
  [`redcap_wider()`](https://agdamsbo.github.io/REDCapCAST/reference/redcap_wider.md):
  updated to accept more formats and allow handling of simple projects
  without repeating instruments and not longitudinal.

- Fix:
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md):
  now handles non-longitudinal project without repeatable instruments.

- NEW:
  [`ds2dd_detailed()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd_detailed.md):
  extension of the
  [`ds2dd()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd.md),
  which serves to preserve as much metadata as possible automatically.
  Depends on a group of helper functions also introduced. Of special
  note is the
  [`guess_time_only_filter()`](https://agdamsbo.github.io/REDCapCAST/reference/guess_time_only_filter.md),
  which will try to guess which columns/variables should be formatted as
  time only formats. Supports hms time format. DETAILED INSTRUCTION AND
  VIGNETTE IS PENDING.

- NEW:
  [`read_redcap_instrument()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_instrument.md):
  convenience function to retrieve complete instrument. Goes a little
  against the focused approach. With
  [`REDCapR::redcap_read()`](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
  you can specify a form to download. You have to also specify the
  record id variable though. This is done for you with
  [`read_redcap_instrument()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_instrument.md).
  Nothing fancy.

- NEW:
  [`shiny_cast()`](https://agdamsbo.github.io/REDCapCAST/reference/shiny_cast.md):
  [Shiny](https://shiny.posit.co/) application to ease the process of
  converting a spreadsheet/data set to a REDCap database. The app runs
  locally and data is transferred securely. You can just create and
  upload the data dictionary, but you can also transfer the given data
  in the same process. I plan to host the app with shinyapps.io, but for
  now you can run it locally.

#### Other

I believe `renv` has now been added and runs correctly. After clone, do
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
to install all necessary package to modify the package. This seems to
always be back and forth. `renv` may be on its way out again.

Added a Code of Conduct.

## REDCapCAST 24.1.1

CRAN release: 2024-01-09

#### Functions

- Fix:
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md):
  checking form names based on data dictionary to allow handling of
  non-longitudinal projects. Prints invalid form names and invalid event
  names. If invalid form names are supplied to
  [`REDCapR::redcap_read()`](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
  (which is the backbone), all forms are exported, which is not what we
  want with a focused approach. Invalid event names will give an output
  with a rather peculiar formatting. Checking of field names validity is
  also added.

## REDCapCAST 23.12.1

CRAN release: 2023-12-20

One new function to ease secure dataset retrieval and a few bug fixes.

#### Functions

- New:
  [`easy_redcap()`](https://agdamsbo.github.io/REDCapCAST/reference/easy_redcap.md)
  function to ease the retrieval of a dataset with
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md)
  with `keyring`-package based key storage, which handles secure API
  set, storage and retrieval. Relies on a small helper function,
  [`get_api_key()`](https://agdamsbo.github.io/REDCapCAST/reference/get_api_key.md),
  which wraps relevant `keyring`-functions. Includes option to cast the
  data in a wide format with flag `widen.data`.
- Fix:
  [`REDCap_split()`](https://agdamsbo.github.io/REDCapCAST/reference/REDCap_split.md):
  when using this function on its own, supplying a data set with check
  boxes would fail if metadata is supplied as a tibble. Metadata is now
  converted to data.frame. Fixed.
- Fix:
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md):
  fixed bug when supplying events.

## REDCapCAST 23.6.2

CRAN release: 2023-07-04

This version marks the introduction of a few helper functions to handle
database creation.

#### Functions

- New:
  [`ds2dd()`](https://agdamsbo.github.io/REDCapCAST/reference/ds2dd.md)
  function migrating from the `stRoke`-package. Assists in building a
  data dictionary for REDCap from a dataset.

- New:
  [`strsplitx()`](https://agdamsbo.github.io/REDCapCAST/reference/strsplitx.md)
  function to ease the string splitting as an extension of
  [`base::strsplit()`](https://rdrr.io/r/base/strsplit.html).
  Inspiration from <https://stackoverflow.com/a/11014253/21019325> and
  <https://www.r-bloggers.com/2018/04/strsplit-but-keeping-the-delimiter/>.

- New: `d2n()` function converts single digits to written numbers. Used
  to sanitize variable and form names in REDCap database creation. For
  more universal number to word I would suggest `english::word()` or
  [`xfun::numbers_to_words()`](https://rdrr.io/pkg/xfun/man/numbers_to_words.html),
  though I have not been testing these.

## REDCapCAST 23.6.1

CRAN release: 2023-06-06

#### Documentation:

- Updated description.
- Look! A hex icon!
- Heading for CRAN.

## REDCapCAST 23.4.1

#### Documentation:

- Aiming for CRAN

## REDCapCAST 23.3.2

#### Documentation:

- Page added. Vignettes to follow.

- GithubActions tests added and code coverage assessed. Badge galore..

## REDCapCAST 23.3.1

#### New name: REDCapCAST

To reflect new functions and the limitation to only working in R, I have
changed the naming of the fork, while still, of course, maintaining the
status as a fork.

The versioning has moved to a monthly naming convention.

The main goal this package is to keep the option to only export a
defined subset of the whole dataset from the REDCap server as is made
possible through the
[`REDCapR::redcap_read()`](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
function, and combine it with the work put into the REDCapRITS package
and the handling of longitudinal projects and/or projects with repeated
instruments.

#### Functions:

- [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md)
  **NEW**: this function is mainly an implementation of the combined use
  of
  [`REDCapR::redcap_read()`](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html)
  and
  [`REDCap_split()`](https://agdamsbo.github.io/REDCapCAST/reference/REDCap_split.md)
  to maintain the focused nature of
  [`REDCapR::redcap_read()`](https://ouhscbbmc.github.io/REDCapR/reference/redcap_read.html),
  to only download the specified data. Also implements tests of valid
  form names and event names. The usual fall-back solution was to get
  all data.

- [`redcap_wider()`](https://agdamsbo.github.io/REDCapCAST/reference/redcap_wider.md)
  **NEW**: this function pivots the long data frames from
  [`read_redcap_tables()`](https://agdamsbo.github.io/REDCapCAST/reference/read_redcap_tables.md)
  using
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

- [`focused_metadata()`](https://agdamsbo.github.io/REDCapCAST/reference/focused_metadata.md)
  **NEW**: a hidden helper function to enable a focused data acquisition
  approach to handle only a subset of metadata corresponding to the
  focused dataset.

#### Notes:

- metadata handling **IMPROVED**: improved handling of different column
  names in matadata (DataDictionary) from REDCap dependent on whether it
  is acquired thorugh the api og downloaded from the server.
