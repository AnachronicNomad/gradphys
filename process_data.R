#----         Imports        ----#
#
# TODO :> these docs
#
#--------------------------------#
library(tidyverse)
library(readxl)
library(forcats)

#----     process_data(...)  ----#
#
# TODO :> these docs
#
#--------------------------------#
process_data <- \(DATA_DIR) {
  ##########
  ##-----##
  ##----##
  ##---##   Read Data
  ##----##
  ##-----##
  ##########
  map(
    list.files(DATA_DIR, pattern="*.xlsx", full.names=T),
    \(.file) {
      #####
      ##  Excel Parse
      #####
      readxl::read_excel(
        .file,
        sheet = 'data',
        col_types = c(
          'text',    # Institution
          'text',    # State; 2-chr factor-level
          'text',    # Highest Degree Offered; 3-level factor of `BS`, `MS`, or `PhD`
          'text',    # Astro Program; 3-level factor of `combined`, `separate`, or `none`
          'text',    # Notes
          'numeric', # First-Term Introductory Physics Course Enrollments
          'numeric', # First-Term Introductory Physical Science and Astronomy Course Enrollments
          'numeric', # Fall Junior Enrollments
          'numeric', # Fall Senior Enrollments
          'numeric', # Fall Total Graduate Student Enrollments
          'numeric', # Fall Non-US Graduate Student Enrollments
          'numeric', # Fall First-Year Graduate Student Enrollments
          'numeric', # Physics Bachelors
          'numeric', # Exiting Physics Masters
          'numeric'  # Physics PhDs
        ),
        na = c('---', ''),
        .name_repair = \(cols) { # unify column names
          cols |>
            gsub('(Fall [1-2]{1}([0-1]|[8-9]){1}([0-9]){1}[0-9]{1})', 'Fall', x = _, perl=TRUE) |>
            gsub('(^(20[0-9]{2}\\-[0-9]{2})\\s+)|(\\-)', '', x = _, perl=TRUE) |>
            gsub('\\s*(\\w+)\\s+', '\\1_', x = _, perl=TRUE)
        }
      ) |>

      #####
      ##  Denote Year
      #####
      mutate(
        Year = parse_number(
          paste0(
            '20',
            gsub(
                paste0(DATA_DIR, "*physrostr([0-9]{2}).xlsx$"),
                "\\1",
                .file))),
        .before = Institution
      )
  }) |>

  ##########
  ##-----##
  ##----##
  ##---##   Process Data
  ##----##
  ##-----##
  ##########
  map( \(.tbl) {

    #####
    ##  transform `*_Enrollments` fields, added in (TODO:> ???)
    #####
    if ('Fall_Total_Graduate_Student_Enrollments' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Fall_Total_Graduate_Student_Enrollments = NA, .name_repair = 'unique') }
    if ('Fall_FirstYear_Graduate_Student_Enrollments' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Fall_FirstYear_Graduate_Student_Enrollments = NA, .name_repair = 'unique') }
    if ('Fall_Senior_Enrollments' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Fall_Senior_Enrollments = NA, .name_repair = 'unique') }
    if ('Fall_Junior_Enrollments' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Fall_Junior_Enrollments = NA, .name_repair = 'unique') }
    if ('Fall_NonUS_Graduate_Student_Enrollments' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Fall_NonUS_Graduate_Student_Enrollments = NA, .name_repair = 'unique') }
    if ('Highest_Physics_Degree_Offered' %in% names(.tbl) == F)
    { .tbl <- .tbl |> add_column(Highest_Physics_Degree_Offered = NA, .name_repair = 'unique') }

    .tbl |>

    #####
    ##  drop unused columns
    #####
    select(-any_of(c('Notes', 'Highest_Degree_Offered'))) |>

    #####
    ##  transform `Highest Physics Degree Offered`, added in 2017
    #####
    mutate(
      Highest_Physics_Degree_Offered = ifelse(
        is.na(Highest_Physics_Degree_Offered),
        ifelse(
          is.na(Physics_PhDs),
          ifelse(
            is.na(Fall_Total_Graduate_Student_Enrollments),
            'BS',
            'MS'
          ),
          'PhD'
        ),
        Highest_Physics_Degree_Offered
    )) |>
    mutate( Highest_Physics_Degree_Offered = as.factor(Highest_Physics_Degree_Offered) ) |>
    mutate(
      `Highest_Physics_Degree_Offered` = fct_relevel( `Highest_Physics_Degree_Offered`, c('BS','MS','PhD'))
    ) |>

    #####
    ##  transform/convert `Astro Program` into factor
    #####
    mutate(
      Astro_Program = case_when(
          is.na(Astro_Program) ~ 'no dept.',
          Astro_Program == 'c' ~ 'combined',
          Astro_Program == 's' ~ 'separate'
        )
    ) |>
    mutate(
      Astro_Program = as.factor(Astro_Program)
    ) |>
    mutate(
      `Astro_Program` = fct_relevel( `Astro_Program`, c('no dept.', 'separate', 'combined'))
    ) |>

    #####
    ##  transform State, Year into factors
    #####
    mutate(State = as.factor(State)) |>
    mutate(Year = as.factor(Year)) |>

    #####
    ##  transform `Appl Phy` -> `Appl Phys`
    #####
    mutate(
      Institution = gsub("(\\(Appl Phy\\))", "\\(Appl Phys\\)", Institution)
    ) |>
    #####
    ##  transform Institution name `College` -> `Coll`, drop apostrophe
    #####
    mutate(
      Institution = gsub("(College(s){0,1})", "Coll", Institution)
    ) |>
    mutate(
      Institution = gsub("'", '', Institution)
    ) |>

    #####
    ## set column order
    #####
    relocate(
      `Year`,
      `Institution`,
      `State`,
      `Highest_Physics_Degree_Offered`,
      `Fall_Total_Graduate_Student_Enrollments`,
      `Physics_PhDs`,
      `Exiting_Physics_Masters`,
      `Fall_FirstYear_Graduate_Student_Enrollments`,
      `Physics_Bachelors`,
      `Fall_Senior_Enrollments`,
      `Fall_Junior_Enrollments`,
      `FirstTerm_Introductory_Physics_Course_Enrollments`,
      `FirstTerm_Introductory_Physical_Science_and_Astronomy_Course_Enrollments`,
      `Fall_NonUS_Graduate_Student_Enrollments`,
      `Astro_Program`
    )
  }) |>

  ##########
  ##-----##
  ##----##
  ##---##   Save Data
  ##----##
  ##-----##
  ##########
  ## unify observation collection
  list_rbind() |>
  ## group observations of Institution by Year, then by State
  group_by(State, Institution, Year) |>
  ## sort-asc within previous group by quantity
  arrange(Physics_PhDs, .by_group = TRUE)
}
