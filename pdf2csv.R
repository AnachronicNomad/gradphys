library(tidyverse)
library(tabulizer)
library(readxl)

PDF_DIR = './data/pdf/'

STATES = c(
  list('ALABAMA', 'AL'),
  list('ALASKA', 'AK'),
  list('ARIZONA', 'AZ'),
  list('ARKANSAS', 'AR'),
  list('CALIFORNIA', 'CA'),
  list('COLORADO', 'CO'),
  list('CONNETICUT', 'CT'),
  list('DELAWARE', 'DE'),
  list('DISTRICT OF COLUMBIA', 'DC'),
  list('FLORIDA', 'FL'),
  list('GEORGIA', 'GA'),
  list('HAWAII', 'HI'),
  list('IDAHO', 'ID'),
  list('ILLINOIS', 'IL'),
  list('INDIANA', 'IN'),
  list('IOWA', 'IA'),
  list('KANSAS', 'KS'),
  list('KENTUCKY', 'KY'),
  list('LOUISIANA', 'LA'),
  list('MAINE', 'ME'),
  list('MARYLAND', 'MD'),
  list('MASSACHUSETTS', 'MA'),
  list('MICHIGAN', 'MI'),
  list('MINNESOTA', 'MN'),
  list('MISSISSIPPI', 'MS'),
  list('MISSOURI', 'MO'),
  list('MONTANA', 'MT'),
  list('NEBRASKA', 'NE'),
  list('NEVADA', 'NV'),
  list('NEW HAMPSHIRE', 'NH'),
  list('NEW JERSEY', 'NJ'),
  list('NEW MEXICO', 'NM'),
  list('NEW YORK', 'NY'),
  list('NORTH CAROLINA', 'NC'),
  list('NORTH DAKOTA', 'ND'),
  list('OHIO', 'OH'),
  list('OKLAHOMA', 'OK'),
  list('OREGON', 'OR'),
  list('PENNSYLVANIA', 'PA'),
  list('PUERTO RICO', 'PR'),
  list('RHODE ISLAND', 'RI'),
  list('SOUTH CAROLINA', 'SC'),
  list('SOUTH DAKOTA', 'SD'),
  list('TENNESSEE', 'TN'),
  list('TEXAS', 'TX'),
  list('UTAH', 'UT'),
  list('VERMONT', 'VT'),
  list('VIRGINIA', 'VA'),
  list('WASHINGTON', 'WA'),
  list('WEST VIRGINIA', 'WV'),
  list('WISCOSIN', 'WI'),
  list('WYOMING', 'WY')
)

files <- list.files(PDF_DIR,
                    pattern = "*.pdf",
                    full.names = T);

files <- list(files[[1]])
data <- NULL;
for (.file in files) {
  tmp <- extract_tables(.file,
                        output = 'data.frame',
                        pages = c(3:get_n_pages(.file)),
                        method = 'lattice',
                        guess = F,
                        columns = list(c(27, 169, 199, 245, 310, 345, 377, 405, 441, 471, 526, 563))
                        #area = list(c(90, 0, 792, 650)) # top,left,bottom,right
                        )

  tbl <- tibble();
  for (i in 1:length(tmp)) {
    colnames(tmp[[i]]) <- c(
      'HIGHEST PHYSICS DEGREE OFFERED',
      'INSTITUTION',
      'ASTRO PROGRAM',
      'FIRST-TERM PHYSICS & ASTRONOMY COURSE ENROLLMENT',
      'FIRST-TERM PHYSICAL SCI & ASTRONOMY COURSE ENROLLMENT',
      'JUNIOR ENROLLMENT',
      'SENIOR ENROLLMENT',
      'TOTAL GRADUATE STUDENT ENROLLMENT',
      'NON-US GRADUATE STUDENT ENROLLMENT',
      'FIRST-YEAR GRADUATE STUDENT ENROLLMENT',
      'PHYSICS BACHELORS',
      'EXITING PHYSICS MASTERS',
      'PHYSICS PhDs'
    );
    tmp[[i]] = as_tibble(tmp[[i]]) |> slice(-c(1:6))

    tbl <-
      tbl |>
      bind_rows(tmp[[i]])
  }

  tbl <-
      tbl |>
      mutate_at(
        c(
          'FIRST-TERM PHYSICS & ASTRONOMY COURSE ENROLLMENT',
          'FIRST-TERM PHYSICAL SCI & ASTRONOMY COURSE ENROLLMENT',
          'JUNIOR ENROLLMENT',
          'SENIOR ENROLLMENT',
          'TOTAL GRADUATE STUDENT ENROLLMENT',
          'NON-US GRADUATE STUDENT ENROLLMENT',
          'FIRST-YEAR GRADUATE STUDENT ENROLLMENT',
          'PHYSICS BACHELORS',
          'EXITING PHYSICS MASTERS',
          'PHYSICS PhDs'
        ),
        as.numeric
      ) |>
      mutate_at(
        c(
          'ASTRO PROGRAM',
          'HIGHEST PHYSICS DEGREE OFFERED'
        ),
        as.factor
      ) |>
      mutate(
        `HIGHEST PHYSICS DEGREE OFFERED` = case_when(
          `HIGHEST PHYSICS DEGREE OFFERED` == "p" ~ "PhD",
          `HIGHEST PHYSICS DEGREE OFFERED` == "m" ~ "MS",
          `HIGHEST PHYSICS DEGREE OFFERED` == "" ~ "BS",
          grepl("\\(([0-9]){1,2}\\)", `HIGHEST PHYSICS DEGREE OFFERED`, perl = T) ~ NA
        )
      ) |>
      mutate(
        `INSTITUTION` = gsub("\\(([0-9]){1,2}\\)", "", `INSTITUTION`, perl = T)
      ) |>
      filter(!(is.na(`HIGHEST PHYSICS DEGREE OFFERED`))) |>
      filter(!(grepl("(CONTINUED|CONT'D)", INSTITUTION, perl = T))) |>
      add_column(STATE = NA) |>
      relocate(
        STATE, .after = INSTITUTION
      )

  print(tbl)
}
