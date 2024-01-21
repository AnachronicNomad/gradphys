library(tidyverse)
library(tabulizer)
library(writexl)

PDF_DIR = './data/pdf/'

STATES = enframe(
  c(
    c('ALABAMA' = 'AL'),
    c('ALASKA' = 'AK'),
    c('ARIZONA' = 'AZ'),
    c('ARKANSAS' = 'AR'),
    c('CALIFORNIA' = 'CA'),
    c('COLORADO' = 'CO'),
    c('CONNECTICUT' = 'CT'),
    c('DELAWARE' = 'DE'),
    c('DISTRICT OF COLUMBIA' = 'DC'),
    c('FLORIDA' = 'FL'),
    c('GEORGIA' = 'GA'),
    c('HAWAII' = 'HI'),
    c('IDAHO' = 'ID'),
    c('ILLINOIS' = 'IL'),
    c('INDIANA' = 'IN'),
    c('IOWA' = 'IA'),
    c('KANSAS' = 'KS'),
    c('KENTUCKY' = 'KY'),
    c('LOUISIANA' = 'LA'),
    c('MAINE' = 'ME'),
    c('MARYLAND' = 'MD'),
    c('MASSACHUSETTS' = 'MA'),
    c('MICHIGAN' = 'MI'),
    c('MINNESOTA' = 'MN'),
    c('MISSISSIPPI' = 'MS'),
    c('MISSOURI' = 'MO'),
    c('MONTANA' = 'MT'),
    c('NEBRASKA' = 'NE'),
    c('NEVADA' = 'NV'),
    c('NEW HAMPSHIRE' = 'NH'),
    c('NEW JERSEY' = 'NJ'),
    c('NEW MEXICO' = 'NM'),
    c('NEW YORK' = 'NY'),
    c('NORTH CAROLINA' = 'NC'),
    c('NORTH DAKOTA' = 'ND'),
    c('OHIO' = 'OH'),
    c('OKLAHOMA' = 'OK'),
    c('OREGON' = 'OR'),
    c('PENNSYLVANIA' = 'PA'),
    c('PUERTO RICO' = 'PR'),
    c('RHODE ISLAND' = 'RI'),
    c('SOUTH CAROLINA' = 'SC'),
    c('SOUTH DAKOTA' = 'SD'),
    c('TENNESSEE' = 'TN'),
    c('TEXAS' = 'TX'),
    c('UTAH' = 'UT'),
    c('VERMONT' = 'VT'),
    c('VIRGINIA' = 'VA'),
    c('WASHINGTON' = 'WA'),
    c('WEST VIRGINIA' = 'WV'),
    c('WISCONSIN' = 'WI'),
    c('WYOMING' = 'WY')
  )
)

files <-
  list.files(PDF_DIR,
             pattern = "*.pdf",
             full.names = T) |>
  purrr::keep(\(.f) { file.exists(.f) && !dir.exists(.f) })

### TODO :> [2006, 2022-] starts on page 3.  [199X, 2005] starts on page 5.
for (.file in files) {
  print(.file)
  tmp <- extract_tables(.file,
                        output = 'data.frame',
                        pages = c(3:(get_n_pages(.file)-2)), ## Year \in [2001, 2022-]
                        #pages = c(5:get_n_pages(.file)), ## Year \in [2000, ]
                        method = 'lattice',
                        guess = F,
                        encoding = 'UTF-8',
                        ## Year \in [2001, 2003]
                        #columns = list(c(46, 176, 221.5, 282, 316.5, 350, 464, 381, 421, 451, 502, 540))
                        ## Year \in [2004, ]
                        #columns = list(c(47, 190, 238, 303, 336, 366.5, 396.5, 436, 466, 514.5, 552)),
                        ## Year \in [2005, ]
                        #columns = list(c(28, 171, 200, 244, 312, 342, 374, 404, 440, 472, 521, 567))
                        ## Year \in [2006, 2007]
                        #columns = list(c(27, 169, 201, 245, 310, 345, 377, 405, 441, 471, 526, 563))
                        ## Year \in [2008, 2011]
                        #columns = list(c(32.5, 174, 189.5, 234, 297, 334, 364, 400, 445, 478, 528, 568.5))
                        ## Year \in [2012, 2013]
                        #columns = list(c(40, 222, 243, 300, 377, 419.5, 464, 506.5, 568.5, 609, 680, 729))
                        ## Year \in [2014, 2016]
                        #columns = list(c(32, 176.5, 191, 232, 293.5, 327.5, 359, 395, 440.5, 474, 523.5, 563))
                        ## Year \in [2017, ]
                        columns = list(c(26.5, 182.5, 197, 235, 302, 335.5, 371.5, 403, 446, 475, 525.5, 565))
                        #area = list(c(90, 0, 792, 650)) # top,left,bottom,right
                        )

  tbl <- tibble();
  for (i in 1:length(tmp)) {
    colnames(tmp[[i]]) <- c(
      'Highest Physics Degree Offered',
      'Institution',
      'Astro Program',
      'First-Term Introductory Physics Course Enrollments',
      'First-Term Introductory Physical Science and Astronomy Course Enrollments',
      'Fall Junior Enrollments',
      'Fall Senior Enrollments',
      'Fall Total Graduate Student Enrollments',
      'Fall Non-US Graduate Student Enrollments',
      'Fall First-Year Graduate Student Enrollments',
      'Physics Bachelors',
      'Exiting Physics Masters',
      'Physics PhDs'
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
          'First-Term Introductory Physics Course Enrollments',
          'First-Term Introductory Physical Science and Astronomy Course Enrollments',
          'Fall Junior Enrollments',
          'Fall Senior Enrollments',
          'Fall Total Graduate Student Enrollments',
          'Fall Non-US Graduate Student Enrollments',
          'Fall First-Year Graduate Student Enrollments',
          'Physics Bachelors',
          'Exiting Physics Masters',
          'Physics PhDs'
        ),
        as.numeric
      ) |>
      mutate_at(
        c(
          'Astro Program',
          'Highest Physics Degree Offered'
        ),
        as.factor
      ) |>
      mutate(
        `Highest Physics Degree Offered` = case_when(
          `Highest Physics Degree Offered` == "p" ~ "PhD",
          `Highest Physics Degree Offered` == "m" ~ "MS",
          `Highest Physics Degree Offered` == "" ~ "BS",
          grepl("\\(([0-9]){1,2}\\)", `Highest Physics Degree Offered`, perl = T) ~ NA
        )
      ) |>
      mutate(
        `Institution` = gsub("\\(([0-9]){1,2}\\)", "", Institution, perl = T)
      ) |>
      filter(!(is.na(`Highest Physics Degree Offered`))) |>
      filter(!(grepl("(CONTINUED|CONT'D)", Institution, perl = T))) |>
      filter(!(grepl("INSTITUTION", Institution, perl = T))) |>
      add_column(`State` = NA)

  ST <- ""
  to_remove <- c()
  for (i in 1:nrow(tbl)) {
    inst = tbl[[i, 'Institution']]
    idx = which(STATES['name'] == inst)
    if ((length(idx) > 0)) {
      ST = STATES[[idx, 'value']]
      to_remove = append(to_remove, i)
    } else {
      tbl[[i, 'State']] = ST
    }
  }

  tbl <- tbl[-to_remove,]

  tbl <-
    tbl |>
    relocate(
      `Institution`,
      `State`,
      `Highest Physics Degree Offered`,
      `Astro Program`,
      `First-Term Introductory Physics Course Enrollments`,
      `First-Term Introductory Physical Science and Astronomy Course Enrollments`,
      `Fall Junior Enrollments`,
      `Fall Senior Enrollments`,
      `Fall Total Graduate Student Enrollments`,
      `Fall Non-US Graduate Student Enrollments`,
      `Fall First-Year Graduate Student Enrollments`,
      `Physics Bachelors`,
      `Exiting Physics Masters`,
      `Physics PhDs`
    )

  .file <- gsub("\\.pdf", ".xlsx", .file)
  write_xlsx(list(data = tbl), .file)
}
