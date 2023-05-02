## table extract from pdfs using tabula python package

needs(tidyverse, reticulate, pdftools, magick, tesseract)
library(srUtils)
srUtils::textextract_initialize()
pdf_1 <- srUtils::textextract(pdf)

table1

  pdf_1 |>
  str_split("\\n") %>%
  enframe() |>
  unnest("value") |>
  slice(464:600) |>
    print(n = 137)

 tesseract::ocr("assignments/images/1-s2.0-S2772411522000337-gr1_lrg.jpg")

 image_ocr(image_read("assignments/images/1-s2.0-S2772411522000337-gr1_lrg.jpg")) |>
   str_split("\\n")

virtualenv_remove("tabula")
virtualenv_create("tabula")

Sys.setenv(RETICULATE_PYTHON = "/Users/julianflowers/.virtualenvs/geemap/bin/python")
use_virtualenv("geemap")

py_install(c("tabula-py", "pdfplumber", "textract" ), pip = TRUE, envname = "geemap")

tabula <- import("tabula")
pd <- import("pandas")

pdf <- "/Users/julianflowers/Dropbox/My Mac (Julians-MBP-2)/Downloads/1-s2.0-S2772411522000337-main.pdf"
df <- tabula$read_pdf(pdf, stream = TRUE, pages = "all")

str(df)

## use pdftools

pdftools::pdf_text(pdf) |>
  str_split("\\\n")
