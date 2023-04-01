##

needs(srUtils, spacyr, reticulate)



py_install(c("textract", "spacy", "spacy-transformers", "transformers"), pip = TRUE, envname = "text_extraction")
use_virtualenv("textextract", required = TRUE)
use_python(python = "/Users/julianflowers/.virtualenvs/textextract/bin/python")
Sys.setenv("RETICULATE_PYTHON" = "/Users/julianflowers/.virtualenvs/textextract/bin/python")

import("textract")
import("spacy")
import("transformers")
import("spacy_transformers")

spacy_download_langmodel_virtualenv(model = "en_core_web_trf", envname = "textextract")

spacy_initialize(model = "en_core_web_trf", virtualenv = "text_extraction")

papers <- here::here("papers")
pdfs <- list.files(papers, "pdf$", full.names = TRUE)

paper1 <- srUtils::textextract(pdfs[1])

anno1 <- spacy_parse(paper1$out)


