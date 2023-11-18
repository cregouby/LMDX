
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LMDX

<!-- badges: start -->
<!-- badges: end -->

A R implementation of LMDX ([Perot et
al. 2023](https://arxiv.org/pdf/2309.10952.pdf)). You provides a pdf
page or file in, and a decoding json schema, and get all entities
extracted from the pdf.

## Installation

You can install the development version of LMDX and its prerequisites
from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mlverse/chattr")
pak::pak("cregouby/LMDX")
```

## Example

We want here to extract the R short reference card pdf file content, and
turn it into a data.frame:

[![R reference card page 1
screenshot](inst/extdata/Short-refcard_1.jpg)](https://cran.r-project.org/doc/contrib/Short-refcard.pdf)

It is a challenge as it is composed of 3 tight columns and packed
between code and highly summarized sentences.

## Step 1 : Form the LLM prompt

prompt is made with the assembly of the document text with layout
information, and the taxonomy, a json representation of the entities to
extract. Taxonomy can be hierarchical like in the following example:

``` r
library(LMDX)
document <- system.file("extdata", "Short-refcard_1.pdf", package = "LMDX")
taxonomy <- jsonlite::minify('{
  "title" : "",
  "paragraph_item": [
    {
      "title": "",
      "description": [],
      "line_item": [
        {
          "command": "",
          "description": "",
          "example": []
        }
      ]
    }
  ]
}')
prompt <- lmdx_prompt(document, taxonomy, segment = "line")
```

## Step 2 : Query the model

The usual way for this is to call an LLM model served online. We use
{chattr} package for that, as it also includes a local model usage
capability.

We query 16 generation of the model with a temperature of 0.5.

``` r
library(chattr)
response <- ch_submit_job(
  prompt = prompt, 
  defaults = chattr_defaults(model_arguments = list("temperature" = 0.5))
  )
```

## Step 3 : Decode the output

This consists in decoding the output and parsing it to a majority-vote
engine :

``` r
r_reference_card_df <- majority_vote(decode_json_result(response))
```
