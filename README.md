
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LMDX

<!-- badges: start -->
<!-- badges: end -->

A R implementation of LMDX ([Perot et
al. 2023](https://arxiv.org/pdf/2309.10952.pdf)).  
You provides a pdf page (or pdf file) in, and a decoding schema (json),
and you get all entities extracted from the pdf.

## Installation

You can install the development version of LMDX and its prerequisites
from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("mlverse/chattr")
pak::pak("cregouby/LMDX")
```

## Example

We want here to extract the [**R short reference card
pdf**](https://cran.r-project.org/doc/contrib/Short-refcard.pdf) file
content, and turn it into a data.frame:

<figure>
<img src="inst/extdata/Short-refcard_1.jpg"
alt="R reference card page 1 screenshot" />
<figcaption aria-hidden="true">R reference card page 1
screenshot</figcaption>
</figure>

It is a challenge as it is composed of 3 tight columns and packed
between code and highly summarized sentences.

## Step 1 : Design your taxonomy

The taxonomy here is a json representation of the entities to extract
from the document. Depending on the LLM model capacity, taxonomy can be
hierarchical like in the following example:

Here we can see that the document is structured in paragraphs like
*Getting Help*, then *Input and output*, and so on. This is the first
layer of the hierarchy, and each paragraph has a title and a
description.  
Then for each paragraph, there is multiple blocks that are made of an R
command, description and maybe an example.

So this is what the taxonomy looks like according to this.

``` r
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
```

## Step 2 : Forge the LLM prompt

**prompt** is made with the assembly of the document text with layout
information and the taxonomy.

``` r
library(LMDX)
document <- system.file("extdata", "Short-refcard_1.pdf", package = "LMDX")
prompt <- lmdx_prompt(document, taxonomy, segment = "line")
```

Let’s have a look at the prompt result :

``` r
prompt[[1]] |> stringr::str_trunc(500)
#> <Document>
#> R Reference Card 132|63
#> by Tom Short, EPRI PEAC, tshort@epri-peac.com 2004-11-07 88|87
#> Granted to the public domain. See www.Rpad.org for the source and latest 141|97
#> version. Includes material from R for Beginners by Emmanuel Paradis (with 141|106
#> permission). 37|116
#> Getting help 53|153
#> Most R functions have online documentation. 73|165
#> help(topic) documentation on topic 101|174
#> ?topic id. 42|184
#> help.search("topic") search the help system 132|193
#> apropos("topic") the names of all...

prompt[[1]] |> stringr::str_trunc(500, side = "left")
#> ...s 661|540
#> = n!/[(n − k)!k!] 576|549
#> na.omit(x) suppresses the observations with missing data (NA) (sup- 672|559
#> presses the corresponding line if x is a matrix or a data frame) 663|569
#> na.fail(x) returns an error message if x contains at least one NA 658|578
#> </Document><Task>
#> From the document, extract the text values and tags of the following entities:
#> {"title":"","paragraph_item":[{"title":"","description":[],"line_item":[{"command":"","description":"","example":[]}]}]}
#> </Task>
#> <Extraction>
```

`prompt` is a list textual prompts conform to the original paper taht
what we want the LLM model to process.

## Step 3 : Query the model

The usual way for this is to call an LLM model served online. We use
{chattr} package for that, as it also includes a local model usage
capability.

We query 16 generation of the model with a **temperature of 0.5**.

``` r
library(chattr)
response <- ch_submit_job(
  prompt = prompt, 
  defaults = chattr_defaults(model_arguments = list("temperature" = 0.5))
  )
```

This is not run here, paper report good result with the PaLMv2 model but
choose your own model and report the result !

## Step 4 : Decode the output

This consists in decoding the output and parsing it to a majority-vote
engine :

``` r
# response
r_reference_card_df <- majority_vote(decode_json_result(response))
```
