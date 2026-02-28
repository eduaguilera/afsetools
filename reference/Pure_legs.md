# Pure Legume Classification

Classifies BNF parameter categories into grain legumes vs. fodder
legumes. Used for categorising BNF contributions in summaries and
diagnostics (e.g.,
[`summarize_bnf`](https://eduaguilera.github.io/afsetools/reference/summarize_bnf.md)).

## Format

A data frame with 10 rows and 2 columns:

- Name_BNF:

  BNF parameter category (matches `BNF` table).

- Cat_leg:

  Legume category: `"Grain"` for pulse and oilseed legumes,
  `"Fodder_pure"` for forage legumes.

## Source

Internal classification.

## Details

Only pure legume categories are included (Leguminous_share = 1 in BNF
table). Mixed systems (swards, meadows, weeds) and non-symbiotic crops
(rice, sugarcane) are excluded.
