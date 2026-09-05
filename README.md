# Creating Welcoming Spaces — Editorial Review Analysis

This repository documents an R/Shiny workflow developed to support qualitative and editorial review for *Creating Welcoming Spaces in Academic Libraries*.

The project treats an edited volume as a curated scholarly conversation rather than a stack of independent chapters. The application supports comparison of proposals, recurring themes, editorial overlap, and a structured rubric while keeping descriptive analysis separate from editorial judgment.

## What is public here

This repository contains shareable code, methodology, documentation, and project structure only.

**It does not contain the submitted proposal workbook, proposal text, author contact information, reviewer notes, reviewer decisions, or other confidential editorial records.** Those materials remain in the authorized editorial workspace.

## Analysis layers

The workflow deliberately keeps five layers distinct:

1. Author-supplied section and approach.
2. Recurring proposal language and project-developed analytic themes.
3. Reviewer-entered keywords, notes, and first-pass decisions.
4. Five-criterion editorial rubric.
5. Collection-level editorial judgment.

Theme counts and overlap are descriptive evidence. They do not automatically determine rubric scores or editorial decisions.

## Editorial rubric

Individual proposals are considered using five weighted criteria:

| Criterion | Weight | Core question |
|---|---:|---|
| Contribution | 35% | What will a reader understand, see differently, or think more deeply about? |
| Adaptability | 25% | Can readers in other contexts draw useful lessons and adapt them? |
| Grounding | 20% | Is the basis appropriate and credible? |
| Clarity | 10% | Can we see the chapter being proposed? |
| Lasting Relevance | 10% | What remains useful beyond the immediate project, technology, institution, or moment? |

Scores support comparison; they do not assemble the book.

## Collection-level questions

Editorial curation also asks whether a proposal strengthens the collection, substantially duplicates another contribution, fills a thematic gap, broadens representation or methodological balance, and offers something readers will return to.

Shared vocabulary is not the same thing as duplication. Overlap identifies places where editors should compare mechanisms, populations, evidence, perspectives, and transferable lessons.

## Project-developed thematic vocabulary

The Shiny workflow uses a project-developed analytic vocabulary aligned with concerns in academic librarianship, including belonging, well-being, accessibility, inclusion/equity, neurodiversity and sensory experience, learning/study spaces, space design, assessment/evidence, user experience, campus partnerships, community engagement, programming, collections engagement, digital/virtual space, emerging technology, intentional practice, adaptability/transferability, and resource-conscious practice.

This vocabulary is **not claimed to be an official ACRL subject-heading system**. It is an analytic codebook developed for this editorial project.

## Data governance

The working editorial workbook is intentionally excluded from version control. `.gitignore` blocks spreadsheet, CSV, ZIP, local R state, environment/credential files, deployment metadata, and the contents of the local `data/` directory except its documentation file.

A local authorized copy of the application can read the current editorial workbook from `data/`. The workbook remains the editorial record; the analytic application is a review and comparison tool.

## Reproducibility and future use

The public repository is intended to preserve the method without publishing contributor submissions. Future examples should use de-identified, synthetic, or otherwise explicitly shareable data.

Any future research use of the submission corpus should be considered separately from its editorial use, including appropriate contributor-consent and institutional ethics/IRB considerations where applicable.

## Editorial principle

> A strong edited volume is a curated scholarly conversation, not a stack of good chapters.
