### CONCURRENT TASK LIMIT
- **Maximum 5 background tasks running simultaneously**
- When launching multiple agents (e.g., for mass audits), batch them in groups of 5
- Wait for the current batch to complete before launching the next batch

---

# Claude Code Instructions

### GIT COMMIT POLICY
- Commits are allowed
- NO Claude Code attribution, NO Co-Authored-By trailers, NO emojis
- Write normal commit messages as if a human wrote them

---

## Local Testing Before PRs (REQUIRED)

**PRs will not be merged until CI passes.** Run these checks locally BEFORE opening a PR:

### CI Checks That Must Pass

| Check | Local Command | What It Tests |
|-------|---------------|---------------|
| R-CMD-check | `devtools::check()` | Package builds, tests pass, no errors/warnings |
| Python tests | `pytest tests/test_pywaschooldata.py -v` | Python wrapper works correctly |
| pkgdown | `pkgdown::build_site()` | Documentation and vignettes render |

### Quick Commands

```r
# R package check (required)
devtools::check()

# Python tests (required)
system("pip install -e ./pywaschooldata && pytest tests/test_pywaschooldata.py -v")

# pkgdown build (required)
pkgdown::build_site()
```

### Pre-PR Checklist

Before opening a PR, verify:
- [ ] `devtools::check()` — 0 errors, 0 warnings
- [ ] `pytest tests/test_pywaschooldata.py` — all tests pass
- [ ] `pkgdown::build_site()` — builds without errors
- [ ] Vignettes render (no `eval=FALSE` hacks)
---

## README Images from Vignettes (REQUIRED)

**NEVER use `man/figures/` or `generate_readme_figs.R` for README images.**

README images MUST come from pkgdown-generated vignette output so they auto-update on merge:

```markdown
![Chart name](https://almartin82.github.io/{package}/articles/{vignette}_files/figure-html/{chunk-name}-1.png)
```

**Why:** Vignette figures regenerate automatically when pkgdown builds. Manual `man/figures/` requires running a separate script and is easy to forget, causing stale/broken images.

---

## README and Vignette Code Matching (REQUIRED)

**CRITICAL RULE (as of 2026-01-08):** ALL code blocks in the README MUST match code in a vignette EXACTLY (1:1 correspondence).

### Why This Matters

The Idaho fix revealed critical bugs when README code didn't match vignettes:
- Wrong district names (lowercase vs ALL CAPS)
- Text claims that contradicted actual data  
- Missing data output in examples

### README Story Structure (REQUIRED)

Every story/section in the README MUST follow this structure:

1. **Claim**: A factual statement about the data
2. **Explication**: Brief explanation of why this matters
3. **Code**: R code that fetches and analyzes the data (MUST exist in a vignette)
4. **Code Output**: Data table/print statement showing actual values (REQUIRED)
5. **Visualization**: Chart from vignette (auto-generated from pkgdown)

### Enforcement

The `state-deploy` skill verifies this before deployment:
- Extracts all README code blocks
- Searches vignettes for EXACT matches
- Fails deployment if code not found in vignettes
- Randomly audits packages for claim accuracy

### What This Prevents

- ❌ Wrong district/entity names (case sensitivity, typos)
- ❌ Text claims that contradict data
- ❌ Broken code that fails silently
- ❌ Missing data output
- ✅ Verified, accurate, reproducible examples

### Example

```markdown
### 1. State enrollment grew 28% since 2002

State added 68,000 students from 2002 to 2026, bucking national trends.

```r
library(arschooldata)
library(dplyr)

enr <- fetch_enr_multi(2002:2026)

enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  filter(end_year %in% c(2002, 2026)) %>%
  mutate(change = n_students - lag(n_students),
         pct_change = round((n_students / lag(n_students) - 1) * 100, 1))
# Prints: 2002=XXX, 2026=YYY, change=ZZZ, pct=PP.P%
```

![Chart](https://almartin82.github.io/arschooldata/articles/...)
```
