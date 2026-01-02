# TODO

## pkgdown Build Issues

### Network Timeout Errors (2026-01-01)

The pkgdown build is failing due to network timeout errors when checking
CRAN/Bioconductor package links.

**Error:**

    Error in `httr2::req_perform(req)`:
    ! Failed to perform HTTP request.
    Caused by error in `curl::curl_fetch_memory()`:
    ! Timeout was reached [cloud.r-project.org]:
    Connection timed out after 10001 milliseconds

**Root Cause:** - pkgdown’s `data_home_sidebar_links()` function checks
CRAN and Bioconductor for package availability - The httr2 timeout is
hardcoded to 10 seconds - Network connectivity to cloud.r-project.org
and bioconductor.org is slow/unreliable

**Workarounds to try:** 1. Retry when network conditions improve 2. The
GitHub Actions workflow may succeed since it runs from GitHub’s
infrastructure 3. Consider adding a custom sidebar configuration to skip
the CRAN link check (if pkgdown supports this)

**Note:** This is an infrastructure/network issue, not a code problem
with the package or vignettes.
