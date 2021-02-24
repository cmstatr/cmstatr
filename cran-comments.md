This re-submission fixes some minor bugs and adds some new functionality.

## Test environments
- win-builder (devel, release, oldrelease)
- local Ubuntu 20.04, R 4.0.4
- GitHub Action runners:
  - Windows, R 4.0.4
  - MacOS, R 4.0.4
  - Ubuntu 16.04, R 4.0.4
  - Ubuntu 16.04, R 3.6.3
  - MacOS, R 4.0.4 (with _R_CHECK_DEPENDS_ONLY_ set)

## R CMD check results
There were no ERRORs or WARNINGs.

One NOTE (only) on Win-builder oldrelease, reproduced below.
This NOTE relates to the TLS certificate for a web page linked from
a vignette. On my Ubuntu 20.04 machine, Firefox trusts this certificate,
but curl does not. On my Windows 10 machine, both Firebox and curl
trust this certificate. So, I guess this has something to do with
which root CAs are trusted by the system and/or browser. For some reason,
that Win-builder machine responsible for oldrelease is not trusting
the TLS certificate. The other two Win-builder machines do trust this
certificate. I don't believe that there is much I can do to correct this.

Found the following (possibly) invalid URLs:
  URL: https://www.cmh17.org/
    From: inst/doc/cmstatr_Tutorial.html
    Status: Error
    Message: libcurl error code 35:
      	error:1407742E:SSL routines:SSL23_GET_SERVER_HELLO:tlsv1 alert protocol version

## Downstream dependencies
There are no downstream dependencies.
