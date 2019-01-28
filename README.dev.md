Welcome to ppx_import, and thanks for willing to contribute to its
development.

## How to release a new version of ppx_import

**Quick release instructions:** Run `git tag -a` + `dune-release`.

The preferred workflow to release `ppx_import` is to use
`dune-release`.

The first (and most important) step is to tag the release and push it
to the main repository.

We recommend you do this manually. As `dune-release` uses `git
describe` to gather versioning information, your tag must be
annotated. Using `git tag -a` or `git tag -s` will do the job. Please
add the version changes to the tag annotation message.

You can also use `dune-release tag`, which will try to infer the
tag information from `CHANGES.md`, however the current heuristics
seem too fragile and the changes list may not be properly updated.

Once the tag is in place, calling `dune-release` will build, lint, run
the tests, create the opam package, upload the archives and
docs to the release page, and submit a pull request to the OPAM
repository.

Under the hood, `dune-release` executes the following 4 commands:

```
dune-release distrib       # Create the distribution archive
dune-release publish       # Publish it on the WWW with its documentation
dune-release opam pkg      # Create an opam package
dune-release opam submit   # Submit it to OCaml's opam repository
```

It is often useful to run the commands separately as to have better
control of the release process.

Note that you will need the proper permissions for the `publish` step,
including setting a Github access token, see `dune-release help
files` for more information.

