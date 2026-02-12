# Contributing

## Build

To check the build with different versions of dependencies we use local dev
dune-workspace files in conjunction with
[dune package management](https://dune.readthedocs.io/en/stable/tutorials/dune-package-management/index.html).

The following environments are checked with dune-workspace-%{ENV} for ENV in:

- 5.4
- 5.3
- 5.2
- 4.14
- v0.14
- v0.13

For example, to check the build with the `5.3` env, run:

```bash
$ dune build --workspace=dune-workspace-5.3
```

If you don't have a default dune display setting you may want to provide the
additional flag `--display=short` for additional incremental output.

## Packages Availability

There are some restrictions. For example, `feather_tests` cannot be built with
the `v0.13` environment. To restrict the set of packages, supply the additional
`--only-packages`, for example:

```bash
$ dune build --workspace=dune-workspace-v0.13 \
    --only-packages="feather,feather_async" \
    --display=short
```
