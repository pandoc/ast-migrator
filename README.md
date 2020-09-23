# ast-migrator

[![GitHub CI][CI badge]](https://github.com/tarleb/ast-migrator/actions)
[![Build status][Travis badge]](https://travis-ci.com/tarleb/ast-migrator)
[![Hackage][Hackage badge]](https://hackage.haskell.org/package/ast-migrator)
[![Stackage Lts][Stackage Lts badge]](http://stackage.org/lts/package/ast-migrator)
[![Stackage Nightly][Stackage Nightly badge]](http://stackage.org/nightly/package/ast-migrator)
[![MIT license][License badge]](LICENSE)

[CI badge]: https://github.com/tarleb/ast-migrator/workflows/CI/badge.svg
[Travis badge]: https://img.shields.io/travis/tarleb/ast-migrator.svg?logo=travis
[Hackage badge]: https://img.shields.io/hackage/v/ast-migrator.svg?logo=haskell
[Stackage Lts badge]: http://stackage.org/package/ast-migrator/badge/lts
[Stackage Nightly badge]: http://stackage.org/package/ast-migrator/badge/nightly
[License badge]: https://img.shields.io/badge/license-MIT-blue.svg

Migrate documents from one pandoc AST schema to another.

## Usage

The `ast-migrator` binary can be used as a compatibility helper for
pandoc filters.

The following example assumes that `my-filter` expects the older
pandoc API version 1.20, while pandoc has been updated and uses API
version 1.22 internally. Without touching the original filter, we
can no longer use the `--filter` options in this situation.

    pandoc --filter=my-filter ...

Instead, pandoc's JSON must explicitly be converted and be passed to
the filter:

    pandoc --to=json ... \
      | ast-migrator --to=1.20 \
      | my-filter \
      | ast-migrator --from=1.20 \
      | pandoc ...

This will convert the JSON representation into a format palatable by
the filter, and then later back into the format expected by pandoc.

For better handling the tool can be added to the filter itself; this
makes the explicit piping unnecessary.
