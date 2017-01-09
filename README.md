# Zstandard bindings for Haskell

This library provides Haskell bindings to the
[Zstandard compression library](http://facebook.github.io/zstd/).

The library is structured to provide several layers of abstraction.

* For the simplest use cases, the top-level
  [`Zstd`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd.html)
  module is the best place to get started.

* If you need to stream a large amount of data with a constant memory
  footprint, use the
  [`Zstd.Streaming`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Streaming.html)
  module. This can also be used as a building block for adapting to
  streaming libraries such as `pipes` and `conduit`.  (If you need to
  use lazy bytestrings instead, see the
  [`Zstd.Lazy`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Lazy.html)
  module.  This is built using the abstractions from the
  `Zstd.Streaming` module.)

* When your usage is dominated by lots of small messages (presumably
  using pre-computed compression dictionaries), use the
  [`Zstd.Efficient`](http://hackage.haskell.org/package/zstd/docs/Codec-Compression-Zstd-Efficient.html)
  module to amortize the cost of allocating and initializing context
  and dictionary values.

## Join in



## API documentation

The APIs should be easy to understand and work with, and you can find
[documentation on Hackage](http://hackage.haskell.org/package/zstd).
