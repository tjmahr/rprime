# Filter levels in or out of a `FrameList` based on attribute values

Filter levels in or out of a `FrameList` based on attribute values

## Usage

``` r
filter_in(frame_list, key, values)

filter_out(frame_list, key, values)
```

## Arguments

- frame_list:

  a list of
  [`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
  objects

- key:

  the name of the attribute to filter in or out

- values:

  the whitelisted or blacklisted values of the attribute

## Value

for `filter_in()`, only log-frames where `key` is one of the `values`
are kept. For `filter_out()`, log-frames where `key` is one of the
`values` are omitted.
