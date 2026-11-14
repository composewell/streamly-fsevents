# Issues

## Memory after the start of a watch with the listing through an fd

Listing a directory through its fd, with `fdopendir`, increased the live
heap after the start of a watch on 28277 directories from 5.3 MB to
5.9 MB, about 21 bytes per directory. The names are read by
`readDirStreamEither` in both versions, the cause is not known.
