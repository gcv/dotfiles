Disk Usage is a file system analyzer: it offers a tabulated view of file
listings sorted by size.  Directory sizes are computed recursively.  The results
are cached for speed.

Run `disk-usage' or `disk-usage-here' to display a listing.
See `describe-mode' for additional bindings, such as
`disk-usage-dired-at-point' to open a `dired' buffer for the current
directory.

Instead of displaying only the current folder, ~disk-usage~ can also display
files in all subfolders recursively with `disk-usage-toggle-recursive'.

Marked files can be trashed with `disk-usage-delete-marked-files'.  When
called with a prefix argument, files are deleted permanently.

Run `disk-usage-by-types' to display statistics of disk usage by file
extensions.

With a prefix argument, cache is updated when reverting the buffer.

You can customize options in the 'disk-usage group.
