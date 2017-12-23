# mailgen

A work-in-progress email generator for use with the digest (and possibly other
things). Totally and utterly incompatible with the old one,
[digest-emails](https://github.com/icdocsoc/digest-emails).

The original idea was to write it in Haskell, but Haskell lacks many of the
libraries that make it convenient to work with HTML, so now this is a NodeJS
project. I've tried to modularise it a bit but there's still more to improve.
It's a WIP :)

Emails are now composed in a custom XML format, which expresses the structure of
the email. This way I can shortcut many of the common digest tasks, like copying
information from the facebook events for the digest, as the new generator is
able to fetch this information. The aim is also to make it fairly simple to add
new components. Components have their own tags. Components aren't currently
self-contained, though this is an aim for the future when I restructure the
project. Right now I really just want to have something that works.
