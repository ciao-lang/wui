# WUI: Web-based User Interface components for Ciao

Components for creating Web-based (HTML and JavaScript) user interfaces
using Ciao active modules.

Used from `ciao-serve` command, predicates from active modules can
answer HTTP requests for HTML documents or AJAX requests (for
JavaScript interaction). We include a simple JavaScript driver for
interaction with active modules.

Additionally this bundle contains support libraries for building
*menus*, *playgrounds*, etc. required by other Ciao components.

## Build
    
This code depends on some third-party tools and libraries. You can use
this script to install them:
```	
$ ciao custom_run . fetch_externals
```
Then you can build the bundle with:
```	
$ ciao build
$ ciao custom_run . dist
```

# TODO:

 - Document and integrate better the generic JS framework for remote
   procedure calls (using active modules and a HTTP/CGI to JSON
   gateway program).

 - Allow client-side Prolog processes using `ciaojs` (bundle)

 - `FormData()` is very limited on Safari. There is no simple
   workaroud, except creating a single form for upload and obtaining
   flag values without the form.

 - This code contains a generic JS framework for creating user
   interfaces (based on `library(menu)`, and external components like
   `CodeMirror`).


